# pep725

<p align="center">
  <img src="presentations/pep725_logo_cpt.png" alt="pep725 logo" width="180"/>
</p>

[![R-CMD-check](https://github.com/matthias-da/pep725/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matthias-da/pep725/actions/workflows/R-CMD-check.yaml)

pep725: an R package for Pan-European Phenological Data Analysis

**pep725** is an R package for analyzing phenological data from the [PEP725 Pan-European Phenology Database](http://www.pep725.eu/) and beyond. It provides tools for data exploration, quality assessment, climatological baseline calculation, trend analysis and visualization of phenology-climate relationships.

## Citation

If you use this R package, please cite:
Templ and Templ (2026). pep725: An R package for Pan-European Phenological Data Analysis. *The Journal of Open Source Software*, under review. 
Until it gets published, please cite:
Templ et al. (2018). Pan European Phenological database (PEP725): a single point of access for European data. Int. J. Biometeorology, 62(6): 1109-1113. doi:10.1007/s00484-018-1512-8

## Features

**Data Access:**
- Import PEP725 data files with automatic cleaning and formatting
- Download pre-generated synthetic datasets for learning and testing
- Generate synthetic data from your own datasets for teaching and sharing

**Core Analysis:**
- `pheno_normals()` - Calculate climatological baselines (WMO-style reference periods)
- `pheno_anomaly()` - Detect deviations from normal phenology
- `pep_quality()` - Assess data quality and assign reliability grades
- `pheno_gradient()` - Quantify elevation and latitude gradients
- `pheno_synchrony()` - Measure spatial coherence of phenological events

**Quality & Validation:**
- `pep_completeness()` - Assess species/phase coverage across stations
- `pep_check_phases()` - Validate phenological phase sequences
- `pep_flag_outliers()` - Detect outliers (30-day rule, MAD, IQR, z-score)

**Advanced Analysis:**
- `pheno_combine()` - Combined time series from multi-station data (robust/mixed/OLS)
- `pheno_trend_turning()` - Sequential Mann-Kendall trend turning point detection
- `pheno_pls()` - PLS regression for temperature-sensitive phenological periods

**Visualization:**
- `pheno_plot_timeseries()` - DOY trend plots over time
- `plot_phenology_trends()` - Robust regression trend analysis
- `pheno_leaflet()` - Interactive maps for station exploration and selection
- `pheno_map()` - Static maps of station networks

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("matthias-da/pep725")
```

## Quick Start

```r
library(pep725)

# Download synthetic data (cached locally after first download)
pep <- pep_download()

# Explore the data
print(pep)
summary(pep)
pep_coverage(pep)

# Filter to a species
wheat <- pep[species == "Triticum aestivum"]

# Calculate phenological normals
normals <- pheno_normals(
  wheat,
  period = 1990:2015,
  by = c("country", "phase_id"),
  min_years = 10
)
print(normals)

# Detect anomalies
anomalies <- pheno_anomaly(
  wheat,
  baseline_period = 1990:2010,
  by = c("country", "phase_id")
)
summary(anomalies)

# Assess data quality
quality <- pep_quality(wheat, by = c("s_id", "phase_id"))
summary(quality)
```

## Data Options

| Option | Best For | Requires Internet |
|--------|----------|-------------------|
| `pep_download()` | Learning, tutorials, reproducible examples | First use only |
| `data(pep_seed)` | Quick offline tests, minimal examples | No |
| `pep_simulate()` | Creating shareable datasets from your own data | No |
| `pep_import()` | Research with actual PEP725 observations | For download |
| `as.pep()` | Working with your own phenological data | No |

**Why synthetic data?** The original PEP725 database requires registration and has usage restrictions. Synthetic data preserves statistical properties while being freely shareable for tutorials and reproducible workflows.

### Working with your own phenological data

You can use all analysis functions on any phenological dataset — it does not have to come from PEP725. Convert your data to a `pep` object with `as.pep()`:

```r
library(data.table)

my_data <- data.table(
  s_id     = c(1, 1, 2, 2),
  lon      = c(8.5, 8.5, 9.1, 9.1),
  lat      = c(47.4, 47.4, 46.9, 46.9),
  alt      = c(400, 400, 550, 550),
  genus    = "Prunus",
  species  = "Prunus avium",
  phase_id = 60,
  year     = c(2022, 2023, 2022, 2023),
  day      = c(105, 98, 112, 103)
)

my_pep <- as.pep(my_data)

# Now use any pep725 analysis function
normals <- pheno_normals(my_pep, period = 2022:2023, min_years = 2)
```

Required columns: `s_id`, `lon`, `lat`, `genus`, `species`, `phase_id`, `year`, `day`. Optional: `alt`, `country`, `subspecies`.

## Spatial Analysis

```r
# Elevation gradient analysis
gradient <- pheno_gradient(
  pep,
  variable = "alt",
  species = "Triticum aestivum",
  phase_id = 60,
  method = "robust"
)
print(gradient)
plot(gradient)

# Spatial synchrony
synchrony <- pheno_synchrony(
  pep,
  species = "Triticum aestivum",
  phase_id = 60,
  by = c("country", "year"),
  min_stations = 3
)
summary(synchrony)
```

## Interactive Mapping

```r
# Launch interactive map for station selection
selected <- pheno_leaflet(pep, label_col = "species")

# Use selected stations for analysis
pep_subset <- pep[s_id %in% selected$s_id]
```

## Built-in Datasets

| Dataset | Description |
|---------|-------------|
| `pep_seed` | Small seed dataset for quick tests (1,319 rows) |

## Vignettes

The package includes detailed vignettes:

- **Getting Started with pep725** - Data loading, exploration, and basic usage
- **Phenological Analysis** - Normals, anomalies, quality assessment, and visualization
- **Spatial Phenological Patterns** - Gradients, synchrony, and mapping
- **Data Quality Assessment** - Quality grading, outlier detection, and completeness

```r
vignette("getting-started", package = "pep725")
vignette("phenological-analysis", package = "pep725")
vignette("spatial-patterns", package = "pep725")
vignette("data-quality", package = "pep725")
```

## Requirements

- R >= 4.1
- Google Maps API key required for `pheno_map()` static maps:
  ```r
  ggmap::register_google(key = "your_api_key")
  ```

## License

GPL-3

## Authors

- Matthias Templ (maintainer)
- Barbara Templ

## Acknowledgements
Dr. Barbara Templ was supported by the Berner Nachwuchsförderung (BNF) program of the University of Bern for developing the **pep725** R package.

## Links

- [GitHub Repository](https://github.com/matthias-da/pep725)
- [Report Issues](https://github.com/matthias-da/pep725/issues)
- [PEP725 Database](http://www.pep725.eu/)
