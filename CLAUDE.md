# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**pep725** is an R package for analyzing phenological data from the PEP725 Pan-European Phenology Database. It provides tools for calculating climatological baselines, detecting anomalies, analyzing spatial gradients, and visualizing phenology-climate relationships.

## Development Commands

```bash
# Load package for development (use instead of library())
Rscript -e "devtools::load_all()"

# Regenerate documentation (NAMESPACE and .Rd files)
Rscript -e "devtools::document()"

# Run R CMD check
R CMD check . --no-manual --no-vignettes

# Quick package check
Rscript -e "devtools::check()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"

# Test loading and basic functionality
Rscript -e "devtools::load_all(); data(pep_seed); print(pep_seed)"
```

## Architecture

### Core Data Structure

The package uses a custom S3 class `pep` that extends `data.table`:

- **Required columns**: `s_id`, `lon`, `lat`, `genus`, `species`, `phase_id`, `year`, `day`
- Constructor: `new_pep(x, validate = TRUE)`
- Subsetting `[.pep` preserves class when structure is valid
- S3 methods: `print.pep`, `summary.pep`, `plot.pep`

### Key S3 Classes

Each analysis function returns a classed object with print/summary/plot methods:

| Class | Function | Purpose |
|-------|----------|---------|
| `pep` | `new_pep()` | Core phenological data |
| `pheno_normals` | `pheno_normals()` | Climate baselines |
| `pheno_anomaly` | `pheno_anomaly()` | Deviation detection |
| `pep_quality` | `pep_quality()` | Quality grading (A/B/C/D) |
| `pheno_gradient` | `pheno_gradient()` | Elevation/latitude analysis |
| `pheno_synchrony` | `pheno_synchrony()` | Spatial coherence |
| `pep_completeness` | `pep_completeness()` | Data coverage assessment |
| `phase_check` | `check_phases()` | Phase validation |
| `pheno_combined` | `pheno_combine()` | Combined time series from multi-station data |
| `pheno_turning` | `pheno_trend_turning()` | Trend turning point detection (Mann-Kendall) |
| `pep_connectivity` | `check_connectivity()` | Station-year connectivity check |
| `pep_outliers` | `flag_outliers()` | Outlier detection (30-day rule, MAD, IQR, z-score) |
| `second_events` | `detect_second_events()` | Second flowering/repeated event detection |
| `pls_phenology` | `pls_phenology()` | PLS regression for phenology-temperature relationships |

### Data Flow

1. **Import**: `pep_import()` reads PEP725 CSV files → `pep` object
2. **Download**: `pep_download()` fetches cached synthetic data → `pep` object
3. **Filter**: Use data.table syntax `pep[species == "..."]` (class preserved)
4. **Analyze**: `pheno_normals()`, `pheno_anomaly()`, etc. → result objects
5. **Visualize**: `plot()` methods or `leaflet_pep()` for interactive maps

### File Organization

- `R/pep_class.R` - Core `pep` class, constructor, validation, `bbch_description()`
- `R/pep_import.R` - CSV import from PEP725 files
- `R/pep_download.R` - Synthetic data download with caching
- `R/simulate_pep.R` - Generate synthetic data from real observations
- `R/pheno_*.R` - Analysis functions (normals, anomaly, gradient, synchrony)
- `R/pep_quality.R` - Quality assessment
- `R/pep_completeness.R` - Species/phase coverage assessment
- `R/check_phases.R` - Phase validation utilities
- `R/pheno_combine.R` - Combined time series estimation (robust/mixed/OLS)
- `R/pheno_trend_turning.R` - Sequential Mann-Kendall trend analysis
- `R/calc_daylength.R` - Photoperiod calculations
- `R/calc_thermal_units.R` - Growing Degree Days (GDD) calculation
- `R/flag_outliers.R` - Outlier detection using 30-day rule or statistical methods
- `R/detect_second_events.R` - Second flowering/repeated event detection
- `R/pls_phenology.R` - Robust PLS analysis for phenology-temperature relationships
- `R/leaflet_pep.R` - Interactive Shiny map gadget
- `R/zzz.R` - Package startup, loads auxiliary datasets

### Dependencies

Uses `data.table` for performance and `robustbase::lmrob` for robust regression. The `pheno_combine()` function uses `quantreg` (LAD regression) and `nlme` (mixed models). The `pls_phenology()` function uses the `pls` package for PLS regression with iteratively reweighted robust estimation. Visualization via `ggplot2` and `leaflet`. Documentation via roxygen2 with markdown mode.

### BBCH Phases

Phenological phases use BBCH codes (e.g., 60=heading/flowering, 65=full flowering, 100=harvest). The `bbch_description()` function provides lookup.

## Documentation Guidelines

### Examples in Roxygen

All examples that use `pep` data must:
1. Be wrapped in `\donttest{}` (preferred for CRAN) since `pep_download()` requires internet
2. Use `\dontrun{}` only for examples that truly cannot run (e.g., require local file paths)
3. Call `pep <- pep_download()` BEFORE using the `pep` variable
4. Use `data(pep_seed)` for small examples that can run without internet

Example structure:
```r
#' @examples
#' \donttest{
#' pep <- pep_download()
#' result <- some_function(pep, ...)
#' }
```

### Cross-References

- Use `\link{pep_download}` instead of `\link{pep}` (pep is not a documented object)
- Dataset documentation requires `@name`, `@docType data`, `@usage data(name)` tags

### Dataset Documentation (R/dataSets.R)

Each dataset needs these roxygen tags:
```r
#' @docType data
#' @keywords datasets
#' @name datasetname
#' @usage data(datasetname)
"datasetname"
```

## Package Data

| Dataset | Description | Location |
|---------|-------------|----------|
| `pep_seed` | Small subset for testing (1,319 rows) | `data/pep_seed.rda` |
| `giss` | NASA GISS temperature anomalies (1880-2024) | `data/giss.rda` |
| `meteoSwiss` | MeteoSwiss phenology observations | `data/meteoSwiss.rda` |

Note: The full `pep` synthetic dataset (~64MB) is downloaded via `pep_download()` and cached locally.

## Current Status

### License
GPL-3 (specified in DESCRIPTION with full text in LICENSE file)

### Known Issues
- Non-standard files at top level: `CLAUDE.md`, `pepperPaper/`

### Excluded Content
- Hail-related functions moved to `inst/scripts/` (excluded via `.Rbuildignore`)

### Key Functions by Category

**Data Access:**
- `pep_download()` - Download synthetic PEP data
- `pep_import()` - Import real PEP725 CSV files
- `simulate_pep()` - Generate synthetic data

**Analysis:**
- `pheno_normals()` - Climatological baselines
- `pheno_anomaly()` - Deviation detection
- `pheno_gradient()` - Elevation/latitude gradients
- `pheno_synchrony()` - Spatial coherence
- `pheno_combine()` - Combined time series
- `pheno_trend_turning()` - Trend turning points
- `pls_phenology()` - Robust PLS analysis

**Quality/Validation:**
- `pep_quality()` - Quality grading
- `pep_completeness()` - Coverage assessment
- `check_phases()` - Phase validation
- `flag_outliers()` - Outlier detection
- `plot_outliers()` - Visualize outliers for inspection
- `detect_second_events()` - Detect second flowering/repeated events
- `plot.second_events()` - Visualize second events (supports `scale = "relative"` for proportional display)

**Visualization:**
- `pheno_plot()` - General phenology plots
- `pheno_plot_hh()` - Heading/harvest specific plots
- `plot_phenology_trends()` - Trend visualization
- `leaflet_pep()` - Interactive maps
- `map_pep()` - Static maps (use `background = "none"` for no API key required)
