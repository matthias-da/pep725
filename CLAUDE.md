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
2. Use `\dontrun{}` for interactive or very slow examples (see below)
3. Call `pep <- pep_download()` BEFORE using the `pep` variable
4. Use `data(pep_seed)` for small examples that can run without internet
5. **Use geographic subsets** for faster computation (critical for R CMD check --run-donttest)

### When to Use `\dontrun{}` vs `\donttest{}`

Use **`\dontrun{}`** for:
- Interactive functions: `leaflet_pep()`, `pep725_demo()`
- Functions requiring API keys: `map_pep()` with Google Maps
- Complex pipelines: `regional_box_ts()`, `regional_box_ts_heading_harvest()`, `pheno_plot()`, `pheno_plot_hh()`
- Simulation: `simulate_pep()` (slow on full dataset)

Use **`\donttest{}`** with subsets for all other examples.

### Example Structure with Subsetting

```r
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Use Swiss/Austrian subset for faster computation
#' pep_alpine <- pep[country %in% c("Switzerland", "Austria")]
#'
#' # Now run the analysis on the subset
#' result <- some_function(pep_alpine, ...)
#' }
```

### Synthetic Data Notes

**Country names** in synthetic data (NOT "Germany"):
- `"Germany-North"`, `"Germany-South"`, `"Switzerland"`, `"Austria"`

**Recommended species** for examples (use both for variety):
- `"Malus domestica"` (Apple) - 70k rows in Alpine subset, 1402 stations, 92 years, 9 phases
- `"Vitis vinifera"` (Grapevine) - 20k rows in Alpine subset, 830 stations, **187 years** (longest!), 13 phases

**Important**: Always verify subset has data before analysis:
```r
vine <- pep[species == "Vitis vinifera" & country %in% c("Switzerland", "Austria")]
stopifnot(nrow(vine) > 0)  # Or use: if (nrow(vine) == 0) stop("No data")
```

Avoid `"Triticum aestivum"` with Germany filter (use Germany-North/Germany-South)

**Available columns** (no `functional_group`):
- `s_id`, `lon`, `lat`, `alt`, `genus`, `species`, `subspecies`, `phase_id`, `year`, `day`, `country`

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
| `meteoSwiss` | MeteoSwiss phenology observations | `data/meteoSwiss.rda` |

Note: The full `pep` synthetic dataset (~64MB) is downloaded via `pep_download()` and cached locally.

## Companion Package: hail

The **hail** package contains climate sensitivity analysis tools that were previously in pep725:

| Component | Location | Usage |
|-----------|----------|-------|
| `giss` dataset | `hail::giss` | `data(giss, package = "hail")` |
| `plot_giss_smooth()` | hail | Climate-phenology smoothed plots |
| `plot_giss_sensitivity()` | hail | Robust sensitivity analysis |

**Workflow with hail:**
```r
library(pep725)
library(hail)

pep <- pep_download()
data(giss, package = "hail")

# Prepare data with pep725
out <- regional_box_ts(pep, giss, species_name = "Triticum aestivum", phase = 60)

# Time series plot (pep725)
pheno_plot(out)

# Climate sensitivity plots (hail)
plot_giss_smooth(out)
plot_giss_sensitivity(out)
```

## Current Status

### JOSS Submission
- **Paper location**: `pep725_JOSS-Paper/JOSS_packagePaper.Rmd`
- **TODO tracker**: `TODO_JOSS_submission.md` - comprehensive checklist of remaining tasks
- **Submission timeline**: July 2026 (requires 6 months public development history)
- **Key pending items**:
  - Add testthat tests
  - Push GitHub Actions workflows (needs PAT with `workflow` scope)
  - Create CODE_OF_CONDUCT.md
  - Fix `opedal2024advancing` reference (wrong DOI)
  - Co-author review (Barbara Templ)

### GitHub Actions (local only - need PAT workflow scope to push)
- `.github/workflows/R-CMD-check.yaml` - R CMD check on macOS/Ubuntu with R release/devel
- `.github/workflows/draft-pdf.yaml` - JOSS paper PDF compilation

### License
GPL-3 (specified in DESCRIPTION with full text in LICENSE file)

### R CMD Check NOTEs (to fix)
- Add `.github`, `CONTRIBUTING.md`, `TODO_JOSS_submission.md`, `pep725_JOSS-Paper`, `CLAUDE.md` to `.Rbuildignore`
- Fix `simulate_pep.R:52` - change `gam()` to `mgcv::gam()`
- Add global variable bindings for `grade_num`, `worst_grade_num` in `plot.pep_quality()`
- Add global variable binding for `..na_cols` in `print.pheno_anomaly()`

### Known Issues
- Non-standard files at top level: `CLAUDE.md`, `CONTRIBUTING.md`, `TODO_JOSS_submission.md`, `pep725_JOSS-Paper/`

### Community Files
- `CONTRIBUTING.md` - Contribution guidelines (bug reports, PRs, code style)
- `CODE_OF_CONDUCT.md` - Contributor Covenant (TODO: create)

### Companion Package
- Climate sensitivity analysis (GISS data, `plot_giss_smooth()`, `plot_giss_sensitivity()`) moved to **hail** package

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
- `pheno_plot()` - Phenology time series plots (uses `\dontrun{}`)
- `pheno_plot_hh()` - Heading/harvest time series plots (uses `\dontrun{}`)
- `pheno_plot_timeseries()` - Time series plots
- `leaflet_pep()` - Interactive maps (uses `\dontrun{}` - interactive Shiny gadget)
- For climate sensitivity plots, use **hail** package: `plot_giss_smooth()`, `plot_giss_sensitivity()`
- `map_pep()` - Static maps (uses `\dontrun{}` - can require API key)

**Regional Analysis (all use `\dontrun{}`):**
- `regional_box_ts()` - Regional phenology compilation
- `regional_box_ts_heading_harvest()` - Heading/harvest regional analysis

**Demo/Interactive (use `\dontrun{}`):**
- `pep725_demo()` - Interactive package demonstration
