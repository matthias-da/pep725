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
| `phase_check` | `pep_check_phases()` | Phase validation |
| `pheno_combined` | `pheno_combine()` | Combined time series from multi-station data |
| `pheno_turning` | `pheno_trend_turning()` | Trend turning point detection (Mann-Kendall) |
| `pep_connectivity` | `pep_check_connectivity()` | Station-year connectivity check |
| `pep_outliers` | `pep_flag_outliers()` | Outlier detection (30-day rule, MAD, IQR, z-score) |
| `second_events` | `pep_second_events()` | Second flowering/repeated event detection |
| `pheno_pls` | `pheno_pls()` | PLS regression for phenology-temperature relationships |

### Data Flow

1. **Import**: `pep_import()` reads PEP725 CSV files → `pep` object
2. **Download**: `pep_download()` fetches cached synthetic data → `pep` object
3. **Filter**: Use data.table syntax `pep[species == "..."]` (class preserved)
4. **Analyze**: `pheno_normals()`, `pheno_anomaly()`, etc. → result objects
5. **Visualize**: `plot()` methods or `pheno_leaflet()` for interactive maps

### File Organization

- `R/pep_class.R` - Core `pep` class, constructor, validation, `bbch_description()`
- `R/pep_import.R` - CSV import from PEP725 files
- `R/pep_download.R` - Synthetic data download with caching
- `R/pep_simulate.R` - Generate synthetic data from real observations
- `R/pheno_*.R` - Analysis functions (normals, anomaly, gradient, synchrony)
- `R/pep_quality.R` - Quality assessment
- `R/pep_completeness.R` - Species/phase coverage assessment
- `R/pep_check_phases.R` - Phase validation utilities
- `R/pheno_combine.R` - Combined time series estimation (robust/mixed/OLS)
- `R/pheno_trend_turning.R` - Sequential Mann-Kendall trend analysis
- `R/calc_daylength.R` - Photoperiod calculations
- `R/calc_thermal_units.R` - Growing Degree Days (GDD) calculation
- `R/pep_flag_outliers.R` - Outlier detection using 30-day rule or statistical methods
- `R/pep_second_events.R` - Second flowering/repeated event detection
- `R/pheno_pls.R` - Robust PLS analysis for phenology-temperature relationships
- `R/pheno_leaflet.R` - Interactive Shiny map gadget
- `R/pheno_map.R` - Static Google Maps visualization
- `R/pheno_regional.R` - Regional phenology compilation
- `R/pheno_regional_hh.R` - Heading/harvest regional analysis
- `R/pep_plot_outliers.R` - Outlier visualization for inspection
- `R/zzz.R` - Package startup, loads auxiliary datasets

### Dependencies

Uses `data.table` for performance and `robustbase::lmrob` for robust regression. The `pheno_combine()` function uses `quantreg` (LAD regression) and `nlme` (mixed models). The `pheno_pls()` function uses the `pls` package for PLS regression with iteratively reweighted robust estimation. Visualization via `ggplot2` and `leaflet`. Documentation via roxygen2 with markdown mode.

**Imports vs Suggests policy:**
- `sp` and `ggmap` are in **Suggests** (not Imports) — they need runtime `requireNamespace()` guards
- `leaflet.extras` is **NOT in DESCRIPTION at all** — archived from CRAN (2026-02-19), so declaring it would trigger CRAN rejection. Used only via runtime `requireNamespace()` + `getExportedValue()` in `pheno_leaflet.R`. Install message points users to `https://trafficonese.r-universe.dev`.
- **Never use blanket `@import` for dplyr/tidyr** — use specific `@importFrom` declarations
- The `%>%` pipe must be explicitly imported: `@importFrom dplyr %>%`
- When a package is not in DESCRIPTION, use `getExportedValue("pkg", "fun")()` instead of `pkg::fun()` to avoid R CMD check warnings

### BBCH Phases

Phenological phases use BBCH codes (e.g., 60=heading/flowering, 65=full flowering, 100=harvest). The `bbch_description()` function provides lookup.

### Naming Convention

Functions follow a two-prefix strategy:
- **`pep_*`** — Data infrastructure: import, download, simulate, quality checks, validation, subsetting helpers
- **`pheno_*`** — Scientific analysis and visualization: normals, anomaly, gradient, synchrony, trends, maps, plots

## Documentation Guidelines

### Examples in Roxygen

All examples that use `pep` data must:
1. Be wrapped in `\donttest{}` (preferred for CRAN) since `pep_download()` requires internet
2. Use `\dontrun{}` for interactive or very slow examples (see below)
3. Call `pep <- pep_download()` BEFORE using the `pep` variable
4. Use `data(pep_seed)` for small examples that can run without internet
5. **Use geographic subsets** for faster computation (critical for R CMD check --run-donttest)

### When to Use `\dontrun{}` vs `\donttest{}`

Use **`\dontrun{}`** ONLY for examples that truly cannot be executed:
- Interactive functions requiring user input: `pheno_leaflet()`, `pep725_demo()`
- Functions requiring missing software/API keys: `pheno_map()` with Google Maps
- Functions requiring local files: `pep_import()` (needs PEP725 CSV files)
- Functions that fail with synthetic data: `pheno_plot()` (depends on `pheno_regional()` which is hardcoded for real data near Changins)

Use **`\donttest{}`** for all other examples that are executable but slow (> 5 sec).
This includes examples needing `pep_download()`, slow analyses, etc.

**CRAN policy**: Never use commented-out code lines in examples. All example code
must be executable. Use `\donttest{}` for slow examples, not comments.

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

Note: The full `pep` synthetic dataset (~64MB) is downloaded via `pep_download()` and cached locally.

## Current Status

### JOSS Submission
- **Paper location**: `paper/` directory in this repo (paper.md, paper.bib, figures/, syntheticPEPFigures/)
- **Paper excluded from R build**: via `^paper$` in `.Rbuildignore`
- **TODO tracker**: `TODO_JOSS_submission.md` - comprehensive checklist of remaining tasks
- **Submission timeline**: July 2026 (requires 6 months public development history)
- **Completed**:
  - testthat tests (95 tests passing)
  - GitHub Actions: R-CMD-check (3 platforms) + JOSS Draft PDF — all passing
  - CODE_OF_CONDUCT.md and CONTRIBUTING.md created
  - Paper converted to JOSS format (paper.md), all formatting issues fixed
  - Paper moved into main repo for JOSS submission requirements
  - Figures: workflow_diagram.png, quality_overview.png, gradient_example.png, anomaly_timeline.png
  - Synthetic PEP comparison plots in syntheticPEPFigures/
  - Example usage section with quality-first workflow
  - All BibTeX entries cleaned and standardized
- **Key pending items**:
  - Fix `opedal2024advancing` reference (fabricated — needs replacement)
  - Co-author review (Barbara Templ)
  - Wait until July 2026 (6-month public development history requirement)

### GitHub Actions
- `.github/workflows/R-CMD-check.yaml` - R CMD check on macOS/Ubuntu with R release/devel
  - `setup-r-dependencies@v2` does NOT support `extra-repositories` input — if needed, inject repos via `.Rprofile` step
- `.github/workflows/draft-pdf.yaml` - JOSS paper PDF compilation

### License
GPL-3 (specified in DESCRIPTION, no bundled LICENSE file)

### CRAN Submission
- **Status**: Resubmitted after initial CRAN feedback (Feb 2026)
- **cran-comments.md**: Explains `leaflet.extras` removal from Suggests
- **R CMD check**: 0 errors, 0 warnings, 0 notes (local and CI)
- **DESCRIPTION Title**: Must NOT include the package name (CRAN policy)
- **NEWS.md version**: Must match DESCRIPTION Version field
- `.Rbuildignore` patterns must be anchored (e.g., `^Rplots\.pdf$` not `Rplots.pdf`)
- **Archived CRAN packages**: Do NOT list in Suggests — use runtime-only `requireNamespace()` + `getExportedValue()` to avoid "not in mainstream repositories" rejection

### R CMD Check NOTEs
- All fixed (`.Rbuildignore`, `mgcv::gam()`, `globalVariables()` declarations)
- Data.table column references need `globalVariables()` entries (e.g., `complete` in `pep_check_phases.R`)

### Known Issues
- Non-standard files at top level: `CLAUDE.md`, `CONTRIBUTING.md`, `TODO_JOSS_submission.md`
- `leaflet.extras` archived from CRAN (2026-02-19) — not declared as dependency, runtime-only detection via `getExportedValue()`

### Community Files
- `CONTRIBUTING.md` - Contribution guidelines (bug reports, PRs, code style)
- `CODE_OF_CONDUCT.md` - Contributor Covenant

### Key Functions by Category

**Data Access:**
- `pep_download()` - Download synthetic PEP data
- `pep_import()` - Import real PEP725 CSV files
- `pep_simulate()` - Generate synthetic data
- `as.pep()` - Convert data.frame/data.table to pep object

**Analysis:**
- `pheno_normals()` - Climatological baselines
- `pheno_anomaly()` - Deviation detection
- `pheno_gradient()` - Elevation/latitude gradients
- `pheno_synchrony()` - Spatial coherence
- `pheno_combine()` - Combined time series
- `pheno_trend_turning()` - Trend turning points
- `pheno_pls()` - Robust PLS analysis

**Quality/Validation:**
- `pep_quality()` - Quality grading
- `pep_completeness()` - Coverage assessment
- `pep_coverage()` - Species/phase coverage summary
- `pep_check_phases()` - Phase validation
- `pep_check_connectivity()` - Station-year connectivity check
- `pep_flag_outliers()` - Outlier detection
- `pep_plot_outliers()` - Visualize outliers for inspection
- `pep_second_events()` - Detect second flowering/repeated events
- `plot.second_events()` - Visualize second events (supports `scale = "relative"` for proportional display)

**Visualization:**
- `pheno_plot()` - Phenology time series plots (uses `\dontrun{}`)
- `pheno_plot_hh()` - Heading/harvest time series plots (uses `\dontrun{}`)
- `pheno_plot_timeseries()` - Time series plots
- `pheno_leaflet()` - Interactive maps (uses `\dontrun{}` - interactive Shiny gadget)
- `pheno_map()` - Static maps (uses `\dontrun{}` - can require API key)

**Regional Analysis (all use `\dontrun{}`):**
- `pheno_regional()` - Regional phenology compilation
- `pheno_regional_hh()` - Heading/harvest regional analysis

**Demo/Interactive (use `\dontrun{}`):**
- `pep725_demo()` - Interactive package demonstration
