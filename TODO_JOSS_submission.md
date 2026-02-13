# TODO: JOSS Submission Requirements

## Critical Blocker

- [ ] **Wait until July 2026** - JOSS requires 6 months public development history (first commit: 2026-01-19)

---

## High Priority

### 1. Add Automated Tests

- [x] Create testthat tests for core functions (95 tests, all passing)

**Files created:**
- [x] `tests/testthat.R` - test runner
- [x] `tests/testthat/test-pep_class.R` - test pep class constructor/validation (17 tests)
- [x] `tests/testthat/test-pheno_normals.R` - test normals calculation (10 tests)
- [x] `tests/testthat/test-pheno_anomaly.R` - test anomaly detection (10 tests)
- [x] `tests/testthat/test-pheno_gradient.R` - test gradient analysis (10 tests)
- [x] `tests/testthat/test-pep_quality.R` - test quality assessment (12 tests)

**DESCRIPTION updates:**
- [x] Add `testthat (>= 3.0.0)` to Suggests
- [x] Add `Config/testthat/edition: 3`

### 2. Add GitHub Actions CI Workflow

- [x] Create `.github/workflows/R-CMD-check.yaml` (created locally)
- [x] **Update GitHub PAT to include `workflow` scope** - Required to push workflow files
  - Go to GitHub → Settings → Developer settings → Personal access tokens
  - Edit token and add `workflow` scope
  - Then run: `git add .github/workflows/ && git commit -m "Add CI workflows" && git push`
  - Workflow files backed up at `/tmp/R-CMD-check.yaml.backup` and `/tmp/draft-pdf.yaml.backup`

```yaml
name: R-CMD-check

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}
    name: ${{ matrix.config.os }} (${{ matrix.config.r }})
    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macos-latest, r: 'release'}
          - {os: ubuntu-latest, r: 'release'}
          - {os: ubuntu-latest, r: 'devel'}

    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
      - uses: r-lib/actions/check-r-package@v2
```

### 3. Create CONTRIBUTING.md

- [x] Create `CONTRIBUTING.md` with:
  - [x] How to report bugs (GitHub Issues)
  - [x] How to suggest features
  - [x] How to submit pull requests
  - [x] Code style guidelines (roxygen2, data.table syntax)
  - [x] Testing requirements

### 4. Create CODE_OF_CONDUCT.md

- [x] Create `CODE_OF_CONDUCT.md` using Contributor Covenant: https://www.contributor-covenant.org/version/2/1/code_of_conduct/

---

## Paper Content Review

### Paper Structure (current sections)

1. **Summary** - Package overview and capabilities
2. **Statement of need** - Analytical challenges addressed
3. **State of the field** - Comparison with existing R packages (16 refs)
4. **Software design** - S3 classes, robust estimation, visualization
5. **Research impact statement** - PEP725 analysis, @templetal2026a methods
6. **AI usage disclosure** - Editorial aid only
7. **Acknowledgements** - PEP725 observers, BNF funding, WSL/Vitasse
8. *Example usage* - Commented out (`<!-- ... -->`)

### Paper Decisions

- [ ] **Example usage section**: Currently commented out (lines 51-83). Decide whether to:
  - [ ] Include it (~100 extra words; current body is ~896 words, well within 750-1750 limit)
  - [ ] Keep it commented out
  - [ ] Move to README/vignettes only

- [ ] **Figures**: JOSS papers often include 1-2 figures. Consider adding:
  - [ ] Example plot output (e.g., gradient analysis, synchrony plot)
  - [ ] Workflow diagram showing package structure
  - [ ] Map visualization example

### Paper Content Improvements

- [x] **Summary section**: Review for clarity and completeness
  - [x] Verify PEP725 statistics are current (13 million obs, 30 countries, 265 species, 46 phenophases)
  - Note: statistics cite @templ2018pep725 and @templetal2026a — check against the 2026 paper

- [x] **Research impact statement**: Strengthened with specifics
  - [x] Added concrete examples: Fu et al. (2015) Nature, Menzel et al. (2020) GCB
  - [x] Referenced PEP725 publication impact (115 papers, 17 Nature, 2 Science) from @templetal2026a
  - [x] Added fu2015declining and menzel2020climate to paper.bib

### Paper Technical Checks

- [x] **Logo image**: `pep725_logo_cpt.png` exists in `pep725_JOSS-Paper/` ✅
- [x] **Bibliography**: 16 references in `paper.bib`
  - [x] All DOIs with actual DOI values verified and resolve correctly (11 DOIs checked) ✅
  - [ ] Verify CRAN package version numbers are still current (5 refs use CRAN URLs only)
  - **Exception**: `opedal2024advancing` DOI is wrong — see Critical Bibliography Fix below
- [x] **ROR identifiers**: Verify affiliation RORs are correct
  - [x] WSL: `04d81q302` (fixed)
  - [x] FHNW: `02gz82p86`

### Critical Bibliography Fix

- [ ] **Fix `opedal2024advancing` reference** - WRONG DOI!
  - Current DOI `10.1038/s41559-024-02336-5` resolves to Campbell et al. (2024) "Herbivore effects increase with latitude across the extent of a foundational seagrass" — a marine ecology paper, NOT phenology
  - The title "Advancing phenological research through long-term data integration" does not appear to be a real publication
  - The listed authors (Opedal, Ovaskainen, et al.) work on metapopulation ecology, not phenological research
  - **This reference appears fabricated. Replace with one of these options:**
    - **Option A (Recommended):** Primack et al. (2023) "Ten best practices for effective phenological research" Int J Biometeorol 67:1509-1522, DOI: 10.1007/s00484-023-02502-7
    - **Option B:** Kharouba et al. (2018) "Global shifts in the phenological synchrony of species interactions" PNAS 115:5211-5216, DOI: 10.1073/pnas.1714511115
    - **Option C:** Ovaskainen et al. (2013) "Community-level phenological response to climate change" PNAS 110:13434-13439, DOI: 10.1073/pnas.1305533110
  - Used in `JOSS_packagePaper.Rmd` line 91: `@opedal2024advancing` in "State of the field" section
  - Update citation in `paper.bib` (lines 181-189)
  - Update citation key in `JOSS_packagePaper.Rmd` if key changes



### Word Count Check

Current: ~896 body words (limit: 750-1750)
- Room to include the commented-out Example usage section (~100 words → ~996 total)
- Room to add figures with captions if desired
- All sections are concise; no section appears overly verbose

---

## Medium Priority

### 5. Convert Paper to Markdown

- [ ] Convert `pep725_JOSS-Paper/JOSS_packagePaper.Rmd` to `paper.md`
  - [ ] Remove R Markdown specific elements
  - [ ] Keep YAML frontmatter
  - [ ] Ensure pure Pandoc Markdown syntax

### 6. Fix Paper Date Format

- [x] Change `date: 2026-02-04` to `date: 4 February 2026`

---

## Low Priority / Optional

### 7. Add README Badge

- [x] Add CI status badge to README.md after CI is set up:
```markdown
[![R-CMD-check](https://github.com/matthias-da/pep725/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matthias-da/pep725/actions/workflows/R-CMD-check.yaml)
```

### 8. Add NEWS.md

- [x] Create `NEWS.md` to track version changes:
```markdown
# pep725 0.1.0

* Initial CRAN/JOSS submission
* Core functions: pheno_normals(), pheno_anomaly(), pheno_gradient(), etc.
```

---

## Checklist Summary

| Item | Status | Priority |
|------|--------|----------|
| 6-month development history | ⏳ Waiting (July 2026) | Blocker |
| testthat tests | ✅ Done (95 tests passing) | High |
| GitHub Actions CI | ⚠️ Created locally, needs PAT update to push | High |
| CONTRIBUTING.md | ✅ Done | High |
| CODE_OF_CONDUCT.md | ✅ Done | Medium |
| Paper format (paper.md) | ❌ Not started | Medium |
| Paper date format | ✅ Done | Low |
| JOSS PDF workflow | ⚠️ Created locally, needs PAT update to push | Medium |
| README badge | ✅ Done | Low |
| NEWS.md | ✅ Done | Low |
| **Paper Content** | | |
| Example usage decision | ❓ Needs decision | Medium |
| Add figures to paper | ❓ Needs decision | Medium |
| Research impact specifics | ✅ Done (Fu 2015, Menzel 2020) | Medium |
| Logo image check | ✅ Verified (exists) | Low |
| Bibliography DOI verification | ✅ All 11 DOIs valid (except opedal) | Low |
| CRAN pkg version check | ❌ Not verified (5 refs) | Low |
| ROR identifiers check | ✅ Fixed | Low |
| Fix opedal2024advancing ref | ❌ Fabricated reference | **High** |
| **GitHub Issues** | | |
| Open issues | 13 open | Various |
| **R CMD Check** | | |
| Fix NOTEs | ✅ All 5 fixed | Medium |
| Co-author review (Barbara) | ❌ Not started | High |
| **Package Review** | | |
| Critical bugs (4) | ✅ All fixed | **High** |
| Package structure (6) | ✅ All fixed | **High** |
| Documentation fixes (6) | ✅ All fixed | Medium |
| Vignette fixes (5) | ⚠️ Partially fixed | Medium |
| Code quality (5) | ❌ Not fixed | Low |
| Consistency (2) | ❌ Not fixed | Low |

---

## GitHub Issues (Open)

### Documentation & Content
- [x] **#32** Refine the README file
- [x] **#30** Add help for non-PEP725 data users (getting-started.Rmd)
- [x] **#28** Take back "Expected Values and Troubleshooting" section (if time for reference search) — 
     [ ]        ⚠️ **Needs Barbara's review** (references verified but content should be checked by domain expert)
- [x] **#27** Remove Part 2
- [x] **#26** Detailed Context - delete

### Papers & Publications
- [ ] **#29** Write PAPER about the package (enhancement)
- [ ] **#16** PAPER about the synthetic data (enhancement)
- [ ] **#14** Write the JOSS paper

### Package Development
- [ ] **#3** Package check (CRAN version)
- [ ] **#8** phenological analysis/quality() plotting the extreme events?
- [ ] **#2** Data repository GitHub to Zenodo

### Outreach
- [ ] **#15** Autogenerated Marketing Material in the presentations folder
- [ ] **#10** Create a video about using the R package

---

## R CMD Check Issues

### NOTEs to Fix
- [x] Add `.github` to `.Rbuildignore`
- [x] Add non-standard files to `.Rbuildignore`: `CONTRIBUTING.md`, `TODO_JOSS_submission.md`, `pep725_JOSS-Paper`, `CLAUDE.md`
- [x] Fix `simulate_pep.R:52` - change `gam()` to `mgcv::gam()`
- [x] Add global variable bindings for `grade_num`, `worst_grade_num` in `plot.pep_quality()`
- [x] Add global variable binding for `..na_cols` in `print.pheno_anomaly()`

---

## Package Review Findings

Comprehensive code, documentation, and vignette review (2026-02-13).

### A. Critical Bugs (fix before CRAN/JOSS)

- [x] **`simulate_pep.R:76`** — NA fallback was a no-op (`NA + rnorm()` = `NA`). Fixed: use species-phase median + jitter as fallback center.
- [x] **`simulate_pep.R:52`** — Unnamespaced `gam()` call. Changed to `mgcv::gam()` (fixed in previous commit).
- [x] **`zzz.R:6`** — Removed `utils::data("meteoSwiss", envir = globalenv())` from `.onAttach()`. Added `LazyData: true` to DESCRIPTION so datasets load on demand.
- [x] **`pheno_gradient.R:133-136`** — `data.table` modify-by-reference bug: `pep[, alt := ...]` modified caller's data before `copy()`. Fixed: moved `copy()` before column renaming.

### B. Package Structure Issues

- [x] **Depends → Imports**: Moved `dplyr`, `robustbase`, and `patchwork` from `Depends` to `Imports` (all calls already namespace-qualified)
- [x] **Imports → Suggests**: Moved `nlme` and `quantreg` to `Suggests` (already had `requireNamespace()` guards and `::` calls)
- [x] **Heavy interactive deps to Suggests**: Moved `shiny`, `miniUI`, `leaflet`, `leaflet.extras` to `Suggests`; namespace-qualified all calls in `leaflet_pep.R`; added proper error message when missing
- [x] **`.Rbuildignore`**: Already fixed in R CMD check NOTEs commit (all entries present)
- [ ] **Test coverage ~10%**: 95 tests cover 5 core functions; no tests for `pep_import()`, `simulate_pep()`, `pheno_combine()`, `pheno_trend_turning()`, `pls_phenology()`, `flag_outliers()`, `detect_second_events()`, `check_phases()`, `pep_completeness()`, visualization functions
- [x] **BBCH lookup consolidated**: Created single `.bbch_lookup` vector in `pep_class.R`; replaced inline copies in `summary.pep()`, `select_phase.R`, `regional_box_ts.R`; `bbch_description()` now uses it too

### C. Documentation Issues

- [x] **`pheno_timeseries.R`** — Fixed `@param`: "DOY" → "day" (day-of-year)
- [x] **`pep_import.R`** — Fixed `@return`: "data.table" → "pep object"
- [x] **`regional_box_ts.R`** — Fixed `@return`: added missing `functional_group` and `phase` fields (no `ts_tidy` was listed)
- [x] **`get_phenological_doys.R`** — Removed duplicate roxygen block (second title/params/return)
- [x] **`R/dataSets.R`** — `pep_seed` uses `NULL` correctly (standard pattern with LazyData); no change needed
- [x] **`R/dataSets.R`** — Removed `pep_synth` documentation (no `pep_synth.rda` exists)

### D. Vignette Issues

- [ ] **`phenological-analysis.Rmd`** — Copy-paste error: text says "Wheat" but code uses `"Malus domestica"` (Apple)
- [x] **`phenological-analysis.Rmd`** — Removed hail/GISS references; updated to PEP-only workflow
- [ ] **`data-quality.Rmd`** — References `obs_id` column which doesn't exist in the `pep` class
- [ ] **`spatial-patterns.Rmd`** — Minor typos to fix (check after Barbara's review of new Expected Values section)
- [ ] **`getting-started.Rmd`** — Review for consistency with updated README

### E. Code Quality Improvements

- [ ] **Mixed dplyr/data.table paradigms**: Several files use both `dplyr::` and `data.table` syntax. Consider standardizing on `data.table` throughout (the package already depends on it)
- [ ] **`eval(parse(text=...))` in `regional_box_ts.R`**: Fragile pattern; replace with direct column access via `data.table` syntax
- [ ] **Deprecated ggplot2 `size` aesthetic**: `plot.pep_quality()` and possibly others use `size =` for line geoms — change to `linewidth =` (deprecated since ggplot2 3.4.0)
- [ ] **Missing `utils::globalVariables()` declarations**: Several files use non-standard evaluation columns without declaring them (beyond the ones already noted in R CMD check NOTEs): check `pheno_combine.R`, `pheno_trend_turning.R`, `detect_second_events.R`, `flag_outliers.R`
- [ ] **`pheno_synchrony.R`** — Consider adding input validation for `min_stations` parameter

### F. Consistency Issues

- [ ] **Function naming**: Most functions use `pheno_` or `pep_` prefix, but `flag_outliers()`, `detect_second_events()`, `check_phases()`, `calc_daylength()`, `calc_thermal_units()` don't — consider renaming for consistency (breaking change, defer to post-JOSS)
- [ ] **`plot()` method coverage**: Not all S3 classes have `plot()` methods (e.g., `pheno_turning`, `pep_connectivity`) — add where useful

---

## Already Completed

- [x] OSI-approved license (GPL-3)
- [x] README with installation instructions
- [x] Vignettes (4 comprehensive vignettes)
- [x] API documentation (roxygen2)
- [x] Author ORCIDs in paper
- [x] Affiliation RORs in paper
- [x] AI usage disclosure in paper
- [x] All required paper sections present
- [x] Word count within limits (773 words)
- [x] Bibliography file (paper.bib)
- [x] Paper date format fixed (4 February 2026)
- [x] Paper affiliation format fixed (quoted strings)
- [x] JOSS PDF compilation workflow (`.github/workflows/draft-pdf.yaml`) - created locally, needs PAT update
- [x] R CMD check CI workflow (`.github/workflows/R-CMD-check.yaml`) - created locally, needs PAT update
- [x] CONTRIBUTING.md with community guidelines
