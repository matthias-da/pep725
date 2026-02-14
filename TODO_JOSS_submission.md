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

### Paper Formatting & Rendering Issues (2026-02-14)

**High priority — will cause rendering failures or reviewer rejection:**

- [ ] **Remove horizontal rules (`---`) between sections** — Lines 43, 49, 85, 93, 101, 109, 119 have `---` separators. JOSS papers should not use horizontal rules; they render as `<hr>` lines in the PDF. Use heading levels only.
- [ ] **Fix citation syntax in Summary** — Line 39: `(PEP725; @templ2018pep725, @templetal2026a)` uses invalid comma-separated bare citations. Should be `[PEP725; @templ2018pep725; @templetal2026a]` (semicolons inside square brackets).
- [ ] **Remove logo HTML from paper body** — Lines 32-34: `<p align="center"><img ...>` won't render in JOSS PDF (Pandoc → LaTeX pipeline). The logo belongs in the repository README only.
- [ ] **Fix Thackeray2016 "et al." in paper.bib** — Line 39: `author = {Thackeray, S. J. et al.}` will render literally. BibTeX must list actual author names (the paper has 47 authors; include at least the first 5-10).
- [ ] **Fix DOI format in Parmesan2006** — paper.bib line 24: `doi = "https://doi.org/10.1146/..."` includes the full URL. The `doi` field should contain only the identifier (`10.1146/annurev.ecolsys.37.091305.110100`). Most styles auto-prepend the URL, so this produces a doubled link.
- [ ] **Fix wrong parameter names in commented-out Example usage** — Lines 51-83: `ref_period` → `period`, `gradient_var` → `variable`. If this section is uncommented, the code will not work.
- [ ] **Fix `volume = {online}` in templetal2026a** — paper.bib line 64: renders as volume "online". Either remove the `volume` field or use `note = {Advance online publication}`.

**Medium priority — reviewers would flag:**

- [ ] **Change R package bib entries from `@article` to `@manual`** — Entries: `tschurr2025dymep`, `tanigu2025phenolocrop`, `phenesse2025`, `Schaberpheno2026`, `Gomezsephora2024`, `Langephenex2017`, `phenomap2020` use `@article` with `journal = {R package version X.Y.Z}`. Should be `@manual` or `@misc`.
- [ ] **Update stale "January 2023" date in research impact** — Line 105: "which as of January 2023 had contributed to at least 115 peer-reviewed publications" is 3 years stale for a Feb 2026 submission. Update with most recent count from @templetal2026a.
- [ ] **Clean up Parmesan2006 bib entry** — (1) Six duplicate `keywords` fields (only last is used); combine into one. (2) Full abstract embedded — remove. (3) Non-standard `type = "Journal Article"` field — remove.

**Low priority — polish:**

- [ ] **Standardize BibTeX key naming** — Mixed conventions (`Piao2019`, `templ2018pep725`, `luedeling2012chillR`, `Schaberpheno2026`). Adopt consistent scheme.
- [ ] **Shorten AI usage disclosure** — Current 3-paragraph disclosure (lines 111-117) is verbose. A concise 1-2 sentence statement would suffice for JOSS.

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
| **Paper Formatting (2026-02-14)** | | |
| Remove `---` horizontal rules | ❌ 7 occurrences | **High** |
| Fix citation syntax (Summary) | ❌ Invalid comma-separated bare `@` | **High** |
| Remove logo HTML from paper | ❌ Won't render in JOSS PDF | **High** |
| Fix Thackeray2016 "et al." | ❌ Must list real authors | **High** |
| Fix DOI format (Parmesan2006) | ❌ Full URL in `doi` field | **High** |
| Fix example parameter names | ❌ `ref_period`/`gradient_var` wrong | **High** |
| Fix `volume = {online}` | ❌ Renders literally | **High** |
| R package bib: `@article`→`@manual` | ❌ 7 entries | Medium |
| Update stale "Jan 2023" date | ❌ 3 years old | Medium |
| Clean up Parmesan2006 bib entry | ❌ Dup keywords, abstract, type | Medium |
| Standardize BibTeX key naming | ❌ Mixed conventions | Low |
| Shorten AI disclosure | ❌ 3 paragraphs → 1-2 sentences | Low |
| **GitHub Issues** | | |
| Open issues | 13 open | Various |
| **R CMD Check** | | |
| Fix NOTEs | ✅ All 5 fixed | Medium |
| Co-author review (Barbara) | ❌ Not started | High |
| **Package Review** | | |
| Critical bugs (4) | ✅ All fixed | **High** |
| Package structure (6) | ✅ All fixed | **High** |
| Documentation fixes (6) | ✅ All fixed | Medium |
| Vignette fixes (5) | ✅ All fixed | Medium |
| Code quality (5) | ⚠️ 4/5 fixed (dplyr/dt deferred) | Low |
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
- [x] **#8** phenological analysis/quality() plotting the extreme events?
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

- [x] **`phenological-analysis.Rmd`** — Fixed copy-paste error: "Wheat" → "Apple" (2 locations)
- [x] **`phenological-analysis.Rmd`** — Removed hail/GISS references; updated to PEP-only workflow
- [x] **`data-quality.Rmd`** — `obs_id` references already removed in prior commit
- [x] **`spatial-patterns.Rmd`** — Fixed typos: "Preffered" → "Preferred", missing period
- [x] **`getting-started.Rmd`** — Fixed: "four ways" → "five ways", removed false claim about auto-loading datasets

### E. Code Quality Improvements

- [ ] **Mixed dplyr/data.table paradigms**: Several files use both `dplyr::` and `data.table` syntax. Consider standardizing on `data.table` throughout (defer to post-JOSS)
- [x] **`eval(parse(text=...))` in `pheno_trend_turning.R`**: Replaced with data.table join (`pep[groups[i], on = by]`)
- [x] **Deprecated ggplot2 `size` aesthetic**: Fixed `size` → `linewidth` in `plot_phenology_trends.R` (geom_line, geom_vline); other `size =` usages are for points/text (correct)
- [x] **Missing `utils::globalVariables()` declarations**: Added `..by` to `pheno_trend_turning.R`; `pheno_combine.R`, `detect_second_events.R`, `flag_outliers.R` already complete
- [x] **`pheno_synchrony.R`** — Already has `min_stations` validation (lines 123-125: must be numeric >= 2)

### F. Consistency Issues

- [x] **`plot()` method coverage**: Not all S3 classes have `plot()` methods (e.g., `pheno_turning`, `pep_connectivity`) — add where useful

---

## G. Detailed Function Review

All 45 exported functions grouped by category. For each, verify: runs without error, documentation complete (params, return, examples), edge cases handled, consistent code style.

### Data Access (5 functions)

- [x] **`pep_download()`** — Reviewed & fixed: replaced fragile `ls(env)[1]` with `load()` return value, corrected cache path docs, now validates via `new_pep()`.
- [x] **`pep_import()`** — Reviewed & fixed: added input validation (missing dir, no CSVs, empty data), `warning()`→`message()`, fixed `@return` roxygen link, removed dead code, enabled `new_pep()` validation, added explicit `sep=";"`.
- [x] **`pep_cache_clear()`** — Reviewed: works correctly, handles empty cache gracefully.
- [x] **`pep_cache_info()`** — Reviewed: returns clean list with path/size/modified/exists.
- [x] **`simulate_pep()`** — Reviewed & fixed: removed vestigial `doy_synth` column, now returns `new_pep()`, added input validation, messages about unmodified rows, improved roxygen docs.

### Class Construction & Utilities (8 functions)

- [x] **`new_pep()`** — Reviewed & fixed: class assignment now deduplicates (`setdiff`) to prevent double `"pep"` class when called on existing pep objects.
- [x] **`as.pep()`** — Reviewed: thin wrapper around `new_pep()`, works correctly. Idempotent, validates columns, converts to data.table.
- [x] **`is.pep()`** — Reviewed: works correctly for pep objects, data.frames, NULL, scalars.
- [x] **`add_country()`** — Reviewed & fixed: NA coordinates now handled (rows get `country = NA`), pre-existing `country` column replaced instead of duplicated, removed `suppressWarnings` no-op and unnecessary `requireNamespace("data.table")`, added `call. = FALSE`, added `copy()` to protect original, updated docs.
- [x] **`add_daylength()`** — Reviewed & fixed: NA values in day/lat now return `NA` daylength instead of crashing (fixed `calc_daylength()` NA handling).
- [x] **`bbch_description()`** — Reviewed: no issues. Handles unknown codes, NA, empty input, sort parameter all correctly.
- [x] **`select_phase()`** — Reviewed: no issues. Correctly uses `.bbch_lookup`, warns on unmapped/missing phases, handles empty results.
- [x] **`coverage()`** — Reviewed: no issues. All kinds work (temporal/geographical/species/all), grouping, minimal data handled.

### Subspecies Utilities (2 functions)

- [x] **`subspecies_report()`** — Reviewed: works correctly. Returns summary_table, wide_table, heatmap. Uses dplyr (known mixed-paradigm, defer post-JOSS).
- [x] **`summarize_subspecies_availability()`** — Reviewed: works correctly. Handles empty results, all metric options, include_empty grid expansion.

### Analysis (8 functions)

- [x] **`pheno_normals()`** — Reviewed & fixed: added `probs` length=6 validation, fixed `by=character(0)` warning, fixed broken roxygen line in `@param phase_id`.
- [x] **`pheno_anomaly()`** — Reviewed & fixed: pre-computed normals merge no longer leaks extra columns (`.x`/`.y` artifacts), fixed "non-parametric" doc (percentile uses `pnorm()`), added globalVariables.
- [x] **`pheno_gradient()`** — Reviewed & fixed: `plot()` crashed when `by` was used (stored raw data instead of aggregated); now stores aggregated data and facets by group.
- [x] **`pheno_synchrony()`** — Reviewed & fixed: critical `phase_id` filter bug (data.table scoping: `dt[dt$phase_id == phase_id]` compared column to itself). Fixed with local variable rename.
- [x] **`pheno_combine()`** — Reviewed: works correctly for all methods (robust/mixed/ols). No `summary()` method but print is informative. Minor doc gaps (undocumented return fields).
- [x] **`pheno_trend_turning()`** — Reviewed & fixed: numeric vector path now produces consistent return structure (was missing `$results` data.table, causing print/plot crashes).
- [x] **`pls_phenology()`** — Reviewed & fixed: wrapped example in `\donttest{}`, removed dead `start_doy` variable. Note: `by` parameter is accepted but silently ignored.
- [x] **`kendall_tau()`** — Reviewed & fixed: corrected documentation — function computes Mann-Kendall Z-statistic (not Kendall's tau). Updated title, description, and return docs.

### Quality & Validation (6 functions)

- [x] **`pep_quality()`** — Reviewed: no bugs. Grades correctly, print/summary/plot all work.
- [x] **`pep_completeness()`** — Reviewed: no bugs. Print and plot work correctly.
- [x] **`check_phases()`** — Reviewed: works correctly. Note: no `plot.phase_check` or `summary.phase_check` methods (missing features, not bugs).
- [x] **`check_phases_multi()`** — Reviewed & fixed: crashed with factor `species_list` input; added `as.character()` coercion.
- [x] **`check_connectivity()`** — Reviewed: works correctly. Note: no `print.pep_connectivity` or `plot.pep_connectivity` methods.
- [x] **`flag_outliers()`** — Reviewed: no bugs. All 4 methods (30day/mad/iqr/zscore) work, print/summary/plot all work.

### Event Detection (2 functions)

- [x] **`detect_second_events()`** — Reviewed: no bugs. All methods (late_season/multiple_per_year/both) work. Print, summary, and all plot types work.
- [x] **`get_phenology_doys()`** — Reviewed: works for valid inputs. Crashes with unhelpful error when requested phase doesn't exist in data (edge case).

### Calculation Utilities (4 functions)

- [x] **`calc_daylength()`** — Reviewed & fixed: NA inputs now produce `NA` output instead of crash, polar edge cases verified correct.
- [x] **`calc_max_daylength()`** — Reviewed & fixed: removed redundant if/else (both branches identical).
- [x] **`calc_thermal_sum()`** — Reviewed: works correctly. Note: row-by-row loop would be slow on large datasets (performance, not correctness).
- [x] **`calc_thermal_units()`** — Reviewed: no bugs. All methods (simple/modified/single_sine) work. Edge cases (NAs, below-base temps, empty input) all handled.

### Visualization (6 functions)

- [x] **`pheno_plot()`** — Reviewed: requires `ts_tidy` element which only comes from `regional_box_ts_heading_harvest()`, not `regional_box_ts()`. Docs reference the wrong function.
- [x] **`pheno_plot_hh()`** — Reviewed: no bugs. Works correctly with `regional_box_ts_heading_harvest()` output.
- [x] **`pheno_plot_timeseries()`** — Reviewed: no bugs. Docs reference `functional_group` column that doesn't exist in synthetic data (cosmetic doc issue).
- [x] **`plot_phenology_trends()`** — Reviewed: uses deprecated `dplyr::do()` (should be `reframe()`). Otherwise works.
- [x] **`plot_outliers()`** — Reviewed: no bugs. All plot types (overview, seasonal, detail, map) work correctly.
- [x] **`map_pep()`** — Reviewed: no `requireNamespace("rnaturalearthdata")` guard when `background = "none"`. Otherwise correct.

### Interactive & Regional (3 functions)

- [x] **`leaflet_pep()`** — Reviewed: no bugs. Has proper `requireNamespace` guards for all Suggests packages (shiny/miniUI/leaflet/leaflet.extras).
- [x] **`regional_box_ts()`** — Reviewed & fixed: `functional_group` filter was a tautology (data.table scoping: compared parameter to itself). Added column existence check and fixed filtering.
- [x] **`regional_box_ts_heading_harvest()`** — Reviewed: no bugs. Clean implementation.

### Demo (1 function)

- [x] **`pep725_demo()`** — Reviewed: no bugs. Uses package functions correctly, example properly in `\dontrun{}`.

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
