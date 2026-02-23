# TODO: JSS Paper for pep725

*Journal of Statistical Software — Paper Plan & Task Tracker*
*Created: 2026-02-23*

---

## Paper Metadata

- **Title (working):** "pep725: Quality-Aware Analysis of Ground-Based Phenological Data in R"
- **Authors:** Matthias Templ, Barbara Templ
- **Target journal:** Journal of Statistical Software (IF ~8.1, Q1)
- **Format:** `.Rnw` (knitr + jss.cls) — will double as package vignette after acceptance
- **Estimated length:** 20-30 pages
- **Expected review timeline:** 1-2 years from submission

---

## Prerequisites (before writing)

- [ ] **Get pep725 accepted on CRAN** — JSS strongly prefers CRAN packages; near-essential for acceptance
- [ ] **Complete JOSS submission first** (July 2026) — avoid conflicts; JOSS is faster
- [ ] **Ensure all 45 functions have complete, correct roxygen documentation** — JSS reviewers inspect help files
- [ ] **Reach >=100 testthat tests** covering all core analysis functions — current: 95 tests
- [ ] **Add `inst/CITATION` file** referencing the JOSS paper (once published) and the JSS paper (once accepted)
- [ ] **Set up JSS LaTeX infrastructure:**
  - [ ] Download `jss.cls` and `jss.bst` from jstatsoft.org
  - [ ] Create `vignettes/pep725-jss.Rnw` skeleton using JSS template
  - [ ] Configure knitr chunk options: `options(prompt = "R> ", continue = "+  ", width = 70)`
  - [ ] Test that `texi2pdf` compiles the template cleanly
- [ ] **Create standalone replication script** `article.R` that reproduces every figure/table (JSS requirement)

---

## Paper Structure & Section TODOs

### Section 1: Introduction (3-4 pages)

**Goal:** Motivate the software, embed in literature, provide paper roadmap.

- [ ] **1.1 Phenology and its importance**
  - [ ] Define phenology; cite Schwartz (2003), Menzel (2002)
  - [ ] Explain relevance for climate change monitoring, agriculture, ecosystem services
  - [ ] Mention PEP725 database: 13M observations, 30+ countries, 265 species, records since 1868
  - [ ] Cite Templ et al. (2018) and Templ et al. (2026) for PEP725 context

- [ ] **1.2 Challenges in phenological data analysis**
  - [ ] Data quality heterogeneity (volunteer observers, station changes, gaps)
  - [ ] Need for outlier detection before analysis
  - [ ] Spatial and temporal coverage imbalances
  - [ ] BBCH coding complexity (265 species x 46 phases)

- [ ] **1.3 Existing software landscape**
  - [ ] Survey R packages: `\pkg{phenor}` (Hufkens et al. 2018), `\pkg{phenofit}` (Kong et al. 2022), `\pkg{rnpn}`, `\pkg{rppo}`, `\pkg{climatrends}`, `\pkg{chillR}`
  - [ ] Key gap: no package addresses data quality assessment + analysis in one framework
  - [ ] Create **comparison table** (Table 1): features vs. packages (quality grading, outlier detection, robust methods, S3 classes, interactive maps, data.table backend)
  - [ ] Discuss complementary vs. competing roles (pep725 = data quality + diagnostics; phenor = process-based models; phenofit = remote sensing)

- [ ] **1.4 Paper outline**
  - [ ] One paragraph roadmap of remaining sections
  - [ ] State package version used (`pep725` version X.Y.Z)

### Section 2: Statistical Methods (5-7 pages)

**Goal:** Present the methodological foundations. Methods must be previously published — JSS is not for new methodology.

- [ ] **2.1 Data quality assessment framework**
  - [ ] Quality grading system (A/B/C/D) — cite basis in WMO guidelines
  - [ ] Completeness metrics: temporal coverage, year span, observation density
  - [ ] Phase validation: sequential BBCH constraints
  - [ ] Station-year connectivity checks
  - [ ] Write equation for quality score composition

- [ ] **2.2 Outlier detection methods**
  - [ ] 30-day rule (deviation from station median) — cite origin
  - [ ] MAD-based detection — cite Rousseeuw & Croux (1993) for MAD
  - [ ] IQR method — describe fence calculation
  - [ ] Z-score method — describe threshold selection
  - [ ] Write equations for each method
  - [ ] Discuss when to use which method (Table 2)

- [ ] **2.3 Phenological normals and anomalies**
  - [ ] WMO climatological standard normals (1991-2020 reference period)
  - [ ] Robust estimation: median, MAD vs. mean, SD
  - [ ] Anomaly metrics: absolute deviation, z-score, percentile rank
  - [ ] Write equations for normal calculation and anomaly standardization

- [ ] **2.4 Spatial analysis: gradients and synchrony**
  - [ ] Elevation gradient: `\code{day} \sim \code{alt}` with robust regression (`\pkg{robustbase}::lmrob`)
  - [ ] Latitude gradient: `\code{day} \sim \code{lat}`
  - [ ] Why robust regression? Cite Maronna et al. (2019) — outlier resistance in spatial phenology
  - [ ] Lapse rate interpretation: reference values 2-4 days/100m (cite Defila & Clot 2001)
  - [ ] Synchrony metrics: SD, CV, range, IQR — cite Menzel et al. (2006)
  - [ ] Equations for all metrics

- [ ] **2.5 Trend analysis**
  - [ ] Mann-Kendall test for monotonic trends — cite Mann (1945), Kendall (1975)
  - [ ] Sequential Mann-Kendall for trend turning points — cite Sneyers (1990)
  - [ ] Kendall's tau as normalized statistic
  - [ ] Equations for test statistic and variance

- [ ] **2.6 Climate sensitivity via PLS regression**
  - [ ] Partial Least Squares regression — cite Wold et al. (2001)
  - [ ] Iteratively reweighted robust PLS estimation
  - [ ] Application: identifying temperature-sensitive periods for phenology
  - [ ] Cite Luedeling & Gassner (2012) for phenology-PLS methodology

- [ ] **2.7 Combined time series estimation**
  - [ ] Multi-station aggregation problem
  - [ ] Three estimators: OLS, robust (LAD via `\pkg{quantreg}`), mixed models (`\pkg{nlme}`)
  - [ ] When to use which estimator

### Section 3: Software Design (3-4 pages)

**Goal:** Describe the package architecture, S3 class system, and design decisions.

- [ ] **3.1 The `pep` S3 class**
  - [ ] Constructor `\code{new\_pep()}` with validation
  - [ ] Required columns: `s_id`, `lon`, `lat`, `genus`, `species`, `phase_id`, `year`, `day`
  - [ ] `data.table` extension for performance
  - [ ] Class-preserving subsetting via `\code{[.pep}`
  - [ ] Show `print`, `summary`, `plot` method signatures

- [ ] **3.2 Analysis result classes**
  - [ ] Table of S3 classes: `pheno_normals`, `pheno_anomaly`, `pep_quality`, `pheno_gradient`, `pheno_synchrony`, `pheno_combined`, `pheno_turning`, `pep_outliers`, etc.
  - [ ] Each with consistent `print`/`summary`/`plot` interface
  - [ ] Design philosophy: every analysis returns a structured object, not a raw data.table

- [ ] **3.3 Data access and synthetic data**
  - [ ] `\code{pep\_download()}` for synthetic data (cached, ~64MB)
  - [ ] `\code{pep\_import()}` for real PEP725 CSV files
  - [ ] `\code{pep\_simulate()}` for generating synthetic data via GAM smoothing
  - [ ] Discuss why synthetic data matters (PEP725 requires registration; reproducible examples)

- [ ] **3.4 Naming conventions and workflow design**
  - [ ] `pep_*` = data infrastructure; `pheno_*` = scientific analysis
  - [ ] Typical workflow: import → quality → filter → analyze → visualize
  - [ ] Include workflow diagram (update from JOSS paper Figure 1)

### Section 4: Illustrations (6-8 pages)

**Goal:** Complete worked examples demonstrating the full analytical workflow. This is the heart of the JSS paper — must be non-trivial.

- [ ] **4.1 Data preparation and exploration**
  - [ ] Load synthetic data: `\code{library("pep725"); pep <- pep\_download()}`
  - [ ] Show `\code{print(pep)}` and `\code{summary(pep, by = "species")}` output
  - [ ] Explore with `\code{pep\_coverage()}`
  - [ ] Subset to case study species: Vitis vinifera (longest records, 187 years) and Malus domestica (most stations)

- [ ] **4.2 Quality assessment workflow**
  - [ ] Run `\code{pep\_quality()}` on grapevine subset
  - [ ] Show quality grade distribution (A/B/C/D)
  - [ ] Demonstrate `\code{pep\_flag\_outliers()}` with different methods — compare results
  - [ ] Visualize outliers with `\code{pep\_plot\_outliers()}` (show all 4 plot types)
  - [ ] Apply quality-based filtering
  - [ ] Check phase validity with `\code{pep\_check\_phases()}`

- [ ] **4.3 Climatological analysis**
  - [ ] Calculate normals: `\code{pheno\_normals()}` for 1991-2020
  - [ ] Show `\code{plot(normals)}` output
  - [ ] Detect anomalies: `\code{pheno\_anomaly()}`
  - [ ] Identify extreme years (e.g., 2007 warm spring)
  - [ ] Show `\code{plot(anomaly)}` — timeline with color-coded deviations

- [ ] **4.4 Spatial patterns**
  - [ ] Elevation gradient: `\code{pheno\_gradient(by = "alt")}`
  - [ ] Compare robust vs. OLS fit — show coefficient table
  - [ ] Latitude gradient: `\code{pheno\_gradient(by = "lat")}`
  - [ ] Synchrony analysis: `\code{pheno\_synchrony()}`
  - [ ] Map visualization with `\code{pheno\_map(background = "none")}`

- [ ] **4.5 Trend analysis and climate sensitivity**
  - [ ] Sequential Mann-Kendall: `\code{pheno\_trend\_turning()}`
  - [ ] Show trend plot with turning point detection
  - [ ] PLS regression: `\code{pheno\_pls()}` identifying temperature-sensitive windows
  - [ ] Show VIP plot highlighting critical periods
  - [ ] Discuss ecological interpretation of results

- [ ] **4.6 Combined time series**
  - [ ] Multi-station aggregation: `\code{pheno\_combine()}`
  - [ ] Compare robust vs. OLS vs. mixed model estimates
  - [ ] Show combined time series plot

### Section 5: Comparison with Existing Software (2-3 pages)

- [ ] **5.1 Feature comparison table**
  - [ ] Rows: quality grading, outlier detection, normals, anomalies, gradients, synchrony, trends, PLS, S3 classes, interactive maps, data.table backend, synthetic data
  - [ ] Columns: `\pkg{pep725}`, `\pkg{phenor}`, `\pkg{phenofit}`, `\pkg{rnpn}`, `\pkg{rppo}`, `\pkg{chillR}`, `\pkg{climatrends}`
  - [ ] Use checkmarks/crosses

- [ ] **5.2 Performance benchmarks**
  - [ ] Benchmark key operations on different dataset sizes (1K, 10K, 100K, 1M, 7M rows)
  - [ ] Compare data.table vs. dplyr-based alternatives where relevant
  - [ ] Report memory usage for large datasets
  - [ ] Use `\pkg{bench}` or `\pkg{microbenchmark}` for timing

- [ ] **5.3 Qualitative comparison**
  - [ ] pep725 = quality-first framework for station data
  - [ ] phenor = process-based phenology modeling (complementary, not competing)
  - [ ] phenofit = remote sensing phenology (different data source)
  - [ ] Discuss when to use which package

### Section 6: Summary and Discussion (unnumbered, 1-2 pages)

- [ ] Synthesis of contributions
- [ ] Current limitations:
  - [ ] Requires PEP725 data format (or conversion via `as.pep()`)
  - [ ] No built-in process-based models (intentional — complementary to phenor)
  - [ ] Interactive map requires Shiny dependencies
- [ ] Future directions:
  - [ ] Integration with climate reanalysis data (ERA5)
  - [ ] Species distribution modeling hooks
  - [ ] Multi-phase sequence analysis
  - [ ] Automated quality reports

### Computational Details (unnumbered section)

- [ ] R version, platform, OS
- [ ] List all package versions used
- [ ] CRAN URL for pep725
- [ ] GitHub repository URL
- [ ] `sessionInfo()` output

### Acknowledgments (unnumbered, American spelling)

- [ ] Funding sources
- [ ] PEP725 database acknowledgment
- [ ] Contributor acknowledgments

### References

- [ ] Compile BibTeX file `jss-pep725.bib`
- [ ] Ensure all titles use Title Style capitalization
- [ ] Protect proper nouns with braces: `{PEP725}`, `{R}`, `{Mann-Kendall}`, `{BBCH}`
- [ ] Include self-citations: Templ et al. (2018), Templ et al. (2026)
- [ ] Cite all compared packages
- [ ] Cite all statistical methods used

### Appendix

- [ ] **A: Complete function reference table** — all 45 functions with brief description
- [ ] **B: BBCH phase code reference** — common codes used in examples

---

## Figures & Tables Plan

### Figures
- [ ] **Figure 1:** Package workflow diagram (updated from JOSS paper)
- [ ] **Figure 2:** Quality grade distribution (bar chart from `plot(pep_quality(...))`)
- [ ] **Figure 3:** Outlier detection comparison (4 methods on same data)
- [ ] **Figure 4:** Phenological normals with uncertainty (from `plot(pheno_normals(...))`)
- [ ] **Figure 5:** Anomaly timeline (from `plot(pheno_anomaly(...))`)
- [ ] **Figure 6:** Elevation gradient with robust vs. OLS fit lines
- [ ] **Figure 7:** Synchrony across stations (from `plot(pheno_synchrony(...))`)
- [ ] **Figure 8:** Trend turning point detection (from `plot(pheno_trend_turning(...))`)
- [ ] **Figure 9:** PLS regression VIP plot (from `plot(pheno_pls(...))`)
- [ ] **Figure 10:** Station map colored by trend (from `pheno_map()`)

### Tables
- [ ] **Table 1:** Feature comparison with existing R packages
- [ ] **Table 2:** Outlier detection methods — when to use which
- [ ] **Table 3:** S3 classes and their methods
- [ ] **Table 4:** Performance benchmarks by dataset size

---

## JSS Style Compliance Checklist

- [ ] Use `\proglang{R}` for all programming language references (including title)
- [ ] Use `\pkg{pep725}` for all package references
- [ ] Use `\code{function_name()}` for all function/argument references
- [ ] Code input uses `CodeInput` environment with `R> ` prompt
- [ ] Code output uses `CodeOutput` environment
- [ ] Output width <= 70 characters
- [ ] `library("pep725")` with quotes (not `library(pep725)`)
- [ ] Paper title in Title Style; section headings in sentence style
- [ ] BibTeX titles in Title Style with protected proper nouns
- [ ] Use `\citet{}` / `\citep{}` (natbib), never `(\cite{})`
- [ ] Use `$p$~value`, `$t$~statistic`
- [ ] Figures: captions below, sentence style, end with period
- [ ] All figures/tables referenced in text with `Figure~\ref{}`
- [ ] No comments inside verbatim code blocks
- [ ] American English throughout (e.g., "Acknowledgments" not "Acknowledgements")

---

## Package Changes Needed for JSS

### Documentation improvements
- [ ] Ensure every function has a `\references{}` section citing the underlying method
- [ ] Add `@references` to roxygen for: `pheno_gradient` (Defila & Clot), `pheno_synchrony` (Menzel et al.), `pheno_trend_turning` (Sneyers), `pheno_pls` (Luedeling & Gassner)
- [ ] Review all `@details` sections for completeness — JSS reviewers check help files

### Testing improvements
- [ ] Add tests for `pheno_pls()`, `pheno_combine()`, `calc_thermal_sum()`
- [ ] Add tests for edge cases: empty subsets, single-station data, all-NA input
- [ ] Target: comprehensive coverage of all exported functions

### Performance
- [ ] Profile key functions on full synthetic dataset (7.24M rows)
- [ ] Optimize any bottlenecks found (document in paper)

### CRAN submission
- [ ] Fix `Author`/`Maintainer` fields in DESCRIPTION (or ensure `Authors@R` works)
- [ ] Resolve any remaining R CMD check NOTEs
- [ ] Submit to CRAN and get accepted before JSS submission

### Vignette integration
- [ ] After JSS acceptance, include paper as `vignettes/pep725-jss.Rnw`
- [ ] Use `\documentclass[nojss]{jss}` for vignette version
- [ ] Add `jss.cls` and `jss.bst` to `vignettes/`

---

## Changes Needed in JOSS Paper (paper/paper.md)

- [ ] **Fix `opedal2024advancing` reference** — currently fabricated, needs replacement with real citation (noted in TODO_JOSS_submission.md)
- [ ] **Cross-reference the JSS paper** once submitted/accepted — add note that methodological details are in the JSS companion paper
- [ ] **Update function count** if new functions are added before JOSS submission
- [ ] **Verify all 16 package comparisons** are still accurate (packages may have been updated)

---

## Timeline

| Phase | Task | Target |
|-------|------|--------|
| 1 | CRAN submission of pep725 | Q2 2026 |
| 2 | JOSS submission | July 2026 |
| 3 | JSS paper skeleton + Section 2 (Methods) | Q3 2026 |
| 4 | Section 3 (Design) + Section 4 (Illustrations) | Q4 2026 |
| 5 | Section 1 (Intro) + Section 5 (Comparison) + benchmarks | Q1 2027 |
| 6 | Internal review (Barbara Templ) | Q1 2027 |
| 7 | JSS submission | Q2 2027 |
| 8 | Expected review period | Q2 2027 - Q2 2028 |

---

## Reference: JSS Submission Checklist (from jstatsoft.org)

1. [ ] Manuscript formatted with `jss.cls` (compiled with `pdfLaTeX`)
2. [ ] Replication materials: `article.R` script + `code.html` output
3. [ ] Software source code (R package tarball or CRAN link)
4. [ ] All results exactly reproducible from replication script
5. [ ] Replication completes within ~1 hour
6. [ ] `\proglang{}`, `\pkg{}`, `\code{}` used consistently
7. [ ] GPL-compatible license (GPL-3: already satisfied)
8. [ ] Package on CRAN
