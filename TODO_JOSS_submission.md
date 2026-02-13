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
- [ ] **Update GitHub PAT to include `workflow` scope** - Required to push workflow files
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

- [ ] **Summary section**: Review for clarity and completeness
  - [ ] Verify PEP725 statistics are current (13 million obs, 30 countries, 265 species, 46 phenophases)
  - Note: statistics cite @templ2018pep725 and @templetal2026a — check against the 2026 paper

- [ ] **Research impact statement**: Strengthen with specifics
  - [ ] Add concrete examples of publications using pep725
  - [ ] Mention specific research projects or collaborations
  - Currently mentions @templetal2026a and adoption in "climate impact studies, ecological analyses, and teaching"
  - Funding (BNF/Bern) and WSL support are already in Acknowledgements

### Paper Technical Checks

- [x] **Logo image**: `pep725_logo_cpt.png` exists in `pep725_JOSS-Paper/` ✅
- [ ] **Bibliography**: 16 references in `paper.bib`
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

### Co-author Review

- [ ] **Barbara Templ review**: Co-author should review and approve:
  - [ ] Accuracy of scientific content
  - [ ] Affiliation correctness (WSL for Barbara, FHNW for Matthias)
  - [ ] AI disclosure statement

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
| Research impact specifics | ❌ Not started | Medium |
| Logo image check | ✅ Verified (exists) | Low |
| Bibliography DOI verification | ✅ All 11 DOIs valid (except opedal) | Low |
| CRAN pkg version check | ❌ Not verified (5 refs) | Low |
| ROR identifiers check | ✅ Fixed | Low |
| Fix opedal2024advancing ref | ❌ Fabricated reference | **High** |
| **GitHub Issues** | | |
| Open issues | 13 open | Various |
| **R CMD Check** | | |
| Fix NOTEs | 5 NOTEs | Medium |
| Co-author review (Barbara) | ❌ Not started | High |

---

## GitHub Issues (Open)

### Documentation & Content
- [ ] **#32** Refine the README file
- [ ] **#30** Add help for non-PEP725 data users (getting-started.Rmd)
- [ ] **#28** Take back "Expected Values and Troubleshooting" section (if time for reference search)
- [ ] **#27** Remove Part 2
- [ ] **#26** Detailed Context - delete

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
- [ ] Add `.github` to `.Rbuildignore`
- [ ] Add non-standard files to `.Rbuildignore`: `CONTRIBUTING.md`, `TODO_JOSS_submission.md`, `pep725_JOSS-Paper`, `CLAUDE.md`
- [ ] Fix `simulate_pep.R:52` - change `gam()` to `mgcv::gam()`
- [ ] Add global variable bindings for `grade_num`, `worst_grade_num` in `plot.pep_quality()`
- [ ] Add global variable binding for `..na_cols` in `print.pheno_anomaly()`

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
