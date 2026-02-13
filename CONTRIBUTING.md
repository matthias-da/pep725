# Contributing to pep725

Thank you for your interest in contributing to pep725! This document provides guidelines for contributing to the package.

## Reporting Bugs

If you find a bug, please open an issue on GitHub with:

- A clear, descriptive title
- A minimal reproducible example (reprex)
- Your R version (`sessionInfo()`)
- The pep725 version (`packageVersion("pep725")`)

Use the [GitHub issue tracker](https://github.com/matthias-da/pep725/issues) to report bugs.

## Suggesting Features

Feature requests are welcome! Please open an issue with:

- A clear description of the feature
- The use case / motivation
- Example of how it would work (if applicable)

## Pull Requests

### Before You Start

1. Open an issue to discuss the change
2. Fork the repository
3. Create a feature branch (`git checkout -b feature/your-feature`)

### Code Style

- Use `data.table` syntax for data manipulation
- Follow [tidyverse style guide](https://style.tidyverse.org/) for R code
- Use roxygen2 for documentation with markdown enabled
- Include `@author` tags in function documentation

### Documentation

- All exported functions must have roxygen2 documentation
- Include `@examples` (use `\donttest{}` for examples requiring internet)
- Update vignettes if functionality changes

### Testing

- Add tests for new functionality in `tests/testthat/`
- Run `devtools::check()` before submitting
- Ensure no new warnings or errors

### Commit Messages

- Use clear, descriptive commit messages
- Reference issue numbers where applicable (e.g., "Fix #42")

### Submitting

1. Push your branch to your fork
2. Open a pull request against `main`
3. Describe the changes and link to relevant issues
4. Wait for CI checks to pass

## Development Setup

```r
# Clone the repository
# git clone https://github.com/matthias-da/pep725.git

# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Load package for development
devtools::load_all()

# Run checks
devtools::check()

# Build documentation
devtools::document()
```

## Code of Conduct

Please note that the pep725 project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Questions?

If you have questions, feel free to open an issue or contact the maintainers.

Thank you for contributing!
