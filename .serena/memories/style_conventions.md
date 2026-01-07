# Code Style and Conventions

## Editor Configuration (.editorconfig)
- **Line endings**: LF (Unix-style)
- **Final newline**: Always insert
- **Charset**: UTF-8
- **Trailing whitespace**: Trim (except markdown/csv)

## R Code Style
- **Indentation**: 2 spaces
- **Naming**: snake_case for variables and functions
- **Packages**: tidyverse style (pipe %>%, dplyr verbs)
- **Documentation**: Roxygen-style comments with `#'`
- **Sections**: Use comment banners with `# ===` separators

### R Function Patterns
```r
#' Description of function
#'
#' @param data Dataframe
#' @param vars Character vector of variable names
#' @return Description of return value
function_name <- function(data, vars, ...) {
  data %>%
    mutate(
      across(all_of(vars), ~ ...)
    )
}
```

### R Data Validation
- Use `assertr` for pipeline validation
- Hard validation with `stop()` for critical errors
- Warning with `warning()` for non-critical issues
- Console feedback with emoji indicators (✓, ⚠, ❌)

## Python Code Style
- **Indentation**: 4 spaces
- **Line length**: 88 characters (black default)
- **Formatting**: black
- **Linting**: ruff (rules: E, F, I, N, W, B, C4, UP)
- **Type hints**: Required (mypy enforced)
- **Imports**: isort with black profile

## Quarto/Markdown Style
- **Indentation**: 2 spaces
- **Code chunks**: Use labels with `#| label: name`
- **Cross-references**: Use `@sec-`, `@fig-`, `@tbl-` prefixes
- **Citations**: Use `[@key]` format

## YAML/JSON Style
- **Indentation**: 2 spaces

## File Naming
- R files: `snake_case.R`
- Python files: `snake_case.py`
- Quarto: `paper.qmd`, `appendix.qmd`
- Data: Descriptive names with processing stage

## Git Practices
- Pre-commit hooks prevent commits to main/master
- Feature branches required
