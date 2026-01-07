# Suggested Commands

## Environment Setup

### Python (uv)
```bash
# Install uv
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install Python and dependencies
uv python install 3.12.12
uv sync

# Activate virtual environment
source .venv/bin/activate
```

### R (renv)
```bash
# Restore R packages
R -e 'renv::restore()'
```

### Pre-commit
```bash
# Install hooks
pre-commit install

# Run all hooks manually
pre-commit run --all-files
```

## Building & Rendering

### Quarto
```bash
# Preview with live reload
quarto preview

# Render all papers
quarto render

# Render specific paper
quarto render papers/paper-two/paper.qmd

# Render with specific profile
quarto render --profile dev
quarto render --profile publish
```

### R Build Pipeline
```bash
# Run R data processing pipeline
Rscript scripts/build.R
```

### Python Build Pipeline
```bash
# Run Python build pipeline
python scripts/build.py
```

## Code Quality

### R Linting
```bash
# Run lintr on R code
Rscript src/r/utils/lint.R
```

### Python Formatting & Linting
```bash
# Format with black
black src/python/

# Lint with ruff
ruff check src/python/

# Type check
mypy src/python/
```

## Testing
```bash
# Python tests
pytest

# R tests (manual)
Rscript src/r/utils/test_identity_functions.R
```

## Data Operations

### Loading SPSS/Stata files in R
```r
library(haven)
data <- read_sav("data/raw/filename.sav")  # SPSS
data <- read_dta("data/raw/filename.dta")  # Stata
```

### Project paths in R
```r
library(here)
data_path <- here("data", "processed", "filename.rds")
```

## Git
```bash
# Check status
git status

# Create feature branch
git checkout -b feature/branch-name

# Stage and commit
git add .
git commit -m "descriptive message"
```

## Utility (macOS/Darwin)
```bash
# List files
ls -la

# Find files
find . -name "*.R" -type f

# Search content
grep -r "pattern" src/

# Navigate
cd path/to/dir
pwd
```
