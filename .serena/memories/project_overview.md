# EconDev-AuthPref Project Overview

## Purpose
Research repository for survey-based social science projects. The project uses Quarto as the authoritative build system for rendering academic papers with integrated R and Python code.

## Tech Stack
- **Primary Language**: R 4.3.0
- **Secondary Language**: Python 3.12.12
- **Document System**: Quarto (manuscript project type)
- **Package Management**:
  - R: renv (renv.lock)
  - Python: uv (pyproject.toml)
- **CI/CD**: GitHub Actions
- **Pre-commit**: Code quality hooks

## Key R Packages
- tidyverse 2.0.0 (data manipulation)
- haven 2.5.3 (SPSS/Stata file reading)
- here 1.0.1 (path management)
- modelsummary 1.4.2 (regression tables)
- gtsummary 1.7.2 (summary tables)
- psych (reliability analysis)
- assertr (data validation)

## Key Python Packages
- pandas, numpy (data)
- matplotlib, seaborn (visualization)
- scikit-learn, statsmodels (analysis)
- jupyter, ipykernel (notebooks)

## Project Structure
```
├── data/
│   ├── raw/          # Original, immutable data
│   ├── interim/      # Intermediate processing
│   └── processed/    # Analysis-ready data
├── papers/
│   ├── paper-two/    # Paper with qmd, refs.bib, appendix
│   └── paper-three/  # Second paper
├── src/
│   ├── python/       # Python modules (ingest, export, validation)
│   └── r/
│       ├── utils/    # Helper functions, composites, recoding
│       ├── models/   # Statistical models
│       └── survey/   # Survey-specific tools
├── notebooks/        # Exploratory analysis
├── outputs/
│   ├── figures/      # Generated figures
│   └── tables/       # Generated tables
└── scripts/          # Build automation (build.R, build.py)
```

## Key Files
- `_quarto.yml` - Main Quarto config (renders papers/*/paper.qmd)
- `_quarto-dev.yml` - Dev profile (verbose, no freeze)
- `_quarto-publish.yml` - Production profile
- `renv.lock` - R package lockfile
- `pyproject.toml` - Python config (uv managed)
- `.pre-commit-config.yaml` - Pre-commit hooks

## Output Formats
- HTML (flatly/darkly themes, ToC, code folding)
- PDF (LaTeX article class, biblatex)
- DOCX

## Citation Style
APA format via `apa.csl`
