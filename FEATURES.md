# Template Features

This project was generated from the **cookiecutter-quarto-socialscience** template and includes the following features:

## 🐍 Python Environment (uv)

- **Python 3.12.12** configured via `.python-version` and `.uvrc`
- **UV package manager** for fast, reliable dependency management
- **pyproject.toml** with curated scientific computing dependencies:
  - Data: pandas, numpy
  - Visualization: matplotlib, seaborn
  - Analysis: scikit-learn, statsmodels
  - Notebooks: jupyter, ipykernel
- **Development tools**: pytest, black, ruff, mypy

## 📊 R Environment (renv)

- **renv.lock** for reproducible R package management
- **Pre-configured packages**:
  - tidyverse, haven, here
  - modelsummary, gtsummary for tables
- **Utility functions** in `src/r/utils/`

## 📝 Quarto Publishing

- **Three environment profiles**:
  - Default: Balanced settings
  - Dev: Verbose output, code visible, no freezing
  - Publish: Production-ready, clean output
- **Multi-format output**: HTML, PDF, DOCX
- **APA citation style** with bibliography support
- **Dark/light theme** support

## 🤖 CI/CD Pipeline

- **GitHub Actions** workflow for automatic rendering
- **UV integration** for fast Python setup
- **renv integration** for R package management
- **Automated deployment** to GitHub Pages
- **Pull request previews**

## 📂 Project Structure

```
project/
├── data/
│   ├── raw/          # Original, immutable data
│   ├── interim/      # Intermediate processing
│   └── processed/    # Analysis-ready data
├── papers/
│   ├── paper-one/    # Complete paper template
│   └── paper-two/    # Second paper template
├── src/
│   ├── python/       # Reusable Python modules
│   └── r/            # Reusable R functions
├── notebooks/        # Exploratory analysis
├── outputs/          # Generated figures/tables
└── scripts/          # Build automation
```

## 📚 Documentation

- **Comprehensive READMEs** for each directory
- **Data management guide** with dictionary templates
- **Notebook usage guidelines**
- **CITATION.cff** for academic citation
- **Code comments and docstrings**

## 🔧 Development Tools

- **Pre-commit hooks** for code quality:
  - Black formatting
  - Ruff linting
  - YAML/Markdown formatting
  - Prevent commits to main
- **.editorconfig** for consistent coding style
- **.gitignore** optimized for Python/R/Quarto
- **.gitattributes** for proper line endings

## 📋 Paper Templates

- **Full academic paper structure**:
  - Abstract, Introduction, Literature Review
  - Methods, Results, Discussion, Conclusion
  - Integrated code chunks (R and Python)
  - Figure and table examples
  - Cross-references and citations
- **Appendix template** with robustness checks
- **Bibliography management** with example entries

## 🛠️ Build Automation

- **Python build script** (`scripts/build.py`)
  - Pipeline orchestration
  - Error handling
  - Progress reporting
- **R build script** (`scripts/build.R`)
  - Data processing workflow
  - Integrated with here package
- **Setup script** (`scripts/setup_project.sh`)
  - One-command project initialization
  - Git, uv, pre-commit setup

## 🔬 Utility Functions

### Python (`src/python/utils.py`)
- `load_data()` - Smart data loading (CSV, Parquet, Stata, SPSS, Excel)
- `save_data()` - Format-agnostic data saving
- `describe_data()` - Comprehensive descriptive statistics
- `get_project_root()` - Path management

### R (`src/r/utils/`)
- `load_data()` / `save_data()` - Data I/O
- `describe_data()` - Descriptive statistics
- `make_table1()` - Publication-ready Table 1
- `freq_table()` - Quick frequency tables

## ✨ Additional Features

- **Cookiecutter variables** throughout templates
- **Post-generation hook** for automatic setup
- **GitHub-ready** with workflow templates
- **Academic citation** support via CITATION.cff
- **Multi-paper support** out of the box
- **Data validation** structure
- **Type hints and documentation** in code
- **Cross-platform** compatibility (macOS, Linux, Windows)

## 🚀 Quick Start

```bash
# 1. Activate Python environment
uv sync
source .venv/bin/activate

# 2. (Optional) Set up R environment
R -e 'renv::restore()'

# 3. Install pre-commit hooks
pre-commit install

# 4. Preview your work
quarto preview

# 5. Render all outputs
quarto render
```

## 📖 Learn More

- **Quarto**: https://quarto.org
- **UV**: https://astral.sh/uv
- **renv**: https://rstudio.github.io/renv/
- **Pre-commit**: https://pre-commit.com

---

**Template Version**: 1.0.0  
**Generated**: now
