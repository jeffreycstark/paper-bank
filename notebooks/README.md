# Notebooks

This directory contains exploratory analysis notebooks.

## Purpose

Notebooks are for:
- **Exploratory data analysis** - initial data exploration
- **Prototyping** - testing analytical approaches
- **Visualization development** - creating and refining plots
- **Ad-hoc analyses** - one-off investigations

## Important Notes

⚠️ **Notebooks are NOT for reproducible research outputs**

- Notebooks should not be cited in papers
- Production code must be moved to `src/`
- Final analyses should be in Quarto documents in `papers/`

## Organization

Use descriptive names with dates:
- `2024-01-15_initial-exploration.ipynb`
- `2024-02-01_demographic-patterns.Rmd`
- `2024-03-10_model-testing.qmd`

## Jupyter Setup

```bash
# Activate virtual environment
source .venv/bin/activate

# Install Jupyter kernel
uv pip install ipykernel
python -m ipykernel install --user --name=econdev-authpref

# Launch Jupyter
jupyter lab
```

## R Markdown Setup

```r
# Install required packages
install.packages(c("rmarkdown", "knitr"))

# Open RStudio and create new R Markdown document
```

## Best Practices

1. **Clear outputs before committing** to version control
2. **Document insights** discovered during exploration
3. **Migrate validated code** to `src/` for reuse
4. **Keep notebooks focused** on one topic
