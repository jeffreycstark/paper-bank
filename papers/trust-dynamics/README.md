# The Sausage Paper

**Full Title**: "I Don't Care How the Sausage Gets Made: Democratic Pragmatism in Asia"

## Paper Overview

This paper examines attitudes toward democracy vs. economic development across 16 Asian countries using six waves of Asian Barometer Survey data (2001-2020). The central puzzle: citizens in democracies are significantly *more* willing to prioritize economic outcomes over democratic process than citizens in authoritarian regimes.

## Key Hypotheses

1. **Grass is Greener (H1)**: Citizens in authoritarian regimes express higher abstract support for democracy while citizens in democracies show greater willingness to trade democracy for economic outcomes.

2. **Education × Regime Interaction (H2)**: In democracies, middle-educated citizens are most pragmatic (inverted-U). In authoritarian regimes, education increases democratic idealization (flat or positive).

3. **Elite Divergence (H3)**: Over time, educated elites across regime types diverge rather than converge in democratic orientations.

4. **Resignation Effect (H4)**: In long-entrenched authoritarian regimes (Cambodia, Vietnam), the "grass is greener" effect weakens as resignation replaces aspiration.

## Directory Structure

```
papers/sausage-paper/
├── README.md                    # This file
├── analysis/                    # All analysis code
│   ├── 00_master_runner.qmd     # Run all analyses in sequence
│   ├── 01_data_preparation.qmd  # Data subsetting and preparation
│   ├── 02_descriptive_analysis.qmd
│   ├── 03_hypothesis_testing.qmd
│   ├── 04_robustness_checks.qmd
│   ├── 05_supplementary_analyses.qmd
│   ├── data_prep_modules/       # Modular R scripts
│   │   ├── 00_run_all.R         # Master runner for modules
│   │   ├── 01_load_data.R       # Load combined dataset
│   │   ├── 02_select_variables.R
│   │   ├── 03_select_countries.R
│   │   ├── 04_create_composites.R
│   │   ├── 05_recode_variables.R
│   │   ├── 06_handle_missing.R
│   │   └── 07_export_analysis_data.R
│   ├── figures/                 # Generated figures
│   └── tables/                  # Generated tables
├── manuscript/                  # Paper manuscript
│   ├── sausage_paper.qmd        # Main Quarto manuscript
│   ├── references.bib           # Bibliography
│   └── apa.csl                  # Citation style (if needed)
├── supplementary/               # Supplementary materials
│   └── appendix.qmd
└── _quarto.yml                  # Quarto project configuration
```

## Data Flow

```
Harmonized Dataset (survey-data-prep/data/processed/abs_harmonized.rds)
    ↓
01_data_preparation.qmd (via data_prep_modules/)
    ↓
Analysis-ready subset (analysis/sausage_analysis.rds)
    ↓
02_descriptive_analysis.qmd → figures/, tables/
    ↓
03_hypothesis_testing.qmd → models, effect sizes
    ↓
04_robustness_checks.qmd → sensitivity tests
    ↓
manuscript/sausage_paper.qmd → Final paper
```

## Key Variables

### Dependent Variables
- `econ_over_democracy`: Priority economic development over democracy (binary/ordinal)
- `democracy_solves_problems`: Democracy can solve problems (Likert)

### Independent Variables
- `education_level`: Respondent education (ordinal)
- `regime_type`: Democracy vs. Autocracy (from V-Dem)
- `country`, `wave`: Grouping variables

### Controls
- `age`, `gender`, `urban_rural`, `income`

## Running the Analysis

1. **Full pipeline** (recommended):
   ```r
   quarto::quarto_render("papers/sausage-paper/analysis/00_master_runner.qmd")
   ```

2. **Individual scripts**:
   ```r
   # Data preparation only
   source("papers/sausage-paper/analysis/data_prep_modules/00_run_all.R")

   # Or render specific QMD
   quarto::quarto_render("papers/sausage-paper/analysis/02_descriptive_analysis.qmd")
   ```

## Output Files

| File | Description |
|------|-------------|
| `analysis/sausage_analysis.rds` | Analysis-ready dataset |
| `analysis/figures/*.png` | Publication-ready figures |
| `analysis/tables/*.html` | Formatted tables |
| `manuscript/sausage_paper.pdf` | Final paper |

## Status

- [ ] Data preparation modules
- [ ] Descriptive analysis
- [ ] Hypothesis testing (H1-H4)
- [ ] Robustness checks
- [ ] Manuscript draft
- [ ] Supplementary materials

## Citation

Stark, J. (2025). "I Don't Care How the Sausage Gets Made: Democratic Pragmatism in Asia." Working Paper.
