# EconDev-AuthPref Project Overview

## Purpose
Papers-only repository for survey-based social science research. Consumes harmonized survey data from the `survey-data-prep` repo. All harmonization infrastructure (YAML engine, codebook tools, raw data, pipeline scripts) has been extracted to survey-data-prep.

## Tech Stack
- **Primary Language**: R
- **Document System**: Quarto (manuscript project type)
- **Package Management**: R: renv (renv.lock)
- **Data Source**: survey-data-prep repo via `_data_config.R`

## Key R Packages
- tidyverse (data manipulation)
- haven (SPSS/Stata file reading)
- here (path management)
- arrow (parquet I/O)
- nnet (multinomial logit)
- kableExtra (tables)
- modelsummary (regression tables)

## Project Structure
```
├── _data_config.R    # Central paths to survey-data-prep harmonized data
├── data/
│   ├── external/     # V-Dem scores, COVID economic data
│   └── processed/    # Paper-specific derived data only
├── papers/
│   ├── meaning-of-democracy-revision/
│   ├── hong-kong-democratic-erosion/
│   ├── trust-dynamics/
│   └── surveys-and-false-positives/
├── outputs/
│   ├── figures/
│   └── tables/
└── scripts/
    └── get_vdem_scores.R
```

## Data Loading Pattern
```r
source(here("_data_config.R"))
d <- readRDS(abs_harmonized_path)    # ABS data
wvs <- arrow::read_parquet(wvs_harmonized_path)  # WVS data
lbs <- readRDS(lbs_harmonized_path)  # LBS data
```

## Key Files
- `_quarto.yml` - Main Quarto config
- `renv.lock` - R package lockfile
- `apa.csl` - APA citation style
