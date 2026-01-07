# Master Harmonized Dataset Report

**Date**: 2026-01-07 18:47:49

## Executive Summary

Successfully extracted and combined all harmonized variables into master datasets.

### Consolidated Variable Count: **6**

### Key Statistics

| Metric | Value |
|--------|-------|
| Concepts | 2 |
| **Consolidated Variables** | **6** |
| Variable-Wave Combinations | 34 |
| Total Data Points (Long Format) | 562698 |

## Consolidated Variables by Concept

| Concept | Variables | Wave Combinations |
|---------|-----------|------------------|
| democracy_satisfaction | 3 | 16 |
| economy | 3 | 18 |

## Output Datasets

### Wave-Specific Files (Wide Format - Recommended)
- `master_w1.rds` - 12217 rows × 5 columns
- `master_w2.rds` - 19798 rows × 6 columns
- `master_w3.rds` - 19436 rows × 6 columns
- `master_w4.rds` - 20667 rows × 6 columns
- `master_w5.rds` - 26951 rows × 5 columns
- `master_w6.rds` - 1242 rows × 6 columns

### Combined Format
- `master_long_format.rds` - Long format (562698 rows × 4 columns)
- `master_long_format.csv` - CSV export

### Metadata
- `master_variable_metadata.csv` - Variable definitions

## Complete Variable Listing

### democracy_satisfaction (3 variables)

- `democracy_satisfaction_dem_sat_national` (6 waves: w1,w2,w3,w4,w5,w6)
- `democracy_satisfaction_gov_sat_national` (6 waves: w1,w2,w3,w4,w5,w6)
- `democracy_satisfaction_hh_income_sat` (4 waves: w2,w3,w4,w6)

### economy (3 variables)

- `economy_econ_national_now` (6 waves: w1,w2,w3,w4,w5,w6)
- `economy_econ_change_1yr` (6 waves: w1,w2,w3,w4,w5,w6)
- `economy_econ_outlook_1yr` (6 waves: w1,w2,w3,w4,w5,w6)


## Data Structure

### Wave-Specific Datasets (Recommended)
Each wave file contains all available variables for that wave in wide format:
```r
# Load wave-specific data
w1_data <- readRDS('outputs/master_w1.rds')
# Each column is a consolidated variable
# Each row is a respondent
```

### Long Format
Alternative stacked format with columns:
- `variable` - variable name
- `wave` - wave (w1-w6)
- `row_id` - respondent ID
- `value` - variable value

## Usage Examples

```r
# Load and inspect
w1 <- readRDS('outputs/master_w1.rds')
dim(w1)  # Check dimensions
names(w1)  # List variables

# Cross-wave analysis
w1_vars <- readRDS('outputs/master_w1.rds')
w2_vars <- readRDS('outputs/master_w2.rds')

# Get specific variable across waves
econ_w1 <- w1_vars$economy_econ_national_now
econ_w2 <- w2_vars$economy_econ_national_now
```

---

**Report Generated**: 2026-01-07 18:47:49
**Total Consolidated Variables: 6**

