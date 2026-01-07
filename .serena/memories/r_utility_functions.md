# R Utility Functions Reference

## Location: src/r/utils/

### recoding.R
**Reversal Functions** (higher value = more of concept after reversal)
- `safe_reverse_3pt(x, data, var_name, missing_codes, validate_all)` - Reverse 3-point scale
- `safe_reverse_4pt(x, data, var_name, missing_codes, validate_all)` - Reverse 4-point scale
- `safe_reverse_5pt(x, data, var_name, missing_codes, validate_all)` - Reverse 5-point scale

**Identity Functions** (no reversal, only missing code handling)
- `safe_3pt_none(x, data, var_name, missing_codes, validate_all)` - Clean 3-point scale
- `safe_4pt_none(x, data, var_name, missing_codes, validate_all)` - Clean 4-point scale
- `safe_5pt_none(x, data, var_name, missing_codes, validate_all)` - Clean 5-point scale

**Scale Collapse**
- `safe_6pt_to_4pt(x, data, var_name, missing_codes, needs_reversal, validate_all)` - Collapse 6-pt to 4-pt

**Common Parameters**:
- `x`: numeric vector to recode
- `data`: dataframe containing the variable (for label extraction)
- `var_name`: variable name in data (string)
- `missing_codes`: values to convert to NA (default: c(-1, 0, 7, 8, 9))
- `validate_all`: character vector of regex patterns to validate against question label

**Usage Example**:
```r
data %>%
  mutate(
    trust_gov_r = safe_reverse_4pt(
      trust_gov,
      data = .,
      var_name = "trust_gov",
      validate_all = c("trust", "government")
    )
  )
```

### data_prep_helpers.R
**Reliability & Composites**
- `create_validated_composite(data, vars, composite_name, min_alpha, method, min_valid)` - Calculate reliability and create validated composite variable
- `normalize_0_1(x)` - Min-max normalization to 0-1 scale
- `standardize_z(x)` - Z-score standardization

**Missing Data**
- `report_missing(data, vars, by_country, label)` - Generate missing data report
- `batch_clean_missing(data, vars, missing_codes)` - Clean missing codes across multiple variables

**Validation**
- `validate_range(data, vars, min, max, label)` - Hard validation for expected range
- `verify_recoding(data, original_vars, recoded_vars, expected_reversal)` - Verify recoding worked

**Descriptives**
- `describe_by_country(data, vars, label)` - Descriptive statistics by country

### helpers.R
- `print_var_info(data, var)` - Print variable metadata (label, values, distribution)
- `summarise_by_country(data, vars, country_var)` - Summarise variables by country
- `check_distribution(data, var, country_var)` - Check variable distribution by country
- `safe_rowmeans(...)` - Row means that converts NaN to NA
- `search_variables(data, keyword, search_in)` - Search for variables by name/label
- `search_across_waves(keyword, file_vector, search_in)` - Search across multiple wave files

### Other Files
- `clear_env.R` - Environment clearing utilities
- `recoding.R` - Variable recoding functions
- `validation.R` - Data validation utilities
- `descriptive_stats.R` - Descriptive statistics functions
- `composites.R` - Composite variable creation
- `load_data.R` - Data loading utilities
- `_load_functions.R` - Function loader helper

## Location: src/r/survey/
- `missing_report.R` - Comprehensive missing data reporting
- `codebook_tools.R` - Codebook generation utilities
- `harmonize_vars.R` - Variable harmonization across waves
- `load_data.R` - Survey data loading

## Usage Pattern
```r
# Load all utility functions
source(here::here("src/r/utils/_load_functions.R"))

# Or load specific file
source(here::here("src/r/utils/data_prep_helpers.R"))
```
