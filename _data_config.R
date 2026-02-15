# _data_config.R
# Central configuration for loading harmonized survey data from survey-data-prep repo.
# Source this file in any analysis script that needs harmonized data.

survey_data_prep <- "/Users/jeffreystark/Development/Research/survey-data-prep"
abs_harmonized_path <- file.path(survey_data_prep, "data/processed/abs_harmonized.rds")
wvs_harmonized_path <- file.path(survey_data_prep, "data/processed/wvs_harmonized.parquet")
lbs_harmonized_path <- file.path(survey_data_prep, "data/processed/lbs_harmonized.rds")
