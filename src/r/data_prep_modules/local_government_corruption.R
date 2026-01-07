# Harmonize local government corruption perception variable
# Loads YAML config and applies consistent harmonization logic
# Handles scale reversal and heterogeneity across waves (W1-W2 reversed vs W3-W6, W6 extended scale)

library(dplyr)
library(tidyr)
library(yaml)
library(here)

source(here::here("src", "r", "data_prep_modules", "0_load_waves.R"))
source(here::here("src", "r", "data_prep_modules", "1_harmonize_funs.R"))

cat("\n=== LOCAL GOVERNMENT CORRUPTION HARMONIZATION ===\n")

# ============================================================================
# STEP 1: Load waves
# ============================================================================

cat("\nStep 1: Loading survey waves...\n")
waves <- load_waves()

# ============================================================================
# STEP 2: Load YAML config
# ============================================================================

cat("\nStep 2: Reading harmonization config...\n")
yaml_path <- here::here("src", "config", "harmonize", "local_government_corruption.yml")
config <- yaml::read_yaml(yaml_path)

cat(sprintf("Found %d variables in config\n", length(config$variables)))

# Define missing codes to treat as NA
# These vary by wave but are consolidated here
missing_codes <- c(-2, -1, 0, 7, 8, 9, 98, 99)
cat(sprintf("Will treat these codes as NA: %s\n", paste(missing_codes, collapse = ", ")))


# ============================================================================
# STEP 3: Harmonize variable
# ============================================================================

cat("\nStep 3: Harmonizing variable...\n")

var_spec <- config$variables[[1]]
var_name <- var_spec$id
var_label <- var_spec$description

cat(sprintf("\n  %s (%s)...\n", var_name, var_label))

# Extract source variables for each wave
harmonized_list <- list()

for (wave_name in names(var_spec$source)) {
  var_code <- var_spec$source[[wave_name]]
  
  # Get harmonization method for this wave
  if (!is.null(var_spec$harmonize$by_wave[[wave_name]])) {
    method <- var_spec$harmonize$by_wave[[wave_name]]$method
    fn_name <- var_spec$harmonize$by_wave[[wave_name]]$fn
  } else {
    method <- var_spec$harmonize$default$method
    fn_name <- NULL
  }

  # Validate that we have the right variable
  if (!(var_code %in% names(waves[[wave_name]]))) {
    warning(sprintf("    ⚠️  %s:%s not found", wave_name, var_code))
    next
  }

  # Extract raw variable
  raw_vec <- extract_var(waves, wave_name, var_code)
  
  # Get wave-specific missing codes if defined
  wave_missing_codes <- var_spec$missing_codes[[wave_name]]
  if (!is.null(wave_missing_codes)) {
    raw_vec[raw_vec %in% wave_missing_codes] <- NA_real_
  } else {
    raw_vec[raw_vec %in% missing_codes] <- NA_real_
  }

  # Apply harmonization method for this wave
  if (method == "identity") {
    harmonized_vec <- raw_vec
  } else if (method == "r_function" && !is.null(fn_name)) {
    # Call the specified R function dynamically
    harmonized_vec <- do.call(fn_name, list(raw_vec))
  } else {
    harmonized_vec <- raw_vec
  }

  cat(sprintf("    ✓ %s: %s (n=%d valid)\n", 
              wave_name, var_code, sum(!is.na(harmonized_vec))))

  # Create wave dataframe with row ID for proper alignment
  wave_df <- tibble(
    wave = wave_name,
    row_id = seq_len(length(harmonized_vec)),
    !!var_name := harmonized_vec,
    source_var = var_code
  )

  harmonized_list[[wave_name]] <- wave_df
}

# Combine waves for this variable
if (length(harmonized_list) > 0) {
  var_data <- bind_rows(harmonized_list)
  cat(sprintf("    ✅ %d observations across %d waves\n",
              nrow(var_data), length(harmonized_list)))

  # Validate
  validate_harmonization(var_data[[var_name]], expected_min = 1, expected_max = 4, var_name = var_name)
}

# ============================================================================
# STEP 4: Create final dataset with identifiers
# ============================================================================

cat("\n\nStep 4: Combining with respondent identifiers...\n")

# Create wave template
wave_template <- var_data %>% select(wave, row_id, source_var)

# Add identifiers from original waves
id_data_list <- list()
for (wave_name in names(waves)) {
  wave_df <- waves[[wave_name]]

  # Find country column (case-insensitive)
  country_col <- grep("^country$", names(wave_df), ignore.case = TRUE, value = TRUE)[1]
  idnumber_col <- grep("^idnumber$", names(wave_df), ignore.case = TRUE, value = TRUE)[1]

  if (!is.na(country_col) && !is.na(idnumber_col)) {
    id_data_list[[wave_name]] <- tibble(
      wave = wave_name,
      row_id = seq_len(nrow(wave_df)),
      country = as.numeric(wave_df[[country_col]]),
      idnumber = as.numeric(wave_df[[idnumber_col]])
    )
  } else {
    cat(sprintf("⚠️  Wave %s: Could not find country/idnumber columns\n", wave_name))
  }
}

# Combine all id data
if (length(id_data_list) > 0) {
  all_ids <- bind_rows(id_data_list)
  
  # Join to harmonized_data
  harmonized_data <- wave_template %>%
    left_join(var_data %>% select(wave, row_id, all_of(var_name)), 
              by = c("wave", "row_id")) %>%
    left_join(all_ids, by = c("wave", "row_id"))
}


# ============================================================================
# STEP 5: Save harmonized dataset
# ============================================================================

cat("\nStep 5: Saving results...\n")

output_path <- here::here("data", "processed", "local_government_corruption_harmonized.rds")
saveRDS(harmonized_data, output_path)

cat(sprintf("✅ Saved to: %s\n", output_path))
cat(sprintf("   Dimensions: %d rows × %d columns\n", nrow(harmonized_data), ncol(harmonized_data)))


# ============================================================================
# STEP 6: Summary report
# ============================================================================

cat("\n=== SUMMARY REPORT ===\n")

cat("\nVariable harmonized:\n")
valid <- sum(!is.na(harmonized_data[[var_name]]))
total <- nrow(harmonized_data)
pct <- round(100 * valid / total, 1)
cat(sprintf("  - %s: %d/%d (%.1f%%)\n", var_name, valid, total, pct))

cat("\nWaves represented:\n")
wave_counts <- harmonized_data %>%
  group_by(wave) %>%
  summarise(n = n(), .groups = "drop")
for (i in seq_len(nrow(wave_counts))) {
  cat(sprintf("  - %s: %d rows\n", wave_counts$wave[i], wave_counts$n[i]))
}

# Check for unique respondents
cat("\nRespondent uniqueness:\n")
unique_respondents <- harmonized_data %>%
  filter(!is.na(country) & !is.na(idnumber)) %>%
  distinct(wave, country, idnumber) %>%
  nrow()
total_rows_with_ids <- harmonized_data %>%
  filter(!is.na(country) & !is.na(idnumber)) %>%
  nrow()
duplicates <- total_rows_with_ids - unique_respondents
cat(sprintf("  Total rows with identifiers: %d\n", total_rows_with_ids))
cat(sprintf("  Unique respondents (wave, country, idnumber): %d\n", unique_respondents))
if (duplicates > 0) {
  cat(sprintf("  Potential duplicate respondents: %d\n", duplicates))
}

cat("\n✅ Harmonization complete!\n")


# ============================================================================
# STEP 7: Strip haven labels
# ============================================================================

cat("\nStep 7: Removing haven labels...\n")
harmonized_data <- strip_haven_labels(harmonized_data)
cat("✅ Haven labels removed\n\n")

# Re-save without labels
saveRDS(harmonized_data, output_path)
cat(sprintf("✅ Final dataset saved: %s\n\n", output_path))
