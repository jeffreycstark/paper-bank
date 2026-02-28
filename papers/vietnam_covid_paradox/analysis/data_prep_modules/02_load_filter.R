# ==============================================================================
# MODULE 02: DATA LOADING AND COUNTRY FILTERING
# ==============================================================================
# Purpose: Load raw AB Wave 6 data and filter to 7 countries of interest
# Dependencies: Module 01 (CONFIG object must exist)
# Inputs: data/processed/w6_all_countries_merged.rds
# Outputs: ab_selected (filtered dataframe), country_name (factor variable)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 02: DATA LOADING AND FILTERING\n")
cat(rep("=", 70), "\n\n")

# ============================================
# LOAD RAW DATA
# ============================================

cat("[loading] Reading Asian Barometer Wave 6 data...\n")

ab_whole <- read_rds(here("data", "processed", "w6_all_countries_merged.rds"))

# ============================================
# STRIP HAVEN LABELS
# ============================================
# Remove haven's labelled class to prevent downstream issues
# with filtering, joins, and model outputs. Raw labelled data
# remains available in w6_all_countries_merged.rds if needed.

cat("[cleaning] Stripping haven labels from all variables...\n")

ab_whole <- ab_whole %>%
  mutate(across(where(haven::is.labelled), ~ as.vector(haven::zap_labels(.))))

cat("✓ Haven labels removed - all variables now base R types\n")

# Hard validation: ensure data loaded
stopifnot("Data file is empty" = nrow(ab_whole) > 0)
stopifnot("Country variable missing" = "country" %in% names(ab_whole))

cat("✓ Raw data loaded:", nrow(ab_whole), "rows x", ncol(ab_whole), "columns\n")

# ============================================
# FILTER TO COUNTRIES OF INTEREST
# ============================================

cat("\n[filtering] Countries in full dataset:\n")
print(table(ab_whole$country))

cat("\n[filtering] Filtering to", length(CONFIG$countries_of_interest), "countries...\n")

ab_selected <- ab_whole %>%
  filter(country %in% CONFIG$countries_of_interest) %>%
  mutate(
    country_name = factor(
      as.character(country),
      levels = names(CONFIG$country_labels),
      labels = CONFIG$country_labels
    )
  )

# Hard validation: ensure filtering worked
ab_selected %>%
  verify(nrow(.) > 0) %>%
  assert(in_set(CONFIG$countries_of_interest), country) %>%
  verify(!any(is.na(country_name)))

cat("✓ Filtered data:", nrow(ab_selected), "rows x", ncol(ab_selected), "columns\n")

# ============================================
# SAMPLE SIZE SUMMARY
# ============================================

cat("\n=== Sample Sizes by Country ===\n")
sample_sizes <- table(ab_selected$country_name)
print(sample_sizes)

cat("\nTotal N:", sum(sample_sizes), "\n")

# ============================================
# COUNTRY-SPECIFIC NOTES
# ============================================

cat("\n=== COUNTRY-SPECIFIC DATA NOTES ===\n")
cat("\nVIETNAM (country code = 11):\n")
cat("  - Missing q143a-e (COVID-specific emergency powers)\n")
cat("  - All other measures present\n\n")

cat("OTHER COUNTRIES:\n")
cat("  - All COVID measures (q138-q145) present\n")
cat("  - Emergency powers q143 and q172 both available\n\n")

# ============================================
# MODULE COMPLETE
# ============================================

cat("✓ MODULE 02 COMPLETE\n")
cat("  - Data loaded from RDS file\n")
cat("  - Filtered to", length(unique(ab_selected$country_name)), "countries\n")
cat("  - Created country_name factor variable\n")
cat(rep("=", 70), "\n\n")

# Clean up intermediate objects
rm(ab_whole, sample_sizes)
