# ==============================================================================
# MODULE 09: VALIDATION AND QUALITY CHECKS (OPTIONAL)
# ==============================================================================
# Purpose: Missing data analysis, construct validation, diagnostics
# Dependencies: Modules 01-08 (all composites created)
# Inputs: ab_selected (with all variables and composites)
# Outputs: Console reports, validation summaries
# Note: This module can be skipped for quick runs (set skip_validation = TRUE in CONFIG)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 09: VALIDATION AND QUALITY CHECKS\n")
cat(rep("=", 70), "\n\n")

# ============================================
# CONSTRUCT RELIABILITY SUMMARY
# ============================================

cat("[validation] Construct reliability summary:\n\n")

cat("Institutional Trust α:",
    round(psych::alpha(ab_selected[paste0("trust_q", 7:15)])$total$raw_alpha, 3), "\n")
cat("Regime Preference α:",
    round(psych::alpha(ab_selected[paste0("regimepref_q", 129:132)])$total$raw_alpha, 3), "\n")
cat("Authoritarian Acceptance α:",
    round(psych::alpha(ab_selected[paste0("auth_q", 168:171)])$total$raw_alpha, 3), "\n")
cat("Emergency Powers α:",
    round(psych::alpha(ab_selected[paste0("emergency_q172", letters[1:5])])$total$raw_alpha, 3), "\n")

# COVID restrictions (exclude Vietnam)
non_vietnam_covid <- ab_selected %>%
  filter(country_name != "Vietnam") %>%
  select(starts_with("covid_restrict_")) %>%
  select(-covid_restrict_composite) %>%
  na.omit()

if (nrow(non_vietnam_covid) > 0) {
  cat("COVID Restrictions α (excl. Vietnam):",
      round(psych::alpha(non_vietnam_covid)$total$raw_alpha, 3), "\n")
}

# ============================================
# MISSING DATA PATTERNS BY COUNTRY
# ============================================

cat("\n[validation] Missing data patterns by construct:\n\n")

# Key composite variables
key_composites <- c(
  "institutional_trust_index",
  "dem_satisfaction_z",
  "dem_legitimacy_z",
  "regime_preference",
  "auth_acceptance",
  "emergency_powers_support",
  "covid_govt_performance"
)

report_missing(ab_selected, key_composites, label = "Key Composites")

# ============================================
# DESCRIPTIVE STATISTICS BY COUNTRY
# ============================================

cat("\n[validation] Descriptive statistics for main composites:\n\n")

describe_by_country(ab_selected, key_composites, label = "Main Composites")

# ============================================
# DATA QUALITY SUMMARY
# ============================================

cat("\n[validation] Overall data quality by country:\n\n")

ab_selected %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    trust_complete = sum(!is.na(institutional_trust_index)),
    dem_complete = sum(!is.na(dem_satisfaction_z) & !is.na(dem_legitimacy_z)),
    auth_complete = sum(!is.na(auth_acceptance)),
    covid_complete = sum(!is.na(covid_govt_performance)),
    all_complete = sum(
      !is.na(institutional_trust_index) &
      !is.na(dem_satisfaction_z) &
      !is.na(auth_acceptance) &
      !is.na(covid_govt_performance)
    ),
    pct_all_complete = round(100 * all_complete / n, 1)
  ) %>%
  print()

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 09 COMPLETE\n")
cat("  - Construct reliability validated\n")
cat("  - Missing data patterns reported\n")
cat("  - Descriptive statistics generated\n")
cat(rep("=", 70), "\n\n")
