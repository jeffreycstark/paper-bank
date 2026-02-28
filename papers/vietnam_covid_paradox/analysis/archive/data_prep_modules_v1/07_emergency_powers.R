# ==============================================================================
# MODULE 07: EMERGENCY POWERS VARIABLES
# ==============================================================================
# Purpose: Recode emergency powers variables, create composite
# Dependencies: Modules 01-06 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q172a-e)
# Outputs: ab_selected (with emergency_powers_support composite)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 07: EMERGENCY POWERS VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# REVERSE-CODE EMERGENCY POWERS VARIABLES
# ============================================

cat("[emergency] Emergency Powers (q172a-e)\n")
cat("  Justification for government use of emergency powers:\n")
cat("    a) Public health crisis (COVID-19)\n")
cat("    b) Economic crisis\n")
cat("    c) Widespread corruption\n")
cat("    d) Security crisis/terrorism\n")
cat("    e) War\n\n")

emergency_vars <- paste0("q172", letters[1:5])

ab_selected <- ab_selected %>%
  mutate(across(all_of(emergency_vars), safe_reverse_4pt, .names = "emergency_{.col}"))

emergency_vars_recoded <- paste0("emergency_q172", letters[1:5])

validate_range(ab_selected, emergency_vars_recoded, 1, 4, "Emergency powers")

# ============================================
# CREATE COMPOSITE WITH RELIABILITY CHECK
# ============================================

cat("\n[composite] Creating emergency powers support composite...\n")

ab_selected <- create_validated_composite(
  data = ab_selected,
  vars = emergency_vars_recoded,
  composite_name = "emergency_powers_support",
  min_alpha = CONFIG$min_alpha,
  method = "cronbach",
  min_valid = round(length(emergency_vars_recoded) * CONFIG$min_items_fraction)
)

# ============================================
# DESCRIPTIVE SUMMARY
# ============================================

cat("\n[summary] Emergency powers support by country:\n\n")

ab_selected %>%
  group_by(country_name) %>%
  summarise(
    covid_health = round(mean(emergency_q172a, na.rm = TRUE), 2),
    economic = round(mean(emergency_q172b, na.rm = TRUE), 2),
    corruption = round(mean(emergency_q172c, na.rm = TRUE), 2),
    security = round(mean(emergency_q172d, na.rm = TRUE), 2),
    war = round(mean(emergency_q172e, na.rm = TRUE), 2),
    composite = round(mean(emergency_powers_support, na.rm = TRUE), 2),
    n = sum(!is.na(emergency_powers_support))
  ) %>%
  print()

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report:\n\n")

report_missing(ab_selected, emergency_vars_recoded, label = "Emergency Powers")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 07 COMPLETE\n")
cat("  - Emergency powers variables reverse-coded (q172a-e)\n")
cat("  - Composite created: emergency_powers_support\n")
cat("  - Higher scores = greater support for emergency powers\n")
cat(rep("=", 70), "\n\n")
