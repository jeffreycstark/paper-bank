# ==============================================================================
# MODULE 06: AUTHORITARIANISM VARIABLES
# ==============================================================================
# Purpose: Recode authoritarianism/regime preference variables, create composites
# Dependencies: Modules 01-05 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q129-q132, q168-q171)
# Outputs: ab_selected (with regime_preference, auth_acceptance composites)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 06: AUTHORITARIANISM VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# REGIME PREFERENCE (q129-q132)
# ============================================

cat("[auth] Regime Preference (q129-q132)\n")
cat("  Higher values = stronger preference for authoritarian regimes\n\n")

regime_vars <- paste0("q", 129:132)

ab_selected <- ab_selected %>%
  mutate(across(all_of(regime_vars), safe_reverse_4pt, .names = "regimepref_{.col}"))

regimepref_vars_recoded <- paste0("regimepref_q", 129:132)

validate_range(ab_selected, regimepref_vars_recoded, 1, 4, "Regime preference")

# Create composite with reliability check
ab_selected <- create_validated_composite(
  data = ab_selected,
  vars = regimepref_vars_recoded,
  composite_name = "regime_preference",
  min_alpha = CONFIG$min_alpha,
  method = "cronbach",
  min_valid = round(length(regimepref_vars_recoded) * CONFIG$min_items_fraction)
)

cat("✓ Regime preference composite created\n")

# ============================================
# AUTHORITARIAN ACCEPTANCE (q168-q171)
# ============================================

cat("\n[auth] Authoritarian Acceptance (q168-q171)\n")
cat("  Higher values = greater acceptance of authoritarian practices\n\n")

auth_vars <- paste0("q", 168:171)

ab_selected <- ab_selected %>%
  mutate(across(all_of(auth_vars), safe_reverse_4pt, .names = "auth_{.col}"))

auth_vars_recoded <- paste0("auth_q", 168:171)

validate_range(ab_selected, auth_vars_recoded, 1, 4, "Authoritarian acceptance")

# Create composite with reliability check
ab_selected <- create_validated_composite(
  data = ab_selected,
  vars = auth_vars_recoded,
  composite_name = "auth_acceptance",
  min_alpha = CONFIG$min_alpha,
  method = "cronbach",
  min_valid = round(length(auth_vars_recoded) * CONFIG$min_items_fraction)
)

cat("✓ Authoritarian acceptance composite created\n")

# ============================================
# DESCRIPTIVE SUMMARY
# ============================================

cat("\n[summary] Authoritarian acceptance by country:\n\n")

ab_selected %>%
  group_by(country_name) %>%
  summarise(
    mean = round(mean(auth_acceptance, na.rm = TRUE), 2),
    sd = round(sd(auth_acceptance, na.rm = TRUE), 2),
    n = sum(!is.na(auth_acceptance))
  ) %>%
  print()

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report:\n\n")

all_auth_vars <- c(regimepref_vars_recoded, auth_vars_recoded)
report_missing(ab_selected, all_auth_vars, label = "Authoritarianism Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 06 COMPLETE\n")
cat("  - Regime preference variables reverse-coded (q129-q132)\n")
cat("  - Authoritarian acceptance variables reverse-coded (q168-q171)\n")
cat("  - Composites created with reliability checks\n")
cat(rep("=", 70), "\n\n")
