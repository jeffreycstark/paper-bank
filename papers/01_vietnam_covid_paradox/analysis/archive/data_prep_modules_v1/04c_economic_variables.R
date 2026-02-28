# ==============================================================================
# ECONOMIC VARIABLES MODULE
# ==============================================================================
# Purpose: Process economic SES and vulnerability variables for sensitivity analysis
# Dependencies: Modules 01-03 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q1-q6, q161-q163, se9, se9c_4, se10, se14, se14a)
# Outputs: ab_selected (with economic variables and composite indices)
# Note: Addresses reviewer concerns about missing economic controls
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ECONOMIC VARIABLES MODULE\n")
cat(rep("=", 70), "\n\n")

# ============================================
# INCOME & SES VARIABLES (KEY FOR REVIEWER)
# ============================================

cat("[economic] Income & SES variables (SE14, SE14a):\n")
cat("  SE14: Monthly household income quintile (1-5)\n")
cat("  SE14a: Income adequacy (1-5, reverse coded)\n\n")

# Income quintile (already correctly coded: 1=lowest, 5=highest)
if ("se14" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(
      income_quintile = case_when(
        se14 %in% 1:5 ~ as.numeric(se14),
        TRUE ~ NA_real_
      )
    )
  cat("✓ Created income_quintile (SE14) - Objective SES (1-5)\n")
}

# Income adequacy (reverse coded so higher = better)
# Original: 1=covers well+save lot, 5=doesn't cover+great difficulty
if ("se14a" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(income_adequacy = safe_reverse_5pt(se14a))
  cat("✓ Created income_adequacy (SE14a) - Subjective wellbeing (1-5, higher = better)\n")
}

# ============================================
# ECONOMIC ANXIETY/VULNERABILITY
# ============================================

cat("\n[economic] Economic anxiety/vulnerability variables (q161-q163):\n")
cat("  q161: Worry about income loss (reverse coded, higher = more worried)\n")
cat("  q162: Economic resilience (higher = could cope better)\n")
cat("  q163: Income fairness (reverse coded, higher = more fair)\n\n")

# Economic anxiety (reverse coded so higher = MORE worried)
# Original: 1=very worried, 4=not worried at all
if ("q161" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_anxiety = safe_reverse_4pt(q161))
  cat("✓ Created econ_anxiety (q161) - Higher = more worried about income loss (1-4)\n")
}

# Economic resilience (already coded: 1=serious difficulty, 3=manage fine)
if ("q162" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(
      econ_resilience = case_when(
        q162 %in% 1:3 ~ as.numeric(q162),
        TRUE ~ NA_real_
      )
    )
  cat("✓ Created econ_resilience (q162) - Ability to cope with income loss (1-3)\n")
}

# Income fairness (reverse coded so higher = MORE fair)
# Original: 1=very fair, 4=very unfair
if ("q163" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(income_fairness = safe_reverse_4pt(q163))
  cat("✓ Created income_fairness (q163) - Perceived economic justice (1-4, higher = more fair)\n")
}

# ============================================
# ECONOMIC EVALUATIONS (Pocketbook & Sociotropic)
# ============================================

cat("\n[economic] Economic evaluations - Pocketbook & Sociotropic (q1-q6):\n")
cat("  q1: NATIONAL economic condition now (reverse coded)\n")
cat("  q2: NATIONAL economic condition retrospective (reverse coded)\n")
cat("  q3: NATIONAL economic condition prospective (reverse coded)\n")
cat("  q4: FAMILY/HOUSEHOLD economic situation now (reverse coded)\n")
cat("  q5: FAMILY/HOUSEHOLD economic condition retrospective (reverse coded)\n")
cat("  q6: FAMILY/HOUSEHOLD economic situation prospective (reverse coded)\n\n")

# All economic evaluation variables use 5-point scale
# Original: 1=very good/better, 5=very bad/worse
# Reverse so higher = better evaluations

# NATIONAL/COUNTRY ECONOMIC CONDITIONS (SOCIOTROPIC)
if ("q1" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_national_now = safe_reverse_5pt(q1))
  cat("✓ Created econ_national_now (q1) - Country economic condition now\n")
}

if ("q2" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_national_retrospective = safe_reverse_5pt(q2))
  cat("✓ Created econ_national_retrospective (q2) - Country economic condition past\n")
}

if ("q3" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_national_prospective = safe_reverse_5pt(q3))
  cat("✓ Created econ_national_prospective (q3) - Country economic condition future\n")
}

# FAMILY/HOUSEHOLD ECONOMIC CONDITIONS (POCKETBOOK)
if ("q4" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_household_now = safe_reverse_5pt(q4))
  cat("✓ Created econ_household_now (q4) - Family economic situation now\n")
}

if ("q5" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_household_retrospective = safe_reverse_5pt(q5))
  cat("✓ Created econ_household_retrospective (q5) - Family economic condition past\n")
}

if ("q6" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(econ_household_prospective = safe_reverse_5pt(q6))
  cat("✓ Created econ_household_prospective (q6) - Family economic situation future\n")
}

# ============================================
# EMPLOYMENT VARIABLES
# ============================================

cat("\n[economic] Employment status variables (SE9, Se9c_4, SE10):\n")
cat("  SE9: Currently employed (binary)\n")
cat("  Se9c_4: White vs blue collar (binary)\n")
cat("  SE10: Main earner in household (binary)\n\n")

if ("se9" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(
      employed = case_when(
        se9 == 1 ~ 1,
        se9 == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
  cat("✓ Created employed (SE9)\n")
}

if ("se9c_4" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(
      white_collar = case_when(
        se9c_4 == 1 ~ 1,
        se9c_4 == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
  cat("✓ Created white_collar (Se9c_4)\n")
}

if ("se10" %in% names(ab_selected)) {
  ab_selected <- ab_selected %>%
    mutate(
      main_earner = case_when(
        se10 == 1 ~ 1,
        se10 == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
  cat("✓ Created main_earner (SE10)\n")
}

# ============================================
# CREATE COMPOSITE INDICES
# ============================================

cat("\n[economic] Creating composite indices:\n")
cat("  - Pocketbook index (q4, q5: family/household economic evaluations)\n")
cat("  - Sociotropic index (q1, q2: country/national economic evaluations)\n")
cat("  - Vulnerability index (anxiety + low resilience)\n\n")

# Check which component variables exist before creating composites
has_household_vars <- all(c("econ_household_now", "econ_household_retrospective") %in% names(ab_selected))
has_national_vars <- all(c("econ_national_now", "econ_national_retrospective") %in% names(ab_selected))
has_vulnerability_vars <- all(c("econ_anxiety", "econ_resilience") %in% names(ab_selected))

if (has_household_vars) {
  ab_selected <- ab_selected %>%
    mutate(
      # Pocketbook: Family/household economic evaluations (q4, q5)
      # Using rowMeans for vectorized performance
      econ_pocketbook_index = rowMeans(
        select(., econ_household_now, econ_household_retrospective),
        na.rm = TRUE
      )
    )
  cat("✓ Created econ_pocketbook_index (q4, q5: family/household economic situation)\n")
}

if (has_national_vars) {
  ab_selected <- ab_selected %>%
    mutate(
      # Sociotropic: Country/national economic evaluations (q1, q2)
      # Using rowMeans for vectorized performance
      econ_sociotropic_index = rowMeans(
        select(., econ_national_now, econ_national_retrospective),
        na.rm = TRUE
      )
    )
  cat("✓ Created econ_sociotropic_index (q1, q2: country/national economic situation)\n")
}

if (has_vulnerability_vars) {
  # Economic vulnerability: combines anxiety + low resilience
  # Higher = more vulnerable
  # Note: econ_anxiety is 1-4 scale, econ_resilience is 1-3 scale
  # Must normalize columns to 0-1 FIRST (vectorized), then compute row mean
  ab_selected <- ab_selected %>%
    mutate(
      n_anxiety = normalize_0_1(econ_anxiety),
      n_resilience_rev = normalize_0_1(4 - econ_resilience)  # Flip so high = vulnerable
    ) %>%
    mutate(
      econ_vulnerability = rowMeans(
        cbind(n_anxiety, n_resilience_rev),
        na.rm = TRUE
      )
    ) %>%
    select(-n_anxiety, -n_resilience_rev)  # Remove intermediate columns
  cat("✓ Created econ_vulnerability (normalized anxiety + reversed resilience, 0-1 scale)\n")
}

# Replace NaN with NA in composite indices
ab_selected <- ab_selected %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.), NA_real_, .)))

# ============================================
# VALIDATE RECODING
# ============================================

cat("\n[validation] Validating variable ranges...\n")

# Validate income variables (1-5 scale)
if ("income_quintile" %in% names(ab_selected)) {
  validate_range(ab_selected, "income_quintile", 1, 5, "Income quintile")
}
if ("income_adequacy" %in% names(ab_selected)) {
  validate_range(ab_selected, "income_adequacy", 1, 5, "Income adequacy")
}

# Validate anxiety/vulnerability variables
if ("econ_anxiety" %in% names(ab_selected)) {
  validate_range(ab_selected, "econ_anxiety", 1, 4, "Economic anxiety")
}
if ("econ_resilience" %in% names(ab_selected)) {
  validate_range(ab_selected, "econ_resilience", 1, 3, "Economic resilience")
}
if ("income_fairness" %in% names(ab_selected)) {
  validate_range(ab_selected, "income_fairness", 1, 4, "Income fairness")
}

# Validate economic evaluation variables (1-5 scale)
econ_eval_vars <- c("econ_national_now", "econ_national_retrospective", "econ_national_prospective",
                    "econ_household_now", "econ_household_retrospective", "econ_household_prospective")
for (var in econ_eval_vars) {
  if (var %in% names(ab_selected)) {
    validate_range(ab_selected, var, 1, 5, var)
  }
}

# Validate composite indices
composite_vars <- c("econ_pocketbook_index", "econ_sociotropic_index", "econ_vulnerability")
for (var in composite_vars) {
  if (var %in% names(ab_selected)) {
    # Composites should also be on similar scales (allowing for flexibility)
    min_val <- min(ab_selected[[var]], na.rm = TRUE)
    max_val <- max(ab_selected[[var]], na.rm = TRUE)
    if (!is.infinite(min_val) && !is.infinite(max_val)) {
      cat(sprintf("  %s: range [%.2f, %.2f]\n", var, min_val, max_val))
    }
  }
}

# Validate binary employment variables
binary_vars <- c("employed", "white_collar", "main_earner")
for (var in binary_vars) {
  if (var %in% names(ab_selected)) {
    min_val <- min(ab_selected[[var]], na.rm = TRUE)
    max_val <- max(ab_selected[[var]], na.rm = TRUE)
    if (min_val < 0 || max_val > 1) {
      stop(sprintf("Range check failed for %s: [%.0f, %.0f], expected [0, 1]",
                   var, min_val, max_val))
    }
  }
}

cat("✓ All variables within expected ranges\n")

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report by country:\n\n")

# Key SES/income variables
if ("income_quintile" %in% names(ab_selected) || "income_adequacy" %in% names(ab_selected)) {
  ses_vars <- intersect(c("income_quintile", "income_adequacy"), names(ab_selected))
  report_missing(ab_selected, ses_vars, label = "SES/Income Variables")
}

# Economic anxiety/vulnerability variables
if (any(c("econ_anxiety", "econ_resilience", "income_fairness") %in% names(ab_selected))) {
  vulnerability_vars <- intersect(c("econ_anxiety", "econ_resilience", "income_fairness"),
                                   names(ab_selected))
  report_missing(ab_selected, vulnerability_vars, label = "Economic Vulnerability Variables")
}

# Composite indices
if (any(c("econ_pocketbook_index", "econ_sociotropic_index", "econ_vulnerability") %in% names(ab_selected))) {
  composite_vars <- intersect(c("econ_pocketbook_index", "econ_sociotropic_index", "econ_vulnerability"),
                               names(ab_selected))
  report_missing(ab_selected, composite_vars, label = "Economic Composite Indices")
}

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ ECONOMIC VARIABLES MODULE COMPLETE\n")
cat("  Variables created:\n")
cat("\n  KEY SES VARIABLES (address reviewer concern):\n")
cat("    - income_quintile: 1-5 (1=lowest, 5=highest) - OBJECTIVE SES\n")
cat("    - income_adequacy: 1-5 (higher = covers needs better) - SUBJECTIVE\n")
cat("\n  ECONOMIC VULNERABILITY:\n")
cat("    - econ_anxiety: 1-4 (higher = more worried about income loss)\n")
cat("    - econ_resilience: 1-3 (higher = could cope better if lost income)\n")
cat("    - income_fairness: 1-4 (higher = perceive income as more fair)\n")
cat("    - econ_vulnerability: composite index (higher = more vulnerable)\n")
cat("\n  ECONOMIC EVALUATIONS:\n")
cat("    - econ_pocketbook_index: family/household (q4, q5)\n")
cat("    - econ_sociotropic_index: country/national (q1, q2)\n")
cat("    - econ_national_now (q1), econ_national_retrospective (q2), econ_national_prospective (q3)\n")
cat("    - econ_household_now (q4), econ_household_retrospective (q5), econ_household_prospective (q6)\n")
cat("\n  EMPLOYMENT:\n")
cat("    - employed, white_collar, main_earner (binary)\n")
cat(rep("=", 70), "\n\n")
