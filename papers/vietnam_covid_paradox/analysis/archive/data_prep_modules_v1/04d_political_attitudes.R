# ==============================================================================
# MODULE 04d: POLITICAL ATTITUDES VARIABLES
# ==============================================================================
# Purpose: Create political attitude variables for robustness/alternative analyses
# Dependencies: Modules 01-03 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q137, q136, q112, q108, q96, q82, q124)
# Outputs: ab_selected (with blind_loyalty, trust_govt_officials, govt_responsiveness,
#          govt_transparency, govt_satisfaction, system_support, democracy preference vars)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 04d: POLITICAL ATTITUDES VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# RECODE 4-POINT POLITICAL ATTITUDE SCALES
# ============================================

cat("[political attitudes] Recoding political attitude variables...\n")
cat("  q137: Blind loyalty to government (reverse: 5 - x)\n")
cat("  q136: Trust in government officials (reverse: 5 - x)\n")
cat("  q112: Government responsiveness (reverse: 5 - x)\n")
cat("  q108: Government transparency (keep as-is, 1-5)\n")
cat("  q96:  Government satisfaction (reverse: 5 - x)\n")
cat("  q82:  System support (reverse: 5 - x)\n\n")

ab_selected <- ab_selected %>%
  mutate(
    # Blind loyalty: q137 reverse coded (higher = more loyal)
    # Original: 1=Strongly agree to 4=Strongly disagree with "citizens should always support govt"
    blind_loyalty = safe_reverse_4pt(q137),

    # Trust in government officials: q136 reverse coded (higher = more trust)
    # Original: 1=Strongly agree to 4=Strongly disagree with "govt officials act in citizens' interests"
    trust_govt_officials = safe_reverse_4pt(q136),

    # Government responsiveness: q112 reverse coded (higher = more responsive)
    # Original: 1=Strongly agree to 4=Strongly disagree with "govt responds to what people want"
    govt_responsiveness = safe_reverse_4pt(q112),

    # Government transparency: q108 keep as-is (higher = more transparent)
    # Note: This is a 5-point scale, just clean missing values
    govt_transparency = if_else(q108 >= 1 & q108 <= 5, as.numeric(q108), NA_real_),

    # Government satisfaction: q96 reverse coded (higher = more satisfied)
    # Original: 1=Very satisfied to 4=Very dissatisfied with "how govt runs country"
    govt_satisfaction = safe_reverse_4pt(q96),

    # System support: q82 reverse coded (higher = more supportive)
    # Original: 1=Strongly agree to 4=Strongly disagree with "our system of govt is best"
    system_support = safe_reverse_4pt(q82)
  )

cat("✓ Recoded 6 political attitude variables\n")

# ============================================
# VALIDATE RECODING
# ============================================

cat("\n[validation] Validating ranges...\n")

# 4-point scales (1-4)
vars_4pt <- c("blind_loyalty", "trust_govt_officials", "govt_responsiveness",
              "govt_satisfaction", "system_support")
validate_range(ab_selected, vars_4pt, 1, 4, "4-point political attitude variables")

# 5-point scale (1-5)
validate_range(ab_selected, "govt_transparency", 1, 5, "Government transparency (5-point)")

# ============================================
# CREATE DEMOCRACY PREFERENCE VARIABLES (q124)
# ============================================

cat("\n[democracy preference] Creating democracy preference indicators from q124...\n")
cat("  q124: 1=Democracy always preferable, 2=Authoritarian can be preferable, 3=Doesn't matter\n\n")

ab_selected <- ab_selected %>%
  mutate(
    # Binary: committed democrat vs. not
    democracy_committed = case_when(
      q124 == 1 ~ 1,              # Democracy always preferable
      q124 %in% c(2, 3) ~ 0,      # Authoritarian acceptable OR indifferent
      TRUE ~ NA_real_
    ),

    # Binary: authoritarian acceptable
    authoritarian_acceptable = case_when(
      q124 == 2 ~ 1,              # Authoritarian can be preferable
      q124 %in% c(1, 3) ~ 0,      # Democrat or indifferent
      TRUE ~ NA_real_
    ),

    # Binary: politically indifferent
    regime_indifferent = case_when(
      q124 == 3 ~ 1,              # Doesn't matter
      q124 %in% c(1, 2) ~ 0,      # Has a preference
      TRUE ~ NA_real_
    )
  )

cat("✓ Created 3 democracy preference indicators:\n")
cat("  - democracy_committed (1 = democracy always preferable)\n")
cat("  - authoritarian_acceptable (1 = authoritarian can be preferable)\n")
cat("  - regime_indifferent (1 = doesn't matter)\n")

# ============================================
# CROSS-TABULATION BY COUNTRY
# ============================================

cat("\n[verification] Cross-tabulation by country:\n\n")

# Political attitudes means by country
cat("=== Political Attitudes Mean by Country ===\n")
political_means <- ab_selected %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    blind_loyalty = round(mean(blind_loyalty, na.rm = TRUE), 2),
    trust_govt_officials = round(mean(trust_govt_officials, na.rm = TRUE), 2),
    govt_responsiveness = round(mean(govt_responsiveness, na.rm = TRUE), 2),
    govt_transparency = round(mean(govt_transparency, na.rm = TRUE), 2),
    govt_satisfaction = round(mean(govt_satisfaction, na.rm = TRUE), 2),
    system_support = round(mean(system_support, na.rm = TRUE), 2)
  )
print(political_means)

# Democracy preference distribution by country
cat("\n=== Democracy Preference % by Country ===\n")
dem_pref_by_country <- ab_selected %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    pct_committed_democrat = round(100 * mean(democracy_committed, na.rm = TRUE), 1),
    pct_auth_acceptable = round(100 * mean(authoritarian_acceptable, na.rm = TRUE), 1),
    pct_indifferent = round(100 * mean(regime_indifferent, na.rm = TRUE), 1)
  )
print(dem_pref_by_country)

# Verification: Vietnam should show high values
cat("\n[verification] Checking Vietnam shows expected high values...\n")
vietnam_means <- ab_selected %>%
  filter(country_name == "Vietnam") %>%
  summarise(
    blind_loyalty = mean(blind_loyalty, na.rm = TRUE),
    trust_govt_officials = mean(trust_govt_officials, na.rm = TRUE),
    system_support = mean(system_support, na.rm = TRUE)
  )

if (vietnam_means$blind_loyalty > 2.5 && vietnam_means$trust_govt_officials > 2.5) {
  cat("✓ Vietnam shows expected high blind_loyalty and trust_govt_officials\n")
} else {
  cat("⚠ Vietnam values may be lower than expected - verify source data\n")
}

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report:\n\n")

all_political_vars <- c(vars_4pt, "govt_transparency",
                        "democracy_committed", "authoritarian_acceptable", "regime_indifferent")
report_missing(ab_selected, all_political_vars, label = "Political Attitudes Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 04d COMPLETE\n")
cat("  - 6 political attitude variables created and validated\n")
cat("  - 3 democracy preference indicators created\n")
cat("  - Cross-country distributions verified\n")
cat(rep("=", 70), "\n\n")
