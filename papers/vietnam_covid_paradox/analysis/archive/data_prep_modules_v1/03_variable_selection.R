# ==============================================================================
# MODULE 03: VARIABLE SELECTION AND CLEANING
# ==============================================================================
# Purpose: Select analysis variables, clean missing codes, recode demographics
# Dependencies: Modules 01-02 (CONFIG, ab_selected)
# Inputs: ab_selected (from Module 02)
# Outputs: ab_selected (with selected vars, clean missing, demographics)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 03: VARIABLE SELECTION AND CLEANING\n")
cat(rep("=", 70), "\n\n")

# ============================================
# DEFINE VARIABLE SETS
# ============================================

cat("[variables] Defining variable sets...\n")

trust_vars <- paste0("q", 7:15)
democracy_vars <- c("q90", "q91", "q106", "q128", "q92", "q95")
political_attitudes_vars <- c("q137", "q136", "q112", "q108", "q96", "q82", "q124")  # Blind loyalty, govt trust, responsiveness, transparency, satisfaction, system support, democracy preference
political_interest_vars <- c("q47", "q48", "q49")
news_source_vars <- c("q53")  # Most important news source (1-6 categorical)
tradeoff_vars <- c("q126", "q127")
authoritarianism_vars <- paste0("q", 129:132)
acceptance_of_auth_vars <- paste0("q", 168:171)
emergency_powers_vars <- paste0("q172", letters[1:5])
corruption_vars <- c("q115", "q116", "q117", "q118", paste0("q119", letters[1:3]), "q79")
covid_vars <- c("q138", "q140", "q141", "q142", "q161", "q162", "q163")
family_impact_vars <- paste0("q139", letters[1:4])
pandemic_gov_powers_vars <- paste0("q143", letters[1:5])
economic_evaluation_vars <- paste0("q", 1:6)  # Pocketbook & sociotropic economic evaluations
demographics <- c("level", "se2", "se3_1", "se5", "se5a", "se9", "se9c_4", "se10", "se14", "se14a")

# Combine all variable names
all_vars <- c(
  "country", "country_name", "idnumber", "year", "month",
  "w", "region",  # Survey weight and geographic clustering variable
  trust_vars, democracy_vars, political_attitudes_vars, political_interest_vars, news_source_vars, tradeoff_vars,
  authoritarianism_vars, acceptance_of_auth_vars, emergency_powers_vars,
  corruption_vars, covid_vars, family_impact_vars, pandemic_gov_powers_vars,
  economic_evaluation_vars,  # ADDED: Pocketbook & sociotropic economic evaluations
  demographics
)

ab_selected <- ab_selected %>% select(all_of(all_vars))

cat("✓ Selected", ncol(ab_selected), "variables for analysis\n")

# ============================================
# CREATE COUNTRY DUMMY VARIABLES
# ============================================

cat("\n[countries] Creating country dummy variables...\n")

ab_selected <- ab_selected %>%
  mutate(
    cambodia = if_else(country_name == "Cambodia", 1, 0),
    thailand = if_else(country_name == "Thailand", 1, 0),
    vietnam = if_else(country_name == "Vietnam", 1, 0)
  )

# Hard validation: ensure valid country names (3 analysis countries only)
ab_selected %>%
  assert(in_set("Cambodia", "Thailand", "Vietnam"), country_name)

cat("✓ Country dummies created for 3 analysis countries\n")

# ============================================
# RECODE MISSING VALUES
# ============================================

cat("\n[missing] Recoding special missing values as NA...\n")

# Step 1: Universal missing codes (all numeric variables)
all_numeric_vars <- names(select(ab_selected, where(is.numeric)))
ab_selected <- batch_clean_missing(ab_selected, all_numeric_vars,
                                   CONFIG$missing_codes_universal)

cat("✓ Universal missing codes recoded:", paste(CONFIG$missing_codes_universal, collapse=", "), "\n")

# Step 2: Short-scale missing codes (7, 8, 9) for specific variables only
short_scale_present <- CONFIG$short_scale_vars[CONFIG$short_scale_vars %in% names(ab_selected)]
ab_selected <- batch_clean_missing(ab_selected, short_scale_present,
                                   c(CONFIG$missing_codes_short_scale))

cat("✓ Short-scale missing codes recoded:", paste(CONFIG$missing_codes_short_scale, collapse=", "), "\n")

# Hard validation: ensure no negative values remain
negative_check <- ab_selected %>%
  summarise(across(where(is.numeric), ~sum(. < 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_negative") %>%
  filter(n_negative > 0)

stopifnot("Negative values still present after recoding" = nrow(negative_check) == 0)
cat("✓ Validation passed: No negative values remain\n")

# ============================================
# RECODE DEMOGRAPHICS
# ============================================

cat("\n[demographics] Recoding demographic variables...\n")

ab_selected <- ab_selected %>%
  mutate(
    # Gender
    gender = case_when(
      se2 == 1 ~ "Male",
      se2 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),

    # Urban/Rural
    urban = case_when(
      level == 1 ~ 0,  # Rural
      level == 2 ~ 1,  # Urban
      TRUE ~ NA_real_
    ),

    # Age (continuous)
    age = if_else(se3_1 > 0 & se3_1 < 120, se3_1, NA_real_),

    # Age groups
    age_group = case_when(
      age < 30 ~ "18-29",
      age < 45 ~ "30-44",
      age < 60 ~ "45-59",
      age >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60+")),

    # Education level (categorical)
    educ_level = case_when(
      se5 %in% c(1, 2, 3) ~ "Primary or less",
      se5 %in% c(4, 5, 6, 7) ~ "Secondary",
      se5 %in% c(8, 9, 10) ~ "University/Tertiary",
      TRUE ~ NA_character_
    ),
    educ_level = factor(educ_level, levels = c("Primary or less", "Secondary", "University/Tertiary")),

    # Years of education (continuous)
    educ_years = if_else(se5a > 0 & se5a < 99, se5a, NA_real_)
  )

cat("✓ Demographics recoded: gender, urban, age, age_group, educ_level, educ_years\n")

# ============================================
# RECODE POLITICAL ENGAGEMENT
# ============================================

cat("\n[political engagement] Recoding political engagement variables...\n")

ab_selected <- ab_selected %>%
  mutate(
    # Political interest (4-point scale)
    interest_politics = safe_reverse_4pt(q47),

    # Follow news (5-point scale reverse: 1=Everyday → 5, 5=Never → 1)
    follow_news = safe_reverse_5pt(q48),

    # Discuss politics (3-point scale)
    discuss_politics = safe_reverse_3pt(q49),

    # News source (1-6 categorical, no reversal)
    news_source = q53,

    # News source categorized for analysis
    # q53: 1=TV, 2=TV/Cable, 3=Internet/social media, 4=Radio, 5=Face-to-face, 6=Other
    news_source_cat = case_when(
      q53 %in% c(1, 2) ~ "Broadcast (TV)",  # Television (traditional + cable)
      q53 == 3 ~ "Digital",                  # Internet and social media
      q53 %in% c(4, 5, 6) ~ "Other",        # Radio, face-to-face, other
      TRUE ~ NA_character_
    ),
    news_source_cat = factor(news_source_cat, levels = c("Broadcast (TV)", "Digital", "Other"))
  )

cat("✓ Political engagement recoded: interest_politics, follow_news, discuss_politics, news_source, news_source_cat\n")

# ============================================
# QUICK VERIFICATION
# ============================================

cat("\n[verification] Running data quality checks...\n")

# Check for impossible demographic values
impossible_age <- sum(ab_selected$age > 120, na.rm = TRUE)
impossible_educ <- sum(ab_selected$educ_years > 30, na.rm = TRUE)

stopifnot("Impossible age values found" = impossible_age == 0)
stopifnot("Impossible education values found" = impossible_educ == 0)

cat("✓ No impossible demographic values\n")

# Verify 10-point scales retained full range
q92_has_789 <- any(ab_selected$q92 %in% 7:9, na.rm = TRUE)
q95_has_789 <- any(ab_selected$q95 %in% 7:9, na.rm = TRUE)

stopifnot("10-point scales missing values 7-9" = q92_has_789 & q95_has_789)
cat("✓ 10-point scales correctly retain values 7-9\n")

# Verify q143 variables availability across countries
cat("\nChecking q143 (pandemic-specific powers) availability:\n")
q143_by_country <- ab_selected %>%
  group_by(country_name) %>%
  summarise(
    q143a_present_pct = round(100 * sum(!is.na(q143a)) / n(), 1),
    n = n()
  )

print(q143_by_country)

# Verify Vietnam q143 variables are 100% missing (as expected)
vietnam_q143_present <- ab_selected %>%
  filter(country_name == "Vietnam") %>%
  summarise(pct_present = round(100 * sum(!is.na(q143a)) / n(), 1)) %>%
  pull(pct_present)

if(vietnam_q143_present == 0) {
  cat("✓ Vietnam q143 variables correctly 100% missing (not asked in survey)\n")
} else {
  warning("Unexpected: Vietnam q143 has ", vietnam_q143_present, "% data (expected 0%)")
}

# Summary statistics
cat("\n=== Demographics Summary ===\n")
ab_selected %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    age_mean = round(mean(age, na.rm = TRUE), 1),
    educ_years_mean = round(mean(educ_years, na.rm = TRUE), 1),
    pct_male = round(100 * sum(gender == "Male", na.rm = TRUE) / n(), 1),
    pct_urban = round(100 * sum(urban == 1, na.rm = TRUE) / n(), 1)
  ) %>%
  print()

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 03 COMPLETE\n")
cat("  -", ncol(ab_selected), "variables selected\n")
cat("  - Missing values recoded\n")
cat("  - Demographics recoded and validated\n")
cat(rep("=", 70), "\n\n")
