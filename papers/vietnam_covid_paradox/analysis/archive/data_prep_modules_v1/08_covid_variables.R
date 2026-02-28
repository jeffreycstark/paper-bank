# ==============================================================================
# MODULE 08: COVID VARIABLES
# ==============================================================================
# Purpose: Recode COVID impact, trust, and government handling variables
# Dependencies: Modules 01-07 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q138-q145)
# Outputs: ab_selected (with COVID variables and composites)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 08: COVID VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# BINARY COVID IMPACT VARIABLES (q138, q139a-d)
# ============================================

cat("[covid] Personal COVID impact variables (binary)...\n")

covid_binary_vars <- c("q138", paste0("q139", letters[1:4]))
covid_binary_names <- c("covid_contracted", "covid_illness_death", "covid_job_loss",
                        "covid_income_loss", "covid_edu_disruption")

ab_selected <- ab_selected %>%
  mutate(
    across(all_of(covid_binary_vars),
           ~case_when(. == 1 ~ 1, . == 2 ~ 0, TRUE ~ NA_real_),
           .names = "{.col}_binary")
  )

# Rename for clarity
for (i in seq_along(covid_binary_vars)) {
  old_name <- paste0(covid_binary_vars[i], "_binary")
  new_name <- covid_binary_names[i]
  ab_selected <- ab_selected %>%
    rename(!!new_name := !!old_name)
}

# Validate binary coding
ab_selected %>% assert(in_set(0, 1, NA), all_of(covid_binary_names))

cat("✓ Binary COVID impact variables created:\n")
cat("  ", paste(covid_binary_names, collapse = ", "), "\n")

# Create impact count and severity (vectorized with rowSums)
ab_selected <- ab_selected %>%
  mutate(
    covid_impact_count = rowSums(select(., all_of(covid_binary_names)), na.rm = TRUE),
    covid_health_trauma = covid_illness_death  # Alias for severe health impact
  )

cat("✓ COVID impact summary variables created\n")

# ============================================
# REVERSE-CODE COVID EVALUATION VARIABLES
# ============================================

cat("\n[covid] COVID evaluation variables...\n")

ab_selected <- ab_selected %>%
  mutate(
    covid_impact_severity = safe_reverse_4pt(q140),  # Economic impact severity
    covid_trust_info = safe_reverse_4pt(q141),       # Trust in COVID info
    covid_govt_handling = safe_reverse_4pt(q142)     # Government handling
  )

covid_eval_vars <- c("covid_impact_severity", "covid_trust_info", "covid_govt_handling")

validate_range(ab_selected, covid_eval_vars, 1, 4, "COVID evaluation")

# Create government performance composite (vectorized with rowMeans)
ab_selected <- ab_selected %>%
  mutate(
    covid_govt_performance = rowMeans(select(., covid_trust_info, covid_govt_handling), na.rm = TRUE)
  )

cat("✓ COVID evaluation variables created\n")

# ============================================
# COVID RESTRICTIONS (q143a-e)
# ============================================

cat("\n[covid] COVID restriction acceptance (q143a-e)...\n")
cat("  Note: Vietnam has 100% missing (not asked in survey)\n")
cat("  Creating composite for non-Vietnam countries only\n\n")

ab_selected <- ab_selected %>%
  mutate(
    across(q143a:q143e, ~safe_reverse_3pt(.x), .names = "covid_restrict_{.col}")
  ) %>%
  rename(
    covid_restrict_elections = covid_restrict_q143a,
    covid_restrict_speech = covid_restrict_q143b,
    covid_restrict_media = covid_restrict_q143c,
    covid_restrict_tracking = covid_restrict_q143d,
    covid_restrict_lockdown = covid_restrict_q143e
  )

# Validate variables
covid_restrict_vars <- c("covid_restrict_elections", "covid_restrict_speech",
                        "covid_restrict_media", "covid_restrict_tracking",
                        "covid_restrict_lockdown")

# Create composite (with reliability check for non-Vietnam countries)
# Vietnam will have NA for this composite since all q143 values are missing
non_vietnam <- ab_selected %>% filter(country_name != "Vietnam")

if (nrow(non_vietnam) > 0) {
  # Create composite for non-Vietnam countries
  non_vietnam <- create_validated_composite(
    data = non_vietnam,
    vars = covid_restrict_vars,
    composite_name = "covid_restrict_composite",
    min_alpha = CONFIG$min_alpha,
    method = "cronbach",
    min_valid = round(length(covid_restrict_vars) * CONFIG$min_items_fraction)
  )

  # Merge back with full dataset (join by both idnumber and country_name to avoid duplicates)
  ab_selected <- ab_selected %>%
    left_join(
      non_vietnam %>% select(idnumber, country_name, covid_restrict_composite),
      by = c("idnumber", "country_name")
    )

  cat("✓ COVID restriction composite created (Vietnam = NA)\n")
} else {
  cat("⚠ No non-Vietnam data for composite\n")
}

# ============================================
# DESCRIPTIVE SUMMARY
# ============================================

cat("\n[summary] COVID variables by country:\n\n")

ab_selected %>%
  group_by(country_name) %>%
  summarise(
    covid_contracted_pct = round(100 * mean(covid_contracted, na.rm = TRUE), 1),
    covid_health_trauma_pct = round(100 * mean(covid_health_trauma, na.rm = TRUE), 1),
    impact_count_mean = round(mean(covid_impact_count, na.rm = TRUE), 2),
    govt_performance_mean = round(mean(covid_govt_performance, na.rm = TRUE), 2),
    restrict_composite_mean = round(mean(covid_restrict_composite, na.rm = TRUE), 2),
    n = n()
  ) %>%
  print()

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report:\n\n")

all_covid_vars <- c(covid_binary_names, covid_eval_vars, covid_restrict_vars)
report_missing(ab_selected, all_covid_vars, label = "COVID Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 08 COMPLETE\n")
cat("  - Personal COVID impact variables created (binary)\n")
cat("  - COVID evaluation variables reverse-coded\n")
cat("  - COVID restriction variables created (Vietnam = NA)\n")
cat("  - Composites: covid_govt_performance, covid_restrict_composite\n")
cat(rep("=", 70), "\n\n")
