# ==============================================================================
# 00_data_preparation.R
# Vietnam COVID Paradox — Data Preparation
#
# Replaces the 10-module data_prep_modules/ pipeline.
# Sources _data_config.R, loads abs_harmonized.rds, filters to Wave 6
# Vietnam/Cambodia/Thailand, builds all variables expected by analysis scripts.
#
# Output: analysis/data/analysis_data.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
})

cat(rep("=", 70), "\n", sep = "")
cat("Vietnam COVID Paradox — Data Preparation\n")
cat(rep("=", 70), "\n\n")

# ==============================================================================
# 1. Load harmonized data
# ==============================================================================

source(here::here("_data_config.R"))

cat("[load] Reading abs_harmonized.rds...\n")
abs <- readRDS(abs_harmonized_path)
cat("✓ Loaded:", nrow(abs), "rows,", ncol(abs), "variables\n\n")

# ==============================================================================
# 2. Filter to Wave 6, Vietnam / Cambodia / Thailand
#    Country codes: 8 = Thailand, 11 = Vietnam, 12 = Cambodia
# ==============================================================================

cat("[filter] Wave 6, countries: Vietnam (11), Cambodia (12), Thailand (8)\n")

d <- abs |>
  filter(wave == 6, country %in% c(8L, 11L, 12L)) |>
  mutate(
    country_name = case_when(
      country == 8L  ~ "Thailand",
      country == 11L ~ "Vietnam",
      country == 12L ~ "Cambodia"
    )
  )

cat("✓ N =", nrow(d), "| Countries:", paste(sort(unique(d$country_name)), collapse = ", "), "\n")
cat("  By country:", paste(
  names(table(d$country_name)),
  as.integer(table(d$country_name)),
  sep = " = ", collapse = "; "
), "\n\n")

# ==============================================================================
# 3. COVID variables
#
# Harmonized uses safe_4pt_none (original ABS direction: 1 = best/positive).
# Analysis scripts expect reverse-coded direction (4 = best/positive).
# Binary impact vars are already 0/1 — no recoding needed.
# ==============================================================================

cat("[covid] Recoding COVID evaluation variables...\n")
cat("  (Harmonized: 1=best; reversing so 4=best, consistent with analysis scripts)\n")

rev4 <- function(x) ifelse(is.na(x), NA_real_, 5L - as.integer(x))

d <- d |>
  mutate(
    # Reverse-code 4-pt evaluation variables
    covid_trust_info      = rev4(covid_trust_govt_info),   # 4 = very trustworthy
    covid_impact_severity = rev4(covid_livelihood_impact), # 4 = very severe impact
    covid_govt_handling   = rev4(covid_govt_handling),     # 4 = handled very well

    # Rename binary impact variables (already 0/1)
    covid_illness_death  = covid_impact_illness,
    covid_job_loss       = covid_impact_job_loss,
    covid_income_loss    = covid_impact_income_loss,
    covid_edu_disruption = covid_impact_education
  )

cat("✓ covid_trust_info, covid_impact_severity, covid_govt_handling: reverse-coded\n")
cat("✓ Binary impact vars renamed (covid_illness_death, covid_job_loss, etc.)\n\n")

# ==============================================================================
# 4. COVID composite variables
# ==============================================================================

cat("[covid] Creating COVID composite variables...\n")

covid_binary_vars <- c(
  "covid_contracted", "covid_illness_death", "covid_job_loss",
  "covid_income_loss", "covid_edu_disruption"
)

d <- d |>
  mutate(
    covid_impact_count    = rowSums(across(all_of(covid_binary_vars)), na.rm = TRUE),
    covid_health_trauma   = covid_illness_death,  # alias for severe health impact
    covid_govt_performance = rowMeans(
      cbind(covid_trust_info, covid_govt_handling), na.rm = TRUE
    )
  )

cat("✓ covid_impact_count (sum of 5 binary impact vars)\n")
cat("✓ covid_govt_performance (mean of covid_trust_info, covid_govt_handling)\n\n")

# COVID restriction composite (q143a-e): not in harmonized dataset.
# Vietnam was already NA in old pipeline; Cambodia/Thailand also not available.
d$covid_restrict_composite <- NA_real_
cat("[covid] NOTE: covid_restrict_composite — q143a-e not in harmonized dataset; set to NA\n\n")

# ==============================================================================
# 5. Institutional trust index
#
# Harmonized trust_* vars already reverse-coded (4 = great deal of trust).
# Using 9 institutional trust items that parallel the old module's q7-q15.
# ==============================================================================

cat("[trust] Building institutional_trust_index (9-item composite)...\n")

trust_vars <- c(
  "trust_civil_service",
  "trust_courts",
  "trust_election_commission",
  "trust_local_government",
  "trust_military",
  "trust_national_government",
  "trust_parliament",
  "trust_police",
  "trust_political_parties"
)

d <- d |>
  mutate(
    institutional_trust_index = rowMeans(across(all_of(trust_vars)), na.rm = TRUE)
  )

cat("✓ institutional_trust_index (mean of", length(trust_vars), "items; 4=most trust)\n\n")

# ==============================================================================
# 6. Democracy satisfaction
#
# democracy_satisfaction (harmonized) = q90, already reverse-coded
# (4 = very satisfied). Used directly as dem_satisfaction.
# NOTE: Old pipeline created a 2-item composite (q90 + q92 standardized).
#       q92 (10-pt "how democratic is current govt") is not in harmonized data.
#       Using q90 alone is a simplification; re-run analysis to check sensitivity.
# ==============================================================================

cat("[democracy] Creating dem_satisfaction from democracy_satisfaction (q90)...\n")

d <- d |>
  mutate(dem_satisfaction = as.numeric(democracy_satisfaction))

cat("✓ dem_satisfaction (4=very satisfied; single-item, q90 equivalent)\n")
cat("  NOTE: Original composite used q90 + q92_clean; q92 not in harmonized data\n\n")

# ==============================================================================
# 7. Authoritarian acceptance
#
# Old pipeline used 4 items (q168-q171): not all available in harmonized W6.
# Using 2 available items: auth_govt_censor_ideas, auth_judges_defer_executive.
# Both already reverse-coded in harmonized (4 = strongly authoritarian).
# ==============================================================================

cat("[auth] Creating auth_acceptance (2-item composite)...\n")

d <- d |>
  mutate(
    auth_acceptance = rowMeans(
      cbind(
        as.numeric(auth_govt_censor_ideas),
        as.numeric(auth_judges_defer_executive)
      ),
      na.rm = TRUE
    )
  )

cat("✓ auth_acceptance (mean of auth_govt_censor_ideas, auth_judges_defer_executive)\n")
cat("  NOTE: Old pipeline used 4 items (q168-q171); harmonized W6 has 2 items\n\n")

# ==============================================================================
# 8. Control variables
#
# econ_anxiety  = econ_worried_lose_income (q161, already reversed: 4=very worried)
# educ_level    = education_level (1-10 ordinal)
# urban         = urban_rural (0=rural, 1=urban)
# income_quintile already present in harmonized data
# ==============================================================================

cat("[controls] Renaming control variables...\n")

d <- d |>
  mutate(
    econ_anxiety = as.numeric(econ_worried_lose_income),  # 4=very worried
    educ_level   = as.numeric(education_level),           # 1-10 ordinal
    urban        = as.integer(urban_rural)                # 0=rural, 1=urban
  )

cat("✓ econ_anxiety (from econ_worried_lose_income; 4=most worried)\n")
cat("✓ educ_level (from education_level; 1-10)\n")
cat("✓ urban (from urban_rural; 1=urban)\n\n")

# ==============================================================================
# 9. Variables not in harmonized dataset — set to NA with warnings
# ==============================================================================

# Emergency powers support (q172a-e): not in harmonized W6 ABS
d$emergency_powers_support <- NA_real_
cat("[NOTE] emergency_powers_support — q172a-e not in harmonized dataset; set to NA\n")

# Regime preference (q129-q132): regime_* vars not present in Wave 6 harmonized
d$regime_preference <- NA_real_
cat("[NOTE] regime_preference — q129-q132 not in harmonized Wave 6 data; set to NA\n\n")

# ==============================================================================
# 9. Summary statistics
# ==============================================================================

key_vars <- c(
  "covid_contracted", "covid_illness_death", "covid_job_loss",
  "covid_income_loss", "covid_edu_disruption",
  "covid_impact_count", "covid_impact_severity",
  "covid_trust_info", "covid_govt_handling", "covid_govt_performance",
  "institutional_trust_index", "dem_satisfaction", "auth_acceptance"
)

cat("[summary] Key variable means by country:\n\n")
d |>
  group_by(country_name) |>
  summarise(
    across(all_of(key_vars), ~round(mean(.x, na.rm = TRUE), 2)),
    n = n(),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()
cat("\n")

# ==============================================================================
# 10. Save output
# ==============================================================================

output_path <- here::here(
  "papers", "vietnam_covid_paradox", "analysis", "data", "analysis_data.rds"
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(d, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  N =", nrow(d), "| Variables =", ncol(d), "\n")
cat(rep("=", 70), "\n", sep = "")
