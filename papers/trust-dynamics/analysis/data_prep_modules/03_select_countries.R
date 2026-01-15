# ==============================================================================
# 03_select_countries.R - Select Countries and Add Regime Classification
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 03: Selecting countries and adding regime classification\n")

library(tidyverse)
library(here)

# ==============================================================================
# Country codes reference (from combined dataset)
# ==============================================================================
# 1=Japan, 2=Hong Kong, 3=Korea, 4=China, 5=Mongolia, 6=Philippines,
# 7=Taiwan, 8=Thailand, 9=Indonesia, 10=Singapore, 11=Vietnam,
# 12=Cambodia, 13=Malaysia, 14=Myanmar, 15=Australia, 18=India

country_labels <- c(
  "1" = "Japan", "2" = "Hong Kong", "3" = "Korea", "4" = "China",
  "5" = "Mongolia", "6" = "Philippines", "7" = "Taiwan", "8" = "Thailand",
  "9" = "Indonesia", "10" = "Singapore", "11" = "Vietnam", "12" = "Cambodia",
  "13" = "Malaysia", "14" = "Myanmar", "15" = "Australia", "18" = "India"
)

# ==============================================================================
# Define regime types based on V-Dem and scholarly consensus
# ==============================================================================

# Democracies (electoral + liberal democracies by V-Dem classification)
democracies <- c(1, 3, 6, 7, 9, 15, 18)  # Japan, Korea, Philippines, Taiwan, Indonesia, Australia, India

# Hybrid/Electoral Autocracies
hybrids <- c(2, 5, 8, 10, 13, 14)  # HK, Mongolia, Thailand, Singapore, Malaysia, Myanmar

# Closed/Electoral Autocracies
autocracies <- c(4, 11, 12)  # China, Vietnam, Cambodia

# ==============================================================================
# Add country name and regime type
# ==============================================================================

sausage_countries <- sausage_subset %>%
  mutate(
    country_name = recode(as.character(country), !!!country_labels),
    regime_type = case_when(
      country %in% democracies ~ "Democracy",
      country %in% hybrids ~ "Hybrid",
      country %in% autocracies ~ "Autocracy",
      TRUE ~ "Unknown"
    ),
    regime_type = factor(regime_type, levels = c("Democracy", "Hybrid", "Autocracy"))
  )

cat("  Country distribution:\n")
print(table(sausage_countries$country_name, useNA = "ifany"))

cat("\n  Regime type distribution:\n")
print(table(sausage_countries$regime_type, useNA = "ifany"))

# ==============================================================================
# Merge V-Dem scores if available
# ==============================================================================

if (exists("vdem_scores") && !is.null(vdem_scores)) {
  # Map wave to approximate year
  wave_years <- c("w1" = 2001, "w2" = 2005, "w3" = 2010,
                  "w4" = 2014, "w5" = 2018, "w6" = 2020)

  sausage_countries <- sausage_countries %>%
    mutate(survey_year = recode(wave, !!!wave_years))

  # Merge V-Dem
  sausage_countries <- sausage_countries %>%
    left_join(
      vdem_scores %>% select(country_code, year, vdem_electoral, vdem_liberal, vdem_regime),
      by = c("country" = "country_code", "survey_year" = "year")
    )

  cat("\n  V-Dem merge successful\n")
  cat("  V-Dem coverage:", sum(!is.na(sausage_countries$vdem_electoral)), "/",
      nrow(sausage_countries), "\n")
}

# ==============================================================================
# Optional: Filter to specific countries for core analysis
# ==============================================================================

# For main analysis, you might want to focus on:
# - Established democracies: Taiwan (7), Korea (3), Japan (1), Philippines (6)
# - Established autocracies: China (4), Vietnam (11), Cambodia (12)
# - Interesting hybrids: Singapore (10), Malaysia (13), Thailand (8)

# Uncomment to filter:
# core_countries <- c(1, 3, 4, 6, 7, 10, 11, 12, 13)
# sausage_countries <- sausage_countries %>%
#   filter(country %in% core_countries)

cat("  -> sausage_countries:", nrow(sausage_countries), "x", ncol(sausage_countries), "\n")

# Store for next module
assign("sausage_countries", sausage_countries, envir = .GlobalEnv)
