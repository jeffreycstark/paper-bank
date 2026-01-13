# ==============================================================================
# 02_select_variables.R - Select Variables for Analysis
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 02: Selecting variables for analysis\n")

library(tidyverse)

# ==============================================================================
# Define variable sets by theoretical role
# ==============================================================================

# Core IVs: Democracy vs Economy Tradeoff
# These capture the "sausage mentality" - prioritizing outcomes over process
core_dvs <- c(
  "dem_vs_econ",                # Q: Choose economy OR democracy (1-5 scale)
  "dem_capable_solving",        # Q: Democracy can solve society's problems
  "dem_always_preferable"       # Q: Democracy is always preferable
)

# Regime Attitudes
regime_attitudes <- c(
  "strongman_rule",             # Support for strong leader
  "expert_rule",                # Support for technocratic rule
  "military_rule",              # Support for military rule
  "single_party_rule"           # Support for one-party rule
)

# Trust Variables
trust_vars <- c(
  "trust_government",
  "trust_parliament",
  "trust_courts",
  "trust_police",
  "trust_military",
  "trust_parties"
)

# Democratic Values
dem_values <- c(
  "democracy_satisfaction",
  "democracy_suitability",
  "democracy_efficacy"
)

# Demographics (controls)
demographics <- c(
  "country",
  "wave",
  "idnumber",
  "age",
  "gender",
  "education_level",
  "education_years",
  "urban_rural",
  "hh_income",
  "subjective_social_status",
  "employed",
  "religion",
  "religiosity_practice"
)

# Political Engagement (potential mediators)
political_engagement <- c(
  "political_interest",
  "political_efficacy_internal",
  "political_efficacy_external",
  "action_demonstration",
  "action_petition",
  "action_campaign",
  "voted_last_election"
)

# Media/Information (potential mediators)
media_vars <- c(
  "news_newspaper",
  "news_tv",
  "news_radio",
  "news_internet",
  "internet_frequency"
)

# ==============================================================================
# Build variable selection list
# ==============================================================================

all_selected_vars <- c(
  demographics,
  core_dvs,
  regime_attitudes,
  trust_vars,
  dem_values,
  political_engagement,
  media_vars
)

# Check which variables exist in the dataset
available_vars <- intersect(all_selected_vars, names(sausage_raw))
missing_vars <- setdiff(all_selected_vars, names(sausage_raw))

cat("  Requested:", length(all_selected_vars), "variables\n")
cat("  Available:", length(available_vars), "variables\n")

if (length(missing_vars) > 0) {
  cat("  Missing variables:\n")
  for (v in missing_vars) {
    cat("    -", v, "\n")
  }
}

# ==============================================================================
# Subset to selected variables
# ==============================================================================

sausage_subset <- sausage_raw %>%
  select(all_of(available_vars))

cat("  -> sausage_subset:", nrow(sausage_subset), "x", ncol(sausage_subset), "\n")

# Store for next module
assign("sausage_subset", sausage_subset, envir = .GlobalEnv)
assign("available_vars", available_vars, envir = .GlobalEnv)
