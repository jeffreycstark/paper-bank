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
  "democracy_efficacy",         # Q: Democracy can solve society's problems
  "dem_always_preferable"       # Q: Democracy is always preferable
)

# Regime Attitudes
regime_attitudes <- c(
  "strongman_rule",             # Support for strong leader (70.8%)
  "expert_rule",                # Support for technocratic rule (73.2%)
  "military_rule",              # Support for military rule (89.9%)
  "single_party_rule"           # Support for one-party rule (81.0%)
)

# Trust Variables
trust_vars <- c(
  "trust_national_government",  # Trust in national government (94.2%)
  "trust_parliament",
  "trust_courts",
  "trust_police",
  "trust_military",
  "trust_political_parties"     # Trust in political parties (90.3%)
)

# Democratic Values
dem_values <- c(
  "democracy_satisfaction",
  "democracy_suitability"
  # Note: democracy_efficacy already in core_dvs
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
  "efficacy_ability_participate",  # Internal efficacy (best match)
  "efficacy_no_influence",         # External efficacy (temporary name)
  "action_demonstration",
  "action_petition",
  "attended_campaign_rally",    # 88.4% coverage
  "voted_last_election",        # Did you vote in last election? All waves
  "voting_frequency"            # How often voted since eligible W2-W6 (76.1%)
)

# Media/Information (potential mediators)
media_vars <- c(
  "pol_news_newspaper",     # 2.7% coverage
  "pol_news_television",    # 1.5% coverage
  "pol_news_radio",         # 3.2% coverage
  "news_internet"           # Renamed from internet_frequency
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
