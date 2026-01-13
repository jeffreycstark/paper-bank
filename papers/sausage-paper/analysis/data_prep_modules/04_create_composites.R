# ==============================================================================
# 04_create_composites.R - Create Composite Variables and Indices
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 04: Creating composite variables and indices\n")

library(tidyverse)
library(psych)  # For alpha() reliability

# ==============================================================================
# Helper function for creating standardized composites
# ==============================================================================

create_composite <- function(data, vars, name, method = "mean") {
  # Check which variables exist
  existing_vars <- intersect(vars, names(data))

  if (length(existing_vars) == 0) {
    cat("    WARNING:", name, "- no variables found\n")
    return(rep(NA, nrow(data)))
  }

  if (length(existing_vars) < length(vars)) {
    cat("    NOTE:", name, "- using", length(existing_vars), "of", length(vars), "vars\n")
  }

  # Calculate composite
  if (method == "mean") {
    result <- rowMeans(data[, existing_vars, drop = FALSE], na.rm = TRUE)
    # Set to NA if all components missing
    all_na <- rowSums(!is.na(data[, existing_vars, drop = FALSE])) == 0
    result[all_na] <- NA
  } else if (method == "sum") {
    result <- rowSums(data[, existing_vars, drop = FALSE], na.rm = TRUE)
    all_na <- rowSums(!is.na(data[, existing_vars, drop = FALSE])) == 0
    result[all_na] <- NA
  } else if (method == "pca") {
    # First principal component
    complete_cases <- complete.cases(data[, existing_vars])
    if (sum(complete_cases) > 100) {
      pca <- prcomp(data[complete_cases, existing_vars], scale. = TRUE)
      result <- rep(NA, nrow(data))
      result[complete_cases] <- pca$x[, 1]
    } else {
      result <- rep(NA, nrow(data))
    }
  }

  return(result)
}

# ==============================================================================
# Create institutional trust index
# ==============================================================================

trust_items <- c("trust_government", "trust_parliament", "trust_courts",
                 "trust_police", "trust_military", "trust_parties")

existing_trust <- intersect(trust_items, names(sausage_countries))

if (length(existing_trust) >= 3) {
  sausage_composites <- sausage_countries %>%
    mutate(
      institutional_trust_index = create_composite(., existing_trust, "trust_index", "mean")
    )

  # Calculate reliability
  if (length(existing_trust) >= 3) {
    trust_alpha <- psych::alpha(sausage_countries[, existing_trust], check.keys = TRUE)
    cat("  Institutional Trust Index: alpha =", round(trust_alpha$total$raw_alpha, 3), "\n")
  }
} else {
  sausage_composites <- sausage_countries
  cat("  Institutional Trust Index: insufficient items\n")
}

# ==============================================================================
# Create authoritarian attitudes index
# ==============================================================================

auth_items <- c("strongman_rule", "expert_rule", "military_rule", "single_party_rule")
existing_auth <- intersect(auth_items, names(sausage_composites))

if (length(existing_auth) >= 2) {
  sausage_composites <- sausage_composites %>%
    mutate(
      authoritarian_attitudes = create_composite(., existing_auth, "auth_attitudes", "mean")
    )

  if (length(existing_auth) >= 3) {
    auth_alpha <- psych::alpha(sausage_composites[, existing_auth], check.keys = TRUE)
    cat("  Authoritarian Attitudes: alpha =", round(auth_alpha$total$raw_alpha, 3), "\n")
  }
} else {
  cat("  Authoritarian Attitudes: insufficient items\n")
}

# ==============================================================================
# Create political engagement index
# ==============================================================================

engagement_items <- c("action_demonstration", "action_petition", "action_campaign")
existing_engagement <- intersect(engagement_items, names(sausage_composites))

if (length(existing_engagement) >= 2) {
  sausage_composites <- sausage_composites %>%
    mutate(
      political_participation = create_composite(., existing_engagement, "participation", "mean")
    )
  cat("  Political Participation: created from", length(existing_engagement), "items\n")
} else {
  cat("  Political Participation: insufficient items\n")
}

# ==============================================================================
# Create education categories for H2 analysis
# ==============================================================================

sausage_composites <- sausage_composites %>%
  mutate(
    # Tertile-based education groups for interaction analysis
    education_tertile = case_when(
      is.na(education_years) ~ NA_character_,
      education_years <= 9 ~ "Low",
      education_years <= 12 ~ "Middle",
      education_years > 12 ~ "High"
    ),
    education_tertile = factor(education_tertile, levels = c("Low", "Middle", "High"))
  )

cat("  Education tertiles created\n")
print(table(sausage_composites$education_tertile, useNA = "ifany"))

# ==============================================================================
# Create age cohorts
# ==============================================================================

sausage_composites <- sausage_composites %>%
  mutate(
    age_cohort = case_when(
      is.na(age) ~ NA_character_,
      age < 30 ~ "18-29",
      age < 40 ~ "30-39",
      age < 50 ~ "40-49",
      age < 60 ~ "50-59",
      TRUE ~ "60+"
    ),
    age_cohort = factor(age_cohort, levels = c("18-29", "30-39", "40-49", "50-59", "60+"))
  )

cat("  Age cohorts created\n")

cat("  -> sausage_composites:", nrow(sausage_composites), "x", ncol(sausage_composites), "\n")

# Store for next module
assign("sausage_composites", sausage_composites, envir = .GlobalEnv)
