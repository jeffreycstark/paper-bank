# 04_attitudinal_mechanisms.R
# Thailand Trust Collapse — Attitudinal Mechanism Tests (H4, H5, Satisfaction)
#
# H4: Democratic expectation updating — rejection of authoritarian rule
# H5: Political engagement moderates trust erosion
# Satisfaction with democracy differential associations
#
# Depends on: 00_data_preparation.R (thailand_panel.rds)
# Also loads political_interest and pol_discuss from ABS harmonized data
#
# Usage: Rscript papers/thailand-trust-collapse/analysis/04_attitudinal_mechanisms.R

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(sandwich)
library(lmtest)

# ── Setup ─────────────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
analysis_dir <- file.path(project_root, "papers/thailand-trust-collapse/analysis")
results_dir <- file.path(analysis_dir, "results")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(2025)

# ── Helper: clustered SEs for stacked models ─────────────────────────────────
tidy_clustered <- function(model, cluster_var) {
  vcov_cl <- vcovCL(model, cluster = cluster_var)
  ct <- coeftest(model, vcov. = vcov_cl)
  tibble(
    term = rownames(ct),
    estimate = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "t value"],
    p.value = ct[, "Pr(>|t|)"]
  )
}

# Load panel data
d <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))

# ── Add political engagement variables from ABS source ────────────────────────

source(file.path(project_root, "_data_config.R"))
abs_source <- readRDS(abs_harmonized_path)

# Merge political_interest and pol_discuss
engagement_vars <- abs_source %>%
  filter(country %in% c(6, 7, 8)) %>%
  select(country, wave, idnumber, political_interest, pol_discuss)

d <- d %>%
  left_join(engagement_vars, by = c("country", "wave", "idnumber"))

cat("Added political_interest:", sum(!is.na(d$political_interest)), "valid obs\n")
cat("Added pol_discuss:", sum(!is.na(d$pol_discuss)), "valid obs\n")

# ── Create rejection-of-authoritarian-rule variables ──────────────────────────
# Original scale: 1 = strongly disapprove ... 4 = strongly approve
# Reverse so higher = more rejection

d <- d %>%
  mutate(
    reject_military = 5 - military_rule,
    reject_strongman = 5 - strongman_rule,
    reject_single_party = 5 - single_party_rule,
    # Composite: mean of all three rejection measures
    reject_authoritarian = rowMeans(
      cbind(reject_military, reject_strongman, reject_single_party),
      na.rm = TRUE
    ),
    reject_authoritarian = if_else(is.nan(reject_authoritarian), NA_real_, reject_authoritarian),
    # Democratic commitment (0-1 scale from dem_always_preferable)
    # 1 = always prefer democracy, 0.5 = sometimes OK with authoritarian, 0 = doesn't matter
    dem_commitment_01 = case_when(
      dem_always_preferable == 1 ~ 1.0,
      dem_always_preferable == 2 ~ 0.5,
      dem_always_preferable == 3 ~ 0.0
    ),
    # Democracy priority (0-1 scale from dem_vs_econ)
    # Original: 1=econ definitely, 2=econ somewhat, 3=dem somewhat, 4=dem definitely, 5=both equally
    # Rescaled: 0=economy priority, 0.5=both equally, 1=democracy priority
    dem_priority_01 = case_when(
      dem_vs_econ == 1 ~ 0.0,
      dem_vs_econ == 2 ~ 0.25,
      dem_vs_econ == 5 ~ 0.5,
      dem_vs_econ == 3 ~ 0.75,
      dem_vs_econ == 4 ~ 1.0
    ),
    # Rescale reject_authoritarian to 0-1 for composite
    reject_authoritarian_01 = (reject_authoritarian - 1) / 3,
    # Democratic commitment composite (0-1): mean of all three 0-1 components
    democratic_commitment = rowMeans(
      cbind(dem_commitment_01, dem_priority_01, reject_authoritarian_01),
      na.rm = TRUE
    ),
    democratic_commitment = if_else(is.nan(democratic_commitment), NA_real_, democratic_commitment)
  )

controls <- "age_centered + female + education_z + is_urban"

# =============================================================================
# H4: Democratic Expectation Updating
# Rejection of authoritarian rule increases in Thailand over time
# =============================================================================

cat("\n=== H4: DEMOCRATIC EXPECTATION UPDATING ===\n")

# ── H4a: Descriptive means by country × wave ────────────────────────────────

h4_means <- d %>%
  group_by(country_name, wave_num) %>%
  summarise(
    n = n(),
    reject_military_mean = mean(reject_military, na.rm = TRUE),
    reject_military_sd = sd(reject_military, na.rm = TRUE),
    reject_strongman_mean = mean(reject_strongman, na.rm = TRUE),
    reject_single_party_mean = mean(reject_single_party, na.rm = TRUE),
    reject_authoritarian_mean = mean(reject_authoritarian, na.rm = TRUE),
    reject_authoritarian_sd = sd(reject_authoritarian, na.rm = TRUE),
    n_valid_military = sum(!is.na(reject_military)),
    .groups = "drop"
  )

cat("\nRejection of military rule by country × wave:\n")
h4_means %>%
  select(country_name, wave_num, n_valid_military, reject_military_mean) %>%
  pivot_wider(names_from = country_name, values_from = c(n_valid_military, reject_military_mean)) %>%
  print()

cat("\nRejection of authoritarian rule (composite) by country × wave:\n")
h4_means %>%
  select(country_name, wave_num, reject_authoritarian_mean) %>%
  pivot_wider(names_from = country_name, values_from = reject_authoritarian_mean) %>%
  print()

# ── H4b: Regression — rejection of military rule × country × wave ───────────

h4_reject_mil <- lm(
  reject_military ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h4_reject_mil_tidy <- tidy(h4_reject_mil, conf.int = TRUE)
cat("\nRejection of military rule regression:\n")
print(h4_reject_mil_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── H4c: Regression — composite rejection × country × wave ──────────────────

h4_reject_composite <- lm(
  reject_authoritarian ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h4_reject_composite_tidy <- tidy(h4_reject_composite, conf.int = TRUE)
cat("\nComposite rejection of authoritarian rule regression:\n")
print(h4_reject_composite_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── H4d: Thailand-only piecewise (pre/post coup) ────────────────────────────

d <- d %>%
  mutate(
    period = case_when(
      wave_num %in% 1:2 ~ "pre_coup",
      wave_num %in% 3:4 ~ "coup_era",
      wave_num %in% 5:6 ~ "protest_era"
    ),
    period = factor(period, levels = c("pre_coup", "coup_era", "protest_era"))
  )

h4_thai_piecewise <- lm(
  reject_military ~ period + age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand"),
  weights = weight
)

h4_thai_pw_tidy <- tidy(h4_thai_piecewise, conf.int = TRUE)
cat("\nThailand piecewise rejection of military rule:\n")
print(h4_thai_pw_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# ── H4e: Democratic values interaction with trust decline ───────────────────
# Does trust decline concentrate among democratic true believers?

cat("\n--- H4e: Democratic commitment × trust decline ---\n")

# Center democratic_commitment for interaction interpretation
d <- d %>%
  mutate(dem_commit_c = democratic_commitment - mean(democratic_commitment, na.rm = TRUE))

# Thailand-only: military trust
h4_commit_thai_mil <- lm(
  trust_military ~ wave_num * dem_commit_c +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand"),
  weights = weight
)

h4_commit_thai_mil_tidy <- tidy(h4_commit_thai_mil, conf.int = TRUE)
cat("\nThailand: Military trust ~ wave × democratic commitment:\n")
print(h4_commit_thai_mil_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Thailand-only: government trust
h4_commit_thai_govt <- lm(
  trust_national_government ~ wave_num * dem_commit_c +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand"),
  weights = weight
)

h4_commit_thai_govt_tidy <- tidy(h4_commit_thai_govt, conf.int = TRUE)
cat("\nThailand: Government trust ~ wave × democratic commitment:\n")
print(h4_commit_thai_govt_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Full three-country model for comparison
h4_commit_full_mil <- lm(
  trust_military ~ wave_num * dem_commit_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h4_commit_full_mil_tidy <- tidy(h4_commit_full_mil, conf.int = TRUE)
cat("\nFull sample: Military trust ~ wave × commitment × country:\n")
print(h4_commit_full_mil_tidy %>%
        filter(str_detect(term, "wave|commit|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Save H4 results
saveRDS(list(
  means = h4_means,
  reject_mil_model = list(
    model = h4_reject_mil, tidy = h4_reject_mil_tidy,
    glance = glance(h4_reject_mil)
  ),
  reject_composite_model = list(
    model = h4_reject_composite, tidy = h4_reject_composite_tidy,
    glance = glance(h4_reject_composite)
  ),
  thai_piecewise = list(
    model = h4_thai_piecewise, tidy = h4_thai_pw_tidy,
    glance = glance(h4_thai_piecewise)
  ),
  commit_thai_mil = list(
    model = h4_commit_thai_mil, tidy = h4_commit_thai_mil_tidy,
    glance = glance(h4_commit_thai_mil)
  ),
  commit_thai_govt = list(
    model = h4_commit_thai_govt, tidy = h4_commit_thai_govt_tidy,
    glance = glance(h4_commit_thai_govt)
  ),
  commit_full_mil = list(
    model = h4_commit_full_mil, tidy = h4_commit_full_mil_tidy,
    glance = glance(h4_commit_full_mil)
  )
), file.path(results_dir, "h4_democratic_expectations.rds"))

cat("H4 results saved.\n\n")

# =============================================================================
# H5: Political Engagement Moderates Trust Erosion
# Higher political interest → steeper trust decline, esp. in Thailand
# =============================================================================

cat("=== H5: POLITICAL ENGAGEMENT AND TRUST EROSION ===\n")

# ── H5a: Descriptive means ──────────────────────────────────────────────────

h5_means <- d %>%
  group_by(country_name, wave_num) %>%
  summarise(
    pol_interest_mean = mean(political_interest, na.rm = TRUE),
    pol_interest_sd = sd(political_interest, na.rm = TRUE),
    pol_discuss_mean = mean(pol_discuss, na.rm = TRUE),
    n_interest = sum(!is.na(political_interest)),
    n_discuss = sum(!is.na(pol_discuss)),
    .groups = "drop"
  )

cat("\nPolitical interest by country × wave:\n")
h5_means %>%
  select(country_name, wave_num, pol_interest_mean) %>%
  pivot_wider(names_from = country_name, values_from = pol_interest_mean) %>%
  print()

# ── H5b: political_interest × wave × country on government trust ────────────

# Center political_interest for interaction interpretation
d <- d %>%
  mutate(pol_interest_c = political_interest - mean(political_interest, na.rm = TRUE))

h5_govt <- lm(
  trust_national_government ~ wave_num * pol_interest_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h5_govt_tidy <- tidy(h5_govt, conf.int = TRUE)
cat("\nH5 Government trust (interest × wave × country):\n")
print(h5_govt_tidy %>%
        filter(str_detect(term, "wave_num|pol_interest|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── H5c: political_interest × wave × country on military trust ──────────────

h5_mil <- lm(
  trust_military ~ wave_num * pol_interest_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h5_mil_tidy <- tidy(h5_mil, conf.int = TRUE)
cat("\nH5 Military trust (interest × wave × country):\n")
print(h5_mil_tidy %>%
        filter(str_detect(term, "wave_num|pol_interest|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── H5d: Same models with pol_discuss ───────────────────────────────────────

d <- d %>%
  mutate(pol_discuss_c = pol_discuss - mean(pol_discuss, na.rm = TRUE))

h5_discuss_govt <- lm(
  trust_national_government ~ wave_num * pol_discuss_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h5_discuss_mil <- lm(
  trust_military ~ wave_num * pol_discuss_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h5_discuss_govt_tidy <- tidy(h5_discuss_govt, conf.int = TRUE)
h5_discuss_mil_tidy <- tidy(h5_discuss_mil, conf.int = TRUE)

cat("\nH5 Government trust (discuss × wave × country):\n")
print(h5_discuss_govt_tidy %>%
        filter(str_detect(term, "wave_num|pol_discuss|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nH5 Military trust (discuss × wave × country):\n")
print(h5_discuss_mil_tidy %>%
        filter(str_detect(term, "wave_num|pol_discuss|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── H5e: Thailand-only models for cleaner interpretation ────────────────────

thai <- d %>% filter(country_name == "Thailand")

h5_thai_govt <- lm(
  trust_national_government ~ wave_num * pol_interest_c +
    age_centered + female + education_z + is_urban,
  data = thai, weights = weight
)

h5_thai_mil <- lm(
  trust_military ~ wave_num * pol_interest_c +
    age_centered + female + education_z + is_urban,
  data = thai, weights = weight
)

h5_thai_govt_tidy <- tidy(h5_thai_govt, conf.int = TRUE)
h5_thai_mil_tidy <- tidy(h5_thai_mil, conf.int = TRUE)

cat("\nThailand-only: Government trust (interest × wave):\n")
print(h5_thai_govt_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("\nThailand-only: Military trust (interest × wave):\n")
print(h5_thai_mil_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Save H5 results
saveRDS(list(
  means = h5_means,
  interest_govt = list(model = h5_govt, tidy = h5_govt_tidy, glance = glance(h5_govt)),
  interest_mil = list(model = h5_mil, tidy = h5_mil_tidy, glance = glance(h5_mil)),
  discuss_govt = list(model = h5_discuss_govt, tidy = h5_discuss_govt_tidy, glance = glance(h5_discuss_govt)),
  discuss_mil = list(model = h5_discuss_mil, tidy = h5_discuss_mil_tidy, glance = glance(h5_discuss_mil)),
  thai_interest_govt = list(model = h5_thai_govt, tidy = h5_thai_govt_tidy, glance = glance(h5_thai_govt)),
  thai_interest_mil = list(model = h5_thai_mil, tidy = h5_thai_mil_tidy, glance = glance(h5_thai_mil))
), file.path(results_dir, "h5_political_engagement.rds"))

cat("H5 results saved.\n\n")

# =============================================================================
# Satisfaction with Democracy — Differential Associations
# =============================================================================

cat("=== SATISFACTION WITH DEMOCRACY ===\n")

# ── Descriptive means ───────────────────────────────────────────────────────

sat_means <- d %>%
  group_by(country_name, wave_num) %>%
  summarise(
    dem_sat_mean = mean(democracy_satisfaction, na.rm = TRUE),
    dem_sat_sd = sd(democracy_satisfaction, na.rm = TRUE),
    n_valid = sum(!is.na(democracy_satisfaction)),
    .groups = "drop"
  )

cat("\nDemocracy satisfaction by country × wave:\n")
sat_means %>%
  select(country_name, wave_num, dem_sat_mean) %>%
  pivot_wider(names_from = country_name, values_from = dem_sat_mean) %>%
  print()

# ── Differential association: dem_satisfaction → govt vs military trust ──────

# Stack govt and military trust for Thailand
thai_long <- d %>%
  filter(country_name == "Thailand") %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    cols = c(trust_national_government, trust_military),
    names_to = "institution",
    values_to = "trust"
  ) %>%
  mutate(is_military = if_else(institution == "trust_military", 1L, 0L)) %>%
  filter(!is.na(trust), !is.na(democracy_satisfaction))

sat_diff <- lm(
  trust ~ democracy_satisfaction * is_military + wave_num +
    age_centered + female + education_z + is_urban,
  data = thai_long, weights = weight
)

sat_diff_tidy <- tidy_clustered(sat_diff, thai_long$respondent_id)
cat("\nDifferential association (Thailand):\n")
print(sat_diff_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# ── Same for full sample with country interactions ──────────────────────────

all_long <- d %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    cols = c(trust_national_government, trust_military),
    names_to = "institution",
    values_to = "trust"
  ) %>%
  mutate(is_military = if_else(institution == "trust_military", 1L, 0L)) %>%
  filter(!is.na(trust), !is.na(democracy_satisfaction))

sat_full <- lm(
  trust ~ democracy_satisfaction * is_military * country_name + wave_num +
    age_centered + female + education_z + is_urban,
  data = all_long, weights = weight
)

sat_full_tidy <- tidy_clustered(sat_full, all_long$respondent_id)
cat("\nFull-sample differential association:\n")
print(sat_full_tidy %>%
        filter(str_detect(term, "democracy|military|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# ── Thailand dem_satisfaction trend regression ──────────────────────────────

sat_trend <- lm(
  democracy_satisfaction ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

sat_trend_tidy <- tidy(sat_trend, conf.int = TRUE)
cat("\nDemocracy satisfaction trend (country × wave):\n")
print(sat_trend_tidy %>%
        filter(str_detect(term, "wave|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Save satisfaction results
saveRDS(list(
  means = sat_means,
  thai_differential = list(model = sat_diff, tidy = sat_diff_tidy, glance = glance(sat_diff)),
  full_differential = list(model = sat_full, tidy = sat_full_tidy, glance = glance(sat_full)),
  trend = list(model = sat_trend, tidy = sat_trend_tidy, glance = glance(sat_trend))
), file.path(results_dir, "sat_democracy.rds"))

cat("Satisfaction results saved.\n\n")

# =============================================================================
# Philippines H3 descriptive values (for manuscript inline)
# =============================================================================

cat("=== PHILIPPINES H3 DESCRIPTIVE VALUES ===\n")

phil_means <- d %>%
  filter(country_name == "Philippines") %>%
  group_by(wave_num) %>%
  summarise(
    trust_mil = mean(trust_military, na.rm = TRUE),
    trust_police = mean(trust_police, na.rm = TRUE),
    trust_govt = mean(trust_national_government, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nPhilippines trust means by wave:\n")
print(phil_means %>% mutate(across(where(is.numeric), ~round(., 2))))

saveRDS(phil_means, file.path(results_dir, "h3_philippines_means.rds"))

# =============================================================================
# Democratic Commitment vs Satisfaction Divergence (Expectation Updating)
# =============================================================================

cat("\n=== DEMOCRATIC COMMITMENT VS SATISFACTION DIVERGENCE ===\n")

# ── Descriptive means by country × wave ─────────────────────────────────────

commitment_means <- d %>%
  group_by(country_name, wave_num) %>%
  summarise(
    dem_commitment = mean(democratic_commitment, na.rm = TRUE),
    dem_commitment_sd = sd(democratic_commitment, na.rm = TRUE),
    dem_satisfaction = mean(democracy_satisfaction, na.rm = TRUE),
    dem_satisfaction_sd = sd(democracy_satisfaction, na.rm = TRUE),
    dem_commitment_01 = mean(dem_commitment_01, na.rm = TRUE),
    dem_priority_01 = mean(dem_priority_01, na.rm = TRUE),
    reject_auth_01 = mean(reject_authoritarian_01, na.rm = TRUE),
    n_commitment = sum(!is.na(democratic_commitment)),
    n_satisfaction = sum(!is.na(democracy_satisfaction)),
    .groups = "drop"
  )

cat("\nDemocratic commitment (composite 0-1) by country × wave:\n")
commitment_means %>%
  select(country_name, wave_num, dem_commitment) %>%
  pivot_wider(names_from = country_name, values_from = dem_commitment) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

cat("\nDemocracy satisfaction by country × wave:\n")
commitment_means %>%
  select(country_name, wave_num, dem_satisfaction) %>%
  pivot_wider(names_from = country_name, values_from = dem_satisfaction) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

cat("\nComponent means for Thailand:\n")
commitment_means %>%
  filter(country_name == "Thailand") %>%
  select(wave_num, dem_commitment_01, dem_priority_01, reject_auth_01, dem_commitment) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

# ── Regression: democratic commitment trend ─────────────────────────────────

commit_trend <- lm(
  democratic_commitment ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

commit_trend_tidy <- tidy(commit_trend, conf.int = TRUE)
cat("\nDemocratic commitment trend (country × wave):\n")
print(commit_trend_tidy %>%
        filter(str_detect(term, "wave|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# =============================================================================
# Ideological Alignment and Trust Erosion (Section 4.X)
# =============================================================================

cat("\n=== IDEOLOGICAL ALIGNMENT AND TRUST EROSION ===\n")

# Center democratic commitment for interaction interpretation
d <- d %>%
  mutate(dem_commit_c = democratic_commitment - mean(democratic_commitment, na.rm = TRUE))

# Create stacked (long) trust data for military vs government comparison
d_stacked <- d %>%
  filter(country_name == "Thailand") %>%
  mutate(respondent_id = row_number()) %>%
  select(respondent_id, wave_num, trust_military, trust_national_government,
         democratic_commitment, dem_commit_c, political_interest,
         age_centered, female, education_z, is_urban, weight) %>%
  pivot_longer(
    cols = c(trust_military, trust_national_government),
    names_to = "trust_type",
    values_to = "trust"
  ) %>%
  mutate(is_military = as.integer(trust_type == "trust_military"))

# ── Model 1: Does democratic commitment predict steeper military trust decline?
cat("\n--- Model 1: Military trust ~ wave * democratic_commitment (Thailand) ---\n")
mod1_ideo <- lm(
  trust_military ~ wave_num * dem_commit_c +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand"),
  weights = weight
)
mod1_ideo_tidy <- tidy(mod1_ideo, conf.int = TRUE)
print(mod1_ideo_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(mod1_ideo), " R² =", round(summary(mod1_ideo)$r.squared, 4), "\n")

# ── Model 2: Is this specific to military trust? (stacked comparison)
cat("\n--- Model 2: Trust ~ wave * democratic_commitment * is_military (Thailand, stacked) ---\n")
mod2_ideo <- lm(
  trust ~ wave_num * dem_commit_c * is_military +
    age_centered + female + education_z + is_urban,
  data = d_stacked, weights = weight
)
mod2_ideo_tidy <- tidy_clustered(mod2_ideo, d_stacked$respondent_id)
print(mod2_ideo_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(mod2_ideo), " R² =", round(summary(mod2_ideo)$r.squared, 4), "\n")

# ── Model 3: Wave 6 cross-section — who distrusts military most?
cat("\n--- Model 3: Wave 6 cross-section — military trust ~ democratic_commitment + political_interest ---\n")
mod3_ideo <- lm(
  trust_military ~ democratic_commitment + political_interest +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand", wave == 6),
  weights = weight
)
mod3_ideo_tidy <- tidy(mod3_ideo, conf.int = TRUE)
print(mod3_ideo_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(mod3_ideo), " R² =", round(summary(mod3_ideo)$r.squared, 4), "\n")

# ── Model 1b: Government trust version for comparison
cat("\n--- Model 1b: Government trust ~ wave * democratic_commitment (Thailand) ---\n")
mod1b_ideo <- lm(
  trust_national_government ~ wave_num * dem_commit_c +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name == "Thailand"),
  weights = weight
)
mod1b_ideo_tidy <- tidy(mod1b_ideo, conf.int = TRUE)
print(mod1b_ideo_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(mod1b_ideo), " R² =", round(summary(mod1b_ideo)$r.squared, 4), "\n")

# ── Three-country comparison
cat("\n--- Model 4: Three-country military trust ~ wave * democratic_commitment * country ---\n")
mod4_ideo <- lm(
  trust_military ~ wave_num * dem_commit_c * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)
mod4_ideo_tidy <- tidy(mod4_ideo, conf.int = TRUE)
cat("Key interaction terms:\n")
print(mod4_ideo_tidy %>%
        filter(str_detect(term, "dem_commit|wave.*commit|commit.*country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(mod4_ideo), " R² =", round(summary(mod4_ideo)$r.squared, 4), "\n")

# ── Save all ideological alignment results ────────────────────────────────────

saveRDS(list(
  means = commitment_means,
  trend = list(model = commit_trend, tidy = commit_trend_tidy, glance = glance(commit_trend)),
  mod1_mil = list(model = mod1_ideo, tidy = mod1_ideo_tidy, glance = glance(mod1_ideo)),
  mod1b_govt = list(model = mod1b_ideo, tidy = mod1b_ideo_tidy, glance = glance(mod1b_ideo)),
  mod2_stacked = list(model = mod2_ideo, tidy = mod2_ideo_tidy, glance = glance(mod2_ideo)),
  mod3_w6_xsec = list(model = mod3_ideo, tidy = mod3_ideo_tidy, glance = glance(mod3_ideo)),
  mod4_threecountry = list(model = mod4_ideo, tidy = mod4_ideo_tidy, glance = glance(mod4_ideo))
), file.path(results_dir, "democratic_commitment.rds"))

cat("\nDemocratic commitment + ideological alignment results saved.\n")

# =============================================================================
# Regional Analysis: Bangkok vs Provinces (W4-W6 only)
# =============================================================================

cat("\n=== REGIONAL ANALYSIS: BANGKOK VS PROVINCES ===\n")

# ── Load region from raw .sav files (W4-W6 only) ─────────────────────────────

library(haven)

# W4 (merged file, Thailand = country 8)
raw_w4 <- read_sav("/Users/jeffreystark/Development/Research/survey-data-prep/data/abs/raw/wave4/W4_v15_merged20250609_release.sav")
region_w4 <- raw_w4 %>%
  filter(country == 8) %>%
  transmute(wave = 4L, idnumber = as.integer(idnumber),
            region = as.character(as_factor(region)))

# W5 (merged file)
raw_w5 <- read_sav("/Users/jeffreystark/Development/Research/survey-data-prep/data/abs/raw/wave5/20230505_W5_merge_15.sav")
region_w5 <- raw_w5 %>%
  filter(COUNTRY == 8) %>%
  transmute(wave = 5L, idnumber = as.integer(IDnumber),
            region = as.character(as_factor(Region)))

# W6 (Thailand-only file)
raw_w6 <- read_sav("/Users/jeffreystark/Development/Research/survey-data-prep/data/abs/raw/wave6/W6_8_Thailand_Release_20250108.sav")
region_w6 <- raw_w6 %>%
  transmute(wave = 6L, idnumber = as.integer(IDNUMBER),
            region = as.character(as_factor(REGION)))

region_all <- bind_rows(region_w4, region_w5, region_w6)
cat("Region data loaded:", nrow(region_all), "obs\n")
cat("Region distribution:\n")
print(table(region_all$region, region_all$wave))

# ── Merge into panel and create Bangkok indicator ─────────────────────────────

d_regional <- d %>%
  filter(country_name == "Thailand", wave_num >= 4) %>%
  left_join(region_all, by = c("wave" = "wave", "idnumber" = "idnumber")) %>%
  mutate(
    is_bangkok = as.integer(region == "Bangkok"),
    region_factor = factor(region, levels = c("South", "Bangkok", "Central", "North", "Northeast"))
  )

cat("\nMerge result:\n")
cat("Total Thai W4-W6:", nrow(d_regional), "\n")
cat("Region matched:", sum(!is.na(d_regional$region)), "\n")
cat("Bangkok:", sum(d_regional$is_bangkok == 1, na.rm = TRUE), "\n")

# ── Descriptive means by region × wave ────────────────────────────────────────

region_means <- d_regional %>%
  group_by(region, wave_num) %>%
  summarise(
    trust_mil = mean(trust_military, na.rm = TRUE),
    trust_govt = mean(trust_national_government, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("\nMilitary trust by region × wave:\n")
region_means %>%
  select(region, wave_num, trust_mil) %>%
  pivot_wider(names_from = wave_num, values_from = trust_mil, names_prefix = "W") %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()

cat("\nGovernment trust by region × wave:\n")
region_means %>%
  select(region, wave_num, trust_govt) %>%
  pivot_wider(names_from = wave_num, values_from = trust_govt, names_prefix = "W") %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()

# ── Model: Bangkok interaction with wave ──────────────────────────────────────

cat("\n--- Military trust ~ wave * is_bangkok ---\n")
reg_mil_bkk <- lm(
  trust_military ~ wave_num * is_bangkok +
    age_centered + female + education_z,
  data = d_regional, weights = weight
)
reg_mil_bkk_tidy <- tidy(reg_mil_bkk, conf.int = TRUE)
print(reg_mil_bkk_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(reg_mil_bkk), " R² =", round(summary(reg_mil_bkk)$r.squared, 4), "\n")

cat("\n--- Government trust ~ wave * is_bangkok ---\n")
reg_govt_bkk <- lm(
  trust_national_government ~ wave_num * is_bangkok +
    age_centered + female + education_z,
  data = d_regional, weights = weight
)
reg_govt_bkk_tidy <- tidy(reg_govt_bkk, conf.int = TRUE)
print(reg_govt_bkk_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(reg_govt_bkk), " R² =", round(summary(reg_govt_bkk)$r.squared, 4), "\n")

# ── Model: Full region interaction ────────────────────────────────────────────

cat("\n--- Military trust ~ wave * region_factor (South = reference) ---\n")
reg_mil_full <- lm(
  trust_military ~ wave_num * region_factor +
    age_centered + female + education_z,
  data = d_regional, weights = weight
)
reg_mil_full_tidy <- tidy(reg_mil_full, conf.int = TRUE)
print(reg_mil_full_tidy %>%
        filter(str_detect(term, "wave|region")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(reg_mil_full), " R² =", round(summary(reg_mil_full)$r.squared, 4), "\n")

cat("\n--- Government trust ~ wave * region_factor (South = reference) ---\n")
reg_govt_full <- lm(
  trust_national_government ~ wave_num * region_factor +
    age_centered + female + education_z,
  data = d_regional, weights = weight
)
reg_govt_full_tidy <- tidy(reg_govt_full, conf.int = TRUE)
print(reg_govt_full_tidy %>%
        filter(str_detect(term, "wave|region")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))
cat("N =", nobs(reg_govt_full), " R² =", round(summary(reg_govt_full)$r.squared, 4), "\n")

# ── Save regional results ─────────────────────────────────────────────────────

saveRDS(list(
  means = region_means,
  bkk_mil = list(model = reg_mil_bkk, tidy = reg_mil_bkk_tidy, glance = glance(reg_mil_bkk)),
  bkk_govt = list(model = reg_govt_bkk, tidy = reg_govt_bkk_tidy, glance = glance(reg_govt_bkk)),
  full_region_mil = list(model = reg_mil_full, tidy = reg_mil_full_tidy, glance = glance(reg_mil_full)),
  full_region_govt = list(model = reg_govt_full, tidy = reg_govt_full_tidy, glance = glance(reg_govt_full))
), file.path(results_dir, "regional_analysis.rds"))

cat("\nRegional analysis results saved.\n")

cat("\n=== ALL ATTITUDINAL MECHANISM RESULTS SAVED ===\n")
cat("Files:\n")
cat(paste(" ", list.files(results_dir, pattern = "h[345]_|sat_|dem|region"), collapse = "\n"), "\n")
