# 03_hypothesis_tests.R
# Thailand Trust Collapse — Formal Hypothesis Tests
#
# Tests all six hypotheses (H1-H6) and saves structured results to results/.
# Depends on: 00_data_preparation.R (must be run first to produce thailand_panel.rds)
#
# Usage: Rscript papers/thailand-trust-collapse/analysis/03_hypothesis_tests.R

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(MASS)
library(sandwich)
library(lmtest)

# ── Setup ─────────────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
analysis_dir <- file.path(project_root, "papers/thailand-trust-collapse/analysis")
results_dir <- file.path(analysis_dir, "results")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(2025)

d <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))
cat("Loaded:", format(nrow(d), big.mark = ","), "obs across",
    length(unique(d$country_name)), "countries,",
    length(unique(d$wave_num)), "waves\n\n")

source(file.path(project_root, "papers/thailand-trust-collapse/R/helpers.R"))

# =============================================================================
# H1: Thailand Exceptionalism
# Thailand has steeper trust decline than Philippines/Taiwan, net of demographics
# =============================================================================

cat("=== H1: THAILAND EXCEPTIONALISM ===\n")

# H1a: Fixed-effects OLS with country × wave interaction — Government trust
h1_govt <- lm(
  trust_national_government ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

cat("H1 Government trust interaction model:\n")
h1_govt_tidy <- tidy(h1_govt, conf.int = TRUE)
print(h1_govt_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# H1b: Fixed-effects OLS with country × wave interaction — Military trust
h1_mil <- lm(
  trust_military ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

cat("\nH1 Military trust interaction model:\n")
h1_mil_tidy <- tidy(h1_mil, conf.int = TRUE)
print(h1_mil_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# H1c: Random-slopes MLM — Government trust
h1_rs_govt <- lmer(
  trust_national_government ~ wave_num +
    age_centered + female + education_z + is_urban +
    (1 + wave_num | country_name),
  data = d, weights = weight, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# H1d: Random-slopes MLM — Military trust
h1_rs_mil <- lmer(
  trust_military ~ wave_num +
    age_centered + female + education_z + is_urban +
    (1 + wave_num | country_name),
  data = d, weights = weight, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# Extract country-specific slopes
h1_re_govt <- ranef(h1_rs_govt)$country_name %>%
  rownames_to_column("country") %>%
  rename(intercept_dev = `(Intercept)`, slope_dev = wave_num) %>%
  mutate(institution = "government")

h1_re_mil <- ranef(h1_rs_mil)$country_name %>%
  rownames_to_column("country") %>%
  rename(intercept_dev = `(Intercept)`, slope_dev = wave_num) %>%
  mutate(institution = "military")

cat("\nCountry-specific slope deviations (government):\n")
print(h1_re_govt %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("\nCountry-specific slope deviations (military):\n")
print(h1_re_mil %>% mutate(across(where(is.numeric), ~round(., 4))))

# Save H1 results
saveRDS(list(
  model = h1_govt,
  tidy = h1_govt_tidy,
  glance = glance(h1_govt)
), file.path(results_dir, "h1_govt_interaction.rds"))

saveRDS(list(
  model = h1_mil,
  tidy = h1_mil_tidy,
  glance = glance(h1_mil)
), file.path(results_dir, "h1_mil_interaction.rds"))

saveRDS(list(
  model = h1_rs_govt,
  tidy = tidy(h1_rs_govt, effects = "fixed", conf.int = TRUE),
  random_effects = h1_re_govt,
  variance = as.data.frame(VarCorr(h1_rs_govt))
), file.path(results_dir, "h1_random_slopes_govt.rds"))

saveRDS(list(
  model = h1_rs_mil,
  tidy = tidy(h1_rs_mil, effects = "fixed", conf.int = TRUE),
  random_effects = h1_re_mil,
  variance = as.data.frame(VarCorr(h1_rs_mil))
), file.path(results_dir, "h1_random_slopes_mil.rds"))

cat("H1 results saved.\n\n")

# =============================================================================
# H2: Institutional Differentiation
# Military trust declines more than government trust in Thailand, esp. W5→W6
# =============================================================================

cat("=== H2: INSTITUTIONAL DIFFERENTIATION ===\n")

# H2a: Three-way interaction in long format
# Stack government and military trust per respondent
d_long <- d %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    cols = c(trust_national_government, trust_military),
    names_to = "institution",
    values_to = "trust"
  ) %>%
  mutate(
    is_military = if_else(institution == "trust_military", 1L, 0L)
  ) %>%
  filter(!is.na(trust))

cat("Long-format data:", format(nrow(d_long), big.mark = ","), "obs\n")

# OLS with survey weights and respondent-clustered SEs
h2_threeway <- lm(
  trust ~ wave_num * is_military * country_name +
    age_centered + female + education_z + is_urban,
  data = d_long, weights = weight
)

h2_threeway_tidy <- tidy_clustered(h2_threeway, d_long$respondent_id)
cat("\nThree-way interaction results (clustered SEs, key terms):\n")
print(h2_threeway_tidy %>%
        filter(str_detect(term, "is_military|wave_num:")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# H2b: W5→W6 acceleration test
# Create period dummies for Thailand
d_long <- d_long %>%
  mutate(
    is_w5w6 = if_else(wave_num >= 5, 1L, 0L),
    is_thailand = if_else(country_name == "Thailand", 1L, 0L)
  )

h2_w5w6 <- lm(
  trust ~ wave_num + is_w5w6 * is_military * is_thailand +
    age_centered + female + education_z + is_urban +
    country_name,
  data = d_long, weights = weight
)

h2_w5w6_tidy <- tidy_clustered(h2_w5w6, d_long$respondent_id)
cat("\nW5-W6 acceleration test (clustered SEs, key terms):\n")
print(h2_w5w6_tidy %>%
        filter(str_detect(term, "is_w5w6|is_military|is_thailand")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Save H2 results
saveRDS(list(
  model = h2_threeway,
  tidy = h2_threeway_tidy,
  glance = glance(h2_threeway)
), file.path(results_dir, "h2_three_way_interaction.rds"))

saveRDS(list(
  model = h2_w5w6,
  tidy = h2_w5w6_tidy,
  glance = glance(h2_w5w6)
), file.path(results_dir, "h2_w5w6_acceleration.rds"))

cat("H2 results saved.\n\n")

# =============================================================================
# H3: Democratic Expectation Updating
# Steepest declines after democratic experience (non-linear time)
# =============================================================================

cat("=== H3: DEMOCRATIC EXPECTATION UPDATING ===\n")

# H3a: Piecewise slopes with political period dummies
d <- d %>%
  mutate(
    period = case_when(
      wave_num %in% 1:2 ~ "pre_coup",
      wave_num %in% 3:4 ~ "coup_era",
      wave_num %in% 5:6 ~ "protest_era"
    ),
    period = factor(period, levels = c("pre_coup", "coup_era", "protest_era"))
  )

h3_piecewise_govt <- lm(
  trust_national_government ~ period * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h3_piecewise_mil <- lm(
  trust_military ~ period * country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h3_pw_govt_tidy <- tidy(h3_piecewise_govt, conf.int = TRUE)
h3_pw_mil_tidy <- tidy(h3_piecewise_mil, conf.int = TRUE)

cat("Piecewise slopes — Government trust (key terms):\n")
print(h3_pw_govt_tidy %>%
        filter(str_detect(term, "period|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nPiecewise slopes — Military trust (key terms):\n")
print(h3_pw_mil_tidy %>%
        filter(str_detect(term, "period|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# H3b: Quadratic specification with country interactions
h3_quad_govt <- lm(
  trust_national_government ~ wave_num + I(wave_num^2) +
    wave_num:country_name + I(wave_num^2):country_name +
    country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h3_quad_mil <- lm(
  trust_military ~ wave_num + I(wave_num^2) +
    wave_num:country_name + I(wave_num^2):country_name +
    country_name +
    age_centered + female + education_z + is_urban,
  data = d, weights = weight
)

h3_q_govt_tidy <- tidy(h3_quad_govt, conf.int = TRUE)
h3_q_mil_tidy <- tidy(h3_quad_mil, conf.int = TRUE)

cat("\nQuadratic specification — Government trust:\n")
print(h3_q_govt_tidy %>%
        filter(str_detect(term, "wave|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nQuadratic specification — Military trust:\n")
print(h3_q_mil_tidy %>%
        filter(str_detect(term, "wave|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Save H3 results
saveRDS(list(
  govt = list(model = h3_piecewise_govt, tidy = h3_pw_govt_tidy, glance = glance(h3_piecewise_govt)),
  mil = list(model = h3_piecewise_mil, tidy = h3_pw_mil_tidy, glance = glance(h3_piecewise_mil))
), file.path(results_dir, "h3_piecewise_slopes.rds"))

saveRDS(list(
  govt = list(model = h3_quad_govt, tidy = h3_q_govt_tidy, glance = glance(h3_quad_govt)),
  mil = list(model = h3_quad_mil, tidy = h3_q_mil_tidy, glance = glance(h3_quad_mil))
), file.path(results_dir, "h3_quadratic.rds"))

cat("H3 results saved.\n\n")

# =============================================================================
# H4: Preference Alignment — Philippines
# Coercive trust stable/rising during Duterte period (W4-W6)
# =============================================================================

cat("=== H4: PREFERENCE ALIGNMENT (PHILIPPINES) ===\n")

phil_w456 <- d %>% filter(country_name == "Philippines", wave_num %in% 4:6)
cat("Philippines W4-W6 sample:", nrow(phil_w456), "obs\n")

h4_mil <- lm(
  trust_military ~ wave_num + age_centered + female + education_z + is_urban,
  data = phil_w456, weights = weight
)

h4_police <- lm(
  trust_police ~ wave_num + age_centered + female + education_z + is_urban,
  data = phil_w456, weights = weight
)

h4_govt <- lm(
  trust_national_government ~ wave_num + age_centered + female + education_z + is_urban,
  data = phil_w456, weights = weight
)

h4_mil_tidy <- tidy(h4_mil, conf.int = TRUE)
h4_police_tidy <- tidy(h4_police, conf.int = TRUE)
h4_govt_tidy <- tidy(h4_govt, conf.int = TRUE)

cat("\nPhilippines military trust (W4-W6):\n")
print(h4_mil_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("\nPhilippines police trust (W4-W6):\n")
print(h4_police_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))
cat("\nPhilippines government trust (W4-W6):\n")
print(h4_govt_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Full-sample contrast: Philippines coercive slopes vs Thailand
h4_contrast_mil <- lm(
  trust_military ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name %in% c("Thailand", "Philippines")),
  weights = weight
)

h4_contrast_police <- lm(
  trust_police ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d %>% filter(country_name %in% c("Thailand", "Philippines")),
  weights = weight
)

# Save H4 results
saveRDS(list(
  phil_military = list(model = h4_mil, tidy = h4_mil_tidy, glance = glance(h4_mil)),
  phil_police = list(model = h4_police, tidy = h4_police_tidy, glance = glance(h4_police)),
  phil_govt = list(model = h4_govt, tidy = h4_govt_tidy, glance = glance(h4_govt)),
  contrast_mil = list(model = h4_contrast_mil, tidy = tidy(h4_contrast_mil, conf.int = TRUE)),
  contrast_police = list(model = h4_contrast_police, tidy = tidy(h4_contrast_police, conf.int = TRUE))
), file.path(results_dir, "h4_philippines_coercive_trust.rds"))

cat("H4 results saved.\n\n")

# =============================================================================
# H5: Depoliticization — Taiwan
# Military trust flat across all waves
# =============================================================================

cat("=== H5: DEPOLITICIZATION (TAIWAN) ===\n")

taiwan <- d %>% filter(country_name == "Taiwan")
cat("Taiwan sample:", nrow(taiwan), "obs\n")

h5_mil <- lm(
  trust_military ~ wave_num + age_centered + female + education_z + is_urban,
  data = taiwan, weights = weight
)

h5_mil_tidy <- tidy(h5_mil, conf.int = TRUE)
cat("\nTaiwan military trust trend:\n")
print(h5_mil_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# F-test: is the wave coefficient = 0?
h5_null <- lm(
  trust_military ~ age_centered + female + education_z + is_urban,
  data = taiwan, weights = weight
)
h5_ftest <- anova(h5_null, h5_mil)
cat("\nF-test (wave = 0):\n")
print(h5_ftest)

# Save H5 results
saveRDS(list(
  model = h5_mil,
  tidy = h5_mil_tidy,
  glance = glance(h5_mil),
  ftest = h5_ftest
), file.path(results_dir, "h5_taiwan_military_stability.rds"))

cat("H5 results saved.\n\n")

# =============================================================================
# H6: Robustness
# =============================================================================

cat("=== H6: ROBUSTNESS ===\n")

# ── H6a: Performance controls ────────────────────────────────────────────────

cat("--- H6a: Performance controls ---\n")

# Add economic satisfaction and democracy satisfaction to H1 models
d_perf <- d %>% filter(!is.na(econ_national_now), !is.na(democracy_satisfaction))
cat("Performance controls sample (complete cases):", nrow(d_perf), "obs\n")

h6a_govt <- lm(
  trust_national_government ~ wave_num * country_name +
    age_centered + female + education_z + is_urban +
    econ_national_now + democracy_satisfaction,
  data = d_perf, weights = weight
)

h6a_mil <- lm(
  trust_military ~ wave_num * country_name +
    age_centered + female + education_z + is_urban +
    econ_national_now + democracy_satisfaction,
  data = d_perf, weights = weight
)

h6a_govt_tidy <- tidy(h6a_govt, conf.int = TRUE)
h6a_mil_tidy <- tidy(h6a_mil, conf.int = TRUE)

cat("\nPerformance-controlled government trust:\n")
print(h6a_govt_tidy %>%
        filter(str_detect(term, "wave_num|econ|democracy")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nPerformance-controlled military trust:\n")
print(h6a_mil_tidy %>%
        filter(str_detect(term, "wave_num|econ|democracy")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  govt = list(model = h6a_govt, tidy = h6a_govt_tidy, glance = glance(h6a_govt)),
  mil = list(model = h6a_mil, tidy = h6a_mil_tidy, glance = glance(h6a_mil)),
  n_obs = nrow(d_perf)
), file.path(results_dir, "h6_performance_controls.rds"))

# ── H6b: Pre-trend check (W1-W4) ─────────────────────────────────────────────

cat("\n--- H6b: Pre-trend check (W1-W4) ---\n")

pre_data <- d %>% filter(wave_num %in% 1:4)
cat("Pre-COVID sample:", nrow(pre_data), "obs\n")

h6b_govt <- lm(
  trust_national_government ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = pre_data, weights = weight
)

h6b_mil <- lm(
  trust_military ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = pre_data, weights = weight
)

h6b_govt_tidy <- tidy(h6b_govt, conf.int = TRUE)
h6b_mil_tidy <- tidy(h6b_mil, conf.int = TRUE)

cat("\nPre-trend government trust:\n")
print(h6b_govt_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nPre-trend military trust:\n")
print(h6b_mil_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  govt = list(model = h6b_govt, tidy = h6b_govt_tidy, glance = glance(h6b_govt)),
  mil = list(model = h6b_mil, tidy = h6b_mil_tidy, glance = glance(h6b_mil)),
  n_obs = nrow(pre_data)
), file.path(results_dir, "h6_pretrend.rds"))

# ── H6c: Ordered logit ──────────────────────────────────────────────────────

cat("\n--- H6c: Ordered logit ---\n")

d_ologit <- d %>%
  mutate(
    trust_govt_ord = factor(trust_national_government, ordered = TRUE),
    trust_mil_ord = factor(trust_military, ordered = TRUE)
  ) %>%
  filter(!is.na(trust_govt_ord), !is.na(trust_mil_ord),
         !is.na(age_centered), !is.na(female),
         !is.na(education_z), !is.na(is_urban))

h6c_govt <- polr(
  trust_govt_ord ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d_ologit, Hess = TRUE
)

h6c_mil <- polr(
  trust_mil_ord ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d_ologit, Hess = TRUE
)

h6c_govt_tidy <- tidy(h6c_govt, conf.int = TRUE)
h6c_mil_tidy <- tidy(h6c_mil, conf.int = TRUE)

cat("Ordered logit — Government trust (key terms):\n")
print(h6c_govt_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nOrdered logit — Military trust (key terms):\n")
print(h6c_mil_tidy %>%
        filter(str_detect(term, "wave_num|country")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  govt = list(model = h6c_govt, tidy = h6c_govt_tidy),
  mil = list(model = h6c_mil, tidy = h6c_mil_tidy),
  n_obs = nrow(d_ologit)
), file.path(results_dir, "h6_ordered_logit.rds"))

# ── H6d: Subgroup analyses ──────────────────────────────────────────────────

cat("\n--- H6d: Subgroup analyses ---\n")

# Age terciles
d <- d %>%
  mutate(
    age_tercile = ntile(age, 3),
    age_group = factor(age_tercile, labels = c("Young", "Middle", "Old")),
    edu_tercile = ntile(education_years, 3),
    edu_group = factor(edu_tercile, labels = c("Low", "Medium", "High"))
  )

run_subgroup <- function(data, subgroup_var, subgroup_val, dv) {
  sub <- data %>% filter(!!sym(subgroup_var) == subgroup_val)
  if (nrow(sub) < 100) return(NULL)
  mod <- lm(
    as.formula(paste(dv, "~ wave_num * country_name +",
                     "age_centered + female + education_z + is_urban")),
    data = sub, weights = weight
  )
  list(
    tidy = tidy(mod, conf.int = TRUE),
    glance = glance(mod),
    n = nrow(sub),
    subgroup = paste(subgroup_var, "=", subgroup_val)
  )
}

subgroup_results <- list()

# Age subgroups
for (ag in c("Young", "Middle", "Old")) {
  subgroup_results[[paste0("age_", ag, "_govt")]] <-
    run_subgroup(d, "age_group", ag, "trust_national_government")
  subgroup_results[[paste0("age_", ag, "_mil")]] <-
    run_subgroup(d, "age_group", ag, "trust_military")
}

# Urban/rural
for (urb in c(0, 1)) {
  label <- if (urb == 1) "urban" else "rural"
  subgroup_results[[paste0(label, "_govt")]] <-
    run_subgroup(d, "is_urban", urb, "trust_national_government")
  subgroup_results[[paste0(label, "_mil")]] <-
    run_subgroup(d, "is_urban", urb, "trust_military")
}

# Education subgroups
for (eg in c("Low", "Medium", "High")) {
  subgroup_results[[paste0("edu_", eg, "_govt")]] <-
    run_subgroup(d, "edu_group", eg, "trust_national_government")
  subgroup_results[[paste0("edu_", eg, "_mil")]] <-
    run_subgroup(d, "edu_group", eg, "trust_military")
}

# Extract Thailand wave slopes from each subgroup for summary
subgroup_summary <- map_dfr(names(subgroup_results), function(nm) {
  res <- subgroup_results[[nm]]
  if (is.null(res)) return(NULL)
  wave_row <- res$tidy %>% filter(term == "wave_num")
  if (nrow(wave_row) == 0) return(NULL)
  tibble(
    subgroup = nm,
    n = res$n,
    thailand_wave_slope = wave_row$estimate,
    se = wave_row$std.error,
    p_value = wave_row$p.value
  )
})

cat("Subgroup summary (Thailand wave slopes):\n")
print(subgroup_summary %>% mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  results = subgroup_results,
  summary = subgroup_summary
), file.path(results_dir, "h6_subgroups.rds"))

# ── H6e: Secondary institutions ─────────────────────────────────────────────

cat("\n--- H6e: Secondary institutions ---\n")

secondary_dvs <- c("trust_courts", "trust_police", "trust_parliament",
                    "trust_political_parties")

h6e_results <- map(secondary_dvs, function(dv) {
  mod <- lm(
    as.formula(paste(dv, "~ wave_num * country_name +",
                     "age_centered + female + education_z + is_urban")),
    data = d
  )
  list(
    dv = dv,
    tidy = tidy(mod, conf.int = TRUE),
    glance = glance(mod)
  )
})
names(h6e_results) <- secondary_dvs

# Summary of wave slopes for Thailand (reference) across all institutions
secondary_summary <- map_dfr(h6e_results, function(res) {
  wave_row <- res$tidy %>% filter(term == "wave_num")
  tibble(
    institution = res$dv,
    thailand_slope = wave_row$estimate,
    se = wave_row$std.error,
    p_value = wave_row$p.value
  )
})

# Add primary institutions for comparison
primary_govt <- h1_govt_tidy %>% filter(term == "wave_num")
primary_mil <- h1_mil_tidy %>% filter(term == "wave_num")

secondary_summary <- bind_rows(
  tibble(institution = "trust_national_government",
         thailand_slope = primary_govt$estimate,
         se = primary_govt$std.error,
         p_value = primary_govt$p.value),
  tibble(institution = "trust_military",
         thailand_slope = primary_mil$estimate,
         se = primary_mil$std.error,
         p_value = primary_mil$p.value),
  secondary_summary
)

cat("Thailand wave slopes across all institutions:\n")
print(secondary_summary %>%
        arrange(thailand_slope) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  models = h6e_results,
  summary = secondary_summary
), file.path(results_dir, "h6_secondary_institutions.rds"))

cat("\nH6 results saved.\n\n")

# =============================================================================
# Summary results object
# =============================================================================

cat("=== BUILDING SUMMARY RESULTS ===\n")

# Collect key numbers for inline manuscript use
thai_govt_slope <- h1_govt_tidy %>%
  filter(term == "wave_num") %>%
  pull(estimate)
thai_mil_slope <- h1_mil_tidy %>%
  filter(term == "wave_num") %>%
  pull(estimate)

phil_govt_interaction <- h1_govt_tidy %>%
  filter(term == "wave_num:country_namePhilippines") %>%
  dplyr::select(estimate, std.error, p.value)
taiwan_govt_interaction <- h1_govt_tidy %>%
  filter(term == "wave_num:country_nameTaiwan") %>%
  dplyr::select(estimate, std.error, p.value)

phil_mil_interaction <- h1_mil_tidy %>%
  filter(term == "wave_num:country_namePhilippines") %>%
  dplyr::select(estimate, std.error, p.value)
taiwan_mil_interaction <- h1_mil_tidy %>%
  filter(term == "wave_num:country_nameTaiwan") %>%
  dplyr::select(estimate, std.error, p.value)

# H2 three-way interaction coefficient
threeway_coef <- h2_threeway_tidy %>%
  filter(str_detect(term, "wave_num:is_military:country_name"))

# H5 Taiwan military slope
taiwan_mil_slope <- h5_mil_tidy %>%
  filter(term == "wave_num") %>%
  dplyr::select(estimate, std.error, p.value)

# Performance controls: do key interactions survive?
h6a_govt_wave_interaction <- h6a_govt_tidy %>%
  filter(str_detect(term, "wave_num:country_name"))
h6a_mil_wave_interaction <- h6a_mil_tidy %>%
  filter(str_detect(term, "wave_num:country_name"))

summary_results <- list(
  # H1 key numbers
  h1 = list(
    thai_govt_slope = thai_govt_slope,
    thai_mil_slope = thai_mil_slope,
    phil_govt_interaction = phil_govt_interaction,
    taiwan_govt_interaction = taiwan_govt_interaction,
    phil_mil_interaction = phil_mil_interaction,
    taiwan_mil_interaction = taiwan_mil_interaction,
    re_govt = h1_re_govt,
    re_mil = h1_re_mil
  ),
  # H2 key numbers
  h2 = list(
    threeway_coefs = threeway_coef,
    threeway_tidy = h2_threeway_tidy,
    w5w6_tidy = h2_w5w6_tidy
  ),
  # H3 key numbers
  h3 = list(
    piecewise_govt = h3_pw_govt_tidy,
    piecewise_mil = h3_pw_mil_tidy,
    quadratic_govt = h3_q_govt_tidy,
    quadratic_mil = h3_q_mil_tidy
  ),
  # H4 key numbers
  h4 = list(
    phil_mil_slope = h4_mil_tidy %>% filter(term == "wave_num"),
    phil_police_slope = h4_police_tidy %>% filter(term == "wave_num"),
    phil_govt_slope = h4_govt_tidy %>% filter(term == "wave_num")
  ),
  # H5 key numbers
  h5 = list(
    taiwan_mil_slope = taiwan_mil_slope,
    ftest_pvalue = h5_ftest$`Pr(>F)`[2]
  ),
  # H6 summary
  h6 = list(
    performance_controls_govt = h6a_govt_wave_interaction,
    performance_controls_mil = h6a_mil_wave_interaction,
    pretrend_govt = h6b_govt_tidy %>% filter(str_detect(term, "wave_num")),
    pretrend_mil = h6b_mil_tidy %>% filter(str_detect(term, "wave_num")),
    ologit_govt = h6c_govt_tidy %>% filter(str_detect(term, "wave_num")),
    ologit_mil = h6c_mil_tidy %>% filter(str_detect(term, "wave_num")),
    secondary_institutions = secondary_summary,
    subgroup_summary = subgroup_summary
  ),
  # Model fit
  model_fit = list(
    h1_govt_r2 = glance(h1_govt)$r.squared,
    h1_mil_r2 = glance(h1_mil)$r.squared,
    h2_threeway_r2 = glance(h2_threeway)$r.squared,
    n_total = nrow(d),
    n_countries = length(unique(d$country_name)),
    n_waves = length(unique(d$wave_num))
  )
)

saveRDS(summary_results, file.path(results_dir, "all_results_summary.rds"))

cat("\n=== ALL RESULTS SAVED ===\n")
cat("Results directory:", results_dir, "\n")
cat("Files:\n")
cat(paste(" ", list.files(results_dir), collapse = "\n"), "\n")
cat("\nRun complete. All 6 hypotheses tested and results saved.\n")
