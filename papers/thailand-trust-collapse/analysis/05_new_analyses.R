# 05_new_analyses.R
# New analyses addressing reviewer priorities:
#   (1) Formal H3 test for Philippines coercive trust (bring regression into main text)
#   (2) DiD specification (Thailand × Post-Wave5 × Military)
#   (3) Predicted probability figure for ordered logit
#   (4) Weighted vs. unweighted comparison for Waves 1–2
#   (5) Institutional breadth formal sensitivity-gradient test
#   (6) democratic_commitment index reliability check
#
# Depends on: thailand_panel.rds and existing results/
# Usage: Rscript papers/thailand-trust-collapse/analysis/05_new_analyses.R

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(MASS)
library(ggplot2)
library(marginaleffects)  # for predicted probabilities from polr

# MASS::select masks dplyr::select — resolve explicitly
select <- dplyr::select

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
analysis_dir  <- file.path(project_root, "papers/thailand-trust-collapse/analysis")
results_dir   <- file.path(analysis_dir, "results")
fig_dir       <- file.path(analysis_dir, "figures")

d <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))
cat("Loaded:", nrow(d), "obs\n")

source(file.path(project_root, "papers/thailand-trust-collapse/R/helpers.R"))

# =============================================================================
# (1) H3 formal test: Philippines coercive vs. non-coercive trust, Waves 4–6
#     NOTE: trust_ngo and trust_local_govt are NOT in thailand_panel.rds.
#     They exist only in the pre-computed institutional_breadth.rds summary.
#     For the formal individual-level test we use the available ABS variables:
#     coercive = military + police; non-coercive = national_government + courts
# =============================================================================
cat("\n=== (1) H3 Philippines stacked coercive test ===\n")

phil_w456 <- d %>%
  filter(country_name == "Philippines", wave_num %in% 4:6) %>%
  mutate(respondent_id = row_number())

# Stack coercive (military, police) vs. non-coercive (govt, NGOs) trust
phil_long <- phil_w456 %>%
  pivot_longer(
    cols = c(trust_military, trust_police,
             trust_national_government, trust_ngos),
    names_to  = "institution",
    values_to = "trust"
  ) %>%
  mutate(
    is_coercive = if_else(institution %in%
                            c("trust_military", "trust_police"), 1L, 0L)
  ) %>%
  filter(!is.na(trust))

# Model: does coercive trust trend differently from non-coercive in Philippines W4–6?
h3_phil_stacked <- lm(
  trust ~ wave_num * is_coercive +
    age_centered + female + education_z + is_urban,
  data   = phil_long,
  weights = weight
)
h3_phil_stacked_tidy <- tidy_clustered(h3_phil_stacked, phil_long$respondent_id)
cat("Philippines coercive vs. non-coercive stacked model:\n")
print(h3_phil_stacked_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Simple per-institution wave slopes (already in h4_phil, re-run for completeness)
h3_phil_simple <- map_dfr(
  c("trust_military", "trust_police",
    "trust_national_government", "trust_ngos"),
  function(dv) {
    mod <- lm(
      as.formula(paste(dv, "~ wave_num +",
                       "age_centered + female + education_z + is_urban")),
      data    = phil_w456,
      weights = weight
    )
    broom::tidy(mod) %>%
      dplyr::filter(term == "wave_num") %>%
      dplyr::mutate(institution = dv)
  }
)
cat("\nPer-institution wave slopes (Philippines W4–6):\n")
print(dplyr::select(h3_phil_simple, institution, estimate, std.error, p.value) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  stacked_model     = h3_phil_stacked,
  stacked_tidy      = h3_phil_stacked_tidy,
  per_institution   = h3_phil_simple,
  n_obs             = nrow(phil_long)
), file.path(results_dir, "h3_phil_formal.rds"))


# =============================================================================
# (2) DiD specification:
#     Trust ~ Post × Military × Thailand + controls
#     Cleaner causal framing than the 3-way wave interaction.
#     "Post" = Wave 5–6 (post-coup consolidation + protest period).
# =============================================================================
cat("\n=== (2) Difference-in-Differences ===\n")

d_long <- d %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    cols      = c(trust_national_government, trust_military),
    names_to  = "institution",
    values_to = "trust"
  ) %>%
  mutate(
    is_military  = if_else(institution == "trust_military", 1L, 0L),
    is_thailand  = if_else(country_name == "Thailand", 1L, 0L),
    post         = if_else(wave_num >= 5, 1L, 0L)   # Wave 5–6 as "treatment" period
  ) %>%
  filter(!is.na(trust))

# Full DiD: trust ~ Post × Military × Thailand
# Reference: non-Thailand, non-military, pre-period
did_model <- lm(
  trust ~ post * is_military * is_thailand +
    wave_num + country_name +           # absorb linear time trend and country FE
    age_centered + female + education_z + is_urban,
  data    = d_long,
  weights = weight
)
did_tidy <- tidy_clustered(did_model, d_long$respondent_id)
cat("DiD model (Post × Military × Thailand):\n")
print(did_tidy %>%
        filter(str_detect(term, "post|is_military|is_thailand")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Also run Wave 6 only as a cross-section robustness check
did_w6 <- lm(
  trust ~ is_military * is_thailand +
    country_name + age_centered + female + education_z + is_urban,
  data    = d_long %>% filter(wave_num == 6),
  weights = weight
)
did_w6_tidy <- tidy_clustered(did_w6,
                               d_long %>% filter(wave_num == 6) %>% pull(respondent_id))
cat("\nWave 6 cross-section (Military × Thailand):\n")
print(did_w6_tidy %>%
        filter(str_detect(term, "is_military|is_thailand")) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  did_model  = did_model,
  did_tidy   = did_tidy,
  did_w6     = did_w6,
  did_w6_tidy = did_w6_tidy,
  n_obs      = nrow(d_long)
), file.path(results_dir, "did_model.rds"))


# =============================================================================
# (3) Predicted probability figure from ordered logit
#     Plots Pr(trust = "none at all") by country × wave
#     More intuitive than log-odds for a general audience.
# =============================================================================
cat("\n=== (3) Predicted probabilities from ordered logit ===\n")

d_ologit <- d %>%
  mutate(
    trust_mil_ord  = factor(trust_military,              ordered = TRUE),
    trust_govt_ord = factor(trust_national_government,   ordered = TRUE)
  ) %>%
  filter(!is.na(trust_mil_ord), !is.na(trust_govt_ord),
         !is.na(age_centered), !is.na(female),
         !is.na(education_z), !is.na(is_urban))

ologit_mil <- polr(
  trust_mil_ord ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d_ologit, Hess = TRUE
)

ologit_govt <- polr(
  trust_govt_ord ~ wave_num * country_name +
    age_centered + female + education_z + is_urban,
  data = d_ologit, Hess = TRUE
)

# Build prediction grid: vary wave by country, hold controls at means
pred_grid <- expand.grid(
  wave_num      = 1:6,
  country_name  = c("Thailand", "Philippines", "Taiwan"),
  age_centered  = 0,
  female        = 0.5,
  education_z   = 0,
  is_urban      = 0.5
)

# Predicted probabilities for each trust category
pred_mil  <- predict(ologit_mil,  newdata = pred_grid, type = "probs")
pred_govt <- predict(ologit_govt, newdata = pred_grid, type = "probs")

colnames(pred_mil)  <- paste0("mil_",  1:4)
colnames(pred_govt) <- paste0("govt_", 1:4)

pred_df <- bind_cols(pred_grid, as.data.frame(pred_mil), as.data.frame(pred_govt))

# Figure: Pr(trust = 1, "none at all") by country × wave
fig_pred <- pred_df %>%
  dplyr::select(wave_num, country_name, mil_1, govt_1) %>%
  pivot_longer(cols = c(mil_1, govt_1),
               names_to = "institution",
               values_to = "prob_none") %>%
  mutate(institution = if_else(institution == "mil_1", "Military", "Government"))

p_pred <- ggplot(fig_pred,
                 aes(x = wave_num, y = prob_none,
                     colour = country_name, linetype = institution)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:6,
                     labels = c("W1\n2001–03","W2\n2005–08","W3\n2010–11",
                                "W4\n2014–16","W5\n2018–20","W6\n2019–22")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  scale_colour_manual(values = c("Thailand" = "#D62728",
                                  "Philippines" = "#1F77B4",
                                  "Taiwan" = "#2CA02C")) +
  labs(
    title   = "Predicted probability of reporting 'no trust at all'",
    subtitle = "Ordered logit predictions holding demographics at sample means",
    x       = "Wave",
    y       = "Pr(trust = 'none at all')",
    colour  = "Country",
    linetype = "Institution",
    caption = "Note: Predictions set age, education, gender, and urbanization at sample means."
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "fig_ologit_pred_probs.png"),
       p_pred, width = 7, height = 5, dpi = 300)
cat("Saved predicted probabilities figure.\n")

saveRDS(list(
  ologit_mil   = ologit_mil,
  ologit_govt  = ologit_govt,
  pred_df      = pred_df,
  fig_data     = fig_pred
), file.path(results_dir, "ologit_pred_probs.rds"))


# =============================================================================
# (4) Weighted vs. unweighted comparison
#     Waves 1–2 treated as self-weighting; demonstrate this is inconsequential.
# =============================================================================
cat("\n=== (4) Weighted vs. unweighted comparison ===\n")

run_weighted_comparison <- function(dv) {
  mod_w <- lm(
    as.formula(paste(dv, "~ wave_num * country_name +",
                     "age_centered + female + education_z + is_urban")),
    data = d, weights = weight
  )
  mod_uw <- lm(
    as.formula(paste(dv, "~ wave_num * country_name +",
                     "age_centered + female + education_z + is_urban")),
    data = d
  )
  bind_rows(
    tidy(mod_w)  %>% mutate(specification = "Weighted"),
    tidy(mod_uw) %>% mutate(specification = "Unweighted")
  ) %>%
    filter(str_detect(term, "wave_num|country")) %>%
    mutate(dv = dv)
}

wt_comparison <- bind_rows(
  run_weighted_comparison("trust_national_government"),
  run_weighted_comparison("trust_military")
) %>%
  select(dv, specification, term, estimate, std.error, p.value) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

cat("Weighted vs. unweighted comparison (key slopes):\n")
print(wt_comparison %>%
        filter(term %in% c("wave_num",
                            "wave_num:country_namePhilippines",
                            "wave_num:country_nameTaiwan")))

saveRDS(wt_comparison, file.path(results_dir, "weighted_unweighted_comparison.rds"))


# =============================================================================
# (5) Institutional breadth formal test
#     Within Thailand Wave 5–6 and Philippines Wave 4–6:
#     Does "sensitive" institution type (government/military) show disproportionate
#     change relative to "non-sensitive" (NGO/local govt)?
#     Stacked model with a "sensitive" indicator.
# =============================================================================
cat("\n=== (5) Institutional breadth formal sensitivity-gradient test ===\n")

# Thailand W5–6
thai_w56 <- d %>%
  filter(country_name == "Thailand", wave_num %in% 5:6) %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    cols      = c(trust_national_government, trust_military,
                  trust_ngos, trust_local_government),
    names_to  = "institution",
    values_to = "trust"
  ) %>%
  mutate(
    # "Sensitive" = directly targeted by 2020-21 protests
    # "Non-sensitive" = NGOs and local government
    is_sensitive = if_else(
      institution %in% c("trust_national_government", "trust_military"), 1L, 0L
    ),
    post = if_else(wave_num == 6, 1L, 0L)
  ) %>%
  filter(!is.na(trust))

ib_thai_model <- lm(
  trust ~ post * is_sensitive +
    age_centered + female + education_z + is_urban,
  data    = thai_w56,
  weights = weight
)
ib_thai_tidy <- tidy_clustered(ib_thai_model, thai_w56$respondent_id)
cat("Thailand W5–6 sensitivity-gradient model:\n")
print(ib_thai_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

# Philippines W4–6
ib_phil_model <- lm(
  trust ~ wave_num * is_coercive +
    age_centered + female + education_z + is_urban,
  data    = phil_long,
  weights = weight
)
# (phil_long already created in section 1 above)
ib_phil_tidy <- tidy_clustered(ib_phil_model, phil_long$respondent_id)
cat("\nPhilippines W4–6 coercive vs. non-coercive sensitivity model:\n")
print(ib_phil_tidy %>% mutate(across(where(is.numeric), ~round(., 4))))

saveRDS(list(
  thai_model  = ib_thai_model,
  thai_tidy   = ib_thai_tidy,
  phil_model  = ib_phil_model,
  phil_tidy   = ib_phil_tidy
), file.path(results_dir, "ib_sensitivity_gradient.rds"))


# =============================================================================
# (6) Democratic commitment index reliability
#     Report Cronbach's alpha and inter-item correlations by country-wave
# =============================================================================
cat("\n=== (6) Democratic commitment index reliability ===\n")

# Items: reject_military (from military_rule reversed),
#        reject_strongman, reject_single_party
# Plus: dem_commitment_01 composite (already constructed)
# Cronbach's alpha by country

# Reverse-code: raw scale is 1=strongly disapprove, 4=strongly approve
# Higher reject_* = stronger rejection of authoritarian rule
# Construct rejection variables (same as 04_attitudinal_mechanisms.R)
d_rel <- d %>%
  mutate(
    reject_military     = 5 - military_rule,
    reject_strongman    = 5 - strongman_rule,
    reject_single_party = 5 - single_party_rule
  ) %>%
  filter(!is.na(reject_military),
         !is.na(reject_strongman),
         !is.na(reject_single_party))

alpha_by_group <- d_rel %>%
  group_by(country_name) %>%
  summarise(
    n        = n(),
    r_mil_sm = cor(reject_military,     reject_strongman,    use = "complete.obs"),
    r_mil_sp = cor(reject_military,     reject_single_party, use = "complete.obs"),
    r_sm_sp  = cor(reject_strongman,    reject_single_party, use = "complete.obs"),
    mean_r   = mean(c(r_mil_sm, r_mil_sp, r_sm_sp)),
    alpha    = (3 * mean_r) / (1 + 2 * mean_r),
    .groups  = "drop"
  )

cat("Democratic commitment scale reliability by country:\n")
print(alpha_by_group %>% mutate(across(where(is.numeric), ~round(., 3))))

# By country-wave
alpha_by_wave <- d_rel %>%
  group_by(country_name, wave_num) %>%
  summarise(
    n     = n(),
    r_avg = mean(c(
      cor(reject_military,     reject_strongman,    use = "complete.obs"),
      cor(reject_military,     reject_single_party, use = "complete.obs"),
      cor(reject_strongman,    reject_single_party, use = "complete.obs")
    )),
    alpha = (3 * r_avg) / (1 + 2 * r_avg),
    .groups = "drop"
  )

cat("\nScale reliability by country-wave:\n")
print(alpha_by_wave %>% mutate(across(where(is.numeric), ~round(., 3))))

saveRDS(list(
  by_country   = alpha_by_group,
  by_wave      = alpha_by_wave
), file.path(results_dir, "dem_commit_reliability.rds"))

cat("\n=== 05_new_analyses.R complete ===\n")
cat("Results saved to:", results_dir, "\n")
cat("Figure saved to:", fig_dir, "\n")
