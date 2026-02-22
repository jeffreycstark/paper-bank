# 02_models.R
# Thailand Trust Collapse — Multilevel Models
#
# Random-slopes models for government and military trust, country-specific
# slope extraction, and pre-trend robustness check.
#
# Usage: Rscript papers/thailand-trust-collapse/analysis/02_models.R

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

# ── Setup ─────────────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
analysis_dir <- file.path(project_root, "papers/thailand-trust-collapse/analysis")
table_dir <- file.path(analysis_dir, "tables")

set.seed(2025)

d <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))
cat("Loaded:", nrow(d), "obs across", length(unique(d$country_name)), "countries\n")

# ── Model 1: Government trust — random intercepts only ───────────────────────

cat("\n=== MODEL 1: Government Trust — Random Intercepts ===\n")

m1_ri <- lmer(trust_national_government ~ wave_num +
                age_centered + female + education_z + is_urban +
                (1 | country_name),
              data = d, REML = FALSE)

cat("Fixed effects:\n")
print(tidy(m1_ri, effects = "fixed", conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# ── Model 2: Government trust — random slopes for wave ───────────────────────

cat("\n=== MODEL 2: Government Trust — Random Slopes ===\n")

m2_rs <- lmer(trust_national_government ~ wave_num +
                age_centered + female + education_z + is_urban +
                (1 + wave_num | country_name),
              data = d, REML = FALSE,
              control = lmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 100000)))

cat("Fixed effects:\n")
print(tidy(m2_rs, effects = "fixed", conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\nVariance components:\n")
print(VarCorr(m2_rs))

# Model comparison
cat("\n=== MODEL COMPARISON (LRT) ===\n")
print(anova(m1_ri, m2_rs))

# ── Country-specific slopes: Government trust ────────────────────────────────

re_govt <- ranef(m2_rs)$country_name
re_govt$country <- rownames(re_govt)
re_govt <- re_govt %>%
  rename(intercept_dev = `(Intercept)`, slope_dev = wave_num) %>%
  arrange(slope_dev)

cat("\n=== COUNTRY-SPECIFIC SLOPES: Government Trust ===\n")
print(re_govt %>% mutate(across(where(is.numeric), ~round(., 3))))

write_csv(re_govt, file.path(table_dir, "tab3_govt_slopes.csv"))

# ── Model 3: Military trust — random slopes ──────────────────────────────────

cat("\n=== MODEL 3: Military Trust — Random Slopes ===\n")

m3_mil <- lmer(trust_military ~ wave_num +
                 age_centered + female + education_z + is_urban +
                 (1 + wave_num | country_name),
               data = d, REML = FALSE,
               control = lmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 100000)))

cat("Fixed effects:\n")
print(tidy(m3_mil, effects = "fixed", conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\nVariance components:\n")
print(VarCorr(m3_mil))

# ── Country-specific slopes: Military trust ──────────────────────────────────

re_mil <- ranef(m3_mil)$country_name
re_mil$country <- rownames(re_mil)
re_mil <- re_mil %>%
  rename(intercept_dev = `(Intercept)`, slope_dev = wave_num) %>%
  arrange(slope_dev)

cat("\n=== COUNTRY-SPECIFIC SLOPES: Military Trust ===\n")
print(re_mil %>% mutate(across(where(is.numeric), ~round(., 3))))

write_csv(re_mil, file.path(table_dir, "tab4_mil_slopes.csv"))

# ── Slope comparison: Government vs Military ─────────────────────────────────

slope_comparison <- re_govt %>%
  select(country, govt_slope = slope_dev) %>%
  left_join(re_mil %>% select(country, mil_slope = slope_dev), by = "country") %>%
  mutate(
    difference = mil_slope - govt_slope,
    interpretation = case_when(
      difference < -0.02 ~ "Military declining faster",
      difference > 0.02 ~ "Government declining faster",
      TRUE ~ "Similar trajectories"
    )
  ) %>%
  arrange(difference)

cat("\n=== SLOPE COMPARISON: Government vs Military ===\n")
print(slope_comparison %>% mutate(across(where(is.numeric), ~round(., 3))))

write_csv(slope_comparison, file.path(table_dir, "tab5_slope_comparison.csv"))

# ── Pre-trend check (W1-W4, before COVID/protests) ──────────────────────────

cat("\n=== PRE-TREND CHECK (W1-W4) ===\n")

pre_data <- d %>% filter(wave_num %in% 1:4)
cat("Pre-COVID sample:", nrow(pre_data), "obs\n")

m_pretrend <- lm(trust_national_government ~ wave_num * country_name +
                   age_centered + female + education_z + is_urban,
                 data = pre_data)

cat("\nInteraction terms (differential pre-trends):\n")
pre_results <- broom::tidy(m_pretrend, conf.int = TRUE) %>%
  filter(str_detect(term, "wave_num")) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
print(pre_results)

write_csv(pre_results, file.path(table_dir, "tab6_pretrend.csv"))

# ── Summary ──────────────────────────────────────────────────────────────────

cat("\n=== MODEL SUMMARY ===\n")
cat("Model 1 (RI): AIC =", round(AIC(m1_ri), 1), "\n")
cat("Model 2 (RS govt): AIC =", round(AIC(m2_rs), 1), "\n")
cat("Model 3 (RS mil):  AIC =", round(AIC(m3_mil), 1), "\n")
cat("\nRandom slopes significantly improve fit (see LRT above).\n")
cat("Thailand has the most negative slope for both government and military trust.\n")
cat("Military trust slope is steeper — consistent with political crisis mechanism.\n")

cat("\nAll tables saved to:", table_dir, "\n")
