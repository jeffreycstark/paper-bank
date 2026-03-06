# 10_binary_recode_rerun.R
# Binary recode of dem_always_preferable and full re-run of all models
# that use it as DV.
#
# dem_always_preferable: 1=always, 2=auth sometimes, 3=doesn't matter
# Old: normalize_01() → 0, 0.5, 1 (WRONG: nominal → ordinal)
# New: dem_pref_binary = if_else(dem_always_preferable == 1, 1L, 0L)
#      → linear probability model; coefficients = pp change in Pr(always)
#
# NOTE on sign: old normalized had higher = LESS preferable, so negative β
# meant "better economy → more preferable." Binary is 1 = always preferable,
# so a POSITIVE β means "better economy → more likely always preferable."
# Signs will flip relative to old results.

library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
results_dir  <- file.path(paper_dir, "analysis/results")
source(file.path(paper_dir, "R", "helpers.R"))

# ── Load data ────────────────────────────────────────────────────────────────
dat <- readRDS(file.path(results_dir, "analysis_data.rds"))
abs_all <- readRDS(abs_harmonized_path)

# Create binary recode
dat <- dat |>
  mutate(dem_pref_binary = if_else(dem_always_preferable == 1, 1L, 0L,
                                    missing = NA_integer_))

# Merge additional variables needed for mechanism tests
abs_sub <- abs_all |> filter(country %in% c(3, 7))
stopifnot(nrow(abs_sub) == nrow(dat))
dat$pol_discuss_raw  <- abs_sub$pol_discuss
dat$pol_news_follow_raw <- abs_sub$pol_news_follow

# Normalize additional vars within country
dat <- dat |>
  group_by(country_label) |>
  mutate(
    pol_discuss_n  = normalize_01(pol_discuss_raw),
    pol_news_n     = normalize_01(pol_news_follow_raw)
  ) |>
  ungroup()

# Derive birth year for cohort analysis
yr_kr <- c("1"=2003,"2"=2006,"3"=2011,"4"=2015,"5"=2019,"6"=2022)
yr_tw <- c("1"=2001,"2"=2006,"3"=2010,"4"=2014,"5"=2019,"6"=2022)

dat <- dat |>
  mutate(
    survey_year = case_when(
      !is.na(int_year) ~ as.integer(int_year),
      country_label == "Korea"  ~ as.integer(yr_kr[as.character(wave)]),
      country_label == "Taiwan" ~ as.integer(yr_tw[as.character(wave)])
    ),
    birth_year = survey_year - age,
    pre_dem = as.integer(birth_year <= 1975)
  )

# Winner/loser
if (!"winner_loser" %in% names(dat)) {
  dat <- dat |>
    mutate(winner_loser = case_when(
      voted_winning_losing == 1 ~ "Winner",
      voted_winning_losing == 2 ~ "Loser",
      TRUE ~ NA_character_
    ))
}

# Wave factor
dat$wave_f <- factor(dat$wave)

kr <- dat |> filter(country_label == "Korea")
tw <- dat |> filter(country_label == "Taiwan")

results <- list()

# ── Helper: extract coefficient for a named term with HC2 SEs ────────────────
extract_term_hc2 <- function(model, term_name) {
  vcv <- sandwich::vcovHC(model, type = "HC2")
  ct  <- lmtest::coeftest(model, vcov = vcv)
  b   <- ct[term_name, "Estimate"]
  se  <- ct[term_name, "Std. Error"]
  tibble(
    term      = term_name,
    estimate  = b,
    std.error = se,
    statistic = ct[term_name, "t value"],
    p.value   = ct[term_name, "Pr(>|t|)"],
    conf.low  = b - 1.96 * se,
    conf.high = b + 1.96 * se
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1-2. WAVE-BY-WAVE OLS: dem_pref_binary ~ econ_index + controls
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("1-2. WAVE-BY-WAVE OLS (Korea & Taiwan)\n")
cat("══════════════════════════════════════════════════\n\n")

wbw_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100 || all(is.na(sub$dem_pref_binary))) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index +", controls))
    m <- lm(f, data = sub)

    wbw_results[[paste(cntry, w, sep = "_")]] <-
      extract_term_hc2(m, "econ_index") |>
      mutate(country = cntry, wave = w,
             r_sq = summary(m)$r.squared, n = nobs(m))
  }
}

wbw_df <- bind_rows(wbw_results)
cat("DV: dem_pref_binary (1 = always preferable, 0 = otherwise)\n\n")
wbw_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(country, wave, estimate, std.error, p.value, sig, r_sq, n) |>
  print(n = 20)

results$wave_by_wave <- wbw_df

# ═══════════════════════════════════════════════════════════════════════════════
# 3. POOLED OLS WITH WAVE FE
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("3. POOLED OLS WITH WAVE FE\n")
cat("══════════════════════════════════════════════════\n\n")

pooled_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)
  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- lm(f, data = sub)

  pooled_results[[cntry]] <-
    extract_term_hc2(m, "econ_index") |>
    mutate(country = cntry, r_sq = summary(m)$r.squared, n = nobs(m))

  cat(cntry, ": econ_index b =", round(coef(m)["econ_index"], 4), "\n")
  tidy_hc2(m) |>
    mutate(sig = sig_stars(p.value)) |>
    select(term, estimate, std.error, p.value, sig) |>
    print(n = 15)
  cat("\n")
}

results$pooled <- bind_rows(pooled_results)

# ═══════════════════════════════════════════════════════════════════════════════
# 4. CROSS-COUNTRY INTERACTION (H5: EconIndex × Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("4. CROSS-COUNTRY INTERACTION (H5)\n")
cat("══════════════════════════════════════════════════\n\n")

both <- dat |> mutate(is_korea = as.numeric(country_label == "Korea"))
f_xc <- as.formula(paste("dem_pref_binary ~ econ_index * is_korea + wave_f +", controls))
m_xc <- lm(f_xc, data = both)

xc_tidy <- tidy_hc2(m_xc) |>
  filter(term %in% c("econ_index", "is_korea", "econ_index:is_korea")) |>
  mutate(sig = sig_stars(p.value))

cat("Cross-country interaction on dem_pref_binary:\n")
print(xc_tidy |> select(term, estimate, std.error, p.value, sig))
cat(sprintf("\nTaiwan econ effect: %.4f\n", xc_tidy$estimate[xc_tidy$term == "econ_index"]))
cat(sprintf("Korea econ effect:  %.4f\n",
            xc_tidy$estimate[xc_tidy$term == "econ_index"] +
              xc_tidy$estimate[xc_tidy$term == "econ_index:is_korea"]))
cat(sprintf("Interaction β₃:     %.4f (p = %.4f)\n",
            xc_tidy$estimate[xc_tidy$term == "econ_index:is_korea"],
            xc_tidy$p.value[xc_tidy$term == "econ_index:is_korea"]))

results$cross_country <- xc_tidy
results$cross_country_n <- nobs(m_xc)
results$cross_country_r2 <- summary(m_xc)$r.squared

# ═══════════════════════════════════════════════════════════════════════════════
# 5. ROBUSTNESS: Institutional trust as control
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("5. ROBUSTNESS: Trust control (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

f_base  <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
f_trust <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls, "+ trust_index"))

m_base  <- lm(f_base,  data = kr)
m_trust <- lm(f_trust, data = kr)

trust_rob <- bind_rows(
  extract_term_hc2(m_base, "econ_index")  |> mutate(model = "Base", n = nobs(m_base)),
  extract_term_hc2(m_trust, "econ_index") |> mutate(model = "With trust", n = nobs(m_trust))
)

cat("Korea — econ_index coefficient with/without trust control:\n")
trust_rob |> select(model, estimate, std.error, p.value, n) |> print()

results$trust_robustness <- trust_rob

# ═══════════════════════════════════════════════════════════════════════════════
# 6. ROBUSTNESS: Individual economic indicators
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("6. ROBUSTNESS: Individual economic predictors (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

econ_vars <- c("econ_national_n", "econ_family_n", "econ_outlook_n",
               "econ_fam_outlook_n", "econ_change_n", "econ_fam_change_n")

indiv_econ <- map_dfr(econ_vars, function(ev) {
  f <- as.formula(paste("dem_pref_binary ~", ev, "+ wave_f +", controls))
  m <- lm(f, data = kr)
  extract_term_hc2(m, ev) |>
    mutate(econ_var = ev, r_sq = summary(m)$r.squared, n = nobs(m))
})

indiv_econ |>
  mutate(sig = sig_stars(p.value)) |>
  select(econ_var, estimate, std.error, p.value, sig, n) |>
  print()

results$indiv_econ <- indiv_econ

# ═══════════════════════════════════════════════════════════════════════════════
# 7. ROBUSTNESS: Logistic regression (replaces ordered logit)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("7. LOGISTIC REGRESSION (replaces ordered logit)\n")
cat("══════════════════════════════════════════════════\n\n")

logit_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)
  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- glm(f, data = sub, family = binomial)

  ct <- coeftest(m, vcov = vcovHC(m, type = "HC2"))
  b  <- ct["econ_index", "Estimate"]
  se <- ct["econ_index", "Std. Error"]

  logit_results[[cntry]] <- tibble(
    country   = cntry,
    estimate  = b,
    std.error = se,
    p.value   = ct["econ_index", "Pr(>|z|)"],
    n         = nobs(m),
    aic       = AIC(m)
  )

  cat(cntry, "logit: econ_index b =", round(b, 4),
      "(SE =", round(se, 4), ", p =", round(ct["econ_index", "Pr(>|z|)"], 4), ")\n")
}

results$logistic <- bind_rows(logit_results)

# ═══════════════════════════════════════════════════════════════════════════════
# 8. ROBUSTNESS: Within-wave z-scored (binary DV, z-scored predictors)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("8. WITHIN-WAVE Z-SCORED PREDICTORS\n")
cat("══════════════════════════════════════════════════\n\n")

controls_z <- "age_z + gender + edu_z + urban_rural + polint_z"

zscore_results <- list()
for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index_z +", controls_z))
    m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
    if (is.null(m)) next

    zscore_results[[paste(cntry, w, sep = "_")]] <-
      extract_term_hc2(m, "econ_index_z") |>
      mutate(country = cntry, wave = w, n = nobs(m))
  }
}

zscore_df <- bind_rows(zscore_results)
zscore_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(country, wave, estimate, std.error, p.value, sig, n) |>
  print(n = 20)

results$zscore <- zscore_df

# ═══════════════════════════════════════════════════════════════════════════════
# 9. SUBGROUP ANALYSIS (age, education, gender — Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("9. SUBGROUP ANALYSIS (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

subgroup_results <- list()

for (split_var in c("age_group", "edu_group", "polint_group")) {
  groups <- kr |> pull(!!sym(split_var)) |> unique() |> na.omit()
  for (grp in groups) {
    sub <- kr |> filter(!!sym(split_var) == grp)
    if (nrow(sub) < 200) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
    m <- lm(f, data = sub)

    subgroup_results[[paste(split_var, grp, sep = ":")]] <-
      extract_term_hc2(m, "econ_index") |>
      mutate(split = split_var, group = grp, n = nobs(m),
             r_sq = summary(m)$r.squared)
  }
}

subgroup_df <- bind_rows(subgroup_results)
subgroup_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(split, group, estimate, std.error, p.value, sig, n) |>
  print(n = 10)

results$subgroups <- subgroup_df

# ═══════════════════════════════════════════════════════════════════════════════
# 10. POLINT SUBGROUP SPLIT (engaged minority)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("10. POLINT SUBGROUP SPLIT — engaged minority (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

polint_split <- list()
for (grp in c("High interest", "Low interest")) {
  sub <- kr |> filter(polint_group == grp)
  if (nrow(sub) < 200) next

  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- lm(f, data = sub)

  polint_split[[grp]] <-
    extract_term_hc2(m, "econ_index") |>
    mutate(group = grp, n = nobs(m), r_sq = summary(m)$r.squared)
}

polint_split_df <- bind_rows(polint_split)
cat("Korea — econ → dem_pref_binary by political interest:\n")
polint_split_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(group, estimate, std.error, p.value, sig, n) |>
  print()

results$polint_split <- polint_split_df

# ═══════════════════════════════════════════════════════════════════════════════
# 11. POLINT CONTINUOUS INTERACTION
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("11. POLINT CONTINUOUS INTERACTION (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

f_int <- as.formula(paste("dem_pref_binary ~ econ_index * polint_n + wave_f +",
                          "age_n + gender + edu_n + urban_rural"))
m_int <- lm(f_int, data = kr)
polint_int_tidy <- tidy_hc2(m_int) |>
  filter(term %in% c("econ_index", "polint_n", "econ_index:polint_n")) |>
  mutate(sig = sig_stars(p.value))

cat("econ_index × polint_n interaction on dem_pref_binary:\n")
polint_int_tidy |> select(term, estimate, std.error, p.value, sig) |> print()
cat("N =", nobs(m_int), ", R2 =", round(summary(m_int)$r.squared, 4), "\n")

results$polint_interaction <- polint_int_tidy
results$polint_interaction_n <- nobs(m_int)

# ═══════════════════════════════════════════════════════════════════════════════
# 12. DISCUSSION FREQUENCY INTERACTION
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("12. DISCUSSION FREQUENCY INTERACTION (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

# Binary split on pol_discuss
kr <- kr |>
  mutate(
    discuss_high = as.integer(pol_discuss_raw >= median(pol_discuss_raw, na.rm = TRUE))
  )

discuss_split <- list()
for (grp in c(0L, 1L)) {
  label <- if (grp == 1) "High discussion" else "Low discussion"
  sub <- kr |> filter(discuss_high == grp, !is.na(dem_pref_binary))
  if (nrow(sub) < 200) next

  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- lm(f, data = sub)

  discuss_split[[label]] <-
    extract_term_hc2(m, "econ_index") |>
    mutate(group = label, n = nobs(m))
}

discuss_split_df <- bind_rows(discuss_split)
cat("Subgroup split on discussion frequency:\n")
discuss_split_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(group, estimate, std.error, p.value, sig, n) |>
  print()

# Also continuous interaction
f_disc_int <- as.formula(paste("dem_pref_binary ~ econ_index * pol_discuss_n + wave_f +",
                               "age_n + gender + edu_n + urban_rural"))
m_disc_int <- lm(f_disc_int, data = kr)
disc_int_tidy <- tidy_hc2(m_disc_int) |>
  filter(term %in% c("econ_index", "pol_discuss_n", "econ_index:pol_discuss_n")) |>
  mutate(sig = sig_stars(p.value))

cat("\nContinuous interaction:\n")
disc_int_tidy |> select(term, estimate, std.error, p.value, sig) |> print()

results$discuss_split <- discuss_split_df
results$discuss_interaction <- disc_int_tidy

# ═══════════════════════════════════════════════════════════════════════════════
# 13. COMPOSITE ENGAGEMENT INTERACTION
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("13. COMPOSITE ENGAGEMENT INTERACTION (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

kr <- kr |>
  mutate(
    engage_composite = rowMeans(cbind(polint_n, pol_discuss_n, pol_news_n), na.rm = TRUE)
  )

f_eng_int <- as.formula(paste("dem_pref_binary ~ econ_index * engage_composite + wave_f +",
                              "age_n + gender + edu_n + urban_rural"))
m_eng_int <- lm(f_eng_int, data = kr)
eng_int_tidy <- tidy_hc2(m_eng_int) |>
  filter(term %in% c("econ_index", "engage_composite", "econ_index:engage_composite")) |>
  mutate(sig = sig_stars(p.value))

cat("econ_index × engagement composite on dem_pref_binary:\n")
eng_int_tidy |> select(term, estimate, std.error, p.value, sig) |> print()
cat("N =", nobs(m_eng_int), ", R2 =", round(summary(m_eng_int)$r.squared, 4), "\n")

results$engage_interaction <- eng_int_tidy
results$engage_interaction_n <- nobs(m_eng_int)

# ═══════════════════════════════════════════════════════════════════════════════
# 14. WINNER/LOSER SUBGROUP
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("14. WINNER/LOSER SUBGROUP (both countries)\n")
cat("══════════════════════════════════════════════════\n\n")

wl_results <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub_c <- dat |> filter(country_label == cntry, !is.na(winner_loser))
  for (grp in c("Winner", "Loser")) {
    sub <- sub_c |> filter(winner_loser == grp)
    if (nrow(sub) < 100) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
    m <- lm(f, data = sub)

    wl_results[[paste(cntry, grp, sep = "_")]] <-
      extract_term_hc2(m, "econ_index") |>
      mutate(country = cntry, group = grp, n = nobs(m))
  }
}

wl_df <- bind_rows(wl_results)
wl_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(country, group, estimate, std.error, p.value, sig, n) |>
  print()

results$winner_loser <- wl_df

# ═══════════════════════════════════════════════════════════════════════════════
# 15. SYSTEM PRIDE MECHANISM PROBE (Taiwan)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("15. SYSTEM PRIDE MODERATION (Taiwan)\n")
cat("══════════════════════════════════════════════════\n\n")

f_pride <- as.formula(paste("dem_pref_binary ~ econ_index * nat_proud_n + wave_f +", controls))
m_pride <- lm(f_pride, data = tw |> filter(!is.na(nat_proud_n)))

pride_tidy <- tidy_hc2(m_pride) |>
  filter(term %in% c("econ_index", "nat_proud_n", "econ_index:nat_proud_n")) |>
  mutate(sig = sig_stars(p.value))

cat("Taiwan: econ × national pride on dem_pref_binary:\n")
pride_tidy |> select(term, estimate, std.error, p.value, sig) |> print()
cat("N =", nobs(m_pride), "\n")

results$pride_moderation_tw <- pride_tidy

# ═══════════════════════════════════════════════════════════════════════════════
# 16. CHINA-THREAT MODERATION (Taiwan)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("16. CHINA-THREAT MODERATION (Taiwan)\n")
cat("══════════════════════════════════════════════════\n\n")

tw_china <- tw |>
  filter(!is.na(china_threat)) |>
  mutate(china_harm_bin = as.numeric(china_threat == "China harmful"))

# Subgroup split
china_split <- list()
for (grp in c("China harmful", "China beneficial")) {
  sub <- tw_china |> filter(china_threat == grp)
  if (nrow(sub) < 100) next

  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- lm(f, data = sub)

  china_split[[grp]] <-
    extract_term_hc2(m, "econ_index") |>
    mutate(group = grp, n = nobs(m))
}

# Interaction
f_china <- as.formula(paste("dem_pref_binary ~ econ_index * china_harm_bin + wave_f +", controls))
m_china <- lm(f_china, data = tw_china)
china_int_tidy <- tidy_hc2(m_china) |>
  filter(term %in% c("econ_index", "china_harm_bin", "econ_index:china_harm_bin")) |>
  mutate(sig = sig_stars(p.value))

cat("Taiwan subgroup split:\n")
bind_rows(china_split) |>
  mutate(sig = sig_stars(p.value)) |>
  select(group, estimate, std.error, p.value, sig, n) |>
  print()

cat("\nInteraction model:\n")
china_int_tidy |> select(term, estimate, std.error, p.value, sig) |> print()

results$china_split_tw <- bind_rows(china_split)
results$china_interaction_tw <- china_int_tidy

# ═══════════════════════════════════════════════════════════════════════════════
# 17. AGE COHORT SPLIT (pre/post-democratization)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("17. AGE COHORT SPLIT (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

cohort_split <- list()
for (cohort_val in c(0L, 1L)) {
  label <- if (cohort_val == 1) "Pre-dem (born <=1975)" else "Post-dem (born >1975)"
  sub <- kr |> filter(pre_dem == cohort_val, !is.na(dem_pref_binary))
  if (nrow(sub) < 200) next

  f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
  m <- lm(f, data = sub)

  cohort_split[[label]] <-
    extract_term_hc2(m, "econ_index") |>
    mutate(group = label, n = nobs(m))
}

cohort_split_df <- bind_rows(cohort_split)
cat("Korea — econ → dem_pref_binary by birth cohort:\n")
cohort_split_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(group, estimate, std.error, p.value, sig, n) |>
  print()

# Formal interaction
f_cohort_int <- as.formula(paste("dem_pref_binary ~ econ_index * pre_dem + wave_f +", controls))
m_cohort_int <- lm(f_cohort_int, data = kr)
cohort_int_tidy <- tidy_hc2(m_cohort_int) |>
  filter(term %in% c("econ_index", "pre_dem", "econ_index:pre_dem")) |>
  mutate(sig = sig_stars(p.value))

cat("\nFormal interaction:\n")
cohort_int_tidy |> select(term, estimate, std.error, p.value, sig) |> print()

results$cohort_split <- cohort_split_df
results$cohort_interaction <- cohort_int_tidy

# ═══════════════════════════════════════════════════════════════════════════════
# 18. WAVE FE MAGNITUDE (Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("18. WAVE FE MAGNITUDE (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

m_w1 <- lm(dem_pref_binary ~ wave_f, data = kr)
m_w2 <- lm(dem_pref_binary ~ wave_f + age_n + gender + edu_n + urban_rural, data = kr)
m_w3 <- lm(as.formula(paste("dem_pref_binary ~ wave_f + econ_index +", controls)), data = kr)

extract_wave_fes <- function(model, label) {
  tidy_hc2(model) |>
    filter(str_detect(term, "^wave_f")) |>
    mutate(model = label) |>
    select(model, term, estimate, std.error, p.value)
}

wave_fe_comp <- bind_rows(
  extract_wave_fes(m_w1, "Wave FE only"),
  extract_wave_fes(m_w2, "Wave FE + demographics"),
  extract_wave_fes(m_w3, "Wave FE + demo + econ")
)

wave_fe_wide <- wave_fe_comp |>
  mutate(coef_str = sprintf("%.3f (p=%.3f)", estimate, p.value)) |>
  select(term, model, coef_str) |>
  pivot_wider(names_from = model, values_from = coef_str)

print(wave_fe_wide, width = 120)

cat("\nR2 progression:\n")
cat("  Wave FE only:         ", round(summary(m_w1)$r.squared, 4), "\n")
cat("  + demographics:       ", round(summary(m_w2)$r.squared, 4), "\n")
cat("  + demo + econ:        ", round(summary(m_w3)$r.squared, 4), "\n")

results$wave_fe_magnitude <- wave_fe_comp
results$wave_fe_r2 <- c(
  wave_only = summary(m_w1)$r.squared,
  plus_demo = summary(m_w2)$r.squared,
  plus_econ = summary(m_w3)$r.squared
)

# ═══════════════════════════════════════════════════════════════════════════════
# 19-20. CROSS-COUNTRY β₃ AND ITEM-SPECIFICITY COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("19-20. ITEM-SPECIFICITY COMPARISON (H4)\n")
cat("══════════════════════════════════════════════════\n\n")

# Run cross-country interaction for all substantive items
substantive_dvs <- c(
  "qual_extent_n"      = "Dem extent (evaluative)",
  "qual_sys_support_n" = "System deserves support",
  "qual_sys_change_n"  = "System needs change",
  "auth_reject_index"  = "Auth rejection index"
)

item_xc_results <- list()

# dem_pref_binary (the abstract item)
item_xc_results[["dem_pref_binary"]] <- xc_tidy |>
  filter(term == "econ_index:is_korea") |>
  mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable (binary)")

for (dv in names(substantive_dvs)) {
  sub <- both |> filter(!is.na(.data[[dv]]))
  f <- as.formula(paste(dv, "~ econ_index * is_korea + wave_f +", controls))
  m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
  if (is.null(m)) next

  item_xc_results[[dv]] <- tidy_hc2(m) |>
    filter(term == "econ_index:is_korea") |>
    mutate(dv = dv, dv_label = substantive_dvs[dv], n = nobs(m))
}

item_xc_df <- bind_rows(item_xc_results)
cat("Cross-country interaction (econ × Korea) by DV:\n")
item_xc_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(dv_label, estimate, std.error, p.value, sig) |>
  print()

results$item_specificity <- item_xc_df

# ═══════════════════════════════════════════════════════════════════════════════
# COMPARISON TABLE: Old normalized vs. new binary (wave-by-wave Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("COMPARISON: Old normalized vs. new binary coefficients\n")
cat("══════════════════════════════════════════════════\n\n")

# Load old model results
old_results <- readRDS(file.path(results_dir, "model_results.rds"))

# Old wave-by-wave for qual_pref_dem_n
old_wbw <- old_results$wave_by_wave |>
  filter(dv == "qual_pref_dem_n") |>
  select(country, wave, estimate_old = estimate, p_old = p.value)

# New wave-by-wave
new_wbw <- wbw_df |>
  select(country, wave, estimate_new = estimate, p_new = p.value)

comparison <- full_join(old_wbw, new_wbw, by = c("country", "wave")) |>
  mutate(
    old_sig = sig_stars(p_old),
    new_sig = sig_stars(p_new),
    # Note: old scale had higher=less preferable, so negative = pro-dem
    # New scale has 1=always preferable, so positive = pro-dem
    # Signs should flip: old negative → new positive (and vice versa)
    sign_consistent = (sign(estimate_old) != sign(estimate_new) |
                        (abs(estimate_old) < 0.01 & abs(estimate_new) < 0.01))
  )

cat("Old DV: qual_pref_dem_n (normalize_01; higher = LESS preferable)\n")
cat("New DV: dem_pref_binary (1 = always preferable, 0 = else)\n")
cat("Signs should FLIP (old negative = new positive)\n\n")

comparison |>
  mutate(
    old_str = sprintf("%.4f%s", estimate_old, old_sig),
    new_str = sprintf("%.4f%s", estimate_new, new_sig)
  ) |>
  select(country, wave, old_str, new_str, sign_consistent) |>
  print(n = 20)

results$comparison <- comparison

# ═══════════════════════════════════════════════════════════════════════════════
# TAIWAN FULL SUBGROUPS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("BONUS: Taiwan subgroups on dem_pref_binary\n")
cat("══════════════════════════════════════════════════\n\n")

tw_sub_results <- list()
for (split_var in c("age_group", "edu_group", "polint_group")) {
  groups <- tw |> pull(!!sym(split_var)) |> unique() |> na.omit()
  for (grp in groups) {
    sub <- tw |> filter(!!sym(split_var) == grp)
    if (nrow(sub) < 200) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index + wave_f +", controls))
    m <- lm(f, data = sub)

    tw_sub_results[[paste(split_var, grp, sep = ":")]] <-
      extract_term_hc2(m, "econ_index") |>
      mutate(split = split_var, group = grp, country = "Taiwan", n = nobs(m))
  }
}

tw_sub_df <- bind_rows(tw_sub_results)
tw_sub_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(split, group, estimate, std.error, p.value, sig, n) |>
  print(n = 10)

results$subgroups_taiwan <- tw_sub_df

# ═══════════════════════════════════════════════════════════════════════════════
# CHECK: Alternative normative items
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("CHECK: Alternative normative items — scale types\n")
cat("══════════════════════════════════════════════════\n\n")

cat("dem_best_form: 1-4 ordinal (agree/disagree) → normalize_01 OK\n")
cat("dem_vs_econ: 1-5 ordinal → normalize_01 OK\n")
cat("democracy_suitability: 1-10 ordinal → normalize_01 OK\n")
cat("dem_extent_current: 1-10 ordinal → normalize_01 OK\n")
cat("dem_always_preferable: 1-3 NOMINAL → normalize_01 WRONG → binary recode applied\n")

# ═══════════════════════════════════════════════════════════════════════════════
# SURVEY-WEIGHTED MODELS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("BONUS: Survey-weighted models on dem_pref_binary\n")
cat("══════════════════════════════════════════════════\n\n")

weighted_results <- list()
for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100) next

    wt_var <- if (all(is.na(sub$weight))) rep(1, nrow(sub)) else sub$weight
    des <- survey::svydesign(ids = ~1, weights = ~wt_var,
                             data = sub |> mutate(wt_var = wt_var))

    f <- as.formula(paste("dem_pref_binary ~ econ_index +", controls))
    m <- tryCatch(survey::svyglm(f, design = des), error = function(e) NULL)
    if (is.null(m)) next

    ct <- broom::tidy(m)
    econ_row <- ct |> filter(term == "econ_index")

    weighted_results[[paste(cntry, w, sep = "_")]] <- tibble(
      country = cntry, wave = w,
      estimate = econ_row$estimate,
      std.error = econ_row$std.error,
      p.value = econ_row$p.value,
      n = nobs(m)
    )
  }
}

weighted_df <- bind_rows(weighted_results)
weighted_df |>
  mutate(sig = sig_stars(p.value)) |>
  select(country, wave, estimate, std.error, p.value, sig, n) |>
  print(n = 20)

results$weighted <- weighted_df

# ═══════════════════════════════════════════════════════════════════════════════
# SAVE
# ═══════════════════════════════════════════════════════════════════════════════
saveRDS(results, file.path(results_dir, "binary_recode_rerun.rds"))
cat("\n\nSaved: analysis/results/binary_recode_rerun.rds\n")
cat("Components:", paste(names(results), collapse = ", "), "\n")
cat("Done.\n")
