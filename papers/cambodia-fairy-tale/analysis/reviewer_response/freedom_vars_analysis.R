# =============================================================================
# Freedom of Speech & Assembly — Reviewer Response Analyses
# Cambodia Fairy Tale paper
#
# Four analyses per reviewer request:
#   1. Descriptive trajectory — wave means + 95% CI
#   2. Controls in main models — wave coefs with/without freedom covariates
#   3. Moderator analysis — wave × freedom interaction
#   4. Nonresponse prediction — logistic: does low freedom predict W6 non-
#      response on dem_country_future?
#
# Variables:
#   dem_free_speech    : "People free to speak without fear" 1(SD)–4(SA)
#   gov_free_to_organize: "People can join any org without fear" 1(SD)–4(SA)
#
# DVs:
#   dem_country_future     : 0–10 (W3/W4/W6 only)
#   democracy_satisfaction : 1–4 ordinal
#   dem_pref_binary        : 1 = democracy always preferable (from 3-cat nominal)
#
# Outputs (all to analysis/reviewer_response/):
#   freedom_descriptive.csv
#   freedom_models_coefs.csv
#   freedom_nonresponse_coefs.csv
#   freedom_moderator_coefs.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(broom)
})

# ---------------------------------------------------------------------------
# 0. Load data
# ---------------------------------------------------------------------------
project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/cambodia-fairy-tale")
rr_dir       <- file.path(paper_dir, "analysis/reviewer_response")
res_dir      <- file.path(paper_dir, "analysis/results")

source(file.path(project_root, "_data_config.R"))
d_full <- readRDS(abs_harmonized_path)

camb <- d_full |>
  filter(country == 12, wave %in% c(2, 3, 4, 6)) |>
  mutate(
    wave_f    = factor(wave, levels = c(2, 3, 4, 6)),
    wave_year = case_when(wave == 2 ~ 2008L,
                          wave == 3 ~ 2012L,
                          wave == 4 ~ 2015L,
                          wave == 6 ~ 2021L),
    # Democracy preference binary (1 = democracy always preferable)
    dem_pref_binary = if_else(dem_always_preferable == 1, 1L, 0L,
                              missing = NA_integer_),
    # Binary freedom splits (≥3 = "somewhat/strongly agree" → free)
    free_speech_hi = if_else(dem_free_speech >= 3, 1L, 0L,
                             missing = NA_integer_),
    free_org_hi    = if_else(gov_free_to_organize >= 3, 1L, 0L,
                             missing = NA_integer_),
    # Controls (standardised for model comparability)
    age_z  = as.numeric(scale(age)),
    edu_z  = as.numeric(scale(education_level)),
    female = as.integer(gender == 0),  # check: 0=female in ABS
    urban  = as.integer(urban_rural == 1)
  )

wave_labels <- c("2" = "W2 (2008)", "3" = "W3 (2012)",
                 "4" = "W4 (2015)", "6" = "W6 (2021)")


# ---------------------------------------------------------------------------
# 1. Descriptive trajectory
# ---------------------------------------------------------------------------
mean_ci <- function(x, conf = 0.95) {
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  t_crit <- qt((1 + conf) / 2, df = n - 1)
  list(mean = m, se = se, ci_lo = m - t_crit * se, ci_hi = m + t_crit * se, n = n)
}

desc <- camb |>
  group_by(wave, wave_year) |>
  summarise(
    n_speech       = sum(!is.na(dem_free_speech)),
    mean_speech    = mean(dem_free_speech, na.rm = TRUE),
    se_speech      = sd(dem_free_speech, na.rm = TRUE) / sqrt(n_speech),
    ci_lo_speech   = mean_speech - qt(0.975, df = n_speech - 1) * se_speech,
    ci_hi_speech   = mean_speech + qt(0.975, df = n_speech - 1) * se_speech,
    n_org          = sum(!is.na(gov_free_to_organize)),
    mean_org       = mean(gov_free_to_organize, na.rm = TRUE),
    se_org         = sd(gov_free_to_organize, na.rm = TRUE) / sqrt(n_org),
    ci_lo_org      = mean_org - qt(0.975, df = n_org - 1) * se_org,
    ci_hi_org      = mean_org + qt(0.975, df = n_org - 1) * se_org,
    pct_speech_hi  = mean(free_speech_hi, na.rm = TRUE) * 100,
    pct_org_hi     = mean(free_org_hi,    na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(wave_label = wave_labels[as.character(wave)]) |>
  select(wave, wave_year, wave_label, everything())

write.csv(desc, file.path(rr_dir, "freedom_descriptive.csv"), row.names = FALSE)
cat("Descriptive table saved.\n")
print(desc |> select(wave_label, mean_speech, ci_lo_speech, ci_hi_speech,
                      mean_org, ci_lo_org, ci_hi_org) |>
        mutate(across(where(is.numeric), \(x) round(x, 3))))

# ---------------------------------------------------------------------------
# 2. Controls in main models
#
# Strategy: OLS for transparency (ordered logit results consistent; noted).
# For each DV, two models:
#   A) DV ~ wave + controls (baseline)
#   B) DV ~ wave + free_speech + free_org + controls
# Report wave coefficients and % attenuation from A to B.
# ---------------------------------------------------------------------------

controls_str <- "age_z + female + edu_z + urban"

run_model_pair <- function(data, dv, wave_ref, controls) {
  df <- data |> filter(!is.na(.data[[dv]]))
  df <- df |> mutate(wave_f = relevel(wave_f, ref = as.character(wave_ref)))

  fA <- as.formula(paste(dv, "~ wave_f +", controls))
  fB <- as.formula(paste(dv, "~ wave_f + dem_free_speech + gov_free_to_organize +",
                         controls))

  mA <- lm(fA, data = df)
  mB <- lm(fB, data = df)

  tA <- tidy(mA, conf.int = TRUE) |>
    filter(grepl("wave_f", term)) |>
    mutate(model = "A_baseline", dv = dv, wave_ref = wave_ref)
  tB <- tidy(mB, conf.int = TRUE) |>
    filter(grepl("wave_f", term)) |>
    mutate(model = "B_freedom_controls", dv = dv, wave_ref = wave_ref)

  bind_rows(tA, tB) |>
    mutate(wave = gsub("wave_f", "", term))
}

model_coefs <- bind_rows(
  # dem_country_future: W3 is reference (W2 not fielded)
  run_model_pair(camb |> filter(wave != 2),
                 "dem_country_future", 3, controls_str),
  # democracy_satisfaction: W2 reference
  run_model_pair(camb, "democracy_satisfaction", 2, controls_str),
  # dem_pref_binary: W2 reference
  run_model_pair(camb, "dem_pref_binary", 2, controls_str)
)

# Add attenuation column: % change in |estimate| from A to B
attenuation <- model_coefs |>
  select(dv, wave, model, estimate) |>
  pivot_wider(names_from = model, values_from = estimate) |>
  mutate(
    attenuation_pct = round((abs(A_baseline) - abs(B_freedom_controls)) /
                              abs(A_baseline) * 100, 1)
  )

model_coefs <- model_coefs |>
  left_join(attenuation |> select(dv, wave, attenuation_pct),
            by = c("dv", "wave"))

write.csv(model_coefs, file.path(rr_dir, "freedom_models_coefs.csv"),
          row.names = FALSE)
cat("\nModel comparison coefficients saved.\n")

# Print key summary
cat("\n=== Wave coefficient attenuation (model A → B) ===\n")
print(attenuation |> arrange(dv, wave) |>
        mutate(across(where(is.numeric), \(x) round(x, 3))))

# Freedom variable coefficients in model B
cat("\n=== Freedom variable coefficients (model B) ===\n")
free_coefs <- bind_rows(
  lapply(c("dem_country_future", "democracy_satisfaction", "dem_pref_binary"),
         function(dv) {
           df <- camb
           if (dv == "dem_country_future") df <- camb |> filter(wave != 2)
           df <- df |> filter(!is.na(.data[[dv]]))
           f  <- as.formula(paste(dv, "~ wave_f + dem_free_speech +",
                                  "gov_free_to_organize +", controls_str))
           tidy(lm(f, data = df), conf.int = TRUE) |>
             filter(grepl("dem_free_speech|gov_free_to_organize", term)) |>
             mutate(dv = dv)
         })
)
print(free_coefs |> select(dv, term, estimate, std.error, p.value, conf.low, conf.high) |>
        mutate(across(where(is.numeric), \(x) round(x, 3))))

# ---------------------------------------------------------------------------
# 3. Moderator analysis
#
# DV ~ wave * free_speech_hi + gov_free_to_organize + controls
# (free_speech as primary moderator; gov_free_to_organize as additive control)
# Report wave × free_speech_hi interaction coefficients.
# Also run with gov_free_to_organize as the moderator.
# ---------------------------------------------------------------------------

run_moderator <- function(data, dv, wave_ref, moderator, controls) {
  df <- data |> filter(!is.na(.data[[dv]]), !is.na(.data[[moderator]]))
  df <- df |> mutate(wave_f = relevel(wave_f, ref = as.character(wave_ref)))

  f <- as.formula(paste(dv, "~ wave_f *", moderator, "+", controls))
  m <- lm(f, data = df)

  tidy(m, conf.int = TRUE) |>
    mutate(dv = dv, moderator = moderator, wave_ref = wave_ref)
}

mod_coefs <- bind_rows(
  # free_speech_hi as moderator
  run_moderator(camb |> filter(wave != 2),
                "dem_country_future", 3, "free_speech_hi", controls_str),
  run_moderator(camb, "democracy_satisfaction", 2, "free_speech_hi", controls_str),
  run_moderator(camb, "dem_pref_binary",        2, "free_speech_hi", controls_str),
  # free_org_hi as moderator
  run_moderator(camb |> filter(wave != 2),
                "dem_country_future", 3, "free_org_hi", controls_str),
  run_moderator(camb, "democracy_satisfaction", 2, "free_org_hi", controls_str),
  run_moderator(camb, "dem_pref_binary",        2, "free_org_hi", controls_str)
)

write.csv(mod_coefs, file.path(rr_dir, "freedom_moderator_coefs.csv"),
          row.names = FALSE)
cat("\nModerator coefficients saved.\n")

cat("\n=== Key interaction terms (wave × freedom) ===\n")
print(mod_coefs |>
        filter(grepl("wave_f.*:", term) | grepl(":wave_f", term)) |>
        select(dv, moderator, term, estimate, std.error, p.value) |>
        mutate(across(where(is.numeric), \(x) round(x, 3))))

# ---------------------------------------------------------------------------
# 4. Nonresponse prediction (Wave 6 only)
#
# DV: missing dem_country_future in W6
# Predictors: dem_free_speech, gov_free_to_organize, education_level, urban
# Logistic regression
# ---------------------------------------------------------------------------

w6 <- camb |>
  filter(wave == 6) |>
  mutate(
    future_missing = as.integer(is.na(dem_country_future))
  )

cat("\nWave 6: N =", nrow(w6),
    "| missing dem_future:", sum(w6$future_missing),
    "(", round(mean(w6$future_missing) * 100, 1), "%)\n")

nr_model <- glm(
  future_missing ~ dem_free_speech + gov_free_to_organize + edu_z + urban,
  data   = w6,
  family = binomial(link = "logit")
)

nr_coefs <- tidy(nr_model, conf.int = TRUE, exponentiate = FALSE) |>
  mutate(
    OR       = exp(estimate),
    OR_lo    = exp(conf.low),
    OR_hi    = exp(conf.high),
    p_stars  = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE            ~ "")
  )

write.csv(nr_coefs, file.path(rr_dir, "freedom_nonresponse_coefs.csv"),
          row.names = FALSE)
cat("\nNonresponse model saved.\n")

cat("\n=== Nonresponse model (logistic, W6 only) ===\n")
print(nr_coefs |> select(term, estimate, std.error, p.value, p_stars, OR, OR_lo, OR_hi) |>
        mutate(across(where(is.numeric), \(x) round(x, 3))))

# Pseudo-R2 (McFadden)
null_model <- glm(future_missing ~ 1, data = w6, family = binomial)
mcfadden_r2 <- 1 - as.numeric(logLik(nr_model)) / as.numeric(logLik(null_model))
cat("McFadden pseudo-R2:", round(mcfadden_r2, 3), "\n")

# ---------------------------------------------------------------------------
# 5. Append key values to inline_stats.rds
# ---------------------------------------------------------------------------
stats <- readRDS(file.path(res_dir, "inline_stats.rds"))

# Descriptive: wave means
stats$free_speech_w2 <- desc$mean_speech[desc$wave == 2]
stats$free_speech_w3 <- desc$mean_speech[desc$wave == 3]
stats$free_speech_w4 <- desc$mean_speech[desc$wave == 4]
stats$free_speech_w6 <- desc$mean_speech[desc$wave == 6]
stats$free_org_w2    <- desc$mean_org[desc$wave == 2]
stats$free_org_w3    <- desc$mean_org[desc$wave == 3]
stats$free_org_w4    <- desc$mean_org[desc$wave == 4]
stats$free_org_w6    <- desc$mean_org[desc$wave == 6]

# % in high-freedom category by wave
stats$pct_speech_hi_w2 <- desc$pct_speech_hi[desc$wave == 2]
stats$pct_speech_hi_w3 <- desc$pct_speech_hi[desc$wave == 3]
stats$pct_speech_hi_w4 <- desc$pct_speech_hi[desc$wave == 4]
stats$pct_speech_hi_w6 <- desc$pct_speech_hi[desc$wave == 6]
stats$pct_org_hi_w2    <- desc$pct_org_hi[desc$wave == 2]
stats$pct_org_hi_w3    <- desc$pct_org_hi[desc$wave == 3]
stats$pct_org_hi_w4    <- desc$pct_org_hi[desc$wave == 4]
stats$pct_org_hi_w6    <- desc$pct_org_hi[desc$wave == 6]

# Nonresponse model: OR for free_speech
nr_speech <- nr_coefs |> filter(term == "dem_free_speech")
nr_org    <- nr_coefs |> filter(term == "gov_free_to_organize")
stats$nr_OR_speech   <- nr_speech$OR
stats$nr_p_speech    <- nr_speech$p.value
stats$nr_OR_org      <- nr_org$OR
stats$nr_p_org       <- nr_org$p.value

# Model B: max attenuation across DVs
stats$max_wave_attenuation_pct <- max(abs(attenuation$attenuation_pct), na.rm = TRUE)

saveRDS(stats, file.path(res_dir, "inline_stats.rds"))
cat("\ninline_stats.rds updated. Total keys:", length(stats), "\n")

cat("\n=== All analyses complete ===\n")
cat("Output files in", rr_dir, ":\n")
cat("  freedom_descriptive.csv\n")
cat("  freedom_models_coefs.csv\n")
cat("  freedom_moderator_coefs.csv\n")
cat("  freedom_nonresponse_coefs.csv\n")
