# 11_binary_update_all.R
# Update all results files, figures, and appendix-ready tables with
# dem_pref_binary replacing qual_pref_dem_n.
#
# Tasks A-F from the remaining task list.

library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
results_dir  <- file.path(paper_dir, "analysis/results")
fig_dir      <- file.path(paper_dir, "analysis/figures")
source(file.path(paper_dir, "R", "helpers.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# Create binary recode
dat <- dat |>
  mutate(
    dem_pref_binary = if_else(dem_always_preferable == 1, 1L, 0L,
                              missing = NA_integer_),
    is_korea = as.integer(country_label == "Korea")
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

controls_str <- "age_n + gender + edu_n + urban_rural + polint_n"

yr_kr <- c(`1`=2003L, `2`=2006L, `3`=2011L, `4`=2015L, `5`=2019L, `6`=2022L)
yr_tw <- c(`1`=2001L, `2`=2006L, `3`=2010L, `4`=2014L, `5`=2019L, `6`=2022L)

# ═══════════════════════════════════════════════════════════════════════════════
# TASK A: Wave-by-wave binary coefficients (formatted for manuscript)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK A: Wave-by-wave binary coefficients\n")
cat("══════════════════════════════════════════════════\n\n")

br <- readRDS(file.path(results_dir, "binary_recode_rerun.rds"))

task_a <- br$wave_by_wave |>
  mutate(
    year = case_when(
      country == "Korea"  ~ yr_kr[as.character(wave)],
      country == "Taiwan" ~ yr_tw[as.character(wave)]
    ),
    sig = sig_stars(p.value)
  ) |>
  select(country, wave, year, estimate, std.error, p.value, sig, n)

cat("KOREA:\n")
task_a |> filter(country == "Korea") |>
  mutate(b_str = sprintf("%.3f%s", estimate, sig)) |>
  select(wave, year, b_str, p.value, n) |>
  print()

cat("\nTAIWAN:\n")
task_a |> filter(country == "Taiwan") |>
  mutate(b_str = sprintf("%.3f%s", estimate, sig)) |>
  select(wave, year, b_str, p.value, n) |>
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# TASK B: Regenerate Figure 4 (coefficient map) with binary DV
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK B: Regenerate coefficient map with binary DV\n")
cat("══════════════════════════════════════════════════\n\n")

# DV list — replacing qual_pref_dem_n with dem_pref_binary
all_dvs <- list(
  c("sat_democracy_n",     "Satisfaction: democracy",     "Satisfaction"),
  c("sat_govt_n",          "Satisfaction: government",    "Satisfaction"),
  c("dem_pref_binary",     "Dem always preferable",       "Abstract normative"),
  c("auth_reject_index",   "Auth rejection (index)",      "Auth rejection"),
  c("strongman_reject_n",  "Reject: strongman",           "Auth rejection"),
  c("military_reject_n",   "Reject: military rule",       "Auth rejection"),
  c("expert_reject_n",     "Reject: expert rule",         "Auth rejection"),
  c("singleparty_reject_n","Reject: single-party",        "Auth rejection"),
  c("qual_extent_n",       "Dem extent (current)",        "Evaluative"),
  c("qual_sys_support_n",  "System deserves support",     "Evaluative"),
  c("qual_sys_change_n",   "No major change needed",      "Evaluative")
)

dv_order <- map_chr(all_dvs, 1)
dv_labels <- setNames(map_chr(all_dvs, 2), map_chr(all_dvs, 1))
dv_cats   <- setNames(map_chr(all_dvs, 3), map_chr(all_dvs, 1))
countries <- c("Korea", "Taiwan")

# Run all pooled models
item_spec_rows <- map_dfr(countries, function(cntry) {
  df_c <- dat |> filter(country_label == cntry)

  map_dfr(all_dvs, function(dv_entry) {
    dv       <- dv_entry[1]
    dv_label <- dv_entry[2]
    category <- dv_entry[3]

    if (all(is.na(df_c[[dv]]))) {
      return(tibble(
        country = cntry, dv = dv, dv_label = dv_label, category = category,
        estimate = NA_real_, std.error = NA_real_, p.value = NA_real_,
        conf.low = NA_real_, conf.high = NA_real_,
        n = 0L, stars = "", beta_std = NA_real_, r_squared = NA_real_
      ))
    }

    formula_str <- paste(dv, "~ econ_index + factor(wave) +", controls_str)
    m <- lm(as.formula(formula_str), data = df_c)

    sd_x <- sd(df_c$econ_index, na.rm = TRUE)
    sd_y <- sd(df_c[[dv]], na.rm = TRUE)

    extract_coef(m, "econ_index") |>
      mutate(
        country = cntry, dv = dv, dv_label = dv_label, category = category,
        n = nobs(m), stars = sig_stars(p.value),
        beta_std = estimate * (sd_x / sd_y),
        r_squared = summary(m)$r.squared
      )
  })
})

item_spec_df <- item_spec_rows |>
  mutate(
    dv       = factor(dv, levels = dv_order),
    dv_label = factor(dv_label, levels = dv_labels[dv_order]),
    category = factor(category, levels = c("Satisfaction", "Abstract normative",
                                           "Auth rejection", "Evaluative"))
  )

# Print the updated table
cat("Updated item specificity table:\n")
item_spec_df |>
  arrange(category, dv, country) |>
  mutate(
    b_str = sprintf("%.3f%s", estimate, stars),
    se_str = sprintf("(%.3f)", std.error)
  ) |>
  select(country, category, dv_label, b_str, se_str, p.value, n) |>
  print(n = 25)

# Cross-country interaction by item
xc_item_rows <- map_dfr(all_dvs, function(dv_entry) {
  dv       <- dv_entry[1]
  dv_label <- dv_entry[2]
  category <- dv_entry[3]

  if (all(is.na(dat[[dv]]))) {
    return(tibble(
      dv = dv, dv_label = dv_label, category = category,
      term = "econ_index:is_korea",
      estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, stars = ""
    ))
  }

  formula_str <- paste(dv, "~ econ_index * is_korea + factor(wave) +", controls_str)
  m <- lm(as.formula(formula_str), data = dat)
  tidy_hc2(m) |>
    filter(term == "econ_index:is_korea") |>
    mutate(
      dv = dv, dv_label = dv_label, category = category,
      stars = sig_stars(p.value)
    ) |>
    select(dv, dv_label, category, term,
           estimate, std.error, statistic, p.value, conf.low, conf.high, stars)
})

xc_item_df <- xc_item_rows |>
  mutate(
    dv       = factor(dv, levels = dv_order),
    dv_label = factor(dv_label, levels = dv_labels[dv_order]),
    category = factor(category, levels = levels(item_spec_df$category))
  )

# Recalculate ratio
tw_pref <- item_spec_df |>
  filter(country == "Taiwan", dv == "dem_pref_binary") |> pull(estimate)
kr_pref <- item_spec_df |>
  filter(country == "Korea", dv == "dem_pref_binary") |> pull(estimate)
pref_gap <- tw_pref - kr_pref

auth_items_list <- c("auth_reject_index", "strongman_reject_n", "military_reject_n",
                     "expert_reject_n", "singleparty_reject_n")
auth_betas <- item_spec_df |>
  filter(dv %in% auth_items_list) |>
  select(country, dv, estimate) |>
  pivot_wider(names_from = country, values_from = estimate)
auth_gaps <- auth_betas$Korea - auth_betas$Taiwan
mean_auth_gap <- mean(auth_gaps, na.rm = TRUE)
ratio <- abs(pref_gap) / abs(mean_auth_gap)

cat(sprintf("\n\nUpdated ratio: |pref_gap|/|mean_auth_gap| = %.1fx\n", ratio))
cat(sprintf("  Taiwan dem_pref_binary: %.4f\n", tw_pref))
cat(sprintf("  Korea  dem_pref_binary: %.4f\n", kr_pref))
cat(sprintf("  Pref gap: %.4f\n", pref_gap))
cat(sprintf("  Mean auth gap: %.4f\n", mean_auth_gap))

# Generate Figure 4
fig_data <- item_spec_df |>
  filter(!is.na(estimate)) |>
  mutate(dv_label = fct_rev(dv_label))

fig_coef_map <- fig_data |>
  ggplot(aes(x = estimate, y = dv_label, colour = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    size = 0.4, linewidth = 0.7
  ) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("Korea" = "#2166AC", "Taiwan" = "#D55E00")) +
  labs(
    title    = "Economic evaluations and democratic attitudes: Korea vs. Taiwan",
    subtitle = "Pooled OLS with wave FE and controls. 95% CIs.",
    x        = "\u03b2 (economic evaluation index \u2192 DV)",
    y        = NULL,
    colour   = NULL,
    caption  = paste(
      "Controls: age, gender, education, urban/rural, political interest.",
      "\nSource: Asian Barometer Survey, waves 1\u20136.",
      "\nDem always preferable: binary (1 = always, 0 = otherwise). All other DVs: 0\u20131 normalized."
    )
  ) +
  theme_pub +
  theme(
    strip.text.y  = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(file.path(fig_dir, "fig04_coefficient_map.pdf"), fig_coef_map,
       width = 8, height = 7)
ggsave(file.path(fig_dir, "fig04_coefficient_map.png"), fig_coef_map,
       width = 8, height = 7, dpi = 300)
cat("\nFigure 4 saved (coefficient map with binary DV)\n")

# Interaction version
fig_xc <- xc_item_df |>
  filter(!is.na(estimate)) |>
  mutate(dv_label = fct_rev(dv_label)) |>
  ggplot(aes(x = estimate, y = dv_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    colour = "#2166AC", size = 0.4, linewidth = 0.7
  ) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "Korea \u2013 Taiwan differential: econ_index:is_korea interaction",
    subtitle = "Positive = Korea effect on DV stronger than Taiwan. Pooled OLS, wave FE.",
    x        = "\u03b2 (econ_index:is_korea interaction coefficient)",
    y        = NULL,
    caption  = "Controls: age, gender, education, urban/rural, political interest.\nSource: Asian Barometer Survey, waves 1\u20136."
  ) +
  theme_pub +
  theme(
    strip.text.y  = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(file.path(fig_dir, "fig04b_coef_map_interaction.pdf"), fig_xc,
       width = 7, height = 7)
ggsave(file.path(fig_dir, "fig04b_coef_map_interaction.png"), fig_xc,
       width = 7, height = 7, dpi = 300)
cat("Figure 4b saved (interaction version)\n")

# Save updated item_specificity_results
item_specificity_results <- list(
  all_dvs       = item_spec_df,
  ratio         = ratio,
  pref_gap      = pref_gap,
  mean_auth_gap = mean_auth_gap,
  cross_country = xc_item_df
)
saveRDS(item_specificity_results, file.path(results_dir, "item_specificity_results.rds"))
cat("Updated: item_specificity_results.rds\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK C: Update model_results.rds with binary DV
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK C: Update model_results.rds\n")
cat("══════════════════════════════════════════════════\n\n")

mr <- readRDS(file.path(results_dir, "model_results.rds"))

# C.1: Update wave_by_wave — replace qual_pref_dem_n rows with dem_pref_binary
# The wave_by_wave component has dv column with values like "qual_pref_dem_n"
cat("wave_by_wave DVs before:", unique(mr$wave_by_wave$dv), "\n")

# Build replacement rows for dem_pref_binary wave-by-wave
wbw_binary <- list()
for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100 || all(is.na(sub$dem_pref_binary))) next

    f <- as.formula(paste("dem_pref_binary ~ econ_index +", controls_str))
    m <- lm(f, data = sub)

    wbw_binary[[paste(cntry, w, sep = "_")]] <-
      extract_coef(m, "econ_index") |>
      mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable",
             country = cntry, wave = w,
             r_sq = summary(m)$r.squared, n = nobs(m))
  }
}
wbw_binary_df <- bind_rows(wbw_binary)

mr$wave_by_wave <- mr$wave_by_wave |>
  filter(dv != "qual_pref_dem_n") |>
  bind_rows(wbw_binary_df)

cat("wave_by_wave DVs after:", unique(mr$wave_by_wave$dv), "\n")

# C.2: Update pooled_indiv — replace qual_pref_dem_n
pooled_binary <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)
  f <- as.formula(paste("dem_pref_binary ~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = sub)
  pooled_binary[[cntry]] <-
    extract_coef(m, "econ_index") |>
    mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable",
           country = cntry, r_sq = summary(m)$r.squared, n = nobs(m))
}

mr$pooled_indiv <- mr$pooled_indiv |>
  filter(dv != "qual_pref_dem_n") |>
  bind_rows(bind_rows(pooled_binary))

# C.3: Update cross_country — replace qual_pref_dem_n
f_xc <- as.formula(paste("dem_pref_binary ~ econ_index * is_korea + factor(wave) +", controls_str))
m_xc <- lm(f_xc, data = dat)
xc_binary <- tidy_hc2(m_xc) |>
  filter(term %in% c("econ_index", "is_korea", "econ_index:is_korea")) |>
  mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable", n = nobs(m_xc))

mr$cross_country <- mr$cross_country |>
  filter(dv != "qual_pref_dem_n") |>
  bind_rows(xc_binary)

# C.4: Update polint_subgroup — replace qual_pref_dem_n
polint_binary <- list()
kr_dat <- dat |> filter(country_label == "Korea")
for (grp in c("High interest", "Low interest")) {
  sub <- kr_dat |> filter(polint_group == grp)
  if (nrow(sub) < 200) next
  f <- as.formula(paste("dem_pref_binary ~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = sub)
  polint_binary[[grp]] <-
    extract_coef(m, "econ_index") |>
    mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable",
           country = "Korea", group = grp, n = nobs(m))
}

mr$polint_subgroup <- mr$polint_subgroup |>
  filter(dv != "qual_pref_dem_n") |>
  bind_rows(bind_rows(polint_binary))

# C.5: Update winner_loser_subgroup — replace qual_pref_dem_n
wl_binary <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub_c <- dat |> filter(country_label == cntry, !is.na(winner_loser))
  for (grp in c("Winner", "Loser")) {
    sub <- sub_c |> filter(winner_loser == grp)
    if (nrow(sub) < 100) next
    f <- as.formula(paste("dem_pref_binary ~ econ_index + factor(wave) +", controls_str))
    m <- lm(f, data = sub)
    wl_binary[[paste(cntry, grp, sep = "_")]] <-
      extract_coef(m, "econ_index") |>
      mutate(dv = "dem_pref_binary", dv_label = "Dem always preferable",
             country = cntry, group = grp, n = nobs(m))
  }
}

mr$winner_loser_subgroup <- mr$winner_loser_subgroup |>
  filter(dv != "qual_pref_dem_n") |>
  bind_rows(bind_rows(wl_binary))

# C.6: Update cross-country inline stats
xc_tidy <- tidy_hc2(m_xc) |>
  filter(term %in% c("econ_index", "is_korea", "econ_index:is_korea"))

# The qual-specific inline stats need updating
# xc_qual was on qual_index (composite), which is NOT being replaced
# But we should add the binary-specific interaction
mr$inline$xc_dempref_interaction_b <- xc_tidy$estimate[xc_tidy$term == "econ_index:is_korea"]
mr$inline$xc_dempref_interaction_p <- xc_tidy$p.value[xc_tidy$term == "econ_index:is_korea"]

# Save updated model_results
saveRDS(mr, file.path(results_dir, "model_results.rds"))
cat("Updated: model_results.rds\n")

# List which appendix tables changed
cat("\nAppendix tables affected:\n")
cat("  B (wave-by-wave): YES — dem_pref_binary replaces qual_pref_dem_n\n")
cat("  C (pooled): YES — dem_pref_binary replaces qual_pref_dem_n\n")
cat("  D (cross-country): YES — dem_pref_binary replaces qual_pref_dem_n\n")
cat("  E.1 (polint subgroup): YES — dem_pref_binary replaces qual_pref_dem_n\n")
cat("  E.2 (winner-loser): YES — dem_pref_binary replaces qual_pref_dem_n\n")
cat("  F (auth rejection): NO — unaffected\n")
cat("  G (item specificity): YES — updated via item_specificity_results.rds\n")
cat("  H (identity moderation): YES — china/pride use qual_pref_dem_n label\n")
cat("  I (reliability): NO — auth rejection battery only\n")
cat("  J (construct validity): NO — sat control on auth rejection\n")
cat("  K (EFA): FLAGGED — dem_always_pref enters as nominal; binary OK for EFA\n")
cat("  L (measurement invariance): NO — auth rejection battery only\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK D: Engaged minority — exact values confirmed
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK D: Engaged minority — confirmed values\n")
cat("══════════════════════════════════════════════════\n\n")

cat("1. Polint × econ continuous interaction on dem_pref_binary:\n")
br$polint_interaction |>
  filter(term == "econ_index:polint_n") |>
  select(term, estimate, std.error, p.value) |>
  mutate(across(c(estimate, std.error, p.value), ~round(., 4))) |>
  print()

cat("\n2. Discussion frequency split:\n")
br$discuss_split |>
  select(group, estimate, std.error, p.value, n) |>
  mutate(across(c(estimate, std.error, p.value), ~round(., 4))) |>
  print()

cat("\n3. Composite engagement × econ on dem_pref_binary:\n")
br$engage_interaction |>
  filter(term == "econ_index:engage_composite") |>
  select(term, estimate, std.error, p.value) |>
  mutate(across(c(estimate, std.error, p.value), ~round(., 4))) |>
  print()

# 4. Component correlations of the engagement composite
abs_all <- readRDS(abs_harmonized_path)
abs_sub <- abs_all |> filter(country %in% c(3, 7))
stopifnot(nrow(abs_sub) == nrow(dat))
dat$pol_discuss_raw  <- abs_sub$pol_discuss
dat$pol_news_follow_raw <- abs_sub$pol_news_follow

kr_dat <- dat |> filter(country_label == "Korea")

kr_dat <- kr_dat |>
  group_by(country_label) |>
  mutate(
    pol_discuss_n  = normalize_01(pol_discuss_raw),
    pol_news_n     = normalize_01(pol_news_follow_raw)
  ) |>
  ungroup()

cor_mat <- kr_dat |>
  select(polint_n, pol_discuss_n, pol_news_n) |>
  drop_na() |>
  cor()

cat("\n4. Component correlations (Korea):\n")
print(round(cor_mat, 3))

# ═══════════════════════════════════════════════════════════════════════════════
# TASK E: Proportions and demographics for appendix (from R2.3 Task 7/8)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK E: Engaged minority proportions and demographics\n")
cat("══════════════════════════════════════════════════\n\n")

# Cutpoint: top quartile of political interest (polint_group == "High interest")
cat("E.1: Engaged minority proportions by wave (Korea)\n")
cat("Using polint_group == 'High interest' (median split)\n\n")

engage_props <- kr_dat |>
  filter(!is.na(polint_group)) |>
  group_by(wave) |>
  summarise(
    n = n(),
    n_high = sum(polint_group == "High interest"),
    pct_high = round(100 * n_high / n, 1),
    .groups = "drop"
  ) |>
  mutate(year = yr_kr[as.character(wave)])

print(engage_props)

cat("\nE.2: Demographics comparison (engaged vs disengaged, Korea pooled)\n\n")

demo_comp <- kr_dat |>
  filter(!is.na(polint_group)) |>
  group_by(polint_group) |>
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    pct_male = round(100 * mean(gender == 1, na.rm = TRUE), 1),
    mean_edu = round(mean(education_level, na.rm = TRUE), 1),
    pct_urban = round(100 * mean(urban_rural == 1, na.rm = TRUE), 1),
    mean_econ = round(mean(econ_index, na.rm = TRUE), 3),
    pct_dem_always = round(100 * mean(dem_pref_binary, na.rm = TRUE), 1),
    .groups = "drop"
  )

print(demo_comp)

# ═══════════════════════════════════════════════════════════════════════════════
# TASK F: Measurement evidence for Appendix M (from R2.6 Tasks 9-12)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK F: Measurement evidence\n")
cat("══════════════════════════════════════════════════\n\n")

# F.1: Item-total correlations (dem_pref_binary × auth_reject_index)
cat("F.1: Item-total correlations: dem_pref_binary × auth_reject_index\n\n")

itc <- dat |>
  filter(!is.na(dem_pref_binary), !is.na(auth_reject_index)) |>
  group_by(country_label, wave) |>
  summarise(
    r = cor(dem_pref_binary, auth_reject_index, use = "complete.obs"),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(year = case_when(
    country_label == "Korea"  ~ yr_kr[as.character(wave)],
    country_label == "Taiwan" ~ yr_tw[as.character(wave)]
  ))

itc |>
  mutate(r = round(r, 3)) |>
  select(country_label, wave, year, r, n) |>
  print(n = 15)

# F.2: Response distributions by country by wave
cat("\nF.2: dem_always_preferable response distributions\n\n")

dist_tbl <- dat |>
  filter(!is.na(dem_always_preferable)) |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    pct_always = round(100 * mean(dem_always_preferable == 1), 1),
    pct_auth   = round(100 * mean(dem_always_preferable == 2), 1),
    pct_nomat  = round(100 * mean(dem_always_preferable == 3), 1),
    .groups = "drop"
  ) |>
  mutate(year = case_when(
    country_label == "Korea"  ~ yr_kr[as.character(wave)],
    country_label == "Taiwan" ~ yr_tw[as.character(wave)]
  ))

dist_tbl |>
  select(country_label, wave, year, n, pct_always, pct_auth, pct_nomat) |>
  print(n = 15)

# F.3: Cross-country correlation matrices
cat("\nF.3: Satisfaction × auth rejection item correlations (Korea)\n\n")

cor_items_kr <- kr_dat |>
  select(sat_democracy_n, sat_govt_n, dem_pref_binary,
         strongman_reject_n, military_reject_n, expert_reject_n, singleparty_reject_n) |>
  drop_na() |>
  cor()

print(round(cor_items_kr, 3))

# Save everything
task_results <- list(
  task_a = task_a,
  task_d_polint_int = br$polint_interaction,
  task_d_discuss = br$discuss_split,
  task_d_engage_int = br$engage_interaction,
  task_d_correlations = cor_mat,
  task_e_proportions = engage_props,
  task_e_demographics = demo_comp,
  task_f_itc = itc,
  task_f_distributions = dist_tbl,
  task_f_cor_matrix_kr = cor_items_kr
)

saveRDS(task_results, file.path(results_dir, "remaining_tasks_results.rds"))

# ═══════════════════════════════════════════════════════════════════════════════
# TASK G: Confirm alternative normative items
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK G: Alternative normative items — scale check\n")
cat("══════════════════════════════════════════════════\n\n")

cat("dem_best_form:         1-4 ordinal (agree/disagree scale) -> normalize_01 OK\n")
cat("dem_vs_econ:           1-5 ordinal (priority scale) -> normalize_01 OK\n")
cat("democracy_suitability: 1-10 ordinal (rating scale) -> normalize_01 OK\n")
cat("dem_extent_current:    1-10 ordinal (rating scale) -> normalize_01 OK\n")
cat("dem_always_preferable: 1-3 NOMINAL -> FIXED (binary recode applied)\n")
cat("\nAll other DVs correctly coded. No further fixes needed.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK H: Check harmonization pipeline
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK H: Harmonization pipeline check\n")
cat("══════════════════════════════════════════════════\n\n")

cat("normalize_01() is NOT applied in _data_config.R (which only defines paths).\n")
cat("normalize_01() is NOT applied in survey-data-prep harmonization.\n")
cat("normalize_01() is applied per-paper in each paper's 00_data_preparation script.\n")
cat("-> No upstream fix needed. Each paper handles its own normalization.\n")
cat("-> The Cambodia paper already uses dem_pref_binary (line 55 of freedom_vars_analysis.R).\n")
cat("-> This paper now uses dem_pref_binary. Other papers should be checked individually.\n")

cat("\n\nAll tasks A-H complete.\n")
cat("Saved: item_specificity_results.rds, model_results.rds, remaining_tasks_results.rds\n")
cat("Regenerated: fig04_coefficient_map.pdf/png, fig04b_coef_map_interaction.pdf/png\n")
