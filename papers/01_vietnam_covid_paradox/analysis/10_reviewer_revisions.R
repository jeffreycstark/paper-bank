# ==============================================================================
# 10_reviewer_revisions.R
# Reviewer-requested analyses:
#   #1  Endogeneity: placebo DV table + first-stage trust-predictor table
#   #5  Ceiling effects: DV distribution figure; binary + Firth logistic (Vietnam)
#   #7  Thailand marginal effects: predicted approval by infection × trust
#
# Outputs (all in analysis/results/ and analysis/output/figures/):
#   rev_placebo_table.rds
#   rev_firststage_table.rds
#   rev_dv_distribution.rds / fig_dv_distribution.png
#   rev_binary_logistic.rds
#   rev_firth_logistic.rds
#   fig_thailand_marginal_effects.png
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(broom)
  library(sandwich)
  library(lmtest)
  library(marginaleffects)
})

# Optional: Firth logistic
firth_ok <- requireNamespace("logistf", quietly = TRUE)
if (!firth_ok) {
  message("logistf not installed — Firth logistic skipped. Install with install.packages('logistf')")
}

cat(rep("=", 70), "\n", sep = "")
cat("Reviewer Revision Analyses\n")
cat(rep("=", 70), "\n\n", sep = "")

# Paths
data_path    <- here("papers", "01_vietnam_covid_paradox", "analysis", "data", "analysis_data.rds")
results_dir  <- here("papers", "01_vietnam_covid_paradox", "analysis", "results")
figures_dir  <- here("papers", "01_vietnam_covid_paradox", "analysis", "output", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

d <- readRDS(data_path)
cat("Loaded: N =", nrow(d), "\n\n")

# Shared controls (same as main models)
controls <- c("educ_level", "urban", "income_quintile",
              "econ_anxiety", "institutional_trust_index", "dem_satisfaction",
              "auth_acceptance", "country_name")

# Helper: robust SE OLS, return tidy with conf int
robust_ols <- function(formula, data) {
  m   <- lm(formula, data = data)
  ct  <- coeftest(m, vcov = vcovHC(m, type = "HC3"))
  ci  <- coefci(m, vcov. = vcovHC(m, type = "HC3"))
  tidy(ct) |>
    mutate(
      conf.low  = ci[, 1],
      conf.high = ci[, 2],
      n         = nobs(m),
      r.squared = summary(m)$r.squared
    )
}

# ==============================================================================
# #1a  PLACEBO TABLE
# Model: placebo_dv ~ covid_contracted + covid_trust_info + controls
# DVs:   (i) covid_govt_handling (main),
#        (ii) gov_sat_national, (iii) sat_president_govt,
#        (iv) system_deserves_support, (v) dem_satisfaction
# ==============================================================================

cat("[#1a] Placebo DV table...\n")

dv_list <- list(
  "Pandemic approval (main)"   = "covid_govt_handling",
  "National govt satisfaction" = "gov_sat_national",
  "Presidential satisfaction"  = "sat_president_govt",
  "System deserves support"    = "system_deserves_support",
  "Democracy satisfaction"     = "dem_satisfaction"
)

placebo_rows <- lapply(names(dv_list), function(label) {
  dv  <- dv_list[[label]]
  rhs <- paste(c("covid_contracted", "covid_trust_info", controls), collapse = " + ")
  f   <- as.formula(paste(dv, "~", rhs))
  dat <- d |> filter(!is.na(.data[[dv]]))
  res <- robust_ols(f, dat)

  # Extract key terms
  infection_row <- res |> filter(term == "covid_contracted") |>
    mutate(dv_label = label, predictor = "COVID infection")
  trust_row     <- res |> filter(term == "covid_trust_info") |>
    mutate(dv_label = label, predictor = "Info trust")

  bind_rows(infection_row, trust_row) |>
    select(dv_label, predictor, estimate, std.error, p.value, conf.low, conf.high, n, r.squared)
})

placebo_table <- bind_rows(placebo_rows)
saveRDS(placebo_table, file.path(results_dir, "rev_placebo_table.rds"))
cat("  Saved: rev_placebo_table.rds\n")
print(
  placebo_table |>
    select(dv_label, predictor, estimate, std.error, p.value) |>
    mutate(across(where(is.numeric), ~round(.x, 3)))
)
cat("\n")

# ==============================================================================
# #1b  FIRST-STAGE STYLE TABLE
# DV: covid_trust_info
# Predictors: educ_level, urban, educ_level*urban, news_internet,
#             pol_news_follow, access_internet, country_name + income_quintile
# Shows trust varies with information-environment predictors, not just approval
# ==============================================================================

cat("[#1b] First-stage trust predictor table...\n")

fs_controls <- c("educ_level", "urban", "educ_level:urban",
                 "news_internet", "pol_news_follow", "access_internet",
                 "income_quintile", "country_name")

fs_formula <- as.formula(
  paste("covid_trust_info ~", paste(fs_controls, collapse = " + "))
)

fs_dat <- d |> filter(!is.na(covid_trust_info))
fs_res <- robust_ols(fs_formula, fs_dat)

firststage_table <- fs_res |>
  filter(!grepl("^country_name|^Intercept", term)) |>
  select(term, estimate, std.error, p.value, conf.low, conf.high)

saveRDS(firststage_table, file.path(results_dir, "rev_firststage_table.rds"))
cat("  Saved: rev_firststage_table.rds\n")
print(firststage_table |> mutate(across(where(is.numeric), ~round(.x, 3))))
cat("  N =", unique(fs_res$n), " | R² =", round(unique(fs_res$r.squared), 3), "\n\n")

# ==============================================================================
# #5a  DV DISTRIBUTION FIGURE
# Bar chart of covid_govt_handling 1-4 by country
# ==============================================================================

cat("[#5a] DV distribution figure...\n")

dv_dist <- d |>
  filter(!is.na(covid_govt_handling)) |>
  mutate(
    approval_cat = factor(covid_govt_handling,
                          levels = 1:4,
                          labels = c("1 (Poorly)", "2", "3", "4 (Very well)"))
  ) |>
  count(country_name, approval_cat) |>
  group_by(country_name) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

saveRDS(dv_dist, file.path(results_dir, "rev_dv_distribution.rds"))

fig_dv <- ggplot(dv_dist, aes(x = approval_cat, y = pct, fill = approval_cat)) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            vjust = -0.4, size = 3) +
  facet_wrap(~country_name, nrow = 1) +
  scale_fill_manual(
    values = c("#d73027", "#fc8d59", "#91bfdb", "#4575b4"),
    guide  = "none"
  ) +
  scale_y_continuous(limits = c(0, 85), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Distribution of Pandemic Approval Ratings by Country",
    subtitle = "COVID-19 government handling (ABS Wave 6)",
    x        = "Response category (1 = handled very poorly, 4 = very well)",
    y        = "Percentage of respondents",
    caption  = "Note: Values may not sum to 100% due to rounding."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold"),
    plot.title         = element_text(face = "bold")
  )

ggsave(file.path(figures_dir, "fig_dv_distribution.png"),
       fig_dv, width = 9, height = 5, dpi = 300)
cat("  Saved: fig_dv_distribution.png\n\n")

# ==============================================================================
# #5b  BINARY DV LOGISTIC — VIETNAM
# DV: approve_high = 1 if covid_govt_handling >= 3
# Controls: same as main model (minus country_name — Vietnam only)
# ==============================================================================

cat("[#5b] Binary logistic robustness (Vietnam)...\n")

vn <- d |>
  filter(country_name == "Vietnam", !is.na(covid_govt_handling)) |>
  mutate(approve_high = as.integer(covid_govt_handling >= 3))

binary_controls <- c("educ_level", "urban", "income_quintile",
                     "econ_anxiety", "institutional_trust_index",
                     "dem_satisfaction", "auth_acceptance")

binary_formula <- as.formula(
  paste("approve_high ~ covid_contracted + covid_trust_info +",
        paste(binary_controls, collapse = " + "))
)

binary_fit <- glm(binary_formula, data = vn, family = binomial(link = "logit"))

# Robust SEs via sandwich
binary_ct  <- coeftest(binary_fit, vcov = vcovHC(binary_fit, type = "HC3"))
binary_ci  <- coefci(binary_fit,  vcov. = vcovHC(binary_fit, type = "HC3"))
binary_res <- tidy(binary_ct) |>
  mutate(
    conf.low     = binary_ci[, 1],
    conf.high    = binary_ci[, 2],
    odds_ratio   = exp(estimate),
    or_conf.low  = exp(conf.low),
    or_conf.high = exp(conf.high),
    model        = "Binary logistic (approve ≥ 3)"
  )

saveRDS(binary_res, file.path(results_dir, "rev_binary_logistic.rds"))
cat("  Saved: rev_binary_logistic.rds\n")
binary_res |>
  filter(term %in% c("covid_contracted", "covid_trust_info")) |>
  select(term, estimate, odds_ratio, p.value) |>
  mutate(across(where(is.numeric), ~round(.x, 3))) |>
  print()
cat("  N =", nrow(vn), "| approvals ≥3:", sum(vn$approve_high, na.rm=TRUE), "\n\n")

# ==============================================================================
# #5c  FIRTH-CORRECTED LOGISTIC — VIETNAM (rare-events correction)
# ==============================================================================

if (firth_ok) {
  cat("[#5c] Firth-corrected logistic (Vietnam)...\n")
  firth_fit <- logistf::logistf(binary_formula, data = vn)
  firth_res <- tibble(
    term      = names(firth_fit$coefficients),
    estimate  = firth_fit$coefficients,
    conf.low  = firth_fit$ci.lower,
    conf.high = firth_fit$ci.upper,
    p.value   = firth_fit$prob,
    model     = "Firth logistic"
  ) |>
    mutate(odds_ratio = exp(estimate))
  saveRDS(firth_res, file.path(results_dir, "rev_firth_logistic.rds"))
  cat("  Saved: rev_firth_logistic.rds\n")
  firth_res |>
    filter(term %in% c("covid_contracted", "covid_trust_info")) |>
    select(term, estimate, odds_ratio, p.value) |>
    mutate(across(where(is.numeric), ~round(.x, 3))) |>
    print()
  cat("\n")
} else {
  cat("[#5c] Firth logistic SKIPPED (logistf not installed)\n\n")
}

# ==============================================================================
# #7  THAILAND MARGINAL EFFECTS PLOT
# Predicted approval by covid_contracted × covid_trust_info (at 3 trust levels)
# Model: within Thailand only, interaction term
# ==============================================================================

cat("[#7] Thailand marginal effects plot...\n")

th <- d |>
  filter(country_name == "Thailand") |>
  filter(!is.na(covid_govt_handling), !is.na(covid_contracted),
         !is.na(covid_trust_info))

th_controls <- c("educ_level", "urban", "income_quintile",
                 "econ_anxiety", "institutional_trust_index",
                 "dem_satisfaction", "auth_acceptance")

th_formula <- as.formula(
  paste("covid_govt_handling ~ covid_contracted * covid_trust_info +",
        paste(th_controls, collapse = " + "))
)

th_model <- lm(th_formula, data = th)

# Marginal predictions at 3 trust levels × 2 infection values
trust_levels <- c(
  "Low (1)"    = 1,
  "Medium (2)" = 2,
  "High (4)"   = 4
)

th_preds <- expand.grid(
  covid_contracted  = c(0, 1),
  covid_trust_info  = trust_levels
) |>
  mutate(
    trust_label  = names(trust_levels)[match(covid_trust_info, trust_levels)],
    trust_label  = factor(trust_label, levels = names(trust_levels)),
    infected     = factor(covid_contracted, levels = c(0, 1),
                          labels = c("Not infected", "Infected"))
  )

# Fill controls at their means
ctrl_means <- th |>
  summarise(across(all_of(th_controls), ~mean(.x, na.rm = TRUE)))

th_newdata <- th_preds |>
  bind_cols(ctrl_means[rep(1, nrow(th_preds)), ])

th_newdata$predicted <- predict(th_model, newdata = th_newdata)

# Bootstrap CIs
set.seed(42)
boot_preds <- replicate(500, {
  boot_samp <- th[sample(nrow(th), replace = TRUE), ]
  boot_mod  <- lm(th_formula, data = boot_samp)
  predict(boot_mod, newdata = th_newdata)
})
th_newdata$ci_low  <- apply(boot_preds, 1, quantile, 0.025)
th_newdata$ci_high <- apply(boot_preds, 1, quantile, 0.975)

fig_th <- ggplot(th_newdata,
                 aes(x = infected, y = predicted,
                     colour = trust_label, group = trust_label)) +
  geom_line(linewidth = 0.9, position = position_dodge(0.15)) +
  geom_point(size = 3,     position = position_dodge(0.15)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.1, linewidth = 0.7,
                position = position_dodge(0.15)) +
  scale_colour_manual(
    values = c("Low (1)" = "#d73027", "Medium (2)" = "#fee090", "High (4)" = "#4575b4"),
    name   = "Trust in\nCOVID info"
  ) +
  scale_y_continuous(limits = c(1, 4), breaks = 1:4,
                     labels = c("1\n(Poorly)", "2", "3", "4\n(Very well)")) +
  labs(
    title    = "Predicted Pandemic Approval by Infection Status and Information Trust",
    subtitle = "Thailand only (ABS Wave 6, N = 1,200); controls at means; 95% bootstrap CIs",
    x        = "Personal COVID infection",
    y        = "Predicted approval rating (1–4)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(figures_dir, "fig_thailand_marginal_effects.png"),
       fig_th, width = 7, height = 5, dpi = 300)
cat("  Saved: fig_thailand_marginal_effects.png\n\n")

# Save Thailand model coefs too
th_res <- coeftest(th_model, vcov = vcovHC(th_model, type = "HC3"))
th_ci  <- coefci(th_model,  vcov. = vcovHC(th_model, type = "HC3"))
th_tidy <- tidy(th_res) |>
  mutate(conf.low = th_ci[,1], conf.high = th_ci[,2])
saveRDS(list(model = th_model, coefs = th_tidy, predictions = th_newdata),
        file.path(results_dir, "rev_thailand_interaction.rds"))
cat("  Saved: rev_thailand_interaction.rds\n\n")

cat(rep("=", 70), "\n", sep = "")
cat("All reviewer revision analyses complete.\n")
cat(rep("=", 70), "\n", sep = "")
