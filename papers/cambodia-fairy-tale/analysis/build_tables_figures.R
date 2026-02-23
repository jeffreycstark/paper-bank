#!/usr/bin/env Rscript
# build_tables_figures.R — regenerate analysis_data.rds, tables, and figures
# Run after any change to the upstream abs_harmonized.rds

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(ggplot2)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/cambodia-fairy-tale")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")
tbl_dir      <- file.path(analysis_dir, "tables")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir,     showWarnings = FALSE, recursive = TRUE)

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

# ── 1. Load and save analysis data ────────────────────────────────────────────
cat("Loading ABS harmonized data...\n")
abs_all <- readRDS(abs_harmonized_path)

dat <- abs_all |>
  filter(country == 12, wave %in% c(2, 3, 4, 6)) |>
  mutate(
    country_label = "Cambodia",
    year = case_when(
      wave == 2 ~ 2008,
      wave == 3 ~ 2012,
      wave == 4 ~ 2015,
      wave == 6 ~ 2022
    ),
    age_n    = normalize_01(age),
    edu_n    = normalize_01(education_level),
    polint_n = normalize_01(political_interest)
  )

cat("Cambodia n =", nrow(dat), "\n")
cat("N per wave:\n"); print(table(dat$wave, dat$year))
saveRDS(dat, file.path(results_dir, "analysis_data.rds"))
cat("Saved: analysis_data.rds\n\n")

# ── 2. Variable sets ───────────────────────────────────────────────────────────
var_meta <- tribble(
  ~domain,                    ~variable,                    ~label,
  "Political Participation",   "action_contact_elected",     "Contacted elected official",
  "Political Participation",   "action_contact_civil_servant","Contacted civil servant",
  "Political Participation",   "community_leader_contact",   "Contacted community leader",
  "Political Participation",   "voted_last_election",        "Voted in last election",
  "Political Participation",   "action_demonstration",       "Attended demonstration",
  "Political Participation",   "action_petition",            "Signed petition",
  "Authoritarian Preferences", "expert_rule",                "Expert rule",
  "Authoritarian Preferences", "single_party_rule",          "Single-party rule",
  "Authoritarian Preferences", "strongman_rule",             "Strongman rule",
  "Authoritarian Preferences", "military_rule",              "Military rule",
  "Democratic Expectations",   "dem_country_future",         "Democratic future (10pt)",
  "Democratic Expectations",   "dem_country_past",           "Democratic past (10pt)",
  "Democratic Expectations",   "dem_country_present_govt",   "Democratic present (10pt)",
  "Corruption",                "corrupt_witnessed",          "Witnessed corruption (binary)",
  "Corruption",                "corrupt_national_govt",      "National govt corruption (1-4)",
  "Corruption",                "corrupt_local_govt",         "Local govt corruption (1-4)",
  "Media & Political Interest","pol_news_follow",            "Follows political news",
  "Media & Political Interest","news_internet",              "Internet news",
  "Media & Political Interest","political_interest",         "Political interest",
  "Media & Political Interest","pol_discuss",                "Discusses politics"
)

all_vars <- unique(var_meta$variable)

# ── 3. Wave means ──────────────────────────────────────────────────────────────
wave_means <- dat |>
  group_by(wave, year) |>
  summarise(
    n = n(),
    across(all_of(all_vars), ~ round(mean(.x, na.rm = TRUE), 3)),
    .groups = "drop"
  )

cat("=== Wave means ===\n")
print(as.data.frame(wave_means))

w2 <- wave_means |> filter(wave == 2)
w3 <- wave_means |> filter(wave == 3)
w4 <- wave_means |> filter(wave == 4)
w6 <- wave_means |> filter(wave == 6)

get_val <- function(wdf, v) {
  val <- wdf[[v]]
  if (length(val) == 0 || all(is.na(val))) NA_real_ else val
}
fmt <- function(x, digits = 2) if_else(is.na(x), "—", sprintf(paste0("%.", digits, "f"), x))
fmt_delta <- function(x) case_when(is.na(x) ~ "—", x > 0 ~ sprintf("+%.2f", x), TRUE ~ sprintf("%.2f", x))

# ── 4. Table 1: Wave 2 baseline ────────────────────────────────────────────────
table1 <- var_meta |>
  mutate(
    w2_mean   = map_dbl(variable, ~ get_val(w2, .x)),
    w2_fmt    = fmt(w2_mean),
    n_fmt     = if_else(is.na(w2_mean), "—", as.character(w2$n))
  ) |>
  select(domain, label, w2_fmt, n_fmt)

saveRDS(table1, file.path(tbl_dir, "table1_w2_baseline.rds"))
cat("\nTable 1 saved.\n")

# ── 5. Table 2: W3→W4 comparison ──────────────────────────────────────────────
table2 <- var_meta |>
  mutate(
    w3_mean   = map_dbl(variable, ~ get_val(w3, .x)),
    w4_mean   = map_dbl(variable, ~ get_val(w4, .x)),
    delta     = round(w4_mean - w3_mean, 3),
    w3_fmt    = fmt(w3_mean),
    w4_fmt    = fmt(w4_mean),
    delta_fmt = fmt_delta(delta)
  ) |>
  select(domain, label, w3_fmt, w4_fmt, delta_fmt)

saveRDS(table2, file.path(tbl_dir, "table2_w3w4_comparison.rds"))
cat("Table 2 saved.\n")

# ── 6. Table 3: Four-wave trajectory ──────────────────────────────────────────
table3 <- var_meta |>
  mutate(
    w2_mean   = map_dbl(variable, ~ get_val(w2, .x)),
    w3_mean   = map_dbl(variable, ~ get_val(w3, .x)),
    w4_mean   = map_dbl(variable, ~ get_val(w4, .x)),
    w6_mean   = map_dbl(variable, ~ get_val(w6, .x)),
    delta     = round(w6_mean - w3_mean, 3),
    w2_fmt    = fmt(w2_mean),
    w3_fmt    = fmt(w3_mean),
    w4_fmt    = fmt(w4_mean),
    w6_fmt    = fmt(w6_mean),
    delta_fmt = fmt_delta(delta)
  ) |>
  select(domain, label, w2_fmt, w3_fmt, w4_fmt, w6_fmt, delta_fmt)

saveRDS(table3, file.path(tbl_dir, "table3_four_wave_trajectory.rds"))
cat("Table 3 saved.\n")

# ── 7. Figure 1: Multi-panel trend plot ───────────────────────────────────────
fig_vars <- tribble(
  ~domain,                    ~variable,                    ~label,
  "Political Participation",   "action_contact_elected",     "Contacted official",
  "Political Participation",   "action_demonstration",       "Attended demonstration",
  "Political Participation",   "voted_last_election",        "Voted (last election)",
  "Authoritarian Preferences", "single_party_rule",          "Single-party rule",
  "Authoritarian Preferences", "strongman_rule",             "Strongman rule",
  "Democratic Expectations",   "dem_country_future",         "Democratic future",
  "Democratic Expectations",   "dem_country_present_govt",   "Democratic present",
  "Corruption",                "corrupt_witnessed",          "Witnessed corruption",
  "Corruption",                "corrupt_national_govt",      "Nat'l govt corruption",
  "Media & Political Interest","pol_news_follow",            "Follows pol. news",
  "Media & Political Interest","political_interest",         "Political interest"
)

fig_data <- dat |>
  select(wave, year, all_of(unique(fig_vars$variable))) |>
  pivot_longer(cols = -c(wave, year), names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(variable, year) |>
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") |>
  left_join(fig_vars |> select(variable, domain, label), by = "variable") |>
  mutate(domain = factor(domain, levels = c(
    "Political Participation", "Authoritarian Preferences",
    "Democratic Expectations", "Corruption", "Media & Political Interest"
  )))

fig1 <- ggplot(fig_data, aes(x = year, y = mean_val, color = label, group = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
           hjust = -0.1, vjust = -0.3, size = 2.5, color = "grey50") +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(2008, 2012, 2015, 2022)) +
  scale_color_manual(values = c(
    "#2166AC", "#B2182B", "#4DAF4A", "#F4A582", "#D6604D",
    "#92C5DE", "#4393C3", "#762A83", "#9970AB", "#1B7837", "#5AAE61"
  )) +
  labs(
    title    = "Political Orientations in Cambodia, 2008-2022",
    subtitle = "Wave means by domain. Dashed line = 2017 CNRP dissolution.",
    x = NULL, y = "Mean value",
    caption  = "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N = 1,000-1,242 per wave)."
  ) +
  theme_pub +
  theme(legend.position = "right", legend.text = element_text(size = 7.5))

ggsave(file.path(fig_dir, "fig1_trend_panels.pdf"), fig1, width = 10, height = 8)
ggsave(file.path(fig_dir, "fig1_trend_panels.png"), fig1, width = 10, height = 8, dpi = 300)
cat("Figure 1 saved.\n")

# ── 8. Print updated news_internet values specifically ───────────────────────
cat("\n=== news_internet values (check against manuscript text) ===\n")
dat |>
  group_by(wave, year) |>
  summarise(
    n_nonmissing = sum(!is.na(news_internet)),
    mean_val     = round(mean(news_internet, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()

cat("\nDONE.\n")
