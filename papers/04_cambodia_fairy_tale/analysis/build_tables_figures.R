#!/usr/bin/env Rscript
# build_tables_figures.R — Task 8 rebuild with gate participation variables

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(ggplot2)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/cambodia-fairy-tale")
results_dir  <- file.path(paper_dir, "analysis/results")
fig_dir      <- file.path(paper_dir, "analysis/figures")
tbl_dir      <- file.path(paper_dir, "analysis/tables")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir,     showWarnings = FALSE, recursive = TRUE)

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

# ── 8a. Build analysis_data.rds with gate variables ───────────────────────────
cat("Loading ABS...\n")
abs_all <- readRDS(abs_harmonized_path)

gate_vars <- c("gate_contact_elected", "gate_contact_civil_servant",
               "gate_contact_influential", "gate_petition",
               "gate_demonstration", "gate_contact_media")

dat <- abs_all |>
  filter(country == 12, wave %in% c(2, 3, 4, 6)) |>
  mutate(
    country_label = "Cambodia",
    year = case_when(
      wave == 2 ~ 2008, wave == 3 ~ 2012,
      wave == 4 ~ 2015, wave == 6 ~ 2022
    ),
    age_n    = normalize_01(age),
    edu_n    = normalize_01(education_level),
    polint_n = normalize_01(political_interest),
    # Ensure gates are numeric 0/1
    across(all_of(gate_vars), ~ as.numeric(as.character(.x)))
  )

cat("N per wave:\n"); print(table(dat$wave, dat$year))
saveRDS(dat, file.path(results_dir, "analysis_data.rds"))
cat("Saved analysis_data.rds\n\n")

# ── Variable metadata ──────────────────────────────────────────────────────────
# Participation: gate variables (binary → report as %)
# community_leader_contact and voted_last_election: no gate, keep as mean/proportion
part_gate_meta <- tribble(
  ~variable,                   ~label,
  "gate_contact_elected",      "Contacted elected official",
  "gate_contact_civil_servant","Contacted civil servant",
  "gate_contact_influential",  "Contacted influential person",
  "gate_petition",             "Signed petition",
  "gate_demonstration",        "Attended demonstration",
  "gate_contact_media",        "Contacted media"
)

part_other_meta <- tribble(
  ~variable,              ~label,
  "community_leader_contact", "Contacted community leader (mean, 1–5)",
  "voted_last_election",      "Voted in last election"
)

non_part_meta <- tribble(
  ~domain,                    ~variable,                  ~label,
  "Authoritarian Preferences", "expert_rule",              "Expert rule",
  "Authoritarian Preferences", "single_party_rule",        "Single-party rule",
  "Authoritarian Preferences", "strongman_rule",           "Strongman rule",
  "Authoritarian Preferences", "military_rule",            "Military rule",
  "Democratic Expectations",   "dem_country_future",       "Democratic future (10pt)",
  "Democratic Expectations",   "dem_country_past",         "Democratic past (10pt)",
  "Democratic Expectations",   "dem_country_present_govt", "Democratic present (10pt)",
  "Corruption",                "corrupt_witnessed",        "Witnessed corruption (binary)",
  "Corruption",                "corrupt_national_govt",    "National govt corruption (1–4)",
  "Corruption",                "corrupt_local_govt",       "Local govt corruption (1–4)",
  "Media & Political Interest","pol_news_follow",          "Follows political news",
  "Media & Political Interest","news_internet",            "Internet news (1–6)",
  "Media & Political Interest","political_interest",       "Political interest",
  "Media & Political Interest","pol_discuss",              "Discusses politics"
)

all_gate_vars    <- part_gate_meta$variable
all_other_vars   <- c(part_other_meta$variable, non_part_meta$variable)

# ── Wave-level summaries ───────────────────────────────────────────────────────
# Gate variables: proportion (mean of 0/1) + valid N
gate_summary <- dat |>
  group_by(wave, year) |>
  summarise(
    wave_n = n(),
    across(all_of(all_gate_vars),
           list(pct = ~ round(mean(.x, na.rm = TRUE) * 100, 1),
                n   = ~ sum(!is.na(.x))),
           .names = "{.col}__{.fn}"),
    .groups = "drop"
  )

# Other variables: mean + valid N
other_summary <- dat |>
  group_by(wave, year) |>
  summarise(
    across(all_of(all_other_vars),
           list(mean = ~ round(mean(.x, na.rm = TRUE), 2),
                n    = ~ sum(!is.na(.x))),
           .names = "{.col}__{.fn}"),
    .groups = "drop"
  )

cat("=== Gate variable proportions (%) by wave ===\n")
gate_summary |>
  select(wave, year, ends_with("__pct")) |>
  rename_with(~ sub("__pct$", "", .x)) |>
  print()

cat("\n=== Gate valid N by wave ===\n")
gate_summary |>
  select(wave, year, ends_with("__n")) |>
  rename_with(~ sub("__n$", "", .x)) |>
  print()

# ── Helper: extract wave value from summary ────────────────────────────────────
get_gate <- function(wdf, v, stat) {
  col <- paste0(v, "__", stat)
  val <- wdf[[col]]
  if (length(val) == 0 || all(is.na(val))) NA else val
}
get_other <- function(wdf, v, stat) {
  col <- paste0(v, "__", stat)
  val <- wdf[[col]]
  if (length(val) == 0 || all(is.na(val))) NA else val
}

fmt_pct <- function(pct, n) {
  if (is.na(pct)) "---" else sprintf("%.1f\\%% (%d)", pct, n)  # \% is LaTeX-escaped percent
}
fmt_mean <- function(m, digits = 2) {
  if (is.na(m) || is.nan(m)) "—" else sprintf(paste0("%.", digits, "f"), m)
}
fmt_delta_pct <- function(d) {
  if (is.na(d)) "—" else if (d > 0) sprintf("+%.1f pp", d) else sprintf("%.1f pp", d)
}
fmt_delta_mean <- function(d) {
  if (is.na(d)) "—" else if (d > 0) sprintf("+%.2f", d) else sprintf("%.2f", d)
}

waves <- list(w2 = 2, w3 = 3, w4 = 4, w6 = 6)
wdf <- list(
  w2 = gate_summary |> filter(wave == 2),
  w3 = gate_summary |> filter(wave == 3),
  w4 = gate_summary |> filter(wave == 4),
  w6 = gate_summary |> filter(wave == 6)
)
odf <- list(
  w2 = other_summary |> filter(wave == 2),
  w3 = other_summary |> filter(wave == 3),
  w4 = other_summary |> filter(wave == 4),
  w6 = other_summary |> filter(wave == 6)
)

# ── 8b. Table 1: W2 Baseline ──────────────────────────────────────────────────
make_part_row_t1 <- function(v, label) {
  pct <- get_gate(wdf$w2, v, "pct")
  n   <- get_gate(wdf$w2, v, "n")
  tibble(domain = "Political Participation (% ever)", label = label,
         w2_fmt = fmt_pct(pct, n))
}
make_other_row_t1 <- function(v, label, dom) {
  m <- get_other(odf$w2, v, "mean")
  n <- get_other(odf$w2, v, "n")
  tibble(domain = dom, label = label,
         w2_fmt = if (is.na(m) || is.nan(m)) "—" else
                  sprintf("%s (%d)", fmt_mean(m), n))
}

table1 <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_part_row_t1(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t1(..1, ..2, "Political Participation")),
  pmap_dfr(non_part_meta,   ~ make_other_row_t1(..2, ..3, ..1))
)

saveRDS(table1, file.path(tbl_dir, "table1_w2_baseline.rds"))
cat("\n=== Table 1 ===\n"); print(table1)

# ── 8b. Table 2: W3→W4 comparison ────────────────────────────────────────────
make_part_row_t2 <- function(v, label) {
  p3 <- get_gate(wdf$w3, v, "pct"); n3 <- get_gate(wdf$w3, v, "n")
  p4 <- get_gate(wdf$w4, v, "pct"); n4 <- get_gate(wdf$w4, v, "n")
  d  <- if (!is.na(p3) && !is.na(p4)) round(p4 - p3, 1) else NA_real_
  tibble(domain = "Political Participation (% ever)", label = label,
         w3_fmt = fmt_pct(p3, n3), w4_fmt = fmt_pct(p4, n4),
         delta_fmt = fmt_delta_pct(d))
}
make_other_row_t2 <- function(v, label, dom) {
  m3 <- get_other(odf$w3, v, "mean"); n3 <- get_other(odf$w3, v, "n")
  m4 <- get_other(odf$w4, v, "mean"); n4 <- get_other(odf$w4, v, "n")
  d  <- if (!is.na(m3) && !is.na(m4) && !is.nan(m3) && !is.nan(m4))
           round(m4 - m3, 2) else NA_real_
  tibble(domain = dom, label = label,
         w3_fmt = if (is.na(m3)||is.nan(m3)) "—" else sprintf("%s (%d)", fmt_mean(m3), n3),
         w4_fmt = if (is.na(m4)||is.nan(m4)) "—" else sprintf("%s (%d)", fmt_mean(m4), n4),
         delta_fmt = fmt_delta_mean(d))
}

table2 <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_part_row_t2(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t2(..1, ..2, "Political Participation")),
  pmap_dfr(non_part_meta,   ~ make_other_row_t2(..2, ..3, ..1))
)

saveRDS(table2, file.path(tbl_dir, "table2_w3w4_comparison.rds"))
cat("\n=== Table 2 ===\n"); print(table2)

# ── 8b. Table 3: Four-wave trajectory ────────────────────────────────────────
make_part_row_t3 <- function(v, label) {
  p2 <- get_gate(wdf$w2, v, "pct"); n2 <- get_gate(wdf$w2, v, "n")
  p3 <- get_gate(wdf$w3, v, "pct"); n3 <- get_gate(wdf$w3, v, "n")
  p4 <- get_gate(wdf$w4, v, "pct"); n4 <- get_gate(wdf$w4, v, "n")
  p6 <- get_gate(wdf$w6, v, "pct"); n6 <- get_gate(wdf$w6, v, "n")
  d  <- if (!is.na(p3) && !is.na(p6)) round(p6 - p3, 1) else NA_real_
  tibble(domain = "Political Participation (% ever)", label = label,
         w2_fmt = fmt_pct(p2, n2), w3_fmt = fmt_pct(p3, n3),
         w4_fmt = fmt_pct(p4, n4), w6_fmt = fmt_pct(p6, n6),
         delta_fmt = fmt_delta_pct(d))
}
make_other_row_t3 <- function(v, label, dom) {
  m2 <- get_other(odf$w2, v, "mean"); n2 <- get_other(odf$w2, v, "n")
  m3 <- get_other(odf$w3, v, "mean"); n3 <- get_other(odf$w3, v, "n")
  m4 <- get_other(odf$w4, v, "mean"); n4 <- get_other(odf$w4, v, "n")
  m6 <- get_other(odf$w6, v, "mean"); n6 <- get_other(odf$w6, v, "n")
  d  <- if (!is.na(m3) && !is.na(m6) && !is.nan(m3) && !is.nan(m6))
           round(m6 - m3, 2) else NA_real_
  fmt_cell <- function(m, n) if (is.na(m)||is.nan(m)) "—" else
              sprintf("%s (%d)", fmt_mean(m), n)
  tibble(domain = dom, label = label,
         w2_fmt = fmt_cell(m2, n2), w3_fmt = fmt_cell(m3, n3),
         w4_fmt = fmt_cell(m4, n4), w6_fmt = fmt_cell(m6, n6),
         delta_fmt = fmt_delta_mean(d))
}

table3 <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_part_row_t3(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t3(..1, ..2, "Political Participation")),
  pmap_dfr(non_part_meta,   ~ make_other_row_t3(..2, ..3, ..1))
)

saveRDS(table3, file.path(tbl_dir, "table3_four_wave_trajectory.rds"))
cat("\n=== Table 3 ===\n"); print(table3)

# ── 8c. Figure 1 rebuild ──────────────────────────────────────────────────────
# Participation facet: gate proportions (as 0-1); other facets: means as before
fig_part_vars <- tibble(
  domain   = "Political Participation",
  variable = c("gate_contact_elected", "gate_demonstration", "voted_last_election"),
  label    = c("Contacted official", "Attended demonstration", "Voted (last election)"),
  is_gate  = c(TRUE, TRUE, FALSE)
)
fig_other_vars <- tibble(
  domain   = c("Authoritarian Preferences","Authoritarian Preferences",
                "Democratic Expectations",  "Democratic Expectations",
                "Corruption",               "Corruption",
                "Media & Political Interest","Media & Political Interest"),
  variable = c("single_party_rule","strongman_rule",
               "dem_country_future","dem_country_present_govt",
               "corrupt_witnessed","corrupt_national_govt",
               "pol_news_follow","political_interest"),
  label    = c("Single-party rule","Strongman rule",
               "Democratic future","Democratic present",
               "Witnessed corruption","Nat'l govt corruption",
               "Follows pol. news","Political interest"),
  is_gate  = FALSE
)
fig_vars <- bind_rows(fig_part_vars, fig_other_vars)

fig_data <- dat |>
  select(wave, year, all_of(unique(fig_vars$variable))) |>
  pivot_longer(cols = -c(wave, year), names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(variable, year) |>
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") |>
  left_join(fig_vars, by = "variable") |>
  mutate(
    # Scale participation gates to % for display
    mean_val = if_else(is_gate, mean_val * 100, mean_val),
    domain = factor(domain, levels = c(
      "Political Participation", "Authoritarian Preferences",
      "Democratic Expectations", "Corruption", "Media & Political Interest"
    ))
  )

fig1 <- ggplot(fig_data, aes(x = year, y = mean_val, color = label, group = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
           hjust = -0.1, vjust = -0.3, size = 2.5, color = "grey50") +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(2008, 2012, 2015, 2022)) +
  scale_color_manual(values = c(
    "#2166AC","#B2182B","#4DAF4A","#F4A582","#D6604D",
    "#92C5DE","#4393C3","#762A83","#9970AB","#1B7837","#5AAE61"
  )) +
  labs(
    title    = "Political Orientations in Cambodia, 2008-2022",
    subtitle = "Participation panel: % who ever engaged (gate). Other panels: wave means.",
    x = NULL, y = NULL,
    caption = "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N = 1,000-1,242 per wave)."
  ) +
  theme_pub +
  theme(legend.position = "right", legend.text = element_text(size = 7.5))

ggsave(file.path(fig_dir, "fig1_trend_panels.pdf"), fig1, width = 10, height = 8)
ggsave(file.path(fig_dir, "fig1_trend_panels.png"), fig1, width = 10, height = 8, dpi = 300)
cat("\nFigure 1 saved.\n")

# ── 8d. Stat verification: gate proportions vs manuscript text ────────────────
cat("\n\n=== 8d. STAT VERIFICATION REPORT ===\n")

# Actual gate proportions from data
actual_gates <- dat |>
  group_by(wave, year) |>
  summarise(
    across(all_of(all_gate_vars),
           list(pct = ~ round(mean(.x, na.rm=TRUE)*100, 1),
                n   = ~ sum(!is.na(.x))),
           .names = "{.col}__{.fn}"),
    .groups = "drop"
  )

# Manuscript search: grep for hardcoded numbers related to participation
# Old frequency means that appear in the manuscript
old_vals <- tribble(
  ~variable,                   ~wave_label, ~wave_num, ~old_val, ~what,
  "action_contact_elected",     "W2",  2,  3.44,  "freq mean",
  "action_contact_elected",     "W3",  3,  4.31,  "freq mean",
  "action_contact_elected",     "W4",  4,  3.08,  "freq mean",
  "action_contact_elected",     "W6",  6,  1.74,  "freq mean",
  "action_contact_civil_servant","W2", 2,  3.43,  "freq mean",
  "action_contact_civil_servant","W3", 3,  4.33,  "freq mean",
  "action_contact_civil_servant","W4", 4,  3.19,  "freq mean",
  "action_contact_civil_servant","W6", 6,  2.47,  "freq mean",
  "action_demonstration",        "W3", 3,  4.48,  "freq mean",
  "action_demonstration",        "W4", 4,  3.07,  "freq mean",
  "action_demonstration",        "W6", 6,  1.29,  "freq mean",
  "action_petition",             "W3", 3,  4.34,  "freq mean",
  "action_petition",             "W4", 4,  3.23,  "freq mean",
  "action_petition",             "W6", 6,  2.14,  "freq mean"
)

gate_map <- c(
  "action_contact_elected"      = "gate_contact_elected",
  "action_contact_civil_servant"= "gate_contact_civil_servant",
  "action_demonstration"        = "gate_demonstration",
  "action_petition"             = "gate_petition"
)

cat(sprintf("%-30s %-5s %-10s %-10s %-6s\n",
            "Variable", "Wave", "Old (freq)", "New (gate%)", "Valid N"))
cat(strrep("-", 65), "\n")

for (i in seq_len(nrow(old_vals))) {
  row    <- old_vals[i, ]
  gvar   <- gate_map[row$variable]
  w_row  <- actual_gates |> filter(wave == row$wave_num)
  pct    <- w_row[[paste0(gvar, "__pct")]]
  n      <- w_row[[paste0(gvar, "__n")]]
  cat(sprintf("%-30s %-5s %-10s %-10s %-6s\n",
              row$variable, row$wave_label,
              sprintf("%.2f", row$old_val),
              if(is.null(pct)||is.na(pct)) "—" else sprintf("%.1f%%", pct),
              if(is.null(n)||is.na(n)) "—" else as.character(n)))
}

# ── 8e. Updated valid-N report ────────────────────────────────────────────────
cat("\n\n=== 8e. UPDATED VALID-N REPORT (gate variables included) ===\n")

all_check_vars <- c(all_gate_vars,
                    "community_leader_contact", "voted_last_election",
                    non_part_meta$variable)

valid_ns <- dat |>
  filter(wave %in% c(2,3,4,6)) |>
  group_by(wave, year) |>
  summarise(
    wave_n = n(),
    across(all_of(all_check_vars), ~ sum(!is.na(.x))),
    .groups = "drop"
  ) |>
  pivot_longer(-c(wave, year, wave_n), names_to = "variable", values_to = "valid_n") |>
  mutate(pct_valid = round(valid_n / wave_n * 100, 1),
         is_gate   = variable %in% all_gate_vars)

cat("\nGate variables — valid N:\n")
valid_ns |> filter(is_gate) |>
  select(wave, year, variable, valid_n, wave_n, pct_valid) |>
  arrange(variable, wave) |>
  print(n = 40)

cat("\nFlagged non-gate variables (>10% missing):\n")
valid_ns |>
  filter(!is_gate) |>
  group_by(wave) |>
  mutate(threshold = 0.90 * max(valid_n)) |>
  ungroup() |>
  filter(valid_n < threshold) |>
  select(wave, year, variable, valid_n, wave_n, pct_valid) |>
  arrange(wave, pct_valid) |>
  print(n = 50)

# ── Inline stats for manuscript ───────────────────────────────────────────────
cat("\n\n=== Computing inline stats for manuscript ===\n")

pull_val <- function(df, wave_num, col_name) {
  filtered <- df |> filter(wave == wave_num)
  if (nrow(filtered) == 0) return(NA_real_)
  if (!col_name %in% names(filtered)) return(NA_real_)
  v <- filtered[[col_name]]
  if (length(v) == 0 || all(is.na(v))) NA_real_ else v[[1]]
}

inline_stats <- list(
  # Gate participation proportions (% — already ×100 in gate_summary)
  gate_elected_w2      = pull_val(gate_summary, 2, "gate_contact_elected__pct"),
  gate_elected_w3      = pull_val(gate_summary, 3, "gate_contact_elected__pct"),
  gate_elected_w4      = pull_val(gate_summary, 4, "gate_contact_elected__pct"),
  gate_elected_w6      = pull_val(gate_summary, 6, "gate_contact_elected__pct"),

  gate_civil_w2        = pull_val(gate_summary, 2, "gate_contact_civil_servant__pct"),
  gate_civil_w3        = pull_val(gate_summary, 3, "gate_contact_civil_servant__pct"),
  gate_civil_w4        = pull_val(gate_summary, 4, "gate_contact_civil_servant__pct"),
  gate_civil_w6        = pull_val(gate_summary, 6, "gate_contact_civil_servant__pct"),

  gate_influential_w2  = pull_val(gate_summary, 2, "gate_contact_influential__pct"),
  gate_influential_w3  = pull_val(gate_summary, 3, "gate_contact_influential__pct"),
  gate_influential_w4  = pull_val(gate_summary, 4, "gate_contact_influential__pct"),
  gate_influential_w6  = pull_val(gate_summary, 6, "gate_contact_influential__pct"),

  gate_petition_w2     = pull_val(gate_summary, 2, "gate_petition__pct"),
  gate_petition_w3     = pull_val(gate_summary, 3, "gate_petition__pct"),
  gate_petition_w4     = pull_val(gate_summary, 4, "gate_petition__pct"),
  gate_petition_w6     = pull_val(gate_summary, 6, "gate_petition__pct"),

  gate_demo_w2         = pull_val(gate_summary, 2, "gate_demonstration__pct"),
  gate_demo_w3         = pull_val(gate_summary, 3, "gate_demonstration__pct"),
  gate_demo_w4         = pull_val(gate_summary, 4, "gate_demonstration__pct"),
  gate_demo_w6         = pull_val(gate_summary, 6, "gate_demonstration__pct"),

  gate_media_w2        = pull_val(gate_summary, 2, "gate_contact_media__pct"),
  gate_media_w3        = pull_val(gate_summary, 3, "gate_contact_media__pct"),
  gate_media_w4        = pull_val(gate_summary, 4, "gate_contact_media__pct"),
  gate_media_w6        = pull_val(gate_summary, 6, "gate_contact_media__pct"),

  # Community leader contact (mean, 1–5)
  community_leader_w2  = pull_val(other_summary, 2, "community_leader_contact__mean"),
  community_leader_w3  = pull_val(other_summary, 3, "community_leader_contact__mean"),
  community_leader_w4  = pull_val(other_summary, 4, "community_leader_contact__mean"),
  community_leader_w6  = pull_val(other_summary, 6, "community_leader_contact__mean"),

  # Voted last election (binary proportion → %)
  voted_w2             = pull_val(other_summary, 2, "voted_last_election__mean") * 100,
  voted_w3             = pull_val(other_summary, 3, "voted_last_election__mean") * 100,
  voted_w4             = pull_val(other_summary, 4, "voted_last_election__mean") * 100,
  voted_w6             = pull_val(other_summary, 6, "voted_last_election__mean") * 100,

  # Authoritarian preferences (means, 1–4)
  expert_rule_w2       = pull_val(other_summary, 2, "expert_rule__mean"),
  expert_rule_w3       = pull_val(other_summary, 3, "expert_rule__mean"),
  expert_rule_w4       = pull_val(other_summary, 4, "expert_rule__mean"),
  expert_rule_w6       = pull_val(other_summary, 6, "expert_rule__mean"),

  single_party_w2      = pull_val(other_summary, 2, "single_party_rule__mean"),
  single_party_w3      = pull_val(other_summary, 3, "single_party_rule__mean"),
  single_party_w4      = pull_val(other_summary, 4, "single_party_rule__mean"),
  single_party_w6      = pull_val(other_summary, 6, "single_party_rule__mean"),

  strongman_w2         = pull_val(other_summary, 2, "strongman_rule__mean"),
  strongman_w3         = pull_val(other_summary, 3, "strongman_rule__mean"),
  strongman_w4         = pull_val(other_summary, 4, "strongman_rule__mean"),
  strongman_w6         = pull_val(other_summary, 6, "strongman_rule__mean"),

  military_w2          = pull_val(other_summary, 2, "military_rule__mean"),
  military_w3          = pull_val(other_summary, 3, "military_rule__mean"),
  military_w4          = pull_val(other_summary, 4, "military_rule__mean"),
  military_w6          = pull_val(other_summary, 6, "military_rule__mean"),

  # Democratic expectations (means, 0–10)
  dem_future_w2        = pull_val(other_summary, 2, "dem_country_future__mean"),
  dem_future_w3        = pull_val(other_summary, 3, "dem_country_future__mean"),
  dem_future_w4        = pull_val(other_summary, 4, "dem_country_future__mean"),
  dem_future_w6        = pull_val(other_summary, 6, "dem_country_future__mean"),

  dem_past_w2          = pull_val(other_summary, 2, "dem_country_past__mean"),
  dem_past_w3          = pull_val(other_summary, 3, "dem_country_past__mean"),
  dem_past_w4          = pull_val(other_summary, 4, "dem_country_past__mean"),
  dem_past_w6          = pull_val(other_summary, 6, "dem_country_past__mean"),

  dem_present_w2       = pull_val(other_summary, 2, "dem_country_present_govt__mean"),
  dem_present_w3       = pull_val(other_summary, 3, "dem_country_present_govt__mean"),
  dem_present_w4       = pull_val(other_summary, 4, "dem_country_present_govt__mean"),
  dem_present_w6       = pull_val(other_summary, 6, "dem_country_present_govt__mean"),

  # Corruption witnessed (binary → %)
  corrupt_witnessed_w2 = pull_val(other_summary, 2, "corrupt_witnessed__mean") * 100,
  corrupt_witnessed_w3 = pull_val(other_summary, 3, "corrupt_witnessed__mean") * 100,
  corrupt_witnessed_w4 = pull_val(other_summary, 4, "corrupt_witnessed__mean") * 100,
  corrupt_witnessed_w6 = pull_val(other_summary, 6, "corrupt_witnessed__mean") * 100,

  # Corruption national/local (means, 1–4)
  corrupt_national_w2  = pull_val(other_summary, 2, "corrupt_national_govt__mean"),
  corrupt_national_w3  = pull_val(other_summary, 3, "corrupt_national_govt__mean"),
  corrupt_national_w4  = pull_val(other_summary, 4, "corrupt_national_govt__mean"),
  corrupt_national_w6  = pull_val(other_summary, 6, "corrupt_national_govt__mean"),

  corrupt_local_w2     = pull_val(other_summary, 2, "corrupt_local_govt__mean"),
  corrupt_local_w3     = pull_val(other_summary, 3, "corrupt_local_govt__mean"),
  corrupt_local_w4     = pull_val(other_summary, 4, "corrupt_local_govt__mean"),
  corrupt_local_w6     = pull_val(other_summary, 6, "corrupt_local_govt__mean"),

  # Media & political interest (means)
  pol_news_w2          = pull_val(other_summary, 2, "pol_news_follow__mean"),
  pol_news_w3          = pull_val(other_summary, 3, "pol_news_follow__mean"),
  pol_news_w4          = pull_val(other_summary, 4, "pol_news_follow__mean"),
  pol_news_w6          = pull_val(other_summary, 6, "pol_news_follow__mean"),

  news_internet_w2     = pull_val(other_summary, 2, "news_internet__mean"),
  news_internet_w3     = pull_val(other_summary, 3, "news_internet__mean"),
  news_internet_w4     = pull_val(other_summary, 4, "news_internet__mean"),
  news_internet_w6     = pull_val(other_summary, 6, "news_internet__mean"),

  pol_interest_w2      = pull_val(other_summary, 2, "political_interest__mean"),
  pol_interest_w3      = pull_val(other_summary, 3, "political_interest__mean"),
  pol_interest_w4      = pull_val(other_summary, 4, "political_interest__mean"),
  pol_interest_w6      = pull_val(other_summary, 6, "political_interest__mean"),

  pol_discuss_w2       = pull_val(other_summary, 2, "pol_discuss__mean"),
  pol_discuss_w3       = pull_val(other_summary, 3, "pol_discuss__mean"),
  pol_discuss_w4       = pull_val(other_summary, 4, "pol_discuss__mean"),
  pol_discuss_w6       = pull_val(other_summary, 6, "pol_discuss__mean"),

  # International orientation — China perceptions (means)
  china_asia_goodharm_w4    = dat |> filter(wave == 4) |> summarise(m = mean(intl_china_asia_goodharm, na.rm=TRUE)) |> pull(m) |> round(2),
  china_asia_goodharm_w6    = dat |> filter(wave == 6) |> summarise(m = mean(intl_china_asia_goodharm, na.rm=TRUE)) |> pull(m) |> round(2),
  future_influence_asia_w4  = dat |> filter(wave == 4) |> summarise(m = mean(intl_future_influence_asia, na.rm=TRUE)) |> pull(m) |> round(2),
  future_influence_asia_w6  = dat |> filter(wave == 6) |> summarise(m = mean(intl_future_influence_asia, na.rm=TRUE)) |> pull(m) |> round(2),

  # N per wave
  n_w2                 = gate_summary |> filter(wave == 2) |> pull(wave_n),
  n_w3                 = gate_summary |> filter(wave == 3) |> pull(wave_n),
  n_w4                 = gate_summary |> filter(wave == 4) |> pull(wave_n),
  n_w6                 = gate_summary |> filter(wave == 6) |> pull(wave_n)
)

saveRDS(inline_stats, file.path(results_dir, "inline_stats.rds"))
cat("Saved inline_stats.rds\n")
cat(sprintf("  dem_future_w3:         %.2f\n", inline_stats$dem_future_w3))
cat(sprintf("  dem_future_w6:         %.2f\n", inline_stats$dem_future_w6))
cat(sprintf("  corrupt_witnessed_w3:  %.1f%%\n", inline_stats$corrupt_witnessed_w3))
cat(sprintf("  corrupt_witnessed_w4:  %.1f%%\n", inline_stats$corrupt_witnessed_w4))
cat(sprintf("  corrupt_witnessed_w6:  %.1f%%\n", inline_stats$corrupt_witnessed_w6))
cat(sprintf("  gate_elected_w2:       %.1f%%\n", inline_stats$gate_elected_w2))
cat(sprintf("  gate_elected_w3:       %.1f%%\n", inline_stats$gate_elected_w3))
cat(sprintf("  gate_elected_w6:       %.1f%%\n", inline_stats$gate_elected_w6))
cat(sprintf("  voted_w3:              %.1f%%\n", inline_stats$voted_w3))
cat(sprintf("  voted_w6:              %.1f%%\n", inline_stats$voted_w6))

cat("\nDONE.\n")
