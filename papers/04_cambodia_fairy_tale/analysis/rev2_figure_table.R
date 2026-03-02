#!/usr/bin/env Rscript
# rev2_figure_table.R — Figure 1 update (Task 5) and Table 3 Δ W4→W6 (Task 6)
#
# Task 5: Add dem_best_form and dem_vs_equality to Democratic Orientations panel
#         (split Democratic Expectations into two sub-panels)
# Task 6: Add Δ W4→W6 column to Table 3 alongside existing W3→W6 column

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/04_cambodia_fairy_tale")
results_dir  <- file.path(paper_dir, "analysis/results")
fig_dir      <- file.path(paper_dir, "analysis/figures")
tbl_dir      <- file.path(paper_dir, "analysis/tables")
rr_dir       <- file.path(paper_dir, "analysis/reviewer_response")

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

cat("Loading ABS...\n")
abs_all <- readRDS(abs_harmonized_path)

gate_vars <- c("gate_contact_elected", "gate_contact_civil_servant",
               "gate_contact_influential", "gate_petition",
               "gate_demonstration", "gate_contact_media")

dat <- abs_all |>
  filter(country == 12, wave %in% c(2, 3, 4, 6)) |>
  mutate(
    year = case_when(
      wave == 2 ~ 2008L, wave == 3 ~ 2012L,
      wave == 4 ~ 2015L, wave == 6 ~ 2021L
    ),
    across(all_of(gate_vars), ~ as.numeric(as.character(.x)))
  )

# ── Wilson and mean CI helpers ─────────────────────────────────────────────────
wilson_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(list(estimate = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
  k  <- sum(x)
  res <- suppressWarnings(prop.test(k, n, conf.level = conf))
  list(estimate = k/n, ci_lower = res$conf.int[1], ci_upper = res$conf.int[2])
}
mean_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(list(estimate = mean(x), ci_lower = NA_real_, ci_upper = NA_real_))
  m  <- mean(x); se <- sd(x) / sqrt(n)
  tc <- qt((1 + conf) / 2, df = n - 1)
  list(estimate = m, ci_lower = m - tc * se, ci_upper = m + tc * se)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 5: Updated Figure 1 — Democratic Orientations panel split into two
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== TASK 5: Updated Figure 1 ===\n")

# All series to plot
fig_vars <- tribble(
  ~domain,                          ~variable,                   ~label,                  ~type,    ~scale,
  "Political Participation",        "gate_contact_influential",  "Contacted influential", "prop",   "pct",
  "Political Participation",        "gate_demonstration",        "Attended demonstration","prop",   "pct",
  "Political Participation",        "voted_last_election",       "Voted (last election)", "prop",   "pct",
  "Authoritarian Preferences",      "single_party_rule",         "Single-party rule",     "mean",   "1-4",
  "Authoritarian Preferences",      "strongman_rule",            "Strongman rule",        "mean",   "1-4",
  "Democratic Expectations",        "dem_country_future",        "Democratic future",     "mean",   "0-10",
  "Democratic Expectations",        "dem_country_present_govt",  "Democratic present",    "mean",   "0-10",
  "Democratic Commitment",          "dem_best_form",             "Dem. best form (1-4)",  "mean",   "1-4",
  "Democratic Commitment",          "dem_vs_equality",           "Dem. vs. equality (1-5)","mean", "1-5",
  "Corruption",                     "corrupt_witnessed",         "Witnessed corruption",  "prop",   "pct",
  "Corruption",                     "corrupt_national_govt",     "Nat'l govt corruption", "mean",   "1-4",
  "Media & Political Interest",     "pol_news_follow",           "Follows pol. news",     "mean",   "1-5",
  "Media & Political Interest",     "political_interest",        "Political interest",    "mean",   "1-4"
)

# Compute CI data for each variable × wave
fig_ci_rows <- list()
for (w in c(2, 3, 4, 6)) {
  yr <- c(`2`=2008L, `3`=2012L, `4`=2015L, `6`=2021L)[as.character(w)]
  dw <- dat |> filter(wave == w)
  for (i in seq_len(nrow(fig_vars))) {
    v   <- fig_vars$variable[i]
    typ <- fig_vars$type[i]
    sc  <- fig_vars$scale[i]
    if (!v %in% names(dw)) next
    vals <- dw[[v]]
    ci   <- if (typ == "prop") wilson_ci(vals) else mean_ci(vals)
    if (is.na(ci$estimate)) next
    # Scale to display units
    scale_mult <- if (sc == "pct") 100 else 1
    fig_ci_rows[[length(fig_ci_rows) + 1]] <- tibble(
      variable  = v,
      wave      = w,
      year      = yr,
      mean_val  = ci$estimate  * scale_mult,
      ci_lower  = ci$ci_lower  * scale_mult,
      ci_upper  = ci$ci_upper  * scale_mult
    )
  }
}

fig_ci_data <- bind_rows(fig_ci_rows) |>
  left_join(fig_vars, by = "variable") |>
  mutate(
    domain = factor(domain, levels = c(
      "Political Participation", "Authoritarian Preferences",
      "Democratic Expectations", "Democratic Commitment",
      "Corruption", "Media & Political Interest"
    ))
  )

# Colors: 13 series — expand palette
my_colors <- c(
  "#2166AC","#B2182B","#4DAF4A",  # Participation
  "#F4A582","#D6604D",            # Auth prefs
  "#4393C3","#92C5DE",            # Dem expectations
  "#762A83","#9970AB",            # Dem commitment
  "#1B7837","#5AAE61",            # Corruption
  "#A6761D","#E6AB02"             # Media/interest
)

fig1_updated <- ggplot(fig_ci_data,
                       aes(x = year, y = mean_val,
                           color = label, group = label)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  geom_vline(xintercept = 2017, linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
           hjust = -0.08, vjust = -0.2, size = 2.2, color = "grey50") +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(2008, 2012, 2015, 2021)) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  labs(
    title    = "Political Orientations in Cambodia, 2008\u20132021",
    subtitle = paste0("Participation: % ever engaged. Dem. Expectations: 0\u201310 scale. ",
                      "Dem. Commitment: raw scales (1\u20134 and 1\u20135). ",
                      "Other panels: wave means. Shading = 95% CI."),
    x = NULL, y = NULL, color = NULL, fill = NULL,
    caption = paste0(
      "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N = 1,000\u20131,242 per wave).\n",
      "CI: Wilson interval for proportions; t-based for means. ",
      "'Democratic Commitment' panel shows raw scale means: dem_best_form (1\u20134), ",
      "dem_vs_equality (1\u20135)."
    )
  ) +
  guides(fill = "none") +
  theme_pub +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 6.5),
    strip.text       = element_text(size = 8),
    plot.subtitle    = element_text(size = 7)
  )

ggsave(file.path(fig_dir, "fig1_trend_panels.pdf"), fig1_updated,
       width = 11, height = 10)
ggsave(file.path(fig_dir, "fig1_trend_panels.png"), fig1_updated,
       width = 11, height = 10, dpi = 300)
cat("Saved updated fig1_trend_panels (.pdf + .png)\n")

# Preview: dem commitment means by wave
cat("\n=== Democratic Commitment means by wave ===\n")
fig_ci_data |>
  filter(domain == "Democratic Commitment") |>
  select(label, year, mean_val) |>
  mutate(mean_val = round(mean_val, 3)) |>
  pivot_wider(names_from = year, values_from = mean_val) |>
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 6: Add Δ W4→W6 column to Table 3
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== TASK 6: Add Δ W4→W6 column to Table 3 ===\n")

ue <- readRDS(file.path(rr_dir, "uncertainty_estimates.rds"))

# Helper: delta between two waves for a variable
delta_pct  <- function(v, wa, wb) {
  a <- ue[ue$variable == v & ue$wave == wa, ]
  b <- ue[ue$variable == v & ue$wave == wb, ]
  if (nrow(a) == 0 || nrow(b) == 0 || is.na(a$estimate[1]) || is.na(b$estimate[1]))
    return(NA_real_)
  round((b$estimate[1] - a$estimate[1]) * 100, 1)
}
delta_mean <- function(v, wa, wb) {
  a <- ue[ue$variable == v & ue$wave == wa, ]
  b <- ue[ue$variable == v & ue$wave == wb, ]
  if (nrow(a) == 0 || nrow(b) == 0 || is.na(a$estimate[1]) || is.na(b$estimate[1]))
    return(NA_real_)
  round(b$estimate[1] - a$estimate[1], 2)
}
fmt_delta_pct  <- function(d) {
  if (is.na(d)) "\u2014" else if (d > 0) sprintf("+%.1f pp", d) else sprintf("%.1f pp", d)
}
fmt_delta_mean <- function(d) {
  if (is.na(d)) "\u2014" else if (d > 0) sprintf("+%.2f", d) else sprintf("%.2f", d)
}

# Load existing table3
table3 <- readRDS(file.path(tbl_dir, "table3_four_wave_trajectory.rds"))
cat("table3 columns:", paste(names(table3), collapse=", "), "\n")
cat("table3 rows:", nrow(table3), "\n")

# Variable metadata mirroring task10_tables_appendix.R
part_gate_meta <- tribble(
  ~variable,                    ~label,
  "gate_contact_elected",       "Contacted elected official",
  "gate_contact_civil_servant", "Contacted civil servant",
  "gate_contact_influential",   "Contacted influential person",
  "gate_petition",              "Signed petition",
  "gate_demonstration",         "Attended demonstration",
  "gate_contact_media",         "Contacted media"
)
part_other_meta <- tribble(
  ~variable,                  ~label,
  "community_leader_contact", "Contacted community leader (mean, 1\u20135)",
  "voted_last_election",      "Voted in last election"
)
non_part_meta <- tribble(
  ~domain,                     ~variable,                   ~label,
  "Authoritarian Preferences", "expert_rule",               "Expert rule",
  "Authoritarian Preferences", "single_party_rule",         "Single-party rule",
  "Authoritarian Preferences", "strongman_rule",            "Strongman rule",
  "Authoritarian Preferences", "military_rule",             "Military rule",
  "Democratic Expectations",   "dem_country_future",        "Democratic future (10pt)",
  "Democratic Expectations",   "dem_country_past",          "Democratic past (10pt)",
  "Democratic Expectations",   "dem_country_present_govt",  "Democratic present (10pt)",
  "Corruption",                "corrupt_witnessed",         "Witnessed corruption (binary)",
  "Corruption",                "corrupt_national_govt",     "National govt corruption (1\u20134)",
  "Corruption",                "corrupt_local_govt",        "Local govt corruption (1\u20134)",
  "Media & Political Interest","pol_news_follow",           "Follows political news",
  "Media & Political Interest","news_internet",             "Internet news (1\u20136)",
  "Media & Political Interest","political_interest",        "Political interest",
  "Media & Political Interest","pol_discuss",               "Discusses politics"
)

# Build a lookup table: variable → type (prop or mean)
var_types <- ue |> distinct(variable, type)

# Compute Δ W4→W6 for each variable in table3
all_vars <- c(part_gate_meta$variable,
              part_other_meta$variable,
              non_part_meta$variable)

delta_w4w6_vec <- character(length(all_vars))
names(delta_w4w6_vec) <- all_vars

for (v in all_vars) {
  vtype <- var_types$type[var_types$variable == v]
  if (length(vtype) == 0) {
    delta_w4w6_vec[v] <- "\u2014"
    next
  }
  if (vtype[1] == "prop") {
    d <- delta_pct(v, 4, 6)
    delta_w4w6_vec[v] <- fmt_delta_pct(d)
  } else {
    d <- delta_mean(v, 4, 6)
    delta_w4w6_vec[v] <- fmt_delta_mean(d)
  }
}

# Add delta_w4w6_fmt column to table3 by matching label
# table3 rows are ordered: part_gate, part_other, non_part (repeated 4x for each variable)
# But table3 has one row per variable (not per variable×wave), so we match by label
all_labels <- c(
  part_gate_meta$label,
  part_other_meta$label,
  non_part_meta$label
)
all_var_names <- c(
  part_gate_meta$variable,
  part_other_meta$variable,
  non_part_meta$variable
)
label_to_delta <- setNames(delta_w4w6_vec, all_labels)

# Match on label (strip unicode for matching)
table3_updated <- table3 |>
  mutate(
    delta_w4w6_fmt = label_to_delta[label]
  ) |>
  relocate(delta_w4w6_fmt, .after = delta_fmt)

# Rename existing delta column to make clear it's W3→W6
table3_updated <- table3_updated |>
  rename(delta_w3w6_fmt = delta_fmt)

cat("\nSpot check (first 6 rows):\n")
table3_updated |>
  head(6) |>
  select(label, w3_fmt, w6_fmt, delta_w3w6_fmt, delta_w4w6_fmt) |>
  print()

# Print Δ W4→W6 for Democratic Expectations variables specifically
cat("\nDelta W4→W6 for Democratic Expectations vars:\n")
dem_rows <- table3_updated |>
  filter(label %in% c("Democratic future (10pt)", "Democratic past (10pt)",
                       "Democratic present (10pt)")) |>
  select(label, delta_w3w6_fmt, delta_w4w6_fmt)
print(dem_rows)

saveRDS(table3_updated, file.path(tbl_dir, "table3_four_wave_trajectory.rds"))
cat("\nSaved updated table3_four_wave_trajectory.rds with Δ W4→W6 column\n")

# ── OUTPUT CHECKLIST ───────────────────────────────────────────────────────────
cat("\n=== OUTPUT CHECKLIST ===\n")
files_out <- c(
  "analysis/figures/fig1_trend_panels.pdf",
  "analysis/figures/fig1_trend_panels.png",
  "analysis/tables/table3_four_wave_trajectory.rds"
)
for (f in files_out) {
  path   <- file.path(paper_dir, f)
  status <- if (file.exists(path)) sprintf("OK  (%.1f KB)", file.size(path)/1024) else "MISSING"
  cat(sprintf("  %-50s %s\n", f, status))
}
cat("\nDONE.\n")
