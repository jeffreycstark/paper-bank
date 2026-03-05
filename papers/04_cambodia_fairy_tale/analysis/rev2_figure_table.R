#!/usr/bin/env Rscript
# rev2_figure_table.R — Figure 1 update (Task 5) and Table 3 Δ W4→W6 (Task 6)
#
# Task 5: Add dem_best_form and dem_vs_equality to Democratic Orientations panel
#         (split Democratic Expectations into two sub-panels)
# Task 6: Add Δ W4→W6 column to Table 3 alongside existing W3→W6 column

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(tikzDevice)
  library(patchwork)
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

# Short labels for direct annotation (right end of each line)
short_labels <- c(
  "Contacted influential"  = "Influential",
  "Attended demonstration" = "Demonstration",
  "Voted (last election)"  = "Voted",
  "Single-party rule"      = "Single-party",
  "Strongman rule"         = "Strongman",
  "Democratic future"      = "Future",
  "Democratic present"     = "Present",
  "Dem. best form (1-4)"   = "Best form",
  "Dem. vs. equality (1-5)"= "Dem. vs. eq.",
  "Witnessed corruption"   = "Witnessed",
  "Nat'l govt corruption"  = "Nat'l govt",
  "Follows pol. news"      = "Pol. news",
  "Political interest"     = "Interest"
)

# Label data: place at highest point of each series, above the line
label_data <- fig_ci_data |>
  group_by(label, domain) |>
  slice_max(mean_val, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(short = short_labels[label])

# Colors: 13 series
my_colors <- c(
  "Contacted influential"   = "#2166AC",
  "Attended demonstration"  = "#B2182B",
  "Voted (last election)"   = "#4DAF4A",
  "Single-party rule"       = "#F4A582",
  "Strongman rule"          = "#D6604D",
  "Democratic future"       = "#4393C3",
  "Democratic present"      = "#92C5DE",
  "Dem. best form (1-4)"    = "#762A83",
  "Dem. vs. equality (1-5)" = "#9970AB",
  "Witnessed corruption"    = "#1B7837",
  "Nat'l govt corruption"   = "#5AAE61",
  "Follows pol. news"       = "#A6761D",
  "Political interest"      = "#E6AB02"
)

# ── Shared plot builder ──────────────────────────────────────────────────────
make_panel_plot <- function(data, lbl_data, title, subtitle, caption) {
  ggplot(data, aes(x = year, y = mean_val, color = label, group = label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label),
                alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 2017, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
             hjust = -0.08, vjust = -0.2, size = 2.2, color = "grey50") +
    geom_text(data = lbl_data,
              aes(x = year, y = mean_val, label = short),
              vjust = -0.8, hjust = 0.5, size = 2.5, show.legend = FALSE) +
    facet_wrap(~domain, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = c(2008, 2012, 2015, 2021),
                       expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = NULL, caption = caption) +
    guides(color = "none", fill = "none") +
    coord_cartesian(clip = "off") +
    theme_pub +
    theme(
      strip.text    = element_text(size = 9, face = "bold"),
      plot.subtitle = element_text(size = 7.5),
      plot.caption  = element_text(size = 6.5)
    )
}

# ═══════════════════════════════════════════════════════════════════════════════
# OPTION 1: Single figure, direct labels, 2×3 layout (no legend)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n--- Option 1: Single figure with direct labels ---\n")

fig1_direct <- make_panel_plot(
  fig_ci_data, label_data,
  title    = "Political Orientations in Cambodia, 2008\u20132021",
  subtitle = paste0("Participation: % ever engaged. Expectations: 0\u201310 scale. ",
                    "Commitment: raw scales (1\u20134 / 1\u20135). Shading = 95% CI."),
  caption  = paste0(
    "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N \u2248 1,000\u20131,242 per wave).\n",
    "CI: Wilson interval for proportions; t-based for means.")
)

# tikz output for LaTeX-native text
tikz(file.path(fig_dir, "fig1_trend_panels.tex"), width = 11, height = 10,
     standAlone = TRUE, sanitize = TRUE, engine = "luatex")
print(fig1_direct)
dev.off()
# Compile tikz → PDF
system2("lualatex", args = c("-interaction=nonstopmode",
  sprintf("-output-directory=%s", fig_dir),
  file.path(fig_dir, "fig1_trend_panels.tex")),
  stdout = FALSE, stderr = FALSE)
# Also save PNG via ggsave (tikz doesn't do raster)
ggsave(file.path(fig_dir, "fig1_trend_panels.png"), fig1_direct,
       width = 11, height = 10, dpi = 300)
cat("Saved fig1_trend_panels (.tex → .pdf + .png)\n")

# ═══════════════════════════════════════════════════════════════════════════════
# OPTION 3: Two figures — behavioral vs. attitudinal, 2-over-1 layout
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n--- Option 3: Two-figure split (2-over-1) ---\n")

behavioral_domains  <- c("Political Participation", "Authoritarian Preferences", "Corruption")
attitudinal_domains <- c("Democratic Expectations", "Democratic Commitment", "Media & Political Interest")

# ── Single-domain plot builder (no facet, used as patchwork pieces) ───────────
make_single_panel <- function(data, lbl_data, panel_title) {
  ggplot(data, aes(x = year, y = mean_val, color = label, group = label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label),
                alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 2017, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
             hjust = -0.08, vjust = -0.2, size = 2.5, color = "grey50") +
    geom_text(data = lbl_data,
              aes(x = year, y = mean_val, label = short),
              vjust = -0.8, hjust = 0.5, size = 3, show.legend = FALSE) +
    scale_x_continuous(breaks = c(2008, 2012, 2015, 2021),
                       expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    labs(title = panel_title, x = NULL, y = NULL) +
    guides(color = "none", fill = "none") +
    coord_cartesian(clip = "off") +
    theme_pub +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.margin = margin(5, 12, 5, 5)
    )
}

# ── Behavioral figure: Participation + Auth Prefs on top, Corruption below ───
bp1 <- make_single_panel(
  fig_ci_data |> filter(domain == "Political Participation"),
  label_data  |> filter(domain == "Political Participation"),
  "Political Participation"
)
bp2 <- make_single_panel(
  fig_ci_data |> filter(domain == "Authoritarian Preferences"),
  label_data  |> filter(domain == "Authoritarian Preferences"),
  "Authoritarian Preferences"
)
bp3 <- make_single_panel(
  fig_ci_data |> filter(domain == "Corruption"),
  label_data  |> filter(domain == "Corruption"),
  "Corruption"
)

fig_behavioral <- (bp1 | bp2) / bp3 +
  plot_annotation(
    title    = "Behavioral Orientations in Cambodia, 2008\u20132021",
    subtitle = "Participation: % ever engaged. Auth. preferences & corruption: wave means. Shading = 95% CI.",
    caption  = paste0(
      "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N \u2248 1,000\u20131,242 per wave). ",
      "CI: Wilson interval for proportions; t-based for means."),
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 8.5, color = "grey30"),
      plot.caption  = element_text(size = 7, color = "grey50", hjust = 0)
    )
  )

# ── Attitudinal figure: Commitment + Media on top, Expectations solo below ───
ap1 <- make_single_panel(
  fig_ci_data |> filter(domain == "Democratic Commitment"),
  label_data  |> filter(domain == "Democratic Commitment"),
  "Democratic Commitment"
)
ap2 <- make_single_panel(
  fig_ci_data |> filter(domain == "Media & Political Interest"),
  label_data  |> filter(domain == "Media & Political Interest"),
  "Media & Political Interest"
)
ap3 <- make_single_panel(
  fig_ci_data |> filter(domain == "Democratic Expectations"),
  label_data  |> filter(domain == "Democratic Expectations"),
  "Democratic Expectations"
)

fig_attitudinal <- (ap1 | ap2) / ap3 +
  plot_annotation(
    title    = "Attitudinal Orientations in Cambodia, 2008\u20132021",
    subtitle = paste0("Expectations: 0\u201310 scale. Commitment: raw scales (1\u20134 / 1\u20135). ",
                      "Media & interest: wave means. Shading = 95% CI."),
    caption  = paste0(
      "Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N \u2248 1,000\u20131,242 per wave). ",
      "CI: Wilson interval for proportions; t-based for means."),
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 8.5, color = "grey30"),
      plot.caption  = element_text(size = 7, color = "grey50", hjust = 0)
    )
  )

# ggsave for behavioral figure (PDF + PNG)
ggsave(file.path(fig_dir, "fig_behavioral.pdf"), fig_behavioral,
       width = 7.5, height = 8, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig_behavioral.png"), fig_behavioral,
       width = 7.5, height = 8, dpi = 300)

# ggsave for attitudinal figure (PDF + PNG)
ggsave(file.path(fig_dir, "fig_attitudinal.pdf"), fig_attitudinal,
       width = 7.5, height = 8, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig_attitudinal.png"), fig_attitudinal,
       width = 7.5, height = 8, dpi = 300)
cat("Saved fig_behavioral + fig_attitudinal (.pdf + .png)\n")

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
# Handle both column naming conventions (delta_fmt from first run, delta_w3w6_fmt from rerun)
delta_col <- if ("delta_fmt" %in% names(table3)) "delta_fmt" else "delta_w3w6_fmt"

table3_updated <- table3 |>
  mutate(
    delta_w4w6_fmt = label_to_delta[label]
  )

# Rename to delta_w3w6_fmt if it hasn't been renamed yet
if ("delta_fmt" %in% names(table3_updated)) {
  table3_updated <- table3_updated |>
    rename(delta_w3w6_fmt = delta_fmt) |>
    relocate(delta_w4w6_fmt, .after = delta_w3w6_fmt)
} else {
  table3_updated <- table3_updated |>
    relocate(delta_w4w6_fmt, .after = delta_w3w6_fmt)
}

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
  "analysis/figures/fig_behavioral.pdf",
  "analysis/figures/fig_behavioral.png",
  "analysis/figures/fig_attitudinal.pdf",
  "analysis/figures/fig_attitudinal.png",
  "analysis/tables/table3_four_wave_trajectory.rds"
)
for (f in files_out) {
  path   <- file.path(paper_dir, f)
  status <- if (file.exists(path)) sprintf("OK  (%.1f KB)", file.size(path)/1024) else "MISSING"
  cat(sprintf("  %-50s %s\n", f, status))
}
cat("\nDONE.\n")
