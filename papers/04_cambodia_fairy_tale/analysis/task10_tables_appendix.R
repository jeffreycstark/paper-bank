#!/usr/bin/env Rscript
# task10_tables_appendix.R — Task 10 implementation
# 10a: Rebuild Tables 1–3 replacing per-cell N with 95% CI
# 10d: Build Table A2 (subgroup splits)
# 10e: Build Table A3 (placebo battery with SE)
# 10f: Extend inline_stats.rds with diagnostic values

suppressPackageStartupMessages({
  library(tidyverse)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/04_cambodia_fairy_tale")
results_dir  <- file.path(paper_dir, "analysis/results")
tbl_dir      <- file.path(paper_dir, "analysis/tables")
rr_dir       <- file.path(paper_dir, "analysis/reviewer_response")

# ── Load source data ───────────────────────────────────────────────────────────
cat("Loading data...\n")
ue <- readRDS(file.path(rr_dir, "uncertainty_estimates.rds"))
sg <- readRDS(file.path(rr_dir, "subgroup_splits.rds"))
pb <- read.csv(file.path(rr_dir, "placebo_battery.csv"), stringsAsFactors = FALSE)

rs_raw <- read.csv(file.path(rr_dir, "response_style_diagnostics.csv"),
                   stringsAsFactors = FALSE)
rs <- rs_raw |> filter(table == "wave_summary")

inline_stats <- readRDS(file.path(results_dir, "inline_stats.rds"))

cat(sprintf("  ue:  %d rows | sg: %d rows | pb: %d rows | rs: %d rows\n",
            nrow(ue), nrow(sg), nrow(pb), nrow(rs)))

# ── CI helpers ─────────────────────────────────────────────────────────────────

# Look up a CI row for variable + wave
get_ue <- function(v, w) {
  row <- ue[ue$variable == v & ue$wave == w, ]
  if (nrow(row) == 0 || is.na(row$estimate[1])) return(NULL)
  row[1, ]
}

# "52.6\% (48.0--57.1)" — prop input on 0-1 scale, ×100 for display
# Vectorized (works in both scalar and dplyr mutate contexts)
fmt_pct_ci <- function(est, lo, hi) {
  ifelse(is.na(est), "---",
         sprintf("%.1f\\%% (%.1f--%.1f)", est * 100, lo * 100, hi * 100))
}

# "1.97 (1.88--2.06)" — raw mean scale
fmt_mean_ci <- function(est, lo, hi, digits = 2) {
  f <- paste0("%.", digits, "f")
  ifelse(is.na(est), "\u2014",
         sprintf(paste0(f, " (", f, "--", f, ")"), est, lo, hi))
}

# Dispatch by variable type
ci_cell <- function(v, w) {
  row <- get_ue(v, w)
  if (is.null(row)) {
    # Check if variable exists but was not collected this wave
    typ_check <- ue$type[ue$variable == v]
    if (length(typ_check) == 0) return("\u2014")
    return(if (typ_check[1] == "prop") "---" else "\u2014")
  }
  if (row$type == "prop") fmt_pct_ci(row$estimate, row$ci_lower, row$ci_upper)
  else                    fmt_mean_ci(row$estimate, row$ci_lower, row$ci_upper)
}

# Delta for proportions (percentage-point difference)
delta_pct <- function(v, wa, wb) {
  a <- get_ue(v, wa); b <- get_ue(v, wb)
  if (is.null(a) || is.null(b)) return(NA_real_)
  round((b$estimate - a$estimate) * 100, 1)
}

# Delta for means (arithmetic difference)
delta_mean <- function(v, wa, wb) {
  a <- get_ue(v, wa); b <- get_ue(v, wb)
  if (is.null(a) || is.null(b)) return(NA_real_)
  round(b$estimate - a$estimate, 2)
}

fmt_delta_pct <- function(d) {
  if (is.na(d)) "\u2014"
  else if (d > 0) sprintf("+%.1f pp", d)
  else sprintf("%.1f pp", d)
}
fmt_delta_mean <- function(d) {
  if (is.na(d)) "\u2014"
  else if (d > 0) sprintf("+%.2f", d)
  else sprintf("%.2f", d)
}

# ── Variable metadata (mirrors build_tables_figures.R) ────────────────────────
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

# ─────────────────────────────────────────────────────────────────────────────
# 10a. Rebuild Table 1 (W2 baseline)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10a. Rebuilding Table 1 (W2 baseline with CI) ===\n")

make_gate_row_t1 <- function(v, label) {
  tibble(domain = "Political Participation (% ever)", label = label,
         w2_fmt = ci_cell(v, 2))
}
make_other_row_t1 <- function(v, label, dom = "Political Participation") {
  tibble(domain = dom, label = label, w2_fmt = ci_cell(v, 2))
}

table1_ci <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_gate_row_t1(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t1(..1, ..2)),
  pmap_dfr(non_part_meta,   ~ make_other_row_t1(..2, ..3, ..1))
)

saveRDS(table1_ci, file.path(tbl_dir, "table1_w2_baseline.rds"))
cat("Saved table1_w2_baseline.rds (CI format)\n")

# ─────────────────────────────────────────────────────────────────────────────
# 10a. Rebuild Table 2 (W3→W4 comparison)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10a. Rebuilding Table 2 (W3\u2192W4 with CI) ===\n")

make_gate_row_t2 <- function(v, label) {
  d <- delta_pct(v, 3, 4)
  tibble(domain = "Political Participation (% ever)", label = label,
         w3_fmt = ci_cell(v, 3), w4_fmt = ci_cell(v, 4),
         delta_fmt = fmt_delta_pct(d))
}
make_other_row_t2 <- function(v, label, dom = "Political Participation") {
  row_ue <- ue[ue$variable == v, ]
  is_prop <- if (nrow(row_ue) > 0) row_ue$type[1] == "prop" else FALSE
  d <- if (is_prop) delta_pct(v, 3, 4) else delta_mean(v, 3, 4)
  tibble(domain = dom, label = label,
         w3_fmt = ci_cell(v, 3), w4_fmt = ci_cell(v, 4),
         delta_fmt = if (is_prop) fmt_delta_pct(d) else fmt_delta_mean(d))
}

table2_ci <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_gate_row_t2(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t2(..1, ..2)),
  pmap_dfr(non_part_meta,   ~ make_other_row_t2(..2, ..3, ..1))
)

saveRDS(table2_ci, file.path(tbl_dir, "table2_w3w4_comparison.rds"))
cat("Saved table2_w3w4_comparison.rds (CI format)\n")

# ─────────────────────────────────────────────────────────────────────────────
# 10a. Rebuild Table 3 (four-wave trajectory)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10a. Rebuilding Table 3 (four-wave with CI) ===\n")

make_gate_row_t3 <- function(v, label) {
  d <- delta_pct(v, 3, 6)
  tibble(domain = "Political Participation (% ever)", label = label,
         w2_fmt = ci_cell(v, 2), w3_fmt = ci_cell(v, 3),
         w4_fmt = ci_cell(v, 4), w6_fmt = ci_cell(v, 6),
         delta_fmt = fmt_delta_pct(d))
}
make_other_row_t3 <- function(v, label, dom = "Political Participation") {
  row_ue <- ue[ue$variable == v, ]
  is_prop <- if (nrow(row_ue) > 0) row_ue$type[1] == "prop" else FALSE
  d <- if (is_prop) delta_pct(v, 3, 6) else delta_mean(v, 3, 6)
  tibble(domain = dom, label = label,
         w2_fmt = ci_cell(v, 2), w3_fmt = ci_cell(v, 3),
         w4_fmt = ci_cell(v, 4), w6_fmt = ci_cell(v, 6),
         delta_fmt = if (is_prop) fmt_delta_pct(d) else fmt_delta_mean(d))
}

table3_ci <- bind_rows(
  pmap_dfr(part_gate_meta,  ~ make_gate_row_t3(..1, ..2)),
  pmap_dfr(part_other_meta, ~ make_other_row_t3(..1, ..2)),
  pmap_dfr(non_part_meta,   ~ make_other_row_t3(..2, ..3, ..1))
)

saveRDS(table3_ci, file.path(tbl_dir, "table3_four_wave_trajectory.rds"))
cat("Saved table3_four_wave_trajectory.rds (CI format)\n")

# Spot-check
cat("\nSpot-check (first 3 rows of Table 3):\n")
print(table3_ci |> head(3) |> select(label, w3_fmt, w6_fmt, delta_fmt))

# ─────────────────────────────────────────────────────────────────────────────
# 10d. Table A2: Subgroup splits
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10d. Building Table A2 (subgroup splits) ===\n")

wave_labels <- c(`2`="Wave 2 (2008)", `3`="Wave 3 (2012)",
                 `4`="Wave 4 (2015)", `6`="Wave 6 (2021)")

# Key variable order matching the subgroup spec in reviewer_response.R
var_order <- c("gate_contact_influential", "dem_country_future",
               "corrupt_witnessed", "single_party_rule",
               "voted_last_election", "political_interest")

make_A2_panel <- function(sg_name) {
  sg_sub <- sg |>
    filter(subgroup_type == sg_name,
           variable %in% var_order) |>
    mutate(
      wave_label = wave_labels[as.character(wave)],
      cell_fmt = case_when(
        type == "prop" ~ fmt_pct_ci(estimate, ci_lower, ci_upper),
        TRUE           ~ fmt_mean_ci(estimate, ci_lower, ci_upper)
      ),
      var_order_idx = match(variable, var_order)
    ) |>
    arrange(var_order_idx, wave) |>
    select(label, wave, wave_label, subgroup_value, cell_fmt)

  # Determine column order for this subgroup type
  col_levels <- switch(sg_name,
    urban_rural = c("Urban", "Rural"),
    age         = c("Under 30", "30-49", "50+"),
    education   = c("Primary or below", "Secondary", "Tertiary")
  )

  wide <- sg_sub |>
    pivot_wider(names_from = subgroup_value, values_from = cell_fmt,
                values_fill = "\u2014") |>
    select(label, wave, wave_label, any_of(col_levels)) |>
    select(-wave)  # keep wave_label for display

  wide
}

tableA2_urban  <- make_A2_panel("urban_rural")
tableA2_age    <- make_A2_panel("age")
tableA2_edu    <- make_A2_panel("education")

saveRDS(list(urban_rural = tableA2_urban,
             age         = tableA2_age,
             education   = tableA2_edu),
        file.path(tbl_dir, "tableA2_subgroups.rds"))
cat("Saved tableA2_subgroups.rds\n")

# Spot-check: gate_influential × age
cat("\nSpot-check: gate_influential × age group:\n")
tableA2_age |>
  filter(label == "Gate influential") |>
  print()

# ─────────────────────────────────────────────────────────────────────────────
# 10e. Table A3: Placebo battery
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10e. Building Table A3 (placebo battery) ===\n")

# Placebo variable order (same as reviewer_response.R)
pb_order <- c("trust_generalized_binary", "trust_generalized_ordinal",
              "nat_proud_citizen", "system_proud", "hh_income_sat",
              "econ_family_now", "econ_family_change", "econ_national_now",
              "democracy_satisfaction")

tableA3 <- pb |>
  mutate(
    wave_label = c(`2`="W2 (2008)", `3`="W3 (2012)",
                   `4`="W4 (2015)", `6`="W6 (2021)")[as.character(wave)],
    # Format: estimate (SE) — proportions shown as %, means as raw
    cell_fmt = case_when(
      type == "prop" ~ sprintf("%.1f\\%% (%.2f)", estimate * 100, se * 100),
      TRUE           ~ sprintf("%.2f (%.3f)", estimate, se)
    ),
    pb_order_idx = match(variable, pb_order)
  ) |>
  filter(!is.na(pb_order_idx)) |>
  arrange(pb_order_idx, wave) |>
  select(label, wave_label, cell_fmt) |>
  pivot_wider(names_from = wave_label, values_from = cell_fmt,
              values_fill = "---")

# Ensure correct column order
col_order <- c("label", "W2 (2008)", "W3 (2012)", "W4 (2015)", "W6 (2021)")
tableA3 <- tableA3 |>
  select(any_of(col_order))

saveRDS(tableA3, file.path(tbl_dir, "tableA3_placebo.rds"))
cat("Saved tableA3_placebo.rds\n")
cat("\nPlacebo table (first 5 rows):\n")
print(tableA3 |> head(5))

# ─────────────────────────────────────────────────────────────────────────────
# 10f. Extend inline_stats with diagnostic values
# ─────────────────────────────────────────────────────────────────────────────
cat("\n=== 10f. Extending inline_stats ===\n")

# ── Response-style diagnostics ─────────────────────────────────────────────
rs_w3 <- rs |> filter(wave == 3)
rs_w4 <- rs |> filter(wave == 4)
rs_w6 <- rs |> filter(wave == 6)

# Item nonresponse rates (×100 = percentage)
inline_stats$na_dem_future_w3        <- rs_w3$na_dem_future * 100
inline_stats$na_dem_future_w6        <- rs_w6$na_dem_future * 100
inline_stats$na_dem_past_w3          <- rs_w3$na_dem_past   * 100
inline_stats$na_dem_past_w6          <- rs_w6$na_dem_past   * 100
inline_stats$mean_na_political_w3    <- rs_w3$mean_na_all_political * 100
inline_stats$mean_na_political_w6    <- rs_w6$mean_na_all_political * 100
inline_stats$straightline_w4         <- rs_w4$straightline_rate * 100
inline_stats$straightline_w6         <- rs_w6$straightline_rate * 100

cat(sprintf("  na_dem_future:      W3=%.0f%%  W6=%.0f%%\n",
            inline_stats$na_dem_future_w3, inline_stats$na_dem_future_w6))
cat(sprintf("  na_dem_past:        W3=%.0f%%  W6=%.0f%%\n",
            inline_stats$na_dem_past_w3, inline_stats$na_dem_past_w6))
cat(sprintf("  mean_na_political:  W3=%.0f%%  W6=%.0f%%\n",
            inline_stats$mean_na_political_w3, inline_stats$mean_na_political_w6))
cat(sprintf("  straightline:       W4=%.0f%%  W6=%.0f%%\n",
            inline_stats$straightline_w4, inline_stats$straightline_w6))

# ── Placebo battery values ─────────────────────────────────────────────────
get_pb <- function(var, wv) {
  row <- pb[pb$variable == var & pb$wave == wv, ]
  if (nrow(row) == 0) return(NA_real_)
  row$estimate[1]
}

# National pride (mean, 1–4)
inline_stats$placebo_natpride_w3 <- get_pb("nat_proud_citizen", 3)
inline_stats$placebo_natpride_w4 <- get_pb("nat_proud_citizen", 4)
inline_stats$placebo_natpride_w6 <- get_pb("nat_proud_citizen", 6)

# Family economic situation (mean, 1–5)
inline_stats$placebo_econFamily_w3 <- get_pb("econ_family_now", 3)
inline_stats$placebo_econFamily_w4 <- get_pb("econ_family_now", 4)
inline_stats$placebo_econFamily_w6 <- get_pb("econ_family_now", 6)

# Economic change 1yr (mean, 1–5)
inline_stats$placebo_econChange_w3 <- get_pb("econ_family_change", 3)
inline_stats$placebo_econChange_w4 <- get_pb("econ_family_change", 4)
inline_stats$placebo_econChange_w6 <- get_pb("econ_family_change", 6)

# Interpersonal trust binary (prop 0-1, ×100 = %)
inline_stats$placebo_trust_binary_w4 <- get_pb("trust_generalized_binary", 4) * 100
inline_stats$placebo_trust_binary_w6 <- get_pb("trust_generalized_binary", 6) * 100

cat(sprintf("  natpride:     W3=%.2f  W4=%.2f  W6=%.2f\n",
            inline_stats$placebo_natpride_w3,
            inline_stats$placebo_natpride_w4,
            inline_stats$placebo_natpride_w6))
cat(sprintf("  econFamily:   W3=%.2f  W4=%.2f  W6=%.2f\n",
            inline_stats$placebo_econFamily_w3,
            inline_stats$placebo_econFamily_w4,
            inline_stats$placebo_econFamily_w6))
cat(sprintf("  econChange:   W3=%.2f  W4=%.2f  W6=%.2f\n",
            inline_stats$placebo_econChange_w3,
            inline_stats$placebo_econChange_w4,
            inline_stats$placebo_econChange_w6))
cat(sprintf("  trust_binary: W4=%.1f%%  W6=%.1f%%\n",
            inline_stats$placebo_trust_binary_w4,
            inline_stats$placebo_trust_binary_w6))

# ── Subgroup: gate_influential × age × waves 4 and 6 ──────────────────────
sg_inf_age <- sg |>
  filter(variable == "gate_contact_influential", subgroup_type == "age")

get_sg <- function(wv, grp) {
  row <- sg_inf_age[sg_inf_age$wave == wv & sg_inf_age$subgroup_value == grp, ]
  if (nrow(row) == 0) return(NA_real_)
  row$estimate[1] * 100  # prop → %
}

inline_stats$sg_influential_under30_w4 <- get_sg(4, "Under 30")
inline_stats$sg_influential_under30_w6 <- get_sg(6, "Under 30")
inline_stats$sg_influential_3049_w4    <- get_sg(4, "30-49")
inline_stats$sg_influential_3049_w6    <- get_sg(6, "30-49")
inline_stats$sg_influential_50plus_w4  <- get_sg(4, "50+")
inline_stats$sg_influential_50plus_w6  <- get_sg(6, "50+")

cat(sprintf("  gate_influential under30: W4=%.1f%%  W6=%.1f%%\n",
            inline_stats$sg_influential_under30_w4,
            inline_stats$sg_influential_under30_w6))
cat(sprintf("  gate_influential 30-49:   W4=%.1f%%  W6=%.1f%%\n",
            inline_stats$sg_influential_3049_w4,
            inline_stats$sg_influential_3049_w6))
cat(sprintf("  gate_influential 50+:     W4=%.1f%%  W6=%.1f%%\n",
            inline_stats$sg_influential_50plus_w4,
            inline_stats$sg_influential_50plus_w6))

saveRDS(inline_stats, file.path(results_dir, "inline_stats.rds"))
cat(sprintf("\nSaved inline_stats.rds  (%d keys)\n", length(inline_stats)))

# ── Summary checklist ─────────────────────────────────────────────────────────
cat("\n=== OUTPUT CHECKLIST ===\n")
files_out <- c(
  "analysis/tables/table1_w2_baseline.rds",
  "analysis/tables/table2_w3w4_comparison.rds",
  "analysis/tables/table3_four_wave_trajectory.rds",
  "analysis/tables/tableA2_subgroups.rds",
  "analysis/tables/tableA3_placebo.rds",
  "analysis/results/inline_stats.rds"
)
for (f in files_out) {
  path   <- file.path(paper_dir, f)
  status <- if (file.exists(path)) sprintf("OK  (%s KB)", round(file.size(path)/1024, 1)) else "MISSING"
  cat(sprintf("  %-50s %s\n", f, status))
}
cat("\nDONE.\n")
