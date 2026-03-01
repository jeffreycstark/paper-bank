# =============================================================================
# SCALE DIAGNOSTIC: How big is the Korea-Taiwan divergence, item by item?
#
# Puts EVERY econ → DV coefficient on the same scale so you can see:
#   1. Where the divergence is huge vs trivial
#   2. Whether Korea's "decoupling" is really across-the-board or item-specific
#   3. The actual magnitude in interpretable units
#
# Output: A single summary table + a coefficient plot
# =============================================================================

library(tidyverse)
library(broom)

# --- Setup ---
project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03_south_korea_decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")
fig_dir      <- file.path(paper_dir, "analysis/figures")

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

# Load the diagnostic-augmented data (or rebuild if needed)
# If you ran taiwan_diagnostic.R, it saved to analysis_data.rds with auth vars
# Otherwise, rebuild here:
dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# --- Rebuild auth rejection vars if missing ---
if (!"strongman_reject_n" %in% names(dat)) {
  auth_vars <- c("strongman_rule", "military_rule", "expert_rule", "single_party_rule")
  missing <- setdiff(auth_vars, names(dat))

  if (length(missing) > 0) {
    abs_all <- readRDS(abs_harmonized_path)
    merge_cols <- c("wave", "country")
    if ("row_id" %in% names(dat) & "row_id" %in% names(abs_all)) {
      merge_cols <- c(merge_cols, "row_id")
    } else if ("idnumber" %in% names(dat) & "idnumber" %in% names(abs_all)) {
      merge_cols <- c(merge_cols, "idnumber")
    }
    available <- intersect(missing, names(abs_all))
    if (length(available) > 0) {
      auth_merge <- abs_all |>
        filter(country %in% c(3, 7)) |>
        select(all_of(c(merge_cols, available)))
      dat <- dat |> left_join(auth_merge, by = merge_cols)
    }
  }

  dat <- dat |>
    group_by(country_label) |>
    mutate(
      strongman_reject_n   = normalize_01(max(strongman_rule, na.rm=T) + 1 - strongman_rule),
      military_reject_n    = normalize_01(max(military_rule, na.rm=T) + 1 - military_rule),
      expert_reject_n      = normalize_01(max(expert_rule, na.rm=T) + 1 - expert_rule),
      singleparty_reject_n = normalize_01(max(single_party_rule, na.rm=T) + 1 - single_party_rule),
    ) |> ungroup() |>
    rowwise() |>
    mutate(
      auth_reject_index = mean(c_across(c(strongman_reject_n, military_reject_n,
                                           expert_reject_n, singleparty_reject_n)),
                               na.rm = TRUE)
    ) |> ungroup()
}

# Also normalize alt normative DVs if available
for (v in c("democracy_suitability", "democracy_efficacy", "dem_best_form")) {
  if (v %in% names(dat)) {
    nv <- paste0(gsub("democracy_", "dem_", v), "_n")
    dat <- dat |> group_by(country_label) |>
      mutate(!!nv := normalize_01(.data[[v]])) |> ungroup()
  }
}

# =============================================================================
# THE BIG TABLE: Every DV, both countries, same pooled specification
# =============================================================================

# Define all DVs with labels and conceptual category
dv_list <- tribble(
  ~varname,                ~label,                          ~category, ~order,
  # --- Satisfaction (baseline: expect strong positive both) ---
  "sat_democracy_n",       "Satisfaction: democracy",       "Satisfaction", 1,
  "sat_govt_n",            "Satisfaction: government",       "Satisfaction", 2,

  # --- The headline item ---
  "qual_pref_dem_n",       "Dem always preferable",          "Abstract normative", 3,

  # --- Substantive auth rejection ---
  "auth_reject_index",     "Auth rejection (index)",         "Auth rejection", 4,
  "strongman_reject_n",    "Reject: strongman",              "Auth rejection", 5,
  "military_reject_n",     "Reject: military rule",          "Auth rejection", 6,
  "expert_reject_n",       "Reject: expert rule",            "Auth rejection", 7,
  "singleparty_reject_n",  "Reject: single-party",           "Auth rejection", 8,

  # --- Evaluative ---
  "qual_extent_n",         "Dem extent (current)",           "Evaluative", 9,
  "qual_sys_support_n",    "System deserves support",        "Evaluative", 10,
  "qual_sys_change_n",     "No major change needed",         "Evaluative", 11,
  "sys_proud_n",           "System pride",                   "Evaluative", 12,
)

# Add conditional items if they exist
if ("dem_suitable_n" %in% names(dat))
  dv_list <- bind_rows(dv_list,
    tibble(varname="dem_suitable_n", label="Dem suitable for country",
           category="Conditional pro-dem", order=13))
if ("dem_efficacy_n" %in% names(dat))
  dv_list <- bind_rows(dv_list,
    tibble(varname="dem_efficacy_n", label="Dem can solve problems",
           category="Conditional pro-dem", order=14))
if ("dem_best_form_n" %in% names(dat))
  dv_list <- bind_rows(dv_list,
    tibble(varname="dem_best_form_n", label="Dem is best form of govt",
           category="Conditional pro-dem", order=15))


# Run all regressions
all_results <- list()

for (i in 1:nrow(dv_list)) {
  v <- dv_list$varname[i]
  if (!v %in% names(dat)) next

  for (cntry in c("Korea", "Taiwan")) {
    sub <- dat |> filter(country_label == cntry, !is.na(.data[[v]]))
    if (nrow(sub) < 200) next

    f <- as.formula(paste(v, "~ econ_index + factor(wave) +", controls))
    m <- lm(f, data = sub)
    coef <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")

    # Also get the SD of the DV for effect size calculation
    dv_sd <- sd(sub[[v]], na.rm = TRUE)
    econ_sd <- sd(sub$econ_index, na.rm = TRUE)

    all_results[[paste(cntry, v)]] <- tibble(
      country = cntry,
      varname = v,
      label = dv_list$label[i],
      category = dv_list$category[i],
      order = dv_list$order[i],
      beta = coef$estimate,
      se = coef$std.error,
      p = coef$p.value,
      ci_lo = coef$conf.low,
      ci_hi = coef$conf.high,
      n = nobs(m),
      r_sq = summary(m)$r.squared,
      dv_sd = dv_sd,
      econ_sd = econ_sd,
      # Effect size: 1-SD change in econ → change in DV, in SD units of DV
      std_beta = (coef$estimate * econ_sd) / dv_sd
    )
  }
}

results <- bind_rows(all_results) |> arrange(order, country)


# =============================================================================
# PRINT THE TABLE
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE COEFFICIENT MAP: econ_index → every DV, pooled with wave FE          ║\n")
cat("║                                                                                 ║\n")
cat("║  β = raw coefficient (0-1 scales)                                               ║\n")
cat("║  std_β = standardized (1-SD econ change → SD units of DV)                       ║\n")
cat("║  Interpretation: std_β of 0.10 = 'small', 0.20 = 'moderate', 0.30+ = 'large'   ║\n")
cat("╚══════════════════════════════════════════════════════════════════════════════════╝\n\n")

# Wide format for side-by-side comparison
wide <- results |>
  select(label, category, order, country, beta, p, std_beta) |>
  pivot_wider(
    names_from = country,
    values_from = c(beta, p, std_beta),
    names_glue = "{country}_{.value}"
  ) |>
  arrange(order) |>
  mutate(
    Korea_sig = case_when(Korea_p < 0.001 ~ "***", Korea_p < 0.01 ~ "**",
                           Korea_p < 0.05 ~ "*", TRUE ~ ""),
    Taiwan_sig = case_when(Taiwan_p < 0.001 ~ "***", Taiwan_p < 0.01 ~ "**",
                            Taiwan_p < 0.05 ~ "*", TRUE ~ ""),
    # Gap: Taiwan β minus Korea β (negative = Taiwan more negative)
    gap = Taiwan_beta - Korea_beta
  )

cat(sprintf("%-28s %-18s │ %8s %-4s %6s │ %8s %-4s %6s │ %8s\n",
            "DV", "Category",
            "Korea β", "", "std_β",
            "Taiwan β", "", "std_β",
            "Gap"))
cat(paste(rep("─", 110), collapse = ""), "\n")

for (i in 1:nrow(wide)) {
  # Print category header
  if (i == 1 || wide$category[i] != wide$category[i-1]) {
    cat(sprintf("\n  [%s]\n", wide$category[i]))
  }

  cat(sprintf("  %-26s │ %8.3f %-4s %6.3f │ %8.3f %-4s %6.3f │ %8.3f\n",
              wide$label[i],
              wide$Korea_beta[i], wide$Korea_sig[i], wide$Korea_std_beta[i],
              wide$Taiwan_beta[i], wide$Taiwan_sig[i], wide$Taiwan_std_beta[i],
              wide$gap[i]))
}

cat(paste(rep("─", 110), collapse = ""), "\n")

# =============================================================================
# THE PUNCHLINE: Where is the gap big vs small?
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  THE SCALE QUESTION: How much of the story is one item?        ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

# Compute share of total divergence attributable to "always preferable"
non_sat <- wide |> filter(category != "Satisfaction")
pref_row <- non_sat |> filter(label == "Dem always preferable")
auth_rows <- non_sat |> filter(category == "Auth rejection")

cat("Gap (Taiwan β − Korea β) on key items:\n\n")
cat(sprintf("  %-35s  gap = %+.3f\n", "Dem always preferable", pref_row$gap))
for (j in 1:nrow(auth_rows)) {
  cat(sprintf("  %-35s  gap = %+.3f\n", auth_rows$label[j], auth_rows$gap[j]))
}

cat(sprintf("\n  Mean gap on auth rejection items:    %+.3f\n",
            mean(auth_rows$gap, na.rm = TRUE)))
cat(sprintf("  Gap on 'always preferable':          %+.3f\n", pref_row$gap))
cat(sprintf("  Ratio (pref gap / auth gap):         %.1fx\n\n",
            abs(pref_row$gap) / abs(mean(auth_rows$gap, na.rm = TRUE))))

cat("  → This ratio tells you how much bigger the headline divergence is\n")
cat("    compared to the substantive items. If it's 5-10x, most of the\n")
cat("    story IS the one cheerleading item.\n")


# =============================================================================
# COEFFICIENT PLOT
# =============================================================================

plot_data <- results |>
  mutate(label = fct_reorder(label, -order))

p <- ggplot(plot_data, aes(x = beta, y = label, color = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi),
                  position = position_dodge(width = 0.5),
                  size = 0.4) +
  scale_color_manual(values = c("Korea" = "#2166AC", "Taiwan" = "#D55E00")) +
  labs(
    title = "Economic evaluations → democratic attitudes: Korea vs Taiwan",
    subtitle = "Pooled OLS with wave FE and standard controls. 95% CIs.",
    x = "β (economic evaluation index → DV, 0-1 scales)",
    y = NULL,
    caption = "Source: ABS Waves 1-6. All variables normalized 0-1."
  ) +
  theme_pub +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "diagnostic_coefficient_map.pdf"),
       p, width = 9, height = 7, dpi = 300)
ggsave(file.path(fig_dir, "diagnostic_coefficient_map.png"),
       p, width = 9, height = 7, dpi = 300)

cat("\n✓ Coefficient plot saved to:\n")
cat("  ", file.path(fig_dir, "diagnostic_coefficient_map.pdf"), "\n")
cat("  ", file.path(fig_dir, "diagnostic_coefficient_map.png"), "\n")

# Save full results
saveRDS(results, file.path(results_dir, "scale_diagnostic_results.rds"))
cat("✓ Full results saved to scale_diagnostic_results.rds\n")
