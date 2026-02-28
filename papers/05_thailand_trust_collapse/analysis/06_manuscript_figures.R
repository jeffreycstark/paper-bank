# 06_manuscript_figures.R
# Thailand Trust Collapse — New Manuscript Figures
#
# Generates five new figures supporting the revised evidence presentation:
#
#   Fig A (fig5): H2 — Thailand military vs government gap
#   Fig B (fig6): Pre-trend reversal with counterfactual projection
#   Fig C (fig7): Thailand vs Philippines coercive institutional trust
#   Fig D (fig8): Democratic expectations vs trust divergence (scissors)
#   Fig E (fig9): Regional military trust decline (slopegraph)
#
# Depends on: thailand_panel.rds and existing results/
# Usage: Rscript papers/05_thailand_trust_collapse/analysis/06_manuscript_figures.R

library(tidyverse)
library(patchwork)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
analysis_dir <- file.path(project_root, "papers/05_thailand_trust_collapse/analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")

set.seed(2025)

# ── Colors and theme (matching existing figures) ──────────────────────────────

source(file.path(project_root, "papers/05_thailand_trust_collapse/R/helpers.R"))

inst_colors <- c(
  "Military"   = "#c0392b",
  "Government" = "#2980b9",
  "Police"     = "#8e44ad"
)

wave_labels <- c(
  "1" = "W1\n(2001-03)",
  "2" = "W2\n(2005-08)",
  "3" = "W3\n(2010-12)",
  "4" = "W4\n(2014-16)",
  "5" = "W5\n(2018-20)",
  "6" = "W6\n(2020-22)"
)

base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "gray92"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, color = "gray40"),
    plot.caption      = element_text(size = 8, color = "gray50", hjust = 0),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(size = 9, lineheight = 1.1)
  )

theme_set(base_theme)

# ── Load data ─────────────────────────────────────────────────────────────────

d           <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))
h4_dem      <- readRDS(file.path(results_dir,  "h4_democratic_expectations.rds"))
h6_pretrend <- readRDS(file.path(results_dir,  "h6_pretrend.rds"))
reg_res     <- readRDS(file.path(results_dir,  "regional_analysis.rds"))

cat("Panel loaded:", nrow(d), "obs across",
    n_distinct(d$country_name), "countries\n")

# ── Compute trust means with SEs (all countries × waves × institutions) ───────

trust_means <- d %>%
  select(country_name, wave_num,
         trust_national_government, trust_military, trust_police) %>%
  pivot_longer(
    cols      = starts_with("trust_"),
    names_to  = "institution",
    values_to = "trust"
  ) %>%
  mutate(institution = case_when(
    institution == "trust_national_government" ~ "Government",
    institution == "trust_military"            ~ "Military",
    institution == "trust_police"              ~ "Police"
  )) %>%
  group_by(country_name, wave_num, institution) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    se_trust   = sd(trust, na.rm = TRUE) / sqrt(sum(!is.na(trust))),
    n          = sum(!is.na(trust)),
    .groups    = "drop"
  )

# ── Rejection-of-military-rule means with SEs ─────────────────────────────────
# military_rule is in d (selected in 00_data_preparation.R)
# Scale: 1 = strongly approve, 4 = strongly disapprove → reverse so higher = more rejection

reject_means <- d %>%
  filter(!is.na(military_rule)) %>%
  mutate(reject_military = 5 - military_rule) %>%
  group_by(country_name, wave_num) %>%
  summarise(
    reject_mean = mean(reject_military, na.rm = TRUE),
    reject_se   = sd(reject_military, na.rm = TRUE) / sqrt(sum(!is.na(reject_military))),
    .groups     = "drop"
  )

# =============================================================================
# FIG A: H2 — Thailand military vs government: the growing gap
# =============================================================================
# Core message: military fell ~1.5x more than government, inconsistent with
# pandemic performance (which predicts the reverse).

thai_milgov <- trust_means %>%
  filter(country_name == "Thailand",
         institution  %in% c("Military", "Government"))

# Wide format for ribbon shading
thai_wide <- thai_milgov %>%
  select(wave_num, institution, mean_trust) %>%
  pivot_wider(names_from = institution, values_from = mean_trust) %>%
  mutate(
    gap_top    = pmax(Military, Government),
    gap_bottom = pmin(Military, Government)
  )

fig_a <- ggplot(thai_milgov,
                aes(x = wave_num, y = mean_trust,
                    color = institution, group = institution)) +
  # Shade the gap between institutions (direction-agnostic)
  geom_ribbon(
    data        = thai_wide,
    aes(x = wave_num, ymin = gap_bottom, ymax = gap_top),
    inherit.aes = FALSE,
    fill        = "#c0392b", alpha = 0.08
  ) +
  # Confidence bands
  geom_ribbon(
    aes(ymin = mean_trust - 1.96 * se_trust,
        ymax = mean_trust + 1.96 * se_trust,
        fill = institution),
    alpha = 0.12
  ) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.2) +
  # Annotation: W6 gap magnitude
  annotate(
    "text",
    x     = 5.85,
    y     = mean(c(thai_wide$Military[thai_wide$wave_num == 6],
                   thai_wide$Government[thai_wide$wave_num == 6])),
    label = "~1.5× gap\n(p < .001)",
    hjust = 0, size = 3.2, color = "#c0392b", fontface = "italic"
  ) +
  # W4 coup marker
  geom_vline(xintercept = 4, linetype = "dotted",
             color = "gray50", linewidth = 0.7) +
  annotate("text", x = 4.08, y = 3.4, label = "2014\ncoup",
           size = 2.8, color = "gray45", hjust = 0) +
  scale_color_manual(values = inst_colors) +
  scale_fill_manual(values  = inst_colors, guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = wave_labels,
                     expand = expansion(mult = c(0.05, 0.12))) +
  scale_y_continuous(limits = c(1.3, 3.6), breaks = seq(1.5, 3.5, 0.5)) +
  labs(
    title    = "A. Thailand: Military Trust Fell 1.5× More Than Government Trust",
    subtitle = "Shaded gap supports H2 (political crisis) over pandemic performance accounts",
    x        = NULL,
    y        = "Mean trust (1–4 scale)",
    caption  = "ABS Waves 1–6; 95% CI bands; OLS three-way interaction p < .001."
  )

ggsave(file.path(fig_dir, "fig5_h2_mil_govt_gap.png"),
       fig_a, width = 8, height = 5, dpi = 300)
cat("Saved: fig5_h2_mil_govt_gap.png\n")

# =============================================================================
# FIG B: Pre-trend reversal — the cliff edge
# =============================================================================
# Core message: Thai military trust was *rising* through W4, then collapsed.
# A dashed counterfactual shows where trust would have gone without the shock.
# This rules out secular decline and points to a discrete political event.

thai_mil_means <- trust_means %>%
  filter(country_name == "Thailand", institution == "Military")

# Pre-trend slope from W1-W4 restricted model
pre_slope_val <- h6_pretrend$mil$tidy %>%
  filter(term == "wave_num") %>%
  pull(estimate)

# Counterfactual: project W1-W4 slope forward from W4 actual mean
w4_actual <- thai_mil_means %>%
  filter(wave_num == 4) %>%
  pull(mean_trust)

counterfactual <- tibble(
  wave_num   = c(4, 5, 6),
  mean_trust = w4_actual + pre_slope_val * (c(4, 5, 6) - 4)
)

# Annotation labels
slope_label  <- paste0("Rising pre-trend\n(β = +",
                        round(abs(pre_slope_val), 3), "/wave)")
w6_drop      <- round(
  thai_mil_means$mean_trust[thai_mil_means$wave_num == 4] -
  thai_mil_means$mean_trust[thai_mil_means$wave_num == 6], 2
)
collapse_label <- paste0("Collapse: −", w6_drop, " pts\n(W5–W6)")

fig_b <- ggplot(thai_mil_means,
                aes(x = wave_num, y = mean_trust)) +
  # Pre-trend / post-trend shading
  annotate("rect", xmin = 0.5, xmax = 4.5,
           ymin = -Inf, ymax = Inf, fill = "#27ae60", alpha = 0.04) +
  annotate("rect", xmin = 4.5, xmax = 6.5,
           ymin = -Inf, ymax = Inf, fill = "#c0392b", alpha = 0.05) +
  # Counterfactual "business as usual" projection
  geom_line(
    data        = counterfactual,
    aes(x = wave_num, y = mean_trust),
    linetype    = "dashed", color = "gray55", linewidth = 1.1,
    inherit.aes = FALSE
  ) +
  # Actual trajectory
  geom_ribbon(
    aes(ymin = mean_trust - 1.96 * se_trust,
        ymax = mean_trust + 1.96 * se_trust),
    fill = "#e67e22", alpha = 0.18
  ) +
  geom_line(color = "#e67e22", linewidth = 1.4) +
  geom_point(color = "#e67e22", size = 3.5) +
  # W4/W5 boundary line
  geom_vline(xintercept = 4.5, linetype = "dotted",
             color = "gray40", linewidth = 0.8) +
  # Protest era annotation
  annotate("text", x = 5.5, y = 3.42,
           label = "2020–21\nProtests", size = 2.9, color = "gray35") +
  # Pre-trend annotation
  annotate("text", x = 2.5, y = 1.65,
           label = slope_label,
           size = 3.1, color = "#27ae60", fontface = "italic") +
  # Collapse annotation
  annotate("text", x = 5.5, y = 1.6,
           label = collapse_label,
           size = 3.1, color = "#c0392b", fontface = "bold") +
  # Counterfactual label
  annotate("text", x = 5.5,
           y = counterfactual$mean_trust[counterfactual$wave_num == 5] + 0.1,
           label = "If pre-2019\ntrend continued",
           size = 2.8, color = "gray50", fontface = "italic") +
  scale_x_continuous(breaks = 1:6, labels = wave_labels) +
  scale_y_continuous(limits = c(1.3, 3.5), breaks = seq(1.5, 3.5, 0.5)) +
  labs(
    title    = "B. Thai Military Trust Was Rising — Until It Wasn't",
    subtitle = "Pre-trend (W1–W4) extrapolated as dashed counterfactual; rules out secular decline",
    x        = NULL,
    y        = "Mean military trust (1–4 scale)",
    caption  = paste0(
      "Pre-trend slope (W1–W4 restricted): β = +", round(abs(pre_slope_val), 3),
      " (p < .001). Full-sample slope: strongly negative. ABS, Thailand only."
    )
  )

ggsave(file.path(fig_dir, "fig6_pretrend_reversal.png"),
       fig_b, width = 8, height = 5, dpi = 300)
cat("Saved: fig6_pretrend_reversal.png\n")

# =============================================================================
# FIG C: Thailand vs Philippines — two divergent coercive trust trajectories
# =============================================================================
# Core message: where state coercion aligned with mass preferences (PH/Duterte),
# military trust held. Where it violated democratic expectations (TH), it collapsed.
# Two-panel design: TH (left) military vs government; PH (right) military vs police.

th_data <- trust_means %>%
  filter(country_name == "Thailand",
         institution  %in% c("Military", "Government"))

ph_data <- trust_means %>%
  filter(country_name == "Philippines",
         institution  %in% c("Military", "Police"))

make_panel <- function(df, country, title_str, shade_start = 3.5) {
  ggplot(df, aes(x = wave_num, y = mean_trust,
                 color = institution, group = institution)) +
    # Duterte/protest era shading
    annotate("rect", xmin = shade_start, xmax = 6.5,
             ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.6) +
    # CI bands
    geom_ribbon(
      aes(ymin = mean_trust - 1.96 * se_trust,
          ymax = mean_trust + 1.96 * se_trust,
          fill = institution),
      alpha = 0.12
    ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = inst_colors) +
    scale_fill_manual(values  = inst_colors, guide = "none") +
    scale_x_continuous(breaks = 1:6, labels = wave_labels) +
    scale_y_continuous(limits = c(1.3, 3.7), breaks = seq(1.5, 3.5, 0.5)) +
    labs(title = title_str, x = NULL,
         y = "Mean trust (1–4 scale)")
}

p_th <- make_panel(th_data, "Thailand",
  "A. Thailand: Military collapses (protests target military-monarchy nexus)") +
  annotate("text", x = 5, y = 3.55,
           label = "Protest era", size = 3, color = "gray40")

p_ph <- make_panel(ph_data, "Philippines",
  "B. Philippines: Coercive trust holds under Duterte (preference alignment)") +
  annotate("text", x = 5, y = 3.55,
           label = "Duterte era", size = 3, color = "gray40")

fig_c <- p_th + p_ph +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "fig7_th_ph_coercive.png"),
       fig_c, width = 12, height = 5.5, dpi = 300)
cat("Saved: fig7_th_ph_coercive.png\n")

# =============================================================================
# FIG D: Scissors — democratic expectations rise as trust falls (H4)
# =============================================================================
# Core message: Thailand is not experiencing generalized cynicism. Citizens
# are applying higher democratic standards (rising rejection of military rule)
# to institutions they perceive as falling short (falling military trust).
# The scissors pattern distinguishes democratic maturation from disengagement.

thai_trust_mil <- trust_means %>%
  filter(country_name == "Thailand", institution == "Military") %>%
  select(wave_num, mean_trust, se_trust) %>%
  rename(value = mean_trust, se = se_trust) %>%
  mutate(measure = "Military trust (declining ↓)")

thai_reject_mil <- reject_means %>%
  filter(country_name == "Thailand") %>%
  select(wave_num, reject_mean, reject_se) %>%
  rename(value = reject_mean, se = reject_se) %>%
  mutate(measure = "Rejection of military rule (rising ↑)")

scissors_data <- bind_rows(thai_trust_mil, thai_reject_mil)

scissors_colors <- c(
  "Military trust (declining ↓)"         = "#c0392b",
  "Rejection of military rule (rising ↑)" = "#2980b9"
)

# Find approximate crossing point for annotation
crossing_wave <- scissors_data %>%
  pivot_wider(id_cols = wave_num, names_from = measure, values_from = value) %>%
  mutate(diff = `Military trust (declining ↓)` -
                `Rejection of military rule (rising ↑)`) %>%
  filter(diff > 0) %>%
  slice_tail(n = 1) %>%
  pull(wave_num)

fig_d <- ggplot(scissors_data,
                aes(x = wave_num, y = value,
                    color = measure, group = measure)) +
  # CI bands
  geom_ribbon(
    aes(ymin = value - 1.96 * se,
        ymax = value + 1.96 * se,
        fill = measure),
    alpha = 0.10
  ) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.2) +
  # Scissors annotation
  annotate("text",
           x     = crossing_wave + 0.5,
           y     = 2.9,
           label = "Scissors:\ntrust ↓, norms ↑",
           size  = 3.2, color = "gray30", fontface = "italic") +
  # W4 marker
  geom_vline(xintercept = 4, linetype = "dotted",
             color = "gray55", linewidth = 0.7) +
  annotate("text", x = 4.1, y = 3.55, label = "2014 coup",
           size = 2.8, color = "gray45", hjust = 0) +
  scale_color_manual(values = scissors_colors) +
  scale_fill_manual(values  = scissors_colors, guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = wave_labels) +
  scale_y_continuous(limits = c(1.5, 3.7), breaks = seq(1.5, 3.5, 0.5)) +
  labs(
    title    = "Democratic Maturation, Not Cynicism: Expectations Rise as Trust Falls",
    subtitle = "Thailand W1–W6: military trust (H1) vs. rejection of military rule (H4). Both 1–4 scale.",
    x        = NULL,
    y        = "Scale value (1–4)",
    caption  = paste0(
      "Rejection of military rule = 5 − military_rule (reversed so higher = more rejection). ",
      "ABS, Thailand only. 95% CI bands."
    )
  )

ggsave(file.path(fig_dir, "fig8_expectations_divergence.png"),
       fig_d, width = 8, height = 5, dpi = 300)
cat("Saved: fig8_expectations_divergence.png\n")

# =============================================================================
# FIG E: Regional military trust decline — slopegraph (W4 → W5 → W6)
# =============================================================================
# Core message: Bangkok (protest epicenter) and Northeast (Thaksin heartland)
# show identical steep declines, demonstrating the collapse transcended the
# Red Shirt / Yellow Shirt divide. The South (royalist stronghold) diverges.

reg_means <- reg_res$means %>%
  filter(!is.na(region))

# Order regions by magnitude of W4 → W6 decline (largest first)
# Select only trust_mil to avoid spurious id columns during pivot
reg_mil_only <- reg_means %>% select(region, wave_num, trust_mil)

region_order <- reg_mil_only %>%
  filter(wave_num %in% c(4, 6)) %>%
  pivot_wider(names_from = wave_num, values_from = trust_mil,
              names_prefix = "w") %>%
  mutate(decline = w4 - w6) %>%
  arrange(desc(decline)) %>%
  pull(region)

# Right-side endpoint labels with decline magnitudes
region_labels <- reg_mil_only %>%
  filter(wave_num %in% c(4, 6)) %>%
  pivot_wider(names_from = wave_num, values_from = trust_mil,
              names_prefix = "w") %>%
  mutate(
    decline = round(w4 - w6, 2),
    label   = paste0(region, "\n(−", decline, ")")
  ) %>%
  select(region, label)

reg_plot <- reg_means %>%
  mutate(
    region           = factor(region, levels = region_order),
    wave_label_short = case_when(
      wave_num == 4 ~ "W4\n(2014–16)",
      wave_num == 5 ~ "W5\n(2018–20)",
      wave_num == 6 ~ "W6\n(2020–22)"
    )
  ) %>%
  left_join(region_labels, by = "region", relationship = "many-to-one")

# Regional color scheme: protest/Thaksin regions vs Southern baseline
region_colors <- c(
  "Bangkok"   = "#c0392b",
  "Northeast" = "#e74c3c",
  "North"     = "#e67e22",
  "Central"   = "#f39c12",
  "South"     = "#7f8c8d"
)

fig_e <- ggplot(reg_plot,
                aes(x = factor(wave_num), y = trust_mil,
                    color = region, group = region)) +
  # Connecting lines
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  # Labels at W6 endpoint
  geom_text(
    data  = reg_plot %>% filter(wave_num == 6),
    aes(label = label),
    hjust = -0.08, size = 3.0,
    show.legend = FALSE
  ) +
  # Reference annotations for Bangkok and Northeast
  annotate("text", x = 0.7, y = 3.18,
           label = "← Protest\nepicenter", size = 2.7,
           color = "#c0392b", hjust = 0) +
  annotate("text", x = 0.7, y = 3.02,
           label = "← Thaksin\nheartland", size = 2.7,
           color = "#e74c3c", hjust = 0) +
  scale_color_manual(values = region_colors) +
  scale_x_discrete(labels = c(
    "4" = "W4\n(2014–16)",
    "5" = "W5\n(2018–20)",
    "6" = "W6\n(2020–22)"
  )) +
  scale_y_continuous(limits = c(1.0, 3.9), breaks = seq(1.0, 3.5, 0.5)) +
  # Expand right margin for labels
  coord_cartesian(clip = "off") +
  theme(
    plot.margin     = margin(r = 100),
    legend.position = "none"
  ) +
  labs(
    title    = "Regional Military Trust: Bangkok and Northeast Converge in Collapse",
    subtitle = "Regions sorted by W4–W6 decline; South (royalist) diverges from protest areas",
    x        = NULL,
    y        = "Mean military trust (1–4 scale)",
    caption  = paste0(
      "Region identifiers available W4–W6 only. N = 3,301 (Thailand, non-missing region). ",
      "Regions ordered by magnitude of W4–W6 decline. ABS, Thailand only."
    )
  )

ggsave(file.path(fig_dir, "fig9_regional_decline.png"),
       fig_e, width = 9, height = 5.5, dpi = 300)
cat("Saved: fig9_regional_decline.png\n")

cat("\n=== All manuscript figures saved to", fig_dir, "===\n")
