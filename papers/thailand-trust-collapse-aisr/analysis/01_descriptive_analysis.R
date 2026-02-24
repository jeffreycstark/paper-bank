# 01_descriptive_analysis.R
# Thailand Trust Collapse — Descriptive Analysis
#
# Trust trajectories, wave-to-wave changes, and the government vs military
# comparison (the "smoking gun" for political crisis mechanism).
#
# Usage: Rscript papers/thailand-trust-collapse/analysis/01_descriptive_analysis.R

library(tidyverse)
library(patchwork)

# ── Setup ─────────────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
analysis_dir <- file.path(project_root, "papers/thailand-trust-collapse/analysis")
fig_dir <- file.path(analysis_dir, "figures")
results_dir <- file.path(analysis_dir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(2025)
theme_set(theme_minimal(base_size = 12))

country_colors <- c(
  "Thailand" = "#e67e22",
  "Philippines" = "#e74c3c",
  "Taiwan" = "#2ecc71"
)

d <- readRDS(file.path(analysis_dir, "thailand_panel.rds"))
cat("Loaded:", nrow(d), "obs\n")

# ── Trust means by country and wave ──────────────────────────────────────────

trust_means <- d %>%
  group_by(country_name, wave_label, wave_num) %>%
  summarise(
    n = n(),
    trust_govt = mean(trust_national_government, na.rm = TRUE),
    trust_govt_se = sd(trust_national_government, na.rm = TRUE) /
                    sqrt(sum(!is.na(trust_national_government))),
    trust_mil = mean(trust_military, na.rm = TRUE),
    trust_mil_se = sd(trust_military, na.rm = TRUE) /
                   sqrt(sum(!is.na(trust_military))),
    trust_political = mean(trust_political, na.rm = TRUE),
    trust_nonpolitical = mean(trust_nonpolitical, na.rm = TRUE),
    dem_sat = mean(democracy_satisfaction, na.rm = TRUE),
    .groups = "drop"
  )

# ── Figure 1: Government trust trajectories ──────────────────────────────────

p_govt <- ggplot(trust_means,
       aes(x = wave_label, y = trust_govt,
           color = country_name, group = country_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = trust_govt - 1.96 * trust_govt_se,
                    ymax = trust_govt + 1.96 * trust_govt_se),
                width = 0.1, linewidth = 0.8) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(limits = c(1.5, 3.5), breaks = seq(1.5, 3.5, 0.5)) +
  labs(
    title = "A. Government Trust (2001-2022)",
    subtitle = "Thailand: From highest to lowest over two decades",
    x = NULL,
    y = "Trust in National Government\n(1 = None, 4 = A great deal)",
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8)
  )

ggsave(file.path(fig_dir, "fig1_govt_trust_trajectories.png"),
       p_govt, width = 8, height = 5, dpi = 300)

# ── Figure 2: Military trust trajectories ────────────────────────────────────

p_mil <- ggplot(trust_means,
       aes(x = wave_label, y = trust_mil,
           color = country_name, group = country_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = trust_mil - 1.96 * trust_mil_se,
                    ymax = trust_mil + 1.96 * trust_mil_se),
                width = 0.1, linewidth = 0.8) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(limits = c(1.5, 3.5), breaks = seq(1.5, 3.5, 0.5)) +
  labs(
    title = "B. Military Trust (2001-2022)",
    subtitle = "Thailand: Military trust collapses even more dramatically",
    x = NULL,
    y = "Trust in Military\n(1 = None, 4 = A great deal)",
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8)
  )

ggsave(file.path(fig_dir, "fig2_mil_trust_trajectories.png"),
       p_mil, width = 8, height = 5, dpi = 300)

# ── Figure 3: Combined panel figure ─────────────────────────────────────────

p_combined <- p_govt + p_mil +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "20-Year Trust Trajectories: Thailand, Philippines, Taiwan (2001-2022)",
    subtitle = "Thailand's institutional trust collapsed; Philippines and Taiwan remained comparatively stable"
  )

ggsave(file.path(fig_dir, "fig3_combined_trajectories.png"),
       p_combined, width = 14, height = 6, dpi = 300)

# ── Figure 4: Bottom-box trends ("none at all") ─────────────────────────────
# Shows % responding "1 = none at all" by wave and country for military and
# government trust. Visualises the distributional collapse in Wave 6.

wave_labels_lookup <- d %>%
  distinct(wave, wave_label) %>%
  arrange(wave)

bottom_box <- d %>%
  filter(!is.na(trust_military) | !is.na(trust_national_government)) %>%
  group_by(country_name, wave) %>%
  summarise(
    none_mil  = mean(trust_military == 1, na.rm = TRUE) * 100,
    none_govt = mean(trust_national_government == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  left_join(wave_labels_lookup, by = "wave") %>%
  pivot_longer(cols = c(none_mil, none_govt),
               names_to = "institution", values_to = "pct") %>%
  mutate(institution = ifelse(institution == "none_mil", "Military", "Government"))

p_bottom_mil <- ggplot(bottom_box %>% filter(institution == "Military"),
       aes(x = wave_label, y = pct,
           color = country_name, group = country_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = 'A. Military Trust: % "None at All"',
    x = NULL,
    y = NULL,
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8)
  )

p_bottom_govt <- ggplot(bottom_box %>% filter(institution == "Government"),
       aes(x = wave_label, y = pct,
           color = country_name, group = country_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = 'B. Government Trust: % "None at All"',
    x = NULL,
    y = NULL,
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8)
  )

p_bottom_combined <- p_bottom_mil + p_bottom_govt +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Share of Respondents Reporting "No Trust at All" in Institutions',
    subtitle = "Thailand's bottom-box share surges in Wave 6 while comparator countries remain stable"
  )

ggsave(file.path(fig_dir, "fig4_bottom_box_trends.png"),
       p_bottom_combined, width = 14, height = 6, dpi = 300)

# Save bottom-box data for inline manuscript use
saveRDS(bottom_box, file.path(results_dir, "bottom_box_trends.rds"))

cat("\nFigure 4 (bottom-box trends) saved.\n")

# ── Institutional breadth: NGOs, local govt, national govt, military ──────────
# Produces institutional_breadth.rds for Online Appendix Table A6 and
# the preference-falsification robustness check in the manuscript body.
# Requires trust_ngo and trust_local_govt in the panel (added in 00_data_preparation.R).

if (all(c("trust_ngo", "trust_local_govt") %in% names(d))) {
  institutional_breadth <- d %>%
    filter(country_name %in% c("Philippines", "Thailand")) %>%
    group_by(country_name, wave) %>%
    summarise(
      trust_ngo       = mean(trust_ngo, na.rm = TRUE),
      trust_local_govt = mean(trust_local_govt, na.rm = TRUE),
      trust_natl_govt  = mean(trust_national_government, na.rm = TRUE),
      trust_military   = mean(trust_military, na.rm = TRUE),
      n_total          = n(),
      .groups = "drop"
    )

  cat("\nInstitutional breadth means (Philippines and Thailand):\n")
  print(institutional_breadth %>%
          mutate(across(where(is.numeric) & !matches("^n_"), ~round(., 2))))

  saveRDS(institutional_breadth, file.path(results_dir, "institutional_breadth.rds"))
  cat("Institutional breadth data saved to results/\n")
} else {
  cat("\nWARNING: trust_ngo or trust_local_govt not found in panel data.\n")
  cat("institutional_breadth.rds NOT updated.\n")
  cat("Ensure 00_data_preparation.R has been re-run after adding these variables.\n")
}

# ── Table 1: Wave-to-wave changes ───────────────────────────────────────────

trust_changes <- trust_means %>%
  arrange(country_name, wave_num) %>%
  group_by(country_name) %>%
  mutate(
    govt_change = trust_govt - lag(trust_govt),
    mil_change = trust_mil - lag(trust_mil)
  ) %>%
  filter(!is.na(govt_change)) %>%
  select(country_name, wave_label, wave_num, trust_govt, trust_mil,
         govt_change, mil_change)

write_csv(trust_changes, file.path(analysis_dir, "tables/tab1_wave_changes.csv"))

cat("\n=== WAVE-TO-WAVE CHANGES ===\n")
print(trust_changes %>% mutate(across(where(is.numeric), ~round(., 3))))

# ── Table 2: Total 20-year change (W1 → W6) ─────────────────────────────────

total_change <- trust_means %>%
  filter(wave_num %in% c(1, 6)) %>%
  select(country_name, wave_num, trust_govt, trust_mil, dem_sat) %>%
  pivot_wider(names_from = wave_num, values_from = c(trust_govt, trust_mil, dem_sat),
              names_sep = "_w") %>%
  mutate(
    govt_change_20yr = trust_govt_w6 - trust_govt_w1,
    mil_change_20yr = trust_mil_w6 - trust_mil_w1,
    mil_govt_ratio = mil_change_20yr / govt_change_20yr
  )

write_csv(total_change, file.path(analysis_dir, "tables/tab2_total_change.csv"))

cat("\n=== 20-YEAR TOTAL CHANGE (W1 → W6) ===\n")
print(total_change %>% mutate(across(where(is.numeric), ~round(., 2))))

# ── The smoking gun: government vs military ──────────────────────────────────

cat("\n=== SMOKING GUN: THAILAND GOVT vs MILITARY ===\n")
thai <- total_change %>% filter(country_name == "Thailand")
cat("Government trust change (W1-W6):", round(thai$govt_change_20yr, 2), "\n")
cat("Military trust change (W1-W6): ", round(thai$mil_change_20yr, 2), "\n")
cat("Military/Government ratio:     ", round(thai$mil_govt_ratio, 1), "x\n")
cat("\nIf COVID/economic performance drove the collapse, government trust should\n")
cat("have fallen more than military trust. Instead, military trust fell",
    round(thai$mil_govt_ratio, 1), "x more.\n")
cat("This points to political crisis (2020-21 protests) as the mechanism.\n")

cat("\nAll figures saved to:", fig_dir, "\n")
cat("All tables saved to:", file.path(analysis_dir, "tables"), "\n")
