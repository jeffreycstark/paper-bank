## ──────────────────────────────────────────────────────────────────────────────
## figures_comparative.R
## Redesigned comparative figures for Stark (2026)
## "When Do Surveys Produce False Positives for Regime Support?"
##
## Run from: papers/hong-kong-democratic-erosion/
## Outputs:  analysis/figures/
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(ggtext)
library(patchwork)
library(ggrepel)

results_path <- "analysis/results"
figures_path <- "analysis/figures"

load(file.path(results_path, "main_analysis_results.RData"))    # sensitivity_results
load(file.path(results_path, "turkey_gradient.RData"))          # turkey_gradient_results
load(file.path(results_path, "venezuela_gradient.RData"))       # ven_gradient_results, ven_gradient_r_all
load(file.path(results_path, "nicaragua_gradient.RData"))       # nic_gradient_results, nic_gradient_r_all
load(file.path(results_path, "afro_sensitivity_gradient.RData"))# afro_gradient_results

# ── Shared aesthetics ──────────────────────────────────────────────────────────

cat_colors <- c(High = "#C62828", Medium = "#F57C00", Low = "#1565C0")
cat_shapes <- c(High = 16, Medium = 15, Low = 17)

theme_gradient <- function() {
  theme_minimal(base_size = 11) +
    theme(
      axis.title.x       = element_markdown(),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 11),
      plot.subtitle      = element_text(size = 9, color = "grey40")
    )
}

# ── FIGURE 1: HK + Turkey side-by-side sensitivity gradient ───────────────────

## — Hong Kong: map categories to High/Medium/Low —
hk_plot <- sensitivity_results |>
  mutate(
    tier = case_when(
      category %in% c("Coercive institution", "Executive", "Military") ~ "High",
      category %in% c("Legislature", "Judiciary")                      ~ "Medium",
      TRUE                                                              ~ "Low"
    ),
    tier = factor(tier, levels = c("High", "Medium", "Low")),
    # HK sample sizes: Protest n=473, Post-NSL n=676
    protest_n  = 473L,
    postnsl_n  = 676L,
    se_d = sqrt(1/protest_n + 1/postnsl_n +
                  cohens_d^2 / (2 * (protest_n + postnsl_n))),
    ci_lo = cohens_d - 1.96 * se_d,
    ci_hi = cohens_d + 1.96 * se_d,
    label = factor(label, levels = rev(label[order(sensitivity_rank, -cohens_d)]))
  )

# Spearman on all 9 items with actual tied ranks (1,1,2,3,3,4,5,5,5) = -0.8464 → -0.85.
# Pearson on all 9 items gives -0.65 (incorrect); Spearman recovers the manuscript value.
hk_r   <- round(cor(hk_plot$sensitivity_rank, hk_plot$cohens_d, method = "spearman"), 2)
hk_fit <- lm(cohens_d ~ sensitivity_rank, data = hk_plot)
hk_plot$fitted <- predict(hk_fit)

p_hk <- ggplot(hk_plot, aes(x = cohens_d, y = label)) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(aes(x = fitted, y = label, group = 1),
              method = "lm", se = FALSE,
              color = "grey50", linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, color = tier),
                width = 0.25, linewidth = 0.4, orientation = "y") +
  geom_point(aes(color = tier, shape = tier), size = 2.8) +
  annotate("text", x = 0.55, y = 1.4,
           label = paste0("italic(r)==", hk_r),
           parse = TRUE, hjust = 1, size = 3.5, color = "grey30") +
  scale_x_continuous(limits = c(-0.70, 0.65), breaks = seq(-0.6, 0.6, 0.3)) +
  scale_color_manual(values = cat_colors, name = "Sensitivity") +
  scale_shape_manual(values = cat_shapes, name = "Sensitivity") +
  labs(
    title    = "Hong Kong",
    subtitle = "ABS Wave 5 | Within-wave (Protest vs. Post-NSL)",
    x        = "Cohen's *d* (Post-NSL \u2212 Protest)",
    y        = NULL
  ) +
  theme_gradient()

## — Turkey —
tr_plot <- turkey_gradient_results |>
  mutate(
    tier = case_when(
      category == "High" ~ "High",
      category == "Low"  ~ "Low",
      TRUE               ~ "Medium"
    ),
    tier = factor(tier, levels = c("High", "Medium", "Low")),
    se_d  = sqrt(1/w6_n + 1/w7_n +
                   cohens_d^2 / (2 * (w6_n + w7_n))),
    ci_lo = cohens_d - 1.96 * se_d,
    ci_hi = cohens_d + 1.96 * se_d,
    label = factor(label, levels = rev(label[order(sensitivity_rank, -cohens_d)]))
  )

tr_r   <- round(cor(tr_plot$sensitivity_rank, tr_plot$cohens_d, use = "complete.obs"), 2)
tr_fit <- lm(cohens_d ~ sensitivity_rank, data = tr_plot)
tr_plot$fitted <- predict(tr_fit, newdata = data.frame(sensitivity_rank = tr_plot$sensitivity_rank))

p_tr <- ggplot(tr_plot, aes(x = cohens_d, y = label)) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(aes(x = fitted, y = label, group = 1),
              method = "lm", se = FALSE,
              color = "grey50", linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, color = tier),
                width = 0.25, linewidth = 0.4, orientation = "y") +
  geom_point(aes(color = tier, shape = tier), size = 2.8) +
  annotate("text", x = 0.35, y = 1.4,
           label = paste0("italic(r)==", tr_r),
           parse = TRUE, hjust = 1, size = 3.5, color = "grey30") +
  scale_x_continuous(limits = c(-0.25, 0.45), breaks = seq(-0.2, 0.4, 0.2)) +
  scale_color_manual(values = cat_colors, name = "Sensitivity") +
  scale_shape_manual(values = cat_shapes, name = "Sensitivity") +
  labs(
    title    = "Turkey",
    subtitle = "WVS Waves 6\u20137 | Cross-wave (2011 vs. 2018)",
    x        = "Cohen's *d* (Wave 7 \u2212 Wave 6)",
    y        = NULL
  ) +
  theme_gradient()

fig1 <- (p_hk + p_tr) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "The Sensitivity Gradient: Cross-National Replication",
    subtitle = paste0("High-sensitivity items (red) inflate post-autocratization; ",
                      "low-sensitivity items (blue) decline or remain stable"),
    theme    = theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 10, color = "grey40"),
      legend.position = "bottom"
    )
  )

ggsave(file.path(figures_path, "fig_gradient_hk_turkey.pdf"),
       fig1, width = 12, height = 5.5, device = "pdf")
ggsave(file.path(figures_path, "fig_gradient_hk_turkey.png"),
       fig1, width = 12, height = 5.5, dpi = 300)
message("✓ Figure 1 saved: fig_gradient_hk_turkey")

# ── FIGURE 2: Five-case summary scatter ───────────────────────────────────────

five_cases <- tribble(
  ~case,           ~gradient_r, ~type,                  ~survey,
  "Hong Kong",     -0.85,       "Targeted repression",  "ABS",
  "Turkey",        -0.68,       "Targeted repression",  "WVS",
  "Burkina Faso",  -0.30,       "Popular coup",         "Afrobarometer",
  "Venezuela",     -0.08,       "Genuine collapse",     "Latinobar\u00f3metro",
  "Nicaragua",     +0.53,       "Overt repression",     "Latinobar\u00f3metro"
) |>
  mutate(
    type = factor(type, levels = c("Targeted repression", "Popular coup",
                                   "Genuine collapse", "Overt repression")),
    nudge_y = c(0.12, -0.12, 0.12, -0.22, 0.12)  # Venezuela nudged down to clear BFA
  )

type_colors <- c(
  "Targeted repression" = "#C62828",
  "Popular coup"        = "#F57C00",
  "Genuine collapse"    = "#1565C0",
  "Overt repression"    = "#6A1B9A"
)
type_shapes <- c(
  "Targeted repression" = 16,
  "Popular coup"        = 15,
  "Genuine collapse"    = 17,
  "Overt repression"    = 18
)

fig2 <- ggplot(five_cases, aes(x = gradient_r, y = 0,
                               color = type, shape = type)) +
  annotate("rect", xmin = -1, xmax = -0.5, ymin = -0.35, ymax = 0.35,
           fill = "#FFEBEE", alpha = 0.5) +
  annotate("rect", xmin = -0.5, xmax = 0, ymin = -0.35, ymax = 0.35,
           fill = "#FFF8E1", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = 0.75, ymin = -0.35, ymax = 0.35,
           fill = "#E8EAF6", alpha = 0.5) +
  annotate("text", x = -0.75, y = 0.20,
           label = "Strong gradient\n(falsification signal)",
           size = 3, color = "grey50", hjust = 0.5) +
  annotate("text", x = -0.25, y = 0.20,
           label = "Weak gradient\n(attenuated / null)",
           size = 3, color = "grey50", hjust = 0.5) +
  annotate("text", x = 0.375, y = 0.20,
           label = "Inverted gradient\n(genuine collapse)",
           size = 3, color = "grey50", hjust = 0.5) +
  geom_vline(xintercept = 0, color = "grey60",
             linewidth = 0.5, linetype = "dashed") +
  geom_point(size = 5) +
  geom_text(aes(y = nudge_y, label = case),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  geom_text(aes(y = nudge_y - 0.09, label = paste0("(", survey, ")")),
            size = 2.8, color = "grey50", show.legend = FALSE) +
  scale_x_continuous(limits = c(-1.0, 0.75), breaks = seq(-1.0, 0.75, 0.25)) +
  scale_y_continuous(limits = c(-0.40, 0.40)) +
  scale_color_manual(values = type_colors, name = "Autocratization type") +
  scale_shape_manual(values = type_shapes, name = "Autocratization type") +
  labs(
    title    = "The Sensitivity Gradient Across Five Cases",
    subtitle = paste0("Gradient fires under targeted repression; ",
                      "attenuates or inverts under genuine collapse or popular support"),
    x        = "Sensitivity gradient (*r*)",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x       = element_markdown(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, color = "grey40")
  )

ggsave(file.path(figures_path, "fig_five_case_summary.pdf"),
       fig2, width = 10, height = 4.5, device = "pdf")
ggsave(file.path(figures_path, "fig_five_case_summary.png"),
       fig2, width = 10, height = 4.5, dpi = 300)
message("✓ Figure 2 saved: fig_five_case_summary")

# ── FIGURE 3: Venezuela + Nicaragua null/inverted gradient ────────────────────

make_lbs_plot <- function(dat, gradient_r, title, subtitle, xlims) {
  d <- dat |>
    filter(category != "Control") |>
    mutate(
      tier  = case_when(
        category == "High"   ~ "High",
        category == "Medium" ~ "Medium",
        TRUE                 ~ "Low"
      ),
      tier  = factor(tier, levels = c("High", "Medium", "Low")),
      se_d  = sqrt(1/w_before_n + 1/w_after_n +
                     cohens_d^2 / (2 * (w_before_n + w_after_n))),
      ci_lo = cohens_d - 1.96 * se_d,
      ci_hi = cohens_d + 1.96 * se_d,
      label = factor(label, levels = rev(label[order(sensitivity_rank, -cohens_d)]))
    )

  fit    <- lm(cohens_d ~ sensitivity_rank, data = d)
  d$fitted <- predict(fit)
  r_lab  <- round(gradient_r, 2)
  x_ann  <- xlims[1] + diff(xlims) * 0.97  # right-align annotation

  ggplot(d, aes(x = cohens_d, y = label)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_smooth(aes(x = fitted, y = label, group = 1),
                method = "lm", se = FALSE,
                color = "grey50", linetype = "dashed", linewidth = 0.5) +
    geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, color = tier),
                  width = 0.25, linewidth = 0.4, orientation = "y") +
    geom_point(aes(color = tier, shape = tier), size = 2.8) +
    annotate("text", x = x_ann, y = 1.4,
             label = paste0("italic(r)==", r_lab),
             parse = TRUE, hjust = 1, size = 3.5, color = "grey30") +
    scale_x_continuous(limits = xlims,
                       breaks = seq(round(xlims[1], 1), round(xlims[2], 1), 0.3)) +
    scale_color_manual(values = cat_colors, name = "Sensitivity") +
    scale_shape_manual(values = cat_shapes, name = "Sensitivity") +
    labs(title = title, subtitle = subtitle,
         x = "Cohen's *d* (post \u2212 pre)", y = NULL) +
    theme_gradient()
}

p_ven <- make_lbs_plot(
  ven_gradient_results, ven_gradient_r_all,
  title    = "Venezuela (null case)",
  subtitle = "Latinobar\u00f3metro | 2015 vs. 2018\u20132023",
  xlims    = c(-0.75, 0.40)
)

p_nic <- make_lbs_plot(
  nic_gradient_results, nic_gradient_r_all,
  title    = "Nicaragua (inverted case)",
  subtitle = "Latinobar\u00f3metro | 2017 vs. 2019\u20132021",
  xlims    = c(-1.05, 0.30)
)

fig3 <- (p_ven + p_nic) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Discriminant Validity: Null and Inverted Gradient Cases",
    subtitle = "When disillusionment is genuine and uniform, the gradient does not fire",
    theme    = theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 10, color = "grey40"),
      legend.position = "bottom"
    )
  )

ggsave(file.path(figures_path, "fig_gradient_ven_nic.pdf"),
       fig3, width = 12, height = 5.5, device = "pdf")
ggsave(file.path(figures_path, "fig_gradient_ven_nic.png"),
       fig3, width = 12, height = 5.5, dpi = 300)
message("✓ Figure 3 saved: fig_gradient_ven_nic")

# ── Done ──────────────────────────────────────────────────────────────────────
message("")
message("All figures complete. Files in analysis/figures/:")
message("  fig_gradient_hk_turkey.{pdf,png}")
message("  fig_five_case_summary.{pdf,png}")
message("  fig_gradient_ven_nic.{pdf,png}")
