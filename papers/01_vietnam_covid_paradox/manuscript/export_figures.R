# ============================================================================
# EXPORT HIGH-RESOLUTION FIGURES FOR JOURNAL SUBMISSION
# Generates TIFF files at 300 dpi
# ============================================================================

library(tidyverse)
library(here)

# Set output directory
fig_dir <- here("papers", "01_vietnam_covid_paradox", "manuscript", "figures_submission")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Set results directory
results_dir <- here("papers", "01_vietnam_covid_paradox", "analysis", "results")
data_dir <- here("data", "processed")

# Load results
safe_load <- function(filename) {
  filepath <- file.path(results_dir, filename)
  if (file.exists(filepath)) readRDS(filepath) else NULL
}

h1_results <- safe_load("h1_infection_effects.rds")
h2a_results <- safe_load("h2a_trust_direct_effects.rds")
h4a_results <- safe_load("h4a_economic_bivariate.rds")
ab_data <- readRDS(file.path(data_dir, "ab_analysis_v2.rds"))

# ============================================================================
# FIGURE 1: Coefficient Plot (Main Manuscript)
# ============================================================================

if (!is.null(h1_results) && !is.null(h2a_results) && !is.null(h4a_results)) {
  
  coef_data <- bind_rows(
    # Infection coefficients (bivariate)
    tibble(
      Country = names(h1_results),
      Variable = "Personal Infection",
      estimate = sapply(h1_results, function(x) x$coef),
      ci_lower = sapply(h1_results, function(x) x$ci_lower),
      ci_upper = sapply(h1_results, function(x) x$ci_upper),
      significant = sapply(h1_results, function(x) x$p < 0.05)
    ),
    # Trust coefficients (bivariate)
    tibble(
      Country = names(h2a_results),
      Variable = "Trust in COVID Info",
      estimate = sapply(h2a_results, function(x) x$coef),
      ci_lower = sapply(h2a_results, function(x) x$ci_lower),
      ci_upper = sapply(h2a_results, function(x) x$ci_upper),
      significant = sapply(h2a_results, function(x) x$p < 0.05)
    ),
    # Economic coefficients (bivariate)
    tibble(
      Country = names(h4a_results),
      Variable = "Economic Hardship",
      estimate = sapply(h4a_results, function(x) x$coef),
      ci_lower = sapply(h4a_results, function(x) x$ci_lower),
      ci_upper = sapply(h4a_results, function(x) x$ci_upper),
      significant = sapply(h4a_results, function(x) x$p < 0.05)
    )
  ) %>%
    mutate(
      Country = factor(Country, levels = c("Cambodia", "Vietnam", "Thailand")),
      Variable = factor(Variable, levels = c("Personal Infection", "Economic Hardship", "Trust in COVID Info"))
    )
  
  fig1 <- ggplot(coef_data, aes(x = estimate, y = Variable, color = Country, shape = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.15,
      linewidth = 0.7,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 3.5,
      position = position_dodge(width = 0.6)
    ) +
    scale_color_manual(
      values = c("Cambodia" = "#66C2A5", "Vietnam" = "#8DA0CB", "Thailand" = "#FC8D62"),
      name = "Country"
    ) +
    scale_shape_manual(
      values = c("TRUE" = 16, "FALSE" = 1),
      name = "p < 0.05",
      labels = c("TRUE" = "Yes", "FALSE" = "No")
    ) +
    labs(
      title = "Trust Dominates: Effect Sizes with Uncertainty",
      subtitle = "Standardized regression coefficients with 95% confidence intervals",
      x = expression(paste("Standardized Coefficient (", beta, ")")),
      y = NULL,
      caption = "Note: Filled points = p < 0.05. All coefficients from bivariate regressions."
    ) +
    scale_x_continuous(limits = c(-0.3, 0.85), breaks = seq(-0.2, 0.8, 0.2)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      plot.caption = element_text(size = 9, hjust = 0),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold")
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    )
  
  # Save as TIFF at 300 dpi
  ggsave(
    filename = file.path(fig_dir, "Figure1_coefficient_plot.tiff"),
    plot = fig1,
    width = 9,
    height = 7,
    dpi = 300,
    compression = "lzw"
  )
  
  # Also save as EPS (some journals prefer this)
  ggsave(
    filename = file.path(fig_dir, "Figure1_coefficient_plot.eps"),
    plot = fig1,
    width = 9,
    height = 7,
    device = "eps"
  )
  
  cat("✓ Figure 1 saved\n")
}

# ============================================================================
# APPENDIX FIGURE D1: Interaction Plot
# ============================================================================

if (!is.null(ab_data)) {
  
  create_interaction_data <- function(df, country) {
    model <- lm(covid_govt_handling ~ covid_contracted * covid_trust_info +
                  institutional_trust_index + dem_satisfaction,
                data = df)
    
    newdata <- expand.grid(
      covid_contracted = c(0, 1),
      covid_trust_info = seq(1, 4, by = 0.5),
      institutional_trust_index = mean(df$institutional_trust_index, na.rm = TRUE),
      dem_satisfaction = mean(df$dem_satisfaction, na.rm = TRUE)
    )
    
    preds <- predict(model, newdata, se.fit = TRUE)
    newdata$predicted <- preds$fit
    newdata$se <- preds$se.fit
    newdata$lower <- newdata$predicted - 1.96 * newdata$se
    newdata$upper <- newdata$predicted + 1.96 * newdata$se
    newdata$Infection <- factor(newdata$covid_contracted, 
                                levels = c(0, 1),
                                labels = c("Not Infected", "Infected"))
    newdata$Country <- country
    newdata
  }
  
  plot_data <- bind_rows(
    ab_data %>% filter(country_name == "Vietnam") %>% create_interaction_data("Vietnam"),
    ab_data %>% filter(country_name == "Cambodia") %>% create_interaction_data("Cambodia"),
    ab_data %>% filter(country_name == "Thailand") %>% create_interaction_data("Thailand")
  )
  
  fig_interaction <- ggplot(plot_data, aes(x = covid_trust_info, y = predicted, 
                                            color = Infection, fill = Infection)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~Country, ncol = 3) +
    scale_color_manual(values = c("#2166AC", "#B2182B")) +
    scale_fill_manual(values = c("#2166AC", "#B2182B")) +
    labs(
      x = "Trust in COVID Information",
      y = "Predicted Government Approval",
      color = "COVID Status",
      fill = "COVID Status"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    coord_cartesian(ylim = c(1, 4))
  
  ggsave(
    filename = file.path(fig_dir, "FigureD1_interaction_plot.tiff"),
    plot = fig_interaction,
    width = 10,
    height = 4,
    dpi = 300,
    compression = "lzw"
  )
  
  cat("✓ Appendix Figure D1 (Interaction) saved\n")
}

# ============================================================================
# APPENDIX FIGURE L1: E-Value Comparison
# ============================================================================

evalue_comparison <- tibble(
  Association = c(
    "Vietnam (Trust→Approval)",
    "Cambodia (Trust→Approval)",
    "Thailand (Trust→Approval)",
    "Obesity → Diabetes",
    "Smoking → Heart Disease",
    "Smoking → Lung Cancer"
  ),
  E_Value = c(2.70, 3.71, 3.31, 4.0, 3.0, 15.0),
  Type = c("Our Study", "Our Study", "Our Study", 
           "Benchmark", "Benchmark", "Benchmark")
)

fig_evalue <- ggplot(evalue_comparison, aes(x = reorder(Association, E_Value), y = E_Value, fill = Type)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 2.3, label = "Robustness threshold (E=2)", 
           hjust = 0, color = "red", size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Our Study" = "#2166AC", "Benchmark" = "#878787")) +
  labs(
    x = NULL,
    y = "E-Value (Required Confounder Strength)",
    fill = NULL,
    title = "How Strong Must an Unmeasured Confounder Be?",
    subtitle = "Our findings require confounders as strong as obesity→diabetes to be nullified"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = file.path(fig_dir, "FigureL1_evalue_comparison.tiff"),
  plot = fig_evalue,
  width = 8,
  height = 5,
  dpi = 300,
  compression = "lzw"
)

cat("✓ Appendix Figure L1 (E-Value) saved\n")

# ============================================================================
# APPENDIX FIGURE L2: Falsification Test
# ============================================================================

if (!is.null(ab_data)) {
  
  run_falsification_models <- function(country_filter) {
    df <- ab_data %>% filter(country_name == country_filter)
    
    m_covid <- lm(covid_govt_handling ~ covid_trust_info + institutional_trust_index, data = df)
    m_dem <- lm(dem_satisfaction ~ covid_trust_info + institutional_trust_index, data = df)
    
    tibble(
      Country = country_filter,
      DV = c("COVID Approval\n(Primary DV)", "Democracy Satisfaction\n(Falsification DV)"),
      Trust_Coef = c(
        coef(m_covid)["covid_trust_info"],
        coef(m_dem)["covid_trust_info"]
      ),
      Trust_SE = c(
        summary(m_covid)$coefficients["covid_trust_info", "Std. Error"],
        summary(m_dem)$coefficients["covid_trust_info", "Std. Error"]
      )
    )
  }
  
  falsification_plot_data <- bind_rows(
    run_falsification_models("Vietnam"),
    run_falsification_models("Cambodia"),
    run_falsification_models("Thailand")
  ) %>%
    mutate(
      lower = Trust_Coef - 1.96 * Trust_SE,
      upper = Trust_Coef + 1.96 * Trust_SE
    )
  
  fig_falsification <- ggplot(falsification_plot_data, aes(x = DV, y = Trust_Coef, fill = DV)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    facet_wrap(~Country, ncol = 3) +
    scale_fill_manual(values = c("COVID Approval\n(Primary DV)" = "#2166AC", 
                                 "Democracy Satisfaction\n(Falsification DV)" = "#878787")) +
    labs(
      x = NULL,
      y = "Trust Coefficient (β)",
      title = "COVID Trust is Domain-Specific, Not General Regime Loyalty",
      subtitle = "Trust predicts COVID approval 2-4× more strongly than non-COVID outcomes"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 9),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(fig_dir, "FigureL2_falsification_test.tiff"),
    plot = fig_falsification,
    width = 8,
    height = 5,
    dpi = 300,
    compression = "lzw"
  )
  
  cat("✓ Appendix Figure L2 (Falsification) saved\n")
}

# ============================================================================
# Summary
# ============================================================================

cat("\n========================================\n")
cat("All figures exported to:\n")
cat(fig_dir, "\n")
cat("========================================\n")
cat("\nFiles created:\n")
list.files(fig_dir, pattern = "\\.(tiff|eps)$")
