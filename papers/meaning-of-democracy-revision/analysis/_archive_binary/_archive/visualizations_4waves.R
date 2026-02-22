# Visualization: Winner/Loser Effect Trajectories 2005-2022
library(tidyverse)
library(ggplot2)

# Set theme
theme_set(theme_minimal(base_size = 12))

# Load the data
data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/papers/meaning-of-democracy/analysis"
by_country_wave <- read_csv(file.path(data_dir, "loser_effect_by_country_4waves.csv"))

# Countries with 3+ waves for trajectory plot
countries_3plus <- by_country_wave %>%
  count(country_name) %>%
  filter(n >= 3) %>%
  pull(country_name)

# =============================================================================
# PLOT 1: Thailand - The Full Arc (Hero Plot)
# =============================================================================

thailand_data <- by_country_wave %>%
  filter(country_name == "Thailand") %>%
  mutate(
    event_label = case_when(
      wave == "W2" ~ "Post-2006\ncoup",
      wave == "W3" ~ "Democrat\ngovt",
      wave == "W4" ~ "2014\nCOUP",
      wave == "W6" ~ "Military-\nbacked"
    )
  )

p1 <- ggplot(thailand_data, aes(x = wave_year, y = loser_effect)) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  # Area under curve
  geom_area(alpha = 0.3, fill = "#E63946") +
  # Line and points
  geom_line(linewidth = 1.2, color = "#E63946") +
  geom_point(size = 4, color = "#E63946") +
  # Event labels
  geom_text(aes(label = event_label), vjust = -1.5, size = 3, lineheight = 0.9) +
  # Value labels
  geom_text(aes(label = paste0(ifelse(loser_effect > 0, "+", ""), loser_effect, " pp")), 
            vjust = 2.5, size = 3.5, fontface = "bold") +
  # Scales
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2020), 
                     labels = c("2006\n(W2)", "2010\n(W3)", "2014\n(W4)", "2020\n(W6)")) +
  scale_y_continuous(limits = c(-5, 22), breaks = seq(-5, 20, 5)) +
  # Labels
  labs(
    title = "Thailand: As Democracy Eroded, Losers Embraced Procedural Values",
    subtitle = "Loser effect = % losers procedural − % winners procedural",
    x = NULL,
    y = "Loser Effect (percentage points)",
    caption = "Data: Asian Barometer Survey Waves 2-6 (2005-2022)\nPositive values = losers more likely to prioritize procedural democracy"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(data_dir, "fig_thailand_trajectory.png"), p1, 
       width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(data_dir, "fig_thailand_trajectory.pdf"), p1, 
       width = 9, height = 6, bg = "white")

cat("Saved Thailand trajectory plot\n")

# =============================================================================
# PLOT 2: Multi-country comparison (countries with 3+ waves)
# =============================================================================

multi_data <- by_country_wave %>%
  filter(country_name %in% countries_3plus) %>%
  mutate(
    highlight = case_when(
      country_name == "Thailand" ~ "Thailand",
      country_name == "South Korea" ~ "South Korea",
      TRUE ~ "Other"
    ),
    highlight = factor(highlight, levels = c("Thailand", "South Korea", "Other"))
  )

p2 <- ggplot(multi_data, aes(x = wave_year, y = loser_effect, 
                              color = country_name, group = country_name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(data = filter(multi_data, highlight == "Other"), 
            linewidth = 0.8, alpha = 0.4) +
  geom_point(data = filter(multi_data, highlight == "Other"), 
             size = 2, alpha = 0.4) +
  # Highlight Thailand
  geom_line(data = filter(multi_data, country_name == "Thailand"), 
            linewidth = 1.5, color = "#E63946") +
  geom_point(data = filter(multi_data, country_name == "Thailand"), 
             size = 4, color = "#E63946") +
  # Highlight South Korea
  geom_line(data = filter(multi_data, country_name == "South Korea"), 
            linewidth = 1.3, color = "#1D3557") +
  geom_point(data = filter(multi_data, country_name == "South Korea"), 
             size = 3.5, color = "#1D3557") +
  # Labels at end
  geom_text(data = multi_data %>% 
              group_by(country_name) %>% 
              filter(wave_year == max(wave_year)) %>%
              ungroup(),
            aes(label = country_name), 
            hjust = -0.1, size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2020),
                     limits = c(2005, 2024)) +
  scale_y_continuous(breaks = seq(-10, 35, 10)) +
  scale_color_manual(values = c(
    "Thailand" = "#E63946",
    "South Korea" = "#1D3557",
    "Taiwan" = "#457B9D",
    "Philippines" = "#2A9D8F",
    "Mongolia" = "#E9C46A",
    "Japan" = "#F4A261",
    "Hong Kong" = "#9B5DE5",
    "China" = "#00BBF9",
    "Vietnam" = "#00F5D4",
    "Malaysia" = "#F15BB5"
  )) +
  labs(
    title = "Trajectories of the Loser Effect Across Asia (2005-2022)",
    subtitle = "Thailand shows dramatic increase; South Korea shows dramatic decrease after power alternation",
    x = NULL,
    y = "Loser Effect (percentage points)",
    caption = "Data: Asian Barometer Survey. Countries with 3+ waves shown.\nLoser effect = % losers procedural − % winners procedural"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(data_dir, "fig_multicountry_trajectory.png"), p2, 
       width = 11, height = 7, dpi = 300, bg = "white")
ggsave(file.path(data_dir, "fig_multicountry_trajectory.pdf"), p2, 
       width = 11, height = 7, bg = "white")

cat("Saved multi-country trajectory plot\n")

# =============================================================================
# PLOT 3: Thailand dual-axis (loser effect + % winners)
# =============================================================================

thailand_long <- thailand_data %>%
  select(wave, wave_year, loser_effect, pct_winner) %>%
  pivot_longer(cols = c(loser_effect, pct_winner), 
               names_to = "measure", values_to = "value") %>%
  mutate(
    measure_label = case_when(
      measure == "loser_effect" ~ "Loser Effect (pp)",
      measure == "pct_winner" ~ "% Identifying as Winners"
    )
  )

# Create dual plot
p3a <- ggplot(thailand_data, aes(x = wave_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_area(aes(y = loser_effect), alpha = 0.3, fill = "#E63946") +
  geom_line(aes(y = loser_effect), linewidth = 1.2, color = "#E63946") +
  geom_point(aes(y = loser_effect), size = 4, color = "#E63946") +
  geom_text(aes(y = loser_effect, 
                label = paste0(ifelse(loser_effect > 0, "+", ""), loser_effect)), 
            vjust = -1, size = 3.5, fontface = "bold", color = "#E63946") +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2020)) +
  scale_y_continuous(limits = c(-5, 22)) +
  labs(title = "Loser Effect", y = "Percentage points", x = NULL) +
  theme(plot.title = element_text(color = "#E63946", face = "bold"))

p3b <- ggplot(thailand_data, aes(x = wave_year)) +
  geom_area(aes(y = pct_winner), alpha = 0.3, fill = "#457B9D") +
  geom_line(aes(y = pct_winner), linewidth = 1.2, color = "#457B9D") +
  geom_point(aes(y = pct_winner), size = 4, color = "#457B9D") +
  geom_text(aes(y = pct_winner, label = paste0(round(pct_winner), "%")), 
            vjust = -1, size = 3.5, fontface = "bold", color = "#457B9D") +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2020),
                     labels = c("2006\n(W2)", "2010\n(W3)", "2014\n(W4)", "2020\n(W6)")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "% Identifying as Winners", y = "Percent", x = NULL) +
  theme(plot.title = element_text(color = "#457B9D", face = "bold"))

# Combine
library(patchwork)
p3 <- p3a / p3b + 
  plot_annotation(
    title = "Thailand: Democratic Erosion in Two Metrics",
    subtitle = "As fewer citizens 'won' elections, losers increasingly valued procedural democracy",
    caption = "Data: Asian Barometer Survey Waves 2-6",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11),
      plot.caption = element_text(color = "gray50", size = 9)
    )
  )

ggsave(file.path(data_dir, "fig_thailand_dual.png"), p3, 
       width = 8, height = 8, dpi = 300, bg = "white")
ggsave(file.path(data_dir, "fig_thailand_dual.pdf"), p3, 
       width = 8, height = 8, bg = "white")

cat("Saved Thailand dual plot\n")

# =============================================================================
# PLOT 4: Slope chart comparing first vs last wave
# =============================================================================

slope_data <- by_country_wave %>%
  filter(country_name %in% countries_3plus) %>%
  group_by(country_name) %>%
  filter(wave_year == min(wave_year) | wave_year == max(wave_year)) %>%
  mutate(period = if_else(wave_year == min(wave_year), "Early", "Late")) %>%
  ungroup() %>%
  select(country_name, period, loser_effect) %>%
  pivot_wider(names_from = period, values_from = loser_effect) %>%
  mutate(
    change = Late - Early,
    direction = if_else(change > 0, "Increased", "Decreased"),
    country_label = paste0(country_name, " (", ifelse(change > 0, "+", ""), round(change, 1), ")")
  ) %>%
  arrange(desc(change))

p4 <- slope_data %>%
  pivot_longer(cols = c(Early, Late), names_to = "period", values_to = "loser_effect") %>%
  mutate(period = factor(period, levels = c("Early", "Late"))) %>%
  ggplot(aes(x = period, y = loser_effect, group = country_name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(color = direction), linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = direction), size = 3) +
  geom_text(data = . %>% filter(period == "Late"),
            aes(label = country_name, color = direction),
            hjust = -0.1, size = 3) +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.4)),
                   labels = c("Early" = "First Wave\n(2006-2010)", 
                              "Late" = "Last Wave\n(2014-2020)")) +
  scale_color_manual(values = c("Increased" = "#E63946", "Decreased" = "#457B9D")) +
  labs(
    title = "Change in Loser Effect: First to Last Wave",
    subtitle = "Thailand shows largest increase; South Korea and Taiwan show largest decreases",
    x = NULL,
    y = "Loser Effect (percentage points)",
    caption = "Data: Asian Barometer Survey. Countries with 3+ waves."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(data_dir, "fig_slope_change.png"), p4, 
       width = 9, height = 7, dpi = 300, bg = "white")
ggsave(file.path(data_dir, "fig_slope_change.pdf"), p4, 
       width = 9, height = 7, bg = "white")

cat("Saved slope chart\n")

# =============================================================================
# PLOT 5: Bar chart - Overall loser effect by wave
# =============================================================================

wave_summary <- tribble(
  ~wave, ~wave_year, ~loser_effect, ~sig,
  "W2", 2006, 6.5, TRUE,
  "W3", 2010, 4.3, TRUE,
  "W4", 2014, 5.1, TRUE,
  "W6", 2020, -1.5, FALSE
)

p5 <- ggplot(wave_summary, aes(x = factor(wave_year), y = loser_effect, fill = sig)) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(ifelse(loser_effect > 0, "+", ""), loser_effect, " pp"),
                vjust = ifelse(loser_effect > 0, -0.5, 1.5)),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
  scale_y_continuous(limits = c(-3, 8), breaks = seq(-2, 8, 2)) +
  labs(
    title = "The Loser Effect Over Time: Pooled Across Countries",
    subtitle = "Losers consistently more procedural—until 2020",
    x = NULL,
    y = "Loser Effect (percentage points)",
    caption = "Green = statistically significant (p < 0.001); Orange = not significant\nN = 34,035 across 14 countries"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(data_dir, "fig_wave_losereffect.png"), p5, 
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(data_dir, "fig_wave_losereffect.pdf"), p5, 
       width = 8, height = 5, bg = "white")

cat("Saved wave bar chart\n")

cat("\n=== ALL VISUALIZATIONS COMPLETE ===\n")
cat("Files saved to:", data_dir, "\n")
