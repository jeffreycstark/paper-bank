# =============================================================================
# Script 10: V-Dem Erosion Moderator Analysis (Revision Package Section 6)
# Goal: Interact loser status with V-Dem change score to show gap widens
#       where democracy has deteriorated.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(nnet)
  library(marginaleffects)
})

here::i_am("papers/02_meaning_of_democracy_revision/manuscript/md-manuscript.qmd")
results_path <- here("papers", "02_meaning_of_democracy_revision", "analysis", "revised", "results")

# --- 1. Load data -------------------------------------------------------
d <- readRDS(here("papers", "02_meaning_of_democracy_revision", "analysis", "data", "w346_main.rds"))
vdem <- readRDS(here("data", "external", "vdem_scores.rds"))

# Wave year mapping (representative fieldwork year per wave)
wave_year_map <- tibble(wave = c(3L, 4L, 6L), vdem_year = c(2011L, 2015L, 2022L))

# --- 2. Merge V-Dem scores ---------------------------------------------
# Baseline = each country's Wave 3 (2011) vdem_liberal score
baseline <- vdem %>%
  filter(year == 2011) %>%
  select(country_code, vdem_baseline = vdem_liberal)

vdem_cw <- vdem %>%
  inner_join(wave_year_map %>% rename(year = vdem_year), by = "year") %>%
  select(country_code, wave, vdem_level = vdem_liberal) %>%
  left_join(baseline, by = "country_code") %>%
  mutate(vdem_change = vdem_level - vdem_baseline)  # negative = erosion

# Merge onto individual-level data
d2 <- d %>%
  left_join(vdem_cw %>% rename(country = country_code), by = c("country", "wave")) %>%
  filter(!is.na(vdem_change), !is.na(loser), !is.na(set1_valid))

# --- 3. Interaction model using Set 1 (representative battery) ---------
# Build a binary procedural indicator (any set combined)
# For simplicity, use the country-wave proc-sub gap already estimated, then
# also run individual-level interaction for Set1+Set2 combined

# Create long-format item-level dataset
item_types <- tibble(
  item = c("Free elections", "Free expression", "Legislature oversight",
           "Media freedom", "Party competition", "Court protection",
           "Protest freedom",
           "Reduce gap rich/poor", "Basic necessities", "Jobs for all",
           "Unemployment aid", "Income equality",
           "No waste", "Quality services", "Clean politics", "Law and order"),
  item_type = c(rep("procedural", 7), rep("substantive", 5), rep("governance", 4))
)

# For the interaction, use a simplified binary procedural outcome per set
# We'll run it on Set 3 (has Media freedom, Party competition, Jobs for all, Law and order)
# which spans proc/sub/gov and has clean observations

d_set3 <- d2 %>%
  filter(set3_valid == 1) %>%
  mutate(
    proc_item = as.integer(set3_choice %in% c(1, 2)),  # Media freedom or Party competition
    loser_n = as.numeric(loser)
  ) %>%
  filter(!is.na(proc_item))

# Interaction model
m_int <- glm(
  proc_item ~ loser_n * vdem_change + factor(country) + factor(wave) +
    age + female + education + urban,
  data = d_set3,
  family = binomial(link = "logit")
)

int_coef <- coef(m_int)["loser_n:vdem_change"]
int_se   <- sqrt(diag(vcov(m_int)))["loser_n:vdem_change"]
cat("Interaction coefficient (loser × vdem_change):", round(int_coef, 4), "\n")
cat("SE:", round(int_se, 4), "\n")
cat("z:", round(int_coef / int_se, 3), "\n")

# --- 4. Country-wave scatter: gap vs V-Dem change ----------------------
cw_gap <- read_csv(file.path(results_path, "country_wave_proc_sub_gap.csv"),
                   show_col_types = FALSE)

# ABS country_name → country code mapping
country_map <- tibble(
  country_name = c("Japan","Hong Kong","South Korea","Mongolia","Philippines",
                   "Taiwan","Thailand","Indonesia","Cambodia","Malaysia",
                   "Myanmar","Singapore","Australia"),
  country      = c(1, 2, 3, 5, 6, 7, 8, 9, 12, 13, 14, 10, 15)
)

scatter_data <- cw_gap %>%
  left_join(country_map, by = "country_name") %>%
  left_join(vdem_cw, by = c("country" = "country_code", "wave")) %>%
  filter(!is.na(vdem_change))

# Correlation
r_all  <- cor(scatter_data$vdem_change, scatter_data$proc_sub_gap * 100, use = "complete.obs")
r_test <- cor.test(scatter_data$vdem_change, scatter_data$proc_sub_gap * 100)

# Without Thailand
scatter_no_thai <- scatter_data %>% filter(country_name != "Thailand")
r_nothai <- cor(scatter_no_thai$vdem_change, scatter_no_thai$proc_sub_gap * 100,
                use = "complete.obs")

cat(sprintf("\nCorrelation (all): r = %.3f, p = %.4f, N = %d\n",
            r_all, r_test$p.value, nrow(scatter_data)))
cat(sprintf("Correlation (excl. Thailand): r = %.3f\n", r_nothai))

# --- 5. Figure ----------------------------------------------------------
label_pts <- c("Thailand W3", "Thailand W4", "Thailand W6",
                "South Korea W3", "South Korea W4", "South Korea W6",
                "Cambodia W3", "Cambodia W4", "Cambodia W6")

p_scatter <- scatter_data %>%
  mutate(
    pt_label = paste(country_name, wave_label),
    show_label = pt_label %in% label_pts,
    label_text = if_else(show_label, paste(country_name, wave_label), NA_character_)
  ) %>%
  ggplot(aes(x = vdem_change, y = proc_sub_gap * 100)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "steelblue",
              alpha = 0.15, linewidth = 0.8) +
  geom_point(aes(color = country_name == "Thailand" | country_name == "South Korea" |
                   country_name == "Cambodia"),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = c("TRUE" = "#D62728", "FALSE" = "gray50"),
                     guide = "none") +
  ggrepel::geom_text_repel(
    aes(label = label_text),
    size = 3, max.overlaps = 20, na.rm = TRUE
  ) +
  annotate("text", x = max(scatter_data$vdem_change, na.rm = TRUE) * 0.9,
           y = max(scatter_data$proc_sub_gap * 100, na.rm = TRUE) * 0.95,
           label = sprintf("r = %.2f, p = %.3f\nN = %d country-waves",
                           r_all, r_test$p.value, nrow(scatter_data)),
           hjust = 1, size = 3.5, color = "steelblue") +
  labs(
    x = "Change in V-Dem Liberal Democracy Index\n(country-wave minus Wave 3 baseline; negative = erosion)",
    y = "Procedural–Substantive Gap\n(percentage points; positive = losers favor procedural)",
    title = "Winner-Loser Gap in Procedural Prioritization by Democratic Change",
    caption = "Note: Each point is one country-wave. Highlighted points are Thailand, South Korea, and Cambodia.\nLine is OLS fit with 95% CI."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Save figure
ggsave(file.path(results_path, "fig_vdem_moderator.pdf"), p_scatter,
       width = 8, height = 6)
ggsave(file.path(results_path, "fig_vdem_moderator.png"), p_scatter,
       width = 8, height = 6, dpi = 300)
cat("Figure saved.\n")

# --- 6. Save summary stats for inline text -----------------------------
vdem_stats <- list(
  r_all      = r_all,
  p_all      = r_test$p.value,
  n_cw       = nrow(scatter_data),
  r_nothai   = r_nothai,
  int_coef   = int_coef,
  int_se     = int_se,
  int_z      = int_coef / int_se
)
saveRDS(vdem_stats, file.path(results_path, "vdem_moderator_stats.rds"))
cat("Stats saved.\n")
cat("Done.\n")
