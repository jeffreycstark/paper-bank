# Winner/Loser × Procedural Orientation: W4 and W6 Combined
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

# Helper functions
get_country_name <- function(x) {
  if (is.character(x)) {
    case_when(x == "Korea" ~ "South Korea", TRUE ~ x)
  } else {
    code <- as.integer(x)
    case_when(
      code == 1 ~ "Japan", code == 2 ~ "South Korea", code == 3 ~ "Mongolia",
      code == 4 ~ "Taiwan", code == 5 ~ "Hong Kong", code == 6 ~ "China",
      code == 7 ~ "Philippines", code == 8 ~ "Thailand", code == 9 ~ "Vietnam",
      code == 10 ~ "Cambodia", code == 11 ~ "Singapore", code == 12 ~ "Myanmar",
      code == 13 ~ "Malaysia", code == 14 ~ "Indonesia", TRUE ~ NA_character_
    )
  }
}

recode_set1 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set2 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)
recode_set3 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set4 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)

# Process W4
cat("Processing W4...\n")
w4_full <- w4 %>%
  mutate(
    wave = "W4",
    wave_year = 2014,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    # W4 uses q88-q91 for meaning of democracy
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index))

cat("W4:", nrow(w4_full), "with winner/loser and procedural data\n")

# Process W6
cat("Processing W6...\n")
w6_full <- w6 %>%
  mutate(
    wave = "W6",
    wave_year = 2020,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    # W6 uses q85-q88
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index))

cat("W6:", nrow(w6_full), "with winner/loser and procedural data\n")

# Combine
combined <- bind_rows(
  w4_full %>% select(wave, wave_year, country_name, winner_loser, 
                     set1_proc:procedural_index),
  w6_full %>% select(wave, wave_year, country_name, winner_loser,
                     set1_proc:procedural_index)
)

cat("\nCombined:", nrow(combined), "observations\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("WINNER/LOSER EFFECT BY WAVE\n")
cat("=============================================================\n\n")

by_wave <- combined %>%
  group_by(wave, winner_loser) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, mean_proc)) %>%
  mutate(loser_effect = mean_proc_Loser - mean_proc_Winner)

cat("Overall by wave:\n")
print(by_wave)

# T-tests by wave
cat("\nT-test W4:\n")
t_w4 <- t.test(procedural_index ~ winner_loser, data = w4_full)
cat("  Winner:", round(t_w4$estimate[2], 3), "Loser:", round(t_w4$estimate[1], 3), 
    "Diff:", round(diff(t_w4$estimate), 3), "p =", format(t_w4$p.value, digits = 3), "\n")

cat("\nT-test W6:\n")
t_w6 <- t.test(procedural_index ~ winner_loser, data = w6_full)
cat("  Winner:", round(t_w6$estimate[2], 3), "Loser:", round(t_w6$estimate[1], 3),
    "Diff:", round(diff(t_w6$estimate), 3), "p =", format(t_w6$p.value, digits = 3), "\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("LOSER EFFECT BY COUNTRY AND WAVE\n")
cat("=============================================================\n\n")

by_country_wave <- combined %>%
  group_by(country_name, wave, winner_loser) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop") %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, mean_proc)) %>%
  mutate(
    loser_effect = round(mean_proc_Loser - mean_proc_Winner, 3),
    pct_winner = round(n_Winner / (n_Winner + n_Loser) * 100, 1)
  ) %>%
  filter((n_Winner + n_Loser) >= 50)  # Minimum sample

# Show all
cat("All country × wave combinations:\n")
by_country_wave %>%
  select(country_name, wave, n_Winner, n_Loser, pct_winner, 
         mean_proc_Winner, mean_proc_Loser, loser_effect) %>%
  mutate(
    mean_proc_Winner = round(mean_proc_Winner, 2),
    mean_proc_Loser = round(mean_proc_Loser, 2)
  ) %>%
  arrange(country_name, wave) %>%
  print(n = 40)

# ============================================================================
cat("\n\n=============================================================\n")
cat("COUNTRIES IN BOTH WAVES: IS THE EFFECT CONSISTENT?\n")
cat("=============================================================\n\n")

# Find countries in both waves
countries_both <- by_country_wave %>%
  group_by(country_name) %>%
  filter(n() == 2) %>%
  ungroup()

cat("Countries with data in both W4 and W6:\n")
countries_both %>%
  select(country_name, wave, loser_effect, pct_winner) %>%
  pivot_wider(names_from = wave, values_from = c(loser_effect, pct_winner)) %>%
  mutate(
    effect_consistent = sign(loser_effect_W4) == sign(loser_effect_W6),
    effect_change = loser_effect_W6 - loser_effect_W4
  ) %>%
  arrange(desc(loser_effect_W6)) %>%
  print()

# ============================================================================
cat("\n\n=============================================================\n")
cat("FOCUS: THAILAND AND CAMBODIA ACROSS WAVES\n")
cat("=============================================================\n\n")

focus_countries <- c("Thailand", "Cambodia")

for (country in focus_countries) {
  cat("\n---", country, "---\n")
  
  country_data <- combined %>% filter(country_name == country)
  
  for (w in c("W4", "W6")) {
    wave_data <- country_data %>% filter(wave == w)
    if (nrow(wave_data) > 0) {
      cat("\n", w, ":\n")
      wave_data %>%
        group_by(winner_loser) %>%
        summarise(
          n = n(),
          mean_proc = round(mean(procedural_index), 2),
          pct_high = round(mean(procedural_index >= 3) * 100, 1),
          .groups = "drop"
        ) %>%
        print()
    }
  }
}

# ============================================================================
cat("\n\n=============================================================\n")
cat("POOLED REGRESSION WITH WAVE FIXED EFFECTS\n")
cat("=============================================================\n\n")

combined$loser <- if_else(combined$winner_loser == "Loser", 1L, 0L)
combined$w6 <- if_else(combined$wave == "W6", 1L, 0L)

# Overall
cat("Pooled OLS: procedural ~ loser + wave\n")
m1 <- lm(procedural_index ~ loser + w6, data = combined)
print(summary(m1))

cat("\nWith interaction (does loser effect differ by wave?):\n")
m2 <- lm(procedural_index ~ loser * w6, data = combined)
print(summary(m2))

# ============================================================================
cat("\n\n=============================================================\n")
cat("SUMMARY: NUMBER OF OBSERVATIONS\n")
cat("=============================================================\n\n")

combined %>%
  count(wave, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n) %>%
  mutate(total = Winner + Loser) %>%
  print()

cat("\nBy country and wave:\n")
combined %>%
  count(country_name, wave) %>%
  pivot_wider(names_from = wave, values_from = n, values_fill = 0) %>%
  mutate(total = W4 + W6) %>%
  arrange(desc(total)) %>%
  print(n = 20)

cat("\n=== DONE ===\n")
