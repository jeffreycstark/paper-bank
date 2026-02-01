# Winner/Loser × Procedural Orientation: W3, W4, W6 (2010-2022)
# NOW WITH THREE WAVES!
library(haven)
library(tidyverse)
library(broom)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

cat("=============================================================\n")
cat("WINNER/LOSER × PROCEDURAL ORIENTATION: W3, W4, W6 (2010-2022)\n")
cat("=============================================================\n\n")

# Load data
w3 <- readRDS(file.path(data_dir, "w3.rds"))
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

# ============================================================================
# Process W3 (uses q33a for winner/loser, q85-q88 for democracy)
# ============================================================================
cat("Processing W3 (2010-12)...\n")
w3_full <- w3 %>%
  mutate(
    wave = "W3",
    wave_year = 2010,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q33a) == 1 ~ "Winner",
      as.integer(q33a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, set1_proc:procedural_index)

cat("  W3:", nrow(w3_full), "observations with winner/loser + procedural data\n")

# ============================================================================
# Process W4 (uses q34a for winner/loser, q88-q91 for democracy)
# ============================================================================
cat("Processing W4 (2014-16)...\n")
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
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, set1_proc:procedural_index)

cat("  W4:", nrow(w4_full), "observations\n")

# ============================================================================
# Process W6 (uses q34a for winner/loser, q85-q88 for democracy)
# ============================================================================
cat("Processing W6 (2019-22)...\n")
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
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, set1_proc:procedural_index)

cat("  W6:", nrow(w6_full), "observations\n")

# ============================================================================
# Combine all three waves
# ============================================================================
combined <- bind_rows(w3_full, w4_full, w6_full)
cat("\nCombined: ", nrow(combined), "observations across 3 waves\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("OVERALL LOSER EFFECT BY WAVE\n")
cat("=============================================================\n\n")

by_wave <- combined %>%
  group_by(wave, wave_year, winner_loser) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop") %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, mean_proc)) %>%
  mutate(
    loser_effect = round(mean_proc_Loser - mean_proc_Winner, 3),
    total_n = n_Winner + n_Loser
  )

print(by_wave)

# T-tests by wave
cat("\nStatistical tests:\n")
for (w in c("W3", "W4", "W6")) {
  wave_data <- combined %>% filter(wave == w)
  t_result <- t.test(procedural_index ~ winner_loser, data = wave_data)
  cat(w, ": Loser - Winner =", round(diff(t_result$estimate), 3), 
      ", t =", round(t_result$statistic, 2),
      ", p =", format(t_result$p.value, digits = 3), "\n")
}

# ============================================================================
cat("\n\n=============================================================\n")
cat("LOSER EFFECT BY COUNTRY AND WAVE (FULL PANEL)\n")
cat("=============================================================\n\n")

by_country_wave <- combined %>%
  group_by(country_name, wave, wave_year) %>%
  filter(n() >= 50) %>%  # Minimum sample
  summarise(
    n_winner = sum(winner_loser == "Winner"),
    n_loser = sum(winner_loser == "Loser"),
    mean_winner = mean(procedural_index[winner_loser == "Winner"]),
    mean_loser = mean(procedural_index[winner_loser == "Loser"]),
    .groups = "drop"
  ) %>%
  mutate(
    loser_effect = round(mean_loser - mean_winner, 3),
    pct_winner = round(n_winner / (n_winner + n_loser) * 100, 1)
  )

# Countries in all 3 waves
countries_all_waves <- by_country_wave %>%
  count(country_name) %>%
  filter(n == 3) %>%
  pull(country_name)

cat("Countries with data in ALL THREE waves:\n")
print(countries_all_waves)

cat("\n\nFull panel data:\n")
by_country_wave %>%
  filter(country_name %in% countries_all_waves) %>%
  select(country_name, wave, pct_winner, loser_effect) %>%
  pivot_wider(names_from = wave, values_from = c(pct_winner, loser_effect)) %>%
  arrange(desc(loser_effect_W6)) %>%
  print()

# ============================================================================
cat("\n\n=============================================================\n")
cat("TRAJECTORY: HOW DID LOSER EFFECT CHANGE OVER TIME?\n")
cat("=============================================================\n\n")

trajectory <- by_country_wave %>%
  filter(country_name %in% countries_all_waves) %>%
  select(country_name, wave, loser_effect) %>%
  pivot_wider(names_from = wave, values_from = loser_effect) %>%
  mutate(
    change_W3_W4 = round(W4 - W3, 3),
    change_W4_W6 = round(W6 - W4, 3),
    change_W3_W6 = round(W6 - W3, 3),
    consistent_positive = (W3 > 0 & W4 > 0 & W6 > 0)
  ) %>%
  arrange(desc(change_W3_W6))

cat("Change in loser effect over time (positive = losers became MORE procedural relative to winners):\n\n")
print(trajectory)

# ============================================================================
cat("\n\n=============================================================\n")
cat("FOCUS: THAILAND AND CAMBODIA TRAJECTORIES\n")
cat("=============================================================\n\n")

for (country in c("Thailand", "Cambodia")) {
  cat("---", country, "---\n\n")
  
  country_data <- by_country_wave %>% 
    filter(country_name == country) %>%
    arrange(wave_year)
  
  print(country_data %>% select(wave, pct_winner, mean_winner, mean_loser, loser_effect))
  cat("\n")
}

# ============================================================================
cat("\n\n=============================================================\n")
cat("REGRESSION: POOLED WITH WAVE AND COUNTRY FIXED EFFECTS\n")
cat("=============================================================\n\n")

combined$loser <- if_else(combined$winner_loser == "Loser", 1L, 0L)

# Model with country and wave FE
cat("OLS: procedural ~ loser + country_FE + wave_FE\n\n")
m1 <- lm(procedural_index ~ loser + country_name + wave, data = combined)
cat("Loser coefficient:", round(coef(m1)["loser"], 3), "\n")
cat("SE:", round(summary(m1)$coefficients["loser", "Std. Error"], 3), "\n")
cat("p-value:", format(summary(m1)$coefficients["loser", "Pr(>|t|)"], digits = 3), "\n")

# Model with country × wave interaction for loser effect
cat("\n\nDoes loser effect vary by country? (loser × country interaction)\n")
m2 <- lm(procedural_index ~ loser * country_name + wave, data = combined)
loser_interactions <- tidy(m2) %>%
  filter(str_detect(term, "loser:")) %>%
  mutate(
    country = str_remove(term, "loser:country_name"),
    estimate = round(estimate, 3)
  ) %>%
  arrange(desc(estimate)) %>%
  select(country, estimate, std.error, p.value)

cat("\nLoser × Country interactions (relative to baseline):\n")
print(loser_interactions, n = 15)

# ============================================================================
cat("\n\n=============================================================\n")
cat("SAMPLE SIZE SUMMARY\n")
cat("=============================================================\n\n")

sample_summary <- combined %>%
  count(wave, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n) %>%
  mutate(total = Winner + Loser)

print(sample_summary)

cat("\nBy country and wave:\n")
combined %>%
  count(country_name, wave) %>%
  pivot_wider(names_from = wave, values_from = n, values_fill = 0) %>%
  mutate(total = W3 + W4 + W6) %>%
  arrange(desc(total)) %>%
  print(n = 20)

# ============================================================================
# Save
# ============================================================================
cat("\n\n--- Saving results ---\n")
output_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/papers/meaning-of-democracy/analysis"
saveRDS(combined, file.path(output_dir, "winner_loser_3waves.rds"))
write_csv(by_country_wave, file.path(output_dir, "loser_effect_by_country_wave.csv"))
write_csv(trajectory, file.path(output_dir, "loser_effect_trajectory.csv"))

cat("Done! Saved to:", output_dir, "\n")
