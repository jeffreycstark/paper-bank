# Winner/Loser × Procedural Orientation: All Countries
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"
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

# Process W6
w6_full <- w6 %>%
  mutate(
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      as.integer(q34a) == 0 ~ "Did not vote",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  )

# ============================================================================
# MAIN ANALYSIS: Winner/Loser effect on procedural orientation by country
# ============================================================================
cat("=============================================================\n")
cat("WINNER/LOSER × PROCEDURAL ORIENTATION: ALL COUNTRIES (W6)\n")
cat("=============================================================\n\n")

cat("Question: Do electoral losers value democratic procedures more?\n")
cat("Hypothesis: Losers want fair elections because that's their path to power.\n")
cat("            Winners care about delivery since they already won.\n\n")

# Summary by country
winner_loser_by_country <- w6_full %>%
  filter(!is.na(winner_loser), !is.na(procedural_index),
         winner_loser %in% c("Winner", "Loser")) %>%
  group_by(country_name, winner_loser) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index),
    pct_high_proc = mean(procedural_index >= 3) * 100,
    .groups = "drop"
  )

# Pivot to compare
comparison <- winner_loser_by_country %>%
  select(country_name, winner_loser, n, mean_proc) %>%
  pivot_wider(
    names_from = winner_loser,
    values_from = c(n, mean_proc)
  ) %>%
  mutate(
    loser_winner_diff = mean_proc_Loser - mean_proc_Winner,
    pct_winner = round(n_Winner / (n_Winner + n_Loser) * 100, 1),
    total_n = n_Winner + n_Loser
  ) %>%
  arrange(desc(loser_winner_diff))

cat("RESULTS: Loser-Winner Difference in Procedural Index\n")
cat("(Positive = Losers more procedural, Negative = Winners more procedural)\n\n")

comparison %>%
  mutate(
    mean_proc_Winner = round(mean_proc_Winner, 2),
    mean_proc_Loser = round(mean_proc_Loser, 2),
    loser_winner_diff = round(loser_winner_diff, 2)
  ) %>%
  select(country_name, n_Winner, n_Loser, pct_winner, 
         mean_proc_Winner, mean_proc_Loser, loser_winner_diff) %>%
  print(n = 20)

# ============================================================================
# Statistical summary
# ============================================================================
cat("\n\n=============================================================\n")
cat("SUMMARY STATISTICS\n")
cat("=============================================================\n\n")

n_positive <- sum(comparison$loser_winner_diff > 0, na.rm = TRUE)
n_total <- sum(!is.na(comparison$loser_winner_diff))
mean_diff <- mean(comparison$loser_winner_diff, na.rm = TRUE)

cat("Countries where LOSERS are more procedural:", n_positive, "/", n_total, "\n")
cat("Average loser-winner difference:", round(mean_diff, 3), "\n")

# T-test across all countries pooled
cat("\n--- Pooled t-test (all countries) ---\n")
pooled_data <- w6_full %>%
  filter(winner_loser %in% c("Winner", "Loser"), !is.na(procedural_index))

t_result <- t.test(procedural_index ~ winner_loser, data = pooled_data)
cat("Winner mean:", round(t_result$estimate["mean in group Winner"], 3), "\n")
cat("Loser mean:", round(t_result$estimate["mean in group Loser"], 3), "\n")
cat("Difference:", round(diff(t_result$estimate), 3), "\n")
cat("t =", round(t_result$statistic, 2), ", p =", format(t_result$p.value, digits = 3), "\n")

# ============================================================================
# Break down by electoral competitiveness
# ============================================================================
cat("\n\n=============================================================\n")
cat("BY ELECTORAL COMPETITIVENESS\n")
cat("=============================================================\n\n")

cat("Is the loser effect stronger in less competitive systems?\n\n")

comparison %>%
  mutate(
    competitiveness = case_when(
      pct_winner > 80 ~ "Dominant (>80% winner)",
      pct_winner > 60 ~ "Leaning (60-80% winner)",
      TRUE ~ "Competitive (<60% winner)"
    )
  ) %>%
  group_by(competitiveness) %>%
  summarise(
    n_countries = n(),
    mean_loser_effect = round(mean(loser_winner_diff, na.rm = TRUE), 3),
    countries = paste(country_name, collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = 150)

# ============================================================================
# Set-by-set breakdown for top countries
# ============================================================================
cat("\n\n=============================================================\n")
cat("SET-BY-SET BREAKDOWN: Where do losers differ most?\n")
cat("=============================================================\n\n")

set_breakdown <- w6_full %>%
  filter(winner_loser %in% c("Winner", "Loser")) %>%
  group_by(country_name, winner_loser) %>%
  summarise(
    n = n(),
    set1 = mean(set1_proc, na.rm = TRUE) * 100,  # Elections/Expression
    set2 = mean(set2_proc, na.rm = TRUE) * 100,  # Oversight/Organize
    set3 = mean(set3_proc, na.rm = TRUE) * 100,  # Media/Multiparty
    set4 = mean(set4_proc, na.rm = TRUE) * 100,  # Protests/Courts
    .groups = "drop"
  )

# Calculate differences
set_diffs <- set_breakdown %>%
  pivot_wider(
    names_from = winner_loser,
    values_from = c(n, set1, set2, set3, set4)
  ) %>%
  mutate(
    diff_set1 = set1_Loser - set1_Winner,
    diff_set2 = set2_Loser - set2_Winner,
    diff_set3 = set3_Loser - set3_Winner,
    diff_set4 = set4_Loser - set4_Winner
  ) %>%
  select(country_name, starts_with("diff")) %>%
  arrange(desc(diff_set3))  # Sort by multiparty/media difference

cat("Loser-Winner difference by set (percentage points):\n")
cat("Set 1: Elections/Expression vs Gap/Waste\n")
cat("Set 2: Oversight/Organize vs Needs/Services\n")
cat("Set 3: Media/Multiparty vs Law&Order/Jobs\n")
cat("Set 4: Protests/Courts vs Corruption/Unemployment\n\n")

set_diffs %>%
  mutate(across(starts_with("diff"), ~round(., 1))) %>%
  print(n = 20)

# Average across countries
cat("\n--- Average set differences (loser - winner) ---\n")
set_diffs %>%
  summarise(
    across(starts_with("diff"), ~round(mean(., na.rm = TRUE), 1))
  ) %>%
  print()

# ============================================================================
# Regression: Does winner/loser predict procedural orientation?
# ============================================================================
cat("\n\n=============================================================\n")
cat("REGRESSION: Winner/Loser effect controlling for demographics\n")
cat("=============================================================\n\n")

# Add demographics
w6_reg <- w6_full %>%
  mutate(
    loser = if_else(winner_loser == "Loser", 1L, 0L),
    education = as.integer(se5),
    age = 2020 - as.integer(se3),
    female = case_when(as.integer(se2) == 2 ~ 1L, as.integer(se2) == 1 ~ 0L, TRUE ~ NA_integer_)
  ) %>%
  filter(winner_loser %in% c("Winner", "Loser"), !is.na(procedural_index))

# By country
cat("OLS by country: procedural_index ~ loser + education + age + female\n")
cat("(Positive 'loser' coefficient = losers more procedural)\n\n")

reg_results <- w6_reg %>%
  group_by(country_name) %>%
  filter(n() >= 100) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(procedural_index ~ loser + education + age + female, data = .x)),
    tidied = map(model, broom::tidy)
  ) %>%
  unnest(tidied) %>%
  filter(term == "loser") %>%
  select(country_name, estimate, std.error, p.value) %>%
  mutate(
    sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", 
                    p.value < 0.05 ~ "*", p.value < 0.1 ~ ".", TRUE ~ ""),
    estimate = round(estimate, 3)
  ) %>%
  arrange(desc(estimate))

print(reg_results, n = 20)

cat("\n=== DONE ===\n")
