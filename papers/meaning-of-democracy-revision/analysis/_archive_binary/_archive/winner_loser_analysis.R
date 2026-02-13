# Q34a: Winner/Loser analysis for Cambodia
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

cat("=== Q34a: Did respondent vote for winning or losing camp? ===\n\n")

# W4 labels
cat("W4 q34a label:", attr(w4$q34a, "label"), "\n")
cat("\nW4 q34a value labels:\n")
if (is.labelled(w4$q34a)) {
  labels <- attr(w4$q34a, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

# W6 - check if it has same meaning (no label but similar distribution)
cat("\n\nW6 q34a distribution (likely same coding):\n")
print(table(as.integer(w6$q34a), useNA = "ifany"))

# Get country name helper
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

# ============================================================================
# W6 Analysis - Cambodia focus
# ============================================================================
cat("\n\n=== W6 CAMBODIA: Winner/Loser by Education & Urban ===\n")

w6_analysis <- w6 %>%
  mutate(
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      as.integer(q34a) == 0 ~ "Did not vote",
      TRUE ~ NA_character_
    ),
    education = as.integer(se5),
    edu_level = case_when(
      education %in% 1:3 ~ "Low",
      education %in% 4:6 ~ "Middle", 
      education %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ),
    urban = case_when(
      as.integer(se14) %in% c(1, 2) ~ "Urban",
      as.integer(se14) %in% c(3, 4) ~ "Rural",
      TRUE ~ NA_character_
    )
  )

# Cambodia only
cambodia <- w6_analysis %>% filter(country_name == "Cambodia")

cat("\nCambodia overall winner/loser:\n")
print(table(cambodia$winner_loser, useNA = "ifany"))

cat("\nCambodia by education:\n")
cambodia %>%
  filter(!is.na(winner_loser), !is.na(edu_level)) %>%
  count(edu_level, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n, values_fill = 0) %>%
  mutate(
    total = Winner + Loser + `Did not vote`,
    pct_winner = round(Winner / total * 100, 1),
    pct_loser = round(Loser / total * 100, 1)
  ) %>%
  print()

cat("\nCambodia by urban/rural:\n")
cambodia %>%
  filter(!is.na(winner_loser), !is.na(urban)) %>%
  count(urban, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n, values_fill = 0) %>%
  mutate(
    total = Winner + Loser + `Did not vote`,
    pct_winner = round(Winner / total * 100, 1),
    pct_loser = round(Loser / total * 100, 1)
  ) %>%
  print()

cat("\nCambodia by education AND urban:\n")
cambodia %>%
  filter(!is.na(winner_loser), !is.na(edu_level), !is.na(urban)) %>%
  count(edu_level, urban, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n, values_fill = 0) %>%
  mutate(
    total = Winner + Loser + `Did not vote`,
    pct_winner = round(Winner / total * 100, 1),
    pct_loser = round(Loser / total * 100, 1)
  ) %>%
  arrange(edu_level, urban) %>%
  print()

# ============================================================================
# Compare to other countries
# ============================================================================
cat("\n\n=== ALL COUNTRIES: % Voting for Winner ===\n")

all_countries <- w6_analysis %>%
  filter(!is.na(winner_loser), winner_loser %in% c("Winner", "Loser")) %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    n_winner = sum(winner_loser == "Winner"),
    n_loser = sum(winner_loser == "Loser"),
    pct_winner = round(n_winner / n * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_winner))

print(all_countries)

# ============================================================================
# Now cross with procedural orientation for Cambodia
# ============================================================================
cat("\n\n=== CAMBODIA: Procedural Orientation by Winner/Loser ===\n")

# Recode functions
recode_set1 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set2 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)
recode_set3 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set4 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)

cambodia_full <- cambodia %>%
  mutate(
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  )

cat("\nProcedural orientation by winner/loser status:\n")
cambodia_full %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  group_by(winner_loser) %>%
  summarise(
    n = n(),
    mean_proc = round(mean(procedural_index), 2),
    pct_high_proc = round(mean(procedural_index >= 3) * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nProcedural orientation by winner/loser AND education:\n")
cambodia_full %>%
  filter(!is.na(winner_loser), !is.na(procedural_index), !is.na(edu_level),
         winner_loser %in% c("Winner", "Loser")) %>%
  group_by(edu_level, winner_loser) %>%
  summarise(
    n = n(),
    mean_proc = round(mean(procedural_index), 2),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, mean_proc)) %>%
  print()
