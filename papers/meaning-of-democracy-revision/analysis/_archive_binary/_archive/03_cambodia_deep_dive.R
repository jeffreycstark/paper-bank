# Cambodia Deep Dive: Who votes for the winning party?
# Testing: Are educated Cambodians more likely to vote for winners (patronage)?

library(tidyverse)
library(haven)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
data_dir <- file.path(project_root, "data/processed")

# Load all waves
w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

# ============================================================================
# Check q34a variable
# ============================================================================
cat("=== Checking q34a (voted for winner/loser) ===\n\n")

cat("W3 q34a exists:", "q34a" %in% names(w3), "\n")
cat("W4 q34a exists:", "q34a" %in% names(w4), "\n")
cat("W6 q34a exists:", "q34a" %in% names(w6), "\n")

# Check labels
cat("\nW3 q34a labels:\n")
if ("q34a" %in% names(w3) && is.labelled(w3$q34a)) {
  print(attr(w3$q34a, "labels"))
}

cat("\nW4 q34a labels:\n")
if ("q34a" %in% names(w4) && is.labelled(w4$q34a)) {
  print(attr(w4$q34a, "labels"))
}

cat("\nW6 q34a labels:\n")
if ("q34a" %in% names(w6) && is.labelled(w6$q34a)) {
  print(attr(w6$q34a, "labels"))
}

# ============================================================================
# Extract Cambodia data with voting and education
# ============================================================================
cat("\n\n=== Extracting Cambodia data ===\n")

# Recode functions
recode_set1 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set2 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)
recode_set3 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set4 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)

get_country_name <- function(x) {
  if (is.character(x)) return(x)
  code <- as.integer(x)
  case_when(code == 10 ~ "Cambodia", TRUE ~ NA_character_)
}

# W3 Cambodia
w3_camb <- w3 %>%
  mutate(country_name = get_country_name(country)) %>%
  filter(country_name == "Cambodia") %>%
  mutate(
    wave = "W3",
    vote_winner = case_when(
      as.integer(q34a) == 1 ~ 1L,  # voted for winner
      as.integer(q34a) == 2 ~ 0L,  # voted for loser
      TRUE ~ NA_integer_
    ),
    education = as.integer(se5),
    edu_level = case_when(
      education %in% 1:3 ~ "Low",
      education %in% 4:6 ~ "Middle", 
      education %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  select(wave, vote_winner, education, edu_level, procedural_index, set1_proc:set4_proc)

cat("W3 Cambodia:", nrow(w3_camb), "rows\n")
cat("W3 q34a distribution:\n")
print(table(w3_camb$vote_winner, useNA = "ifany"))

# W4 Cambodia
w4_camb <- w4 %>%
  mutate(country_name = get_country_name(country)) %>%
  filter(country_name == "Cambodia") %>%
  mutate(
    wave = "W4",
    vote_winner = case_when(
      as.integer(q34a) == 1 ~ 1L,
      as.integer(q34a) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    education = as.integer(se5),
    edu_level = case_when(
      education %in% 1:3 ~ "Low",
      education %in% 4:6 ~ "Middle",
      education %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  select(wave, vote_winner, education, edu_level, procedural_index, set1_proc:set4_proc)

cat("\nW4 Cambodia:", nrow(w4_camb), "rows\n")
cat("W4 q34a distribution:\n")
print(table(w4_camb$vote_winner, useNA = "ifany"))

# W6 Cambodia
w6_camb <- w6 %>%
  filter(country == "Cambodia") %>%
  mutate(
    wave = "W6",
    vote_winner = case_when(
      as.integer(q34a) == 1 ~ 1L,
      as.integer(q34a) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    education = as.integer(se5),
    edu_level = case_when(
      education %in% 1:3 ~ "Low",
      education %in% 4:6 ~ "Middle",
      education %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc
  ) %>%
  select(wave, vote_winner, education, edu_level, procedural_index, set1_proc:set4_proc)

cat("\nW6 Cambodia:", nrow(w6_camb), "rows\n")
cat("W6 q34a distribution:\n")
print(table(w6_camb$vote_winner, useNA = "ifany"))

# Combine
camb_all <- bind_rows(w3_camb, w4_camb, w6_camb)
cat("\nCombined Cambodia:", nrow(camb_all), "rows\n")

# ============================================================================
# ANALYSIS 1: Education and voting for winner
# ============================================================================
cat("\n\n================================================================\n")
cat("HYPOTHESIS: Educated Cambodians vote for winner (patronage)\n")
cat("================================================================\n")

vote_by_edu <- camb_all %>%
  filter(!is.na(vote_winner), !is.na(edu_level)) %>%
  group_by(edu_level) %>%
  summarise(
    n = n(),
    n_voted = sum(!is.na(vote_winner)),
    pct_winner = mean(vote_winner, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(edu_level = factor(edu_level, levels = c("Low", "Middle", "High"))) %>%
  arrange(edu_level)

cat("\nVoted for winning party by education (all waves):\n")
print(vote_by_edu)

# By wave
cat("\nBy wave:\n")
vote_by_edu_wave <- camb_all %>%
  filter(!is.na(vote_winner), !is.na(edu_level)) %>%
  group_by(wave, edu_level) %>%
  summarise(
    n = n(),
    pct_winner = mean(vote_winner, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = edu_level, values_from = c(n, pct_winner))

print(vote_by_edu_wave)

# ============================================================================
# ANALYSIS 2: Cross-tab voting and procedural orientation
# ============================================================================
cat("\n\n================================================================\n")
cat("KEY TEST: Does voting for winner predict procedural orientation?\n")
cat("================================================================\n")

vote_proc <- camb_all %>%
  filter(!is.na(vote_winner), !is.na(procedural_index)) %>%
  mutate(vote_label = if_else(vote_winner == 1, "Voted Winner", "Voted Loser")) %>%
  group_by(vote_label) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index, na.rm = TRUE),
    sd_proc = sd(procedural_index, na.rm = TRUE),
    pct_high_proc = mean(procedural_index >= 3, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nProcedural orientation by vote choice:\n")
print(vote_proc)

# By wave
cat("\nBy wave:\n")
vote_proc_wave <- camb_all %>%
  filter(!is.na(vote_winner), !is.na(procedural_index)) %>%
  mutate(vote_label = if_else(vote_winner == 1, "Winner", "Loser")) %>%
  group_by(wave, vote_label) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = vote_label, values_from = c(n, mean_proc)) %>%
  mutate(diff = mean_proc_Winner - mean_proc_Loser)

print(vote_proc_wave)

# ============================================================================
# ANALYSIS 3: Three-way relationship
# ============================================================================
cat("\n\n================================================================\n")
cat("THREE-WAY: Education × Vote × Procedural\n")
cat("================================================================\n")

three_way <- camb_all %>%
  filter(!is.na(vote_winner), !is.na(edu_level), !is.na(procedural_index)) %>%
  mutate(
    vote_label = if_else(vote_winner == 1, "Winner", "Loser"),
    edu_level = factor(edu_level, levels = c("Low", "Middle", "High"))
  ) %>%
  group_by(edu_level, vote_label) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMean procedural index by education and vote:\n")
three_way %>%
  pivot_wider(names_from = vote_label, values_from = c(n, mean_proc)) %>%
  print()

# ============================================================================
# ANALYSIS 4: Set-by-set breakdown for Cambodia
# ============================================================================
cat("\n\n================================================================\n")
cat("CAMBODIA SET-BY-SET: Which tradeoffs drive the pattern?\n")
cat("================================================================\n")

set_by_vote <- camb_all %>%
  filter(!is.na(vote_winner)) %>%
  mutate(vote_label = if_else(vote_winner == 1, "Winner", "Loser")) %>%
  group_by(vote_label) %>%
  summarise(
    n = n(),
    set1_pct = mean(set1_proc, na.rm = TRUE) * 100,  # elections/expression vs gap/waste
    set2_pct = mean(set2_proc, na.rm = TRUE) * 100,  # oversight/organize vs needs/services
    set3_pct = mean(set3_proc, na.rm = TRUE) * 100,  # media/multiparty vs law/jobs
    set4_pct = mean(set4_proc, na.rm = TRUE) * 100,  # protests/courts vs corruption/unemploy
    .groups = "drop"
  )

cat("\nSet 1: Elections/Expression vs Gap/No Waste\n")
cat("Set 2: Oversight/Organize vs Basic Needs/Services\n")
cat("Set 3: Media/Multiparty vs Law&Order/Jobs\n")
cat("Set 4: Protests/Courts vs Corruption/Unemployment\n\n")
print(set_by_vote)

cat("\nDifference (Winner - Loser) by set:\n")
set_diff <- set_by_vote %>%
  pivot_longer(cols = starts_with("set"), names_to = "set", values_to = "pct") %>%
  pivot_wider(names_from = vote_label, values_from = pct) %>%
  mutate(diff = Winner - Loser)
print(set_diff)

# ============================================================================
# ANALYSIS 5: Education and set-by-set
# ============================================================================
cat("\n\n================================================================\n")
cat("EDUCATION × SET: Where do educated Cambodians differ?\n")
cat("================================================================\n")

set_by_edu <- camb_all %>%
  filter(!is.na(edu_level)) %>%
  mutate(edu_level = factor(edu_level, levels = c("Low", "Middle", "High"))) %>%
  group_by(edu_level) %>%
  summarise(
    n = n(),
    set1 = mean(set1_proc, na.rm = TRUE) * 100,
    set2 = mean(set2_proc, na.rm = TRUE) * 100,
    set3 = mean(set3_proc, na.rm = TRUE) * 100,
    set4 = mean(set4_proc, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n% Procedural by education and set:\n")
print(set_by_edu)

cat("\nGradient (High - Low education):\n")
gradient <- tibble(
  set = c("Set 1 (Elections)", "Set 2 (Oversight)", "Set 3 (Media)", "Set 4 (Protests)"),
  high = as.numeric(set_by_edu[set_by_edu$edu_level == "High", 3:6]),
  low = as.numeric(set_by_edu[set_by_edu$edu_level == "Low", 3:6]),
  gradient = high - low
)
print(gradient)

cat("\n=== DONE ===\n")
