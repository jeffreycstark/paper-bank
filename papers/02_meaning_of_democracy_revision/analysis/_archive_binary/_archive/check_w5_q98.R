# Check W5 q98 - the "essential element" question
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"
w5 <- readRDS(file.path(data_dir, "w5.rds"))

cat("=== W5 Q98: Essential Element of Democracy ===\n\n")

cat("Full label:\n")
print(attr(w5$q98, "label"))

cat("\nValue labels:\n")
if (is.labelled(w5$q98)) {
  labels <- attr(w5$q98, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\nDistribution:\n")
print(table(as.integer(w5$q98), useNA = "ifany"))

# So W5 is NOT comparable to W3/W4/W6 for the "meaning of democracy" battery
# But it DOES have q34a for winner/loser

cat("\n\n=== SUMMARY OF AVAILABLE DATA ===\n")
cat("\nWINNER/LOSER + 4-SET DEMOCRACY MEANING:\n")
cat("  W3 (2010): Has 4-set battery, NO winner/loser ❌\n")
cat("  W4 (2014): Has 4-set battery, HAS winner/loser ✅\n")
cat("  W5 (2016-17): Different battery, HAS winner/loser ❌\n")
cat("  W6 (2019-22): Has 4-set battery, HAS winner/loser ✅\n")

cat("\n\nConclusion: Can only use W4 + W6 for winner/loser × procedural analysis\n")

# Let's at least see what W5 winner/loser looks like
cat("\n\n=== W5 WINNER/LOSER DISTRIBUTION ===\n")

w5_summary <- w5 %>%
  mutate(
    country_code = as.integer(COUNTRY),
    country_name = case_when(
      country_code == 1 ~ "Japan",
      country_code == 2 ~ "Hong Kong",
      country_code == 3 ~ "South Korea",
      country_code == 4 ~ "China",
      country_code == 5 ~ "Mongolia",
      country_code == 6 ~ "Philippines",
      country_code == 7 ~ "Taiwan",
      country_code == 8 ~ "Thailand",
      country_code == 9 ~ "Indonesia",
      country_code == 10 ~ "Singapore",
      country_code == 11 ~ "Vietnam",
      country_code == 12 ~ "Cambodia",
      country_code == 13 ~ "Malaysia",
      country_code == 14 ~ "Myanmar",
      country_code == 15 ~ "Australia",
      country_code == 18 ~ "India",
      TRUE ~ NA_character_
    ),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(winner_loser)) %>%
  count(country_name, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n, values_fill = 0) %>%
  mutate(
    total = Winner + Loser,
    pct_winner = round(Winner / total * 100, 1)
  ) %>%
  arrange(desc(pct_winner))

cat("\nW5 (2016-17) winner/loser by country:\n")
print(w5_summary, n = 20)

# Timing note
cat("\n\n=== TIMING CONTEXT ===\n")
cat("\nW4: 2014 - Thailand coup happened May 2014\n")
cat("W5: 2016-2017 - Post-coup Thailand, pre-CNRP dissolution Cambodia\n")
cat("W6: 2019-2022 - Post-CNRP dissolution Cambodia (2017), continued military rule Thailand\n")
