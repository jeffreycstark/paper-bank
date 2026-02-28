# Check winner/loser variable across waves
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"

w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

cat("=== WINNER/LOSER VARIABLE AVAILABILITY ===\n\n")

# W3
cat("W3:\n")
cat("  q34a exists:", "q34a" %in% names(w3), "\n")
# Look for similar variables
w3_vote_vars <- names(w3)[str_detect(tolower(names(w3)), "win|lose|vote|q34")]
cat("  Related vars:", paste(w3_vote_vars, collapse = ", "), "\n")

if ("q34a" %in% names(w3)) {
  cat("  q34a label:", attr(w3$q34a, "label"), "\n")
  cat("  Distribution:\n")
  print(table(as.integer(w3$q34a), useNA = "ifany"))
}

# W4
cat("\nW4:\n")
cat("  q34a exists:", "q34a" %in% names(w4), "\n")
w4_vote_vars <- names(w4)[str_detect(tolower(names(w4)), "win|lose|vote|q34")]
cat("  Related vars:", paste(w4_vote_vars, collapse = ", "), "\n")

if ("q34a" %in% names(w4)) {
  cat("  q34a label:", attr(w4$q34a, "label"), "\n")
  cat("  Distribution:\n")
  print(table(as.integer(w4$q34a), useNA = "ifany"))
}

# W6
cat("\nW6:\n")
cat("  q34a exists:", "q34a" %in% names(w6), "\n")
w6_vote_vars <- names(w6)[str_detect(tolower(names(w6)), "win|lose|vote|q34")]
cat("  Related vars:", paste(w6_vote_vars, collapse = ", "), "\n")

if ("q34a" %in% names(w6)) {
  cat("  q34a label:", attr(w6$q34a, "label"), "\n")
  cat("  Distribution:\n")
  print(table(as.integer(w6$q34a), useNA = "ifany"))
}

# Check W4 in detail since we know it has the variable
cat("\n\n=== W4 DETAILED CHECK ===\n")
cat("W4 q34a value labels:\n")
if (is.labelled(w4$q34a)) {
  labels <- attr(w4$q34a, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

# Check by country for W4
cat("\nW4 winner/loser by country:\n")
w4 %>%
  mutate(
    country_code = as.integer(country),
    country_name = case_when(
      country_code == 1 ~ "Japan", country_code == 2 ~ "South Korea", 
      country_code == 3 ~ "Mongolia", country_code == 4 ~ "Taiwan",
      country_code == 7 ~ "Philippines", country_code == 8 ~ "Thailand",
      country_code == 9 ~ "Vietnam", country_code == 10 ~ "Cambodia",
      country_code == 14 ~ "Indonesia", TRUE ~ paste0("Code_", country_code)
    ),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ "Other/NA"
    )
  ) %>%
  filter(winner_loser %in% c("Winner", "Loser")) %>%
  count(country_name, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n, values_fill = 0) %>%
  mutate(
    total = Winner + Loser,
    pct_winner = round(Winner / total * 100, 1)
  ) %>%
  arrange(desc(pct_winner)) %>%
  print(n = 20)
