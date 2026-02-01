# Check W3 vote choice variable and see if we can construct winner/loser
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"
w3 <- readRDS(file.path(data_dir, "w3.rds"))

cat("=== W3 VOTE CHOICE VARIABLE (q34) ===\n\n")

cat("q34 label:\n")
print(attr(w3$q34, "label"))

cat("\nq34 value labels:\n")
if (is.labelled(w3$q34)) {
  labels <- attr(w3$q34, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\nq34 distribution:\n")
print(table(as.integer(w3$q34), useNA = "ifany"))

# Check if there's a party-specific variable
cat("\n\nLooking for party choice variables...\n")
party_vars <- names(w3)[str_detect(tolower(names(w3)), "party|vote|q34")]
print(party_vars)

# Check q34 by country
cat("\n\n=== Q34 BY COUNTRY ===\n")

get_country_name <- function(code) {
  case_when(
    code == 1 ~ "Japan", code == 2 ~ "South Korea", code == 3 ~ "Mongolia",
    code == 4 ~ "Taiwan", code == 5 ~ "Hong Kong", code == 6 ~ "China",
    code == 7 ~ "Philippines", code == 8 ~ "Thailand", code == 9 ~ "Vietnam",
    code == 10 ~ "Cambodia", code == 11 ~ "Singapore", code == 12 ~ "Myanmar",
    code == 13 ~ "Malaysia", code == 14 ~ "Indonesia", TRUE ~ NA_character_
  )
}

w3_by_country <- w3 %>%
  mutate(
    country_code = as.integer(country),
    country_name = get_country_name(country_code),
    q34_val = as.integer(q34)
  )

# Show distribution by country
for (ctry in unique(w3_by_country$country_name)) {
  if (!is.na(ctry)) {
    cat("\n---", ctry, "---\n")
    ctry_data <- w3_by_country %>% filter(country_name == ctry)
    print(table(ctry_data$q34_val, useNA = "ifany"))
  }
}

# Also check W4 q34 to see if it's the same structure
cat("\n\n=== W4 Q34 FOR COMPARISON ===\n")
w4 <- readRDS(file.path(data_dir, "w4.rds"))

cat("W4 q34 label:\n")
print(attr(w4$q34, "label"))

cat("\nW4 q34 value labels:\n")
if (is.labelled(w4$q34)) {
  labels <- attr(w4$q34, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

# Check if W4 has country-specific party codes
cat("\n\nW4 q34 by country (first few):\n")
w4 %>%
  mutate(
    country_code = as.integer(country),
    country_name = get_country_name(country_code),
    q34_val = as.integer(q34)
  ) %>%
  filter(country_name == "Thailand") %>%
  pull(q34_val) %>%
  table(useNA = "ifany") %>%
  print()
