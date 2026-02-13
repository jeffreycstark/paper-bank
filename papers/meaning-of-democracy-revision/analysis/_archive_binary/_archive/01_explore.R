# Meaning of Democracy: Exploratory Analysis
# W3/W4/W6 Harmonization (2010-2022)

library(tidyverse)
library(haven)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
data_dir <- file.path(project_root, "data/processed")
output_dir <- file.path(project_root, "papers/meaning-of-democracy/analysis")

# Load wave data
cat("Loading data...\n")
w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

cat("W3:", nrow(w3), "rows,", ncol(w3), "cols\n")
cat("W4:", nrow(w4), "rows,", ncol(w4), "cols\n")
cat("W6:", nrow(w6), "rows,", ncol(w6), "cols\n")

# Check country variable types
cat("\nCountry variable types:\n")
cat("W3:", class(w3$country), "\n")
cat("W4:", class(w4$country), "\n")
cat("W6:", class(w6$country), "\n")

cat("\nW6 countries:\n")
print(unique(w6$country))

# ============================================================================
# Recode functions
# ============================================================================
recode_set1 <- function(x) {
  case_when(
    x %in% c(2, 4) ~ 1L,  # elections, expression = procedural
    x %in% c(1, 3) ~ 0L,  # gap, no waste = substantive
    TRUE ~ NA_integer_
  )
}

recode_set2 <- function(x) {
  case_when(
    x %in% c(1, 3) ~ 1L,  # oversight, organize = procedural
    x %in% c(2, 4) ~ 0L,  # basic needs, services = substantive
    TRUE ~ NA_integer_
  )
}

recode_set3 <- function(x) {
  case_when(
    x %in% c(2, 4) ~ 1L,  # media, multiparty = procedural
    x %in% c(1, 3) ~ 0L,  # law/order, jobs = substantive
    TRUE ~ NA_integer_
  )
}

recode_set4 <- function(x) {
  case_when(
    x %in% c(1, 3) ~ 1L,  # protests, courts = procedural
    x %in% c(2, 4) ~ 0L,  # corruption, unemployment = substantive
    TRUE ~ NA_integer_
  )
}

# ============================================================================
# Helper to get country name from code or keep as-is if already character
# ============================================================================
get_country_name <- function(x) {
  if (is.character(x)) {
    # Standardize names
    case_when(
      x == "Korea" ~ "South Korea",
      TRUE ~ x
    )
  } else {
    # Numeric codes
    code <- as.integer(x)
    case_when(
      code == 1 ~ "Japan",
      code == 2 ~ "South Korea",
      code == 3 ~ "Mongolia",
      code == 4 ~ "Taiwan",
      code == 5 ~ "Hong Kong",
      code == 6 ~ "China",
      code == 7 ~ "Philippines",
      code == 8 ~ "Thailand",
      code == 9 ~ "Vietnam",
      code == 10 ~ "Cambodia",
      code == 11 ~ "Singapore",
      code == 12 ~ "Myanmar",
      code == 13 ~ "Malaysia",
      code == 14 ~ "Indonesia",
      code == 15 ~ "India",
      TRUE ~ paste0("Code_", code)
    )
  }
}

# ============================================================================
# Extract and recode W3
# ============================================================================
cat("\n--- Processing W3 ---\n")

w3_dem <- w3 %>%
  mutate(
    wave = "W3",
    wave_year = 2010,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + 
              (!is.na(set3_proc)) + (!is.na(set4_proc))
  ) %>%
  select(country_name, wave, wave_year, set1_proc:n_valid)

cat("W3 countries:\n")
print(table(w3_dem$country_name))
cat("\nW3 procedural index distribution:\n")
print(table(w3_dem$procedural_index, useNA = "ifany"))

# ============================================================================
# Extract and recode W4
# ============================================================================
cat("\n--- Processing W4 ---\n")

w4_dem <- w4 %>%
  mutate(
    wave = "W4",
    wave_year = 2014,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + 
              (!is.na(set3_proc)) + (!is.na(set4_proc))
  ) %>%
  select(country_name, wave, wave_year, set1_proc:n_valid)

cat("W4 countries:\n")
print(table(w4_dem$country_name))
cat("\nW4 procedural index distribution:\n")
print(table(w4_dem$procedural_index, useNA = "ifany"))

# ============================================================================
# Extract and recode W6
# ============================================================================
cat("\n--- Processing W6 ---\n")

w6_dem <- w6 %>%
  mutate(
    wave = "W6",
    wave_year = 2020,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + 
              (!is.na(set3_proc)) + (!is.na(set4_proc))
  ) %>%
  select(country_name, wave, wave_year, set1_proc:n_valid)

cat("W6 countries:\n")
print(table(w6_dem$country_name))
cat("\nW6 procedural index distribution:\n")
print(table(w6_dem$procedural_index, useNA = "ifany"))

# ============================================================================
# Combine
# ============================================================================
cat("\n--- Combining Waves ---\n")

dem_panel <- bind_rows(w3_dem, w4_dem, w6_dem) %>%
  filter(n_valid >= 3)  # At least 3 of 4 sets answered

cat("Combined panel:", nrow(dem_panel), "rows\n")
cat("\nCountries by wave:\n")
print(table(dem_panel$country_name, dem_panel$wave))

# ============================================================================
# Analysis 1: Country means
# ============================================================================
cat("\n\n========================================\n")
cat("=== ANALYSIS 1: COUNTRY MEANS ===\n")
cat("========================================\n")

country_wave_means <- dem_panel %>%
  group_by(country_name, wave, wave_year) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index, na.rm = TRUE),
    sd_proc = sd(procedural_index, na.rm = TRUE),
    pct_high_proc = mean(procedural_index >= 3, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 100)

cat("\nMean Procedural Index by Country and Wave:\n")
cat("(0 = fully substantive, 4 = fully procedural)\n\n")
country_wave_means %>%
  arrange(wave, desc(mean_proc)) %>%
  print(n = 50)

# ============================================================================
# Analysis 2: Change over time
# ============================================================================
cat("\n\n========================================\n")
cat("=== ANALYSIS 2: CHANGE OVER TIME ===\n")
cat("========================================\n")

change_summary <- country_wave_means %>%
  select(country_name, wave, mean_proc) %>%
  pivot_wider(names_from = wave, values_from = mean_proc) %>%
  mutate(
    change_w3_w4 = W4 - W3,
    change_w4_w6 = W6 - W4,
    change_w3_w6 = W6 - W3
  ) %>%
  arrange(desc(change_w3_w6))

cat("\nChange in Procedural Orientation:\n")
print(change_summary, width = 120)

# ============================================================================
# Analysis 3: Set-by-set breakdown
# ============================================================================
cat("\n\n========================================\n")
cat("=== ANALYSIS 3: SET-BY-SET BREAKDOWN ===\n")
cat("========================================\n")

cat("\nSet descriptions:\n")
cat("Set 1: Elections/Expression vs Narrow Gap/No Waste\n")
cat("Set 2: Legislature Oversight/Organize vs Basic Needs/Public Services\n")
cat("Set 3: Media Freedom/Multiparty vs Law&Order/Jobs\n")
cat("Set 4: Protests/Courts vs Clean Politics/Unemployment Aid\n")

set_breakdown <- dem_panel %>%
  group_by(country_name, wave) %>%
  summarise(
    n = n(),
    set1_pct = mean(set1_proc, na.rm = TRUE) * 100,
    set2_pct = mean(set2_proc, na.rm = TRUE) * 100,
    set3_pct = mean(set3_proc, na.rm = TRUE) * 100,
    set4_pct = mean(set4_proc, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 100)

cat("\n% Choosing Procedural Option by Set (W6 only, sorted by Set1):\n")
set_breakdown %>%
  filter(wave == "W6") %>%
  arrange(desc(set1_pct)) %>%
  print()

cat("\n% Choosing Procedural Option by Set (W3 only, sorted by Set1):\n")
set_breakdown %>%
  filter(wave == "W3") %>%
  arrange(desc(set1_pct)) %>%
  print()

# ============================================================================
# Analysis 4: Interesting patterns
# ============================================================================
cat("\n\n========================================\n")
cat("=== ANALYSIS 4: KEY FINDINGS ===\n")
cat("========================================\n")

# Overall by set and wave
overall_by_set <- dem_panel %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    set1 = mean(set1_proc, na.rm = TRUE) * 100,
    set2 = mean(set2_proc, na.rm = TRUE) * 100,
    set3 = mean(set3_proc, na.rm = TRUE) * 100,
    set4 = mean(set4_proc, na.rm = TRUE) * 100,
    overall = mean(procedural_index, na.rm = TRUE) / 4 * 100,
    .groups = "drop"
  )

cat("\nOverall % procedural by set and wave:\n")
print(overall_by_set)

# Most procedural countries
cat("\nMost procedural countries (W3):\n")
country_wave_means %>%
  filter(wave == "W3") %>%
  arrange(desc(mean_proc)) %>%
  head(5) %>%
  print()

cat("\nMost procedural countries (W6):\n")
country_wave_means %>%
  filter(wave == "W6") %>%
  arrange(desc(mean_proc)) %>%
  head(5) %>%
  print()

# Most substantive countries
cat("\nMost substantive countries (W3):\n")
country_wave_means %>%
  filter(wave == "W3") %>%
  arrange(mean_proc) %>%
  head(5) %>%
  print()

cat("\nMost substantive countries (W6):\n")
country_wave_means %>%
  filter(wave == "W6") %>%
  arrange(mean_proc) %>%
  head(5) %>%
  print()

# ============================================================================
# Save results
# ============================================================================
cat("\n\n========================================\n")
cat("=== SAVING RESULTS ===\n")
cat("========================================\n")

saveRDS(dem_panel, file.path(output_dir, "dem_panel_w3w4w6.rds"))
saveRDS(country_wave_means, file.path(output_dir, "country_wave_means.rds"))
saveRDS(set_breakdown, file.path(output_dir, "set_breakdown.rds"))

write_csv(country_wave_means, file.path(output_dir, "country_wave_means.csv"))
write_csv(set_breakdown, file.path(output_dir, "set_breakdown.csv"))

cat("Saved:\n")
cat("  - dem_panel_w3w4w6.rds (", nrow(dem_panel), " observations)\n")
cat("  - country_wave_means.rds/.csv\n")
cat("  - set_breakdown.rds/.csv\n")

cat("\n=== DONE ===\n")
