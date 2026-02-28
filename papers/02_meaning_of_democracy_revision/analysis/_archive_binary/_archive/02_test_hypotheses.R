# Meaning of Democracy: Testing the "Naive Proceduralist" Hypothesis
# Simplified version with better variable handling

library(tidyverse)
library(haven)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
data_dir <- file.path(project_root, "data/processed")
output_dir <- file.path(project_root, "papers/meaning-of-democracy/analysis")

cat("Loading data...\n")
w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

# ============================================================================
# Check W4 age variable
# ============================================================================
cat("\n--- Checking W4 age/birth year variables ---\n")
cat("W4 se3_1 sample:\n")
print(head(as.integer(w4$se3_1), 10))
cat("W4 se3_2 sample:\n") 
print(head(as.integer(w4$se3_2), 10))

# se3_1 looks like birth year (4-digit years), se3_2 might be age
# Let's verify
cat("\nW4 se3_1 range:", range(as.integer(w4$se3_1), na.rm = TRUE), "\n")
cat("W4 se3_2 range:", range(as.integer(w4$se3_2), na.rm = TRUE), "\n")

# ============================================================================
# Recode functions
# ============================================================================
recode_set1 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set2 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)
recode_set3 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set4 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)

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
      code == 13 ~ "Malaysia", code == 14 ~ "Indonesia", TRUE ~ paste0("Code_", code)
    )
  }
}

# ============================================================================
# Process each wave
# ============================================================================
cat("\n--- Processing W3 ---\n")
w3_full <- w3 %>%
  mutate(
    wave = "W3", wave_year = 2010,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + (!is.na(set3_proc)) + (!is.na(set4_proc)),
    female = case_when(as.integer(se2) == 2 ~ 1L, as.integer(se2) == 1 ~ 0L, TRUE ~ NA_integer_),
    birth_year = as.integer(se3),
    age = 2010 - birth_year,
    education = as.integer(se5)
  ) %>%
  mutate(
    age_cohort = case_when(age < 30 ~ "18-29", age < 45 ~ "30-44", age < 60 ~ "45-59", age >= 60 ~ "60+", TRUE ~ NA_character_),
    edu_level = case_when(education %in% 1:3 ~ "Low", education %in% 4:6 ~ "Middle", education %in% 7:10 ~ "High", TRUE ~ NA_character_)
  ) %>%
  select(country_name, wave, wave_year, set1_proc:procedural_index, n_valid, female, age, age_cohort, education, edu_level)
cat("W3:", nrow(w3_full), "rows\n")

cat("\n--- Processing W4 ---\n")
w4_full <- w4 %>%
  mutate(
    wave = "W4", wave_year = 2014,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + (!is.na(set3_proc)) + (!is.na(set4_proc)),
    female = case_when(as.integer(se2) == 2 ~ 1L, as.integer(se2) == 1 ~ 0L, TRUE ~ NA_integer_),
    birth_year = as.integer(se3_1),  # W4 uses se3_1 for birth year
    age = 2014 - birth_year,
    education = as.integer(se5)
  ) %>%
  mutate(
    age_cohort = case_when(age < 30 ~ "18-29", age < 45 ~ "30-44", age < 60 ~ "45-59", age >= 60 ~ "60+", TRUE ~ NA_character_),
    edu_level = case_when(education %in% 1:3 ~ "Low", education %in% 4:6 ~ "Middle", education %in% 7:10 ~ "High", TRUE ~ NA_character_)
  ) %>%
  select(country_name, wave, wave_year, set1_proc:procedural_index, n_valid, female, age, age_cohort, education, edu_level)
cat("W4:", nrow(w4_full), "rows\n")

cat("\n--- Processing W6 ---\n")
w6_full <- w6 %>%
  mutate(
    wave = "W6", wave_year = 2020,
    country_name = get_country_name(country),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    n_valid = (!is.na(set1_proc)) + (!is.na(set2_proc)) + (!is.na(set3_proc)) + (!is.na(set4_proc)),
    female = case_when(as.integer(se2) == 2 ~ 1L, as.integer(se2) == 1 ~ 0L, TRUE ~ NA_integer_),
    birth_year = as.integer(se3),
    age = 2020 - birth_year,
    education = as.integer(se5),
    urban = case_when(as.integer(se14) %in% c(1, 2) ~ 1L, as.integer(se14) %in% c(3, 4) ~ 0L, TRUE ~ NA_integer_)
  ) %>%
  mutate(
    age_cohort = case_when(age < 30 ~ "18-29", age < 45 ~ "30-44", age < 60 ~ "45-59", age >= 60 ~ "60+", TRUE ~ NA_character_),
    edu_level = case_when(education %in% 1:3 ~ "Low", education %in% 4:6 ~ "Middle", education %in% 7:10 ~ "High", TRUE ~ NA_character_)
  ) %>%
  select(country_name, wave, wave_year, set1_proc:procedural_index, n_valid, female, age, age_cohort, education, edu_level, urban)
cat("W6:", nrow(w6_full), "rows\n")

# ============================================================================
# Combine
# ============================================================================
cat("\n--- Combining ---\n")
w3_full$urban <- NA_integer_
w4_full$urban <- NA_integer_

dem_full <- bind_rows(w3_full, w4_full, w6_full) %>%
  filter(n_valid >= 3, is.na(age) | (age >= 18 & age <= 100))

cat("Combined:", nrow(dem_full), "rows\n")
cat("Age coverage:", sum(!is.na(dem_full$age)), "\n")
cat("Education coverage:", sum(!is.na(dem_full$edu_level)), "\n")

# ============================================================================
# ANALYSIS 1: Education
# ============================================================================
cat("\n\n================================================================\n")
cat("HYPOTHESIS 1: More educated = MORE SUBSTANTIVE\n")
cat("================================================================\n")

edu_overall <- dem_full %>%
  filter(!is.na(edu_level), !is.na(procedural_index)) %>%
  mutate(edu_level = factor(edu_level, levels = c("Low", "Middle", "High"))) %>%
  group_by(edu_level) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop")

cat("\nOverall education effect:\n")
print(edu_overall)

edu_by_country <- dem_full %>%
  filter(!is.na(edu_level), !is.na(procedural_index)) %>%
  group_by(country_name, edu_level) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop") %>%
  pivot_wider(names_from = edu_level, values_from = c(n, mean_proc)) %>%
  mutate(edu_gradient = mean_proc_High - mean_proc_Low) %>%
  arrange(edu_gradient)

cat("\nEducation gradient by country (NEGATIVE = educated more substantive):\n")
print(edu_by_country, width = 120)

# ============================================================================
# ANALYSIS 2: Age
# ============================================================================
cat("\n\n================================================================\n")
cat("HYPOTHESIS 2: Older = MORE PROCEDURAL, Younger = MORE SUBSTANTIVE\n")
cat("================================================================\n")

age_overall <- dem_full %>%
  filter(!is.na(age_cohort), !is.na(procedural_index)) %>%
  mutate(age_cohort = factor(age_cohort, levels = c("18-29", "30-44", "45-59", "60+"))) %>%
  group_by(age_cohort) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop")

cat("\nOverall age effect:\n")
print(age_overall)

age_by_country <- dem_full %>%
  filter(!is.na(age_cohort), !is.na(procedural_index)) %>%
  group_by(country_name, age_cohort) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop") %>%
  pivot_wider(names_from = age_cohort, values_from = c(n, mean_proc)) %>%
  mutate(age_gradient = `mean_proc_60+` - `mean_proc_18-29`) %>%
  arrange(desc(age_gradient))

cat("\nAge gradient by country (POSITIVE = old more procedural):\n")
print(age_by_country, width = 150)

# ============================================================================
# ANALYSIS 3: Urban (W6 only)
# ============================================================================
cat("\n\n================================================================\n")
cat("HYPOTHESIS 3: Urban = MORE SUBSTANTIVE (W6 only)\n")
cat("================================================================\n")

urban_overall <- dem_full %>%
  filter(wave == "W6", !is.na(urban), !is.na(procedural_index)) %>%
  mutate(location = if_else(urban == 1, "Urban", "Rural")) %>%
  group_by(location) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop")

cat("\nOverall urban effect:\n")
print(urban_overall)

urban_by_country <- dem_full %>%
  filter(wave == "W6", !is.na(urban), !is.na(procedural_index)) %>%
  mutate(location = if_else(urban == 1, "Urban", "Rural")) %>%
  group_by(country_name, location) %>%
  summarise(n = n(), mean_proc = mean(procedural_index), .groups = "drop") %>%
  pivot_wider(names_from = location, values_from = c(n, mean_proc)) %>%
  mutate(urban_effect = mean_proc_Urban - mean_proc_Rural) %>%
  arrange(urban_effect)

cat("\nUrban effect by country (NEGATIVE = urban more substantive):\n")
print(urban_by_country, width = 120)

# ============================================================================
# ANALYSIS 4: Country-level democratic experience
# ============================================================================
cat("\n\n================================================================\n")
cat("COUNTRY-LEVEL: More democratic experience = MORE SUBSTANTIVE?\n")
cat("================================================================\n")

dem_experience <- tribble(
  ~country_name, ~dem_years,
  "Australia", 120, "Japan", 75, "South Korea", 35, "Taiwan", 35,
  "Philippines", 35, "Thailand", 30, "Mongolia", 30, "Indonesia", 25,
  "Cambodia", 5, "Vietnam", 0, "Myanmar", 10, "Singapore", 0,
  "Malaysia", 5, "China", 0, "Hong Kong", 0
)

country_means <- dem_full %>%
  filter(wave == "W6") %>%
  group_by(country_name) %>%
  summarise(n = n(), mean_proc = mean(procedural_index, na.rm = TRUE), .groups = "drop") %>%
  left_join(dem_experience, by = "country_name") %>%
  filter(!is.na(dem_years), n >= 100) %>%
  arrange(dem_years)

cat("\nCountry data:\n")
print(country_means)

if (nrow(country_means) >= 4) {
  cor_result <- cor.test(country_means$dem_years, country_means$mean_proc)
  cat("\nCorrelation: Democratic years vs Procedural index\n")
  cat("  r =", round(cor_result$estimate, 3), "\n")
  cat("  p =", round(cor_result$p.value, 3), "\n")
  cat("\n  NEGATIVE r = More experience → MORE SUBSTANTIVE (supports hypothesis)\n")
}

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n================================================================\n")
cat("SUMMARY OF FINDINGS\n")
cat("================================================================\n")

# Education summary
edu_neg <- sum(edu_by_country$edu_gradient < 0, na.rm = TRUE)
edu_total <- sum(!is.na(edu_by_country$edu_gradient))
cat("\n1. EDUCATION: ", edu_neg, "/", edu_total, " countries show educated = MORE SUBSTANTIVE\n")

# Age summary  
age_pos <- sum(age_by_country$age_gradient > 0, na.rm = TRUE)
age_total <- sum(!is.na(age_by_country$age_gradient))
cat("2. AGE: ", age_pos, "/", age_total, " countries show older = MORE PROCEDURAL\n")

# Urban summary
urban_neg <- sum(urban_by_country$urban_effect < 0, na.rm = TRUE)
urban_total <- sum(!is.na(urban_by_country$urban_effect))
cat("3. URBAN: ", urban_neg, "/", urban_total, " countries show urban = MORE SUBSTANTIVE\n")

# ============================================================================
# Save
# ============================================================================
cat("\n\n--- Saving ---\n")
saveRDS(dem_full, file.path(output_dir, "dem_full_with_demographics.rds"))
write_csv(edu_by_country, file.path(output_dir, "education_by_country.csv"))
write_csv(age_by_country, file.path(output_dir, "age_by_country.csv"))
write_csv(urban_by_country, file.path(output_dir, "urban_by_country.csv"))
write_csv(country_means, file.path(output_dir, "country_dem_experience.csv"))
cat("Done!\n")
