library(tidyverse)
library(haven)

# Load data
d <- readRDS("/Users/jeffreystark/Development/Research/econdev-authpref/data/processed/w6_cambodia_only.rds")

# Convert haven_labelled to numeric and recode
df <- d %>%
  transmute(
    age = as.numeric(SE3_1),
    gender = as.numeric(SE2),
    edu_level = as.numeric(SE5),
    edu_years = as.numeric(SE5a),
    urban_rural = as.numeric(LEVEL)
  ) %>%
  # Recode missing values (97, 98, 99 = NA)
  mutate(
    across(everything(), ~ifelse(. %in% c(97, 98, 99), NA, .))
  ) %>%
  # Create factors with clear labels
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    urban_rural = factor(urban_rural, levels = c(1, 2), labels = c("Rural", "Urban")),
    age_cohort = cut(age,
                     breaks = c(17, 29, 44, 59, 100),
                     labels = c("18-29", "30-44", "45-59", "60+")),
    edu_level_label = factor(edu_level,
                             levels = 1:10,
                             labels = c("No formal education",
                                        "Incomplete primary",
                                        "Complete primary",
                                        "Incomplete secondary",
                                        "Complete secondary",
                                        "Incomplete high school",
                                        "Complete high school",
                                        "Some university",
                                        "University degree",
                                        "Postgraduate"))
  )

cat("===============================================================================\n")
cat("  EDUCATION ANALYSIS: CAMBODIA, ASIAN BAROMETER WAVE 6\n")
cat("===============================================================================\n\n")

cat("Sample sizes after cleaning:\n")
cat("  Total N:", nrow(df), "\n")
cat("  Valid age:", sum(!is.na(df$age)), "\n")
cat("  Valid gender:", sum(!is.na(df$gender)), "\n")
cat("  Valid edu_years:", sum(!is.na(df$edu_years)), "\n")
cat("  Valid urban_rural:", sum(!is.na(df$urban_rural)), "\n\n")

# ============================================
# TABLE 1: Mean years of education
# ============================================
cat("-------------------------------------------------------------------------------\n")
cat("TABLE 1: Mean Years of Formal Education by Gender, Age Cohort, and Location\n")
cat("         Cell format: Mean (SD) [n]\n")
cat("-------------------------------------------------------------------------------\n\n")

table1 <- df %>%
  filter(!is.na(age_cohort), !is.na(gender), !is.na(urban_rural), !is.na(edu_years)) %>%
  group_by(gender, age_cohort, urban_rural) %>%
  summarise(
    n = n(),
    mean = mean(edu_years, na.rm = TRUE),
    sd = sd(edu_years, na.rm = TRUE),
    .groups = "drop"
  )

# Wide format for dissertation
table1_wide <- table1 %>%
  mutate(cell = sprintf("%.1f (%.1f) [%d]", mean, sd, n)) %>%
  select(gender, age_cohort, urban_rural, cell) %>%
  pivot_wider(names_from = urban_rural, values_from = cell)

# Add totals by gender and cohort
table1_totals <- df %>%
  filter(!is.na(age_cohort), !is.na(gender), !is.na(edu_years)) %>%
  group_by(gender, age_cohort) %>%
  summarise(
    n = n(),
    mean = mean(edu_years, na.rm = TRUE),
    sd = sd(edu_years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Total = sprintf("%.1f (%.1f) [%d]", mean, sd, n)) %>%
  select(gender, age_cohort, Total)

table1_final <- table1_wide %>%
  left_join(table1_totals, by = c("gender", "age_cohort")) %>%
  rename(Gender = gender, `Age Cohort` = age_cohort)

print(table1_final, n = 20)

# ============================================
# TABLE 2: Education level by age cohort
# ============================================
cat("\n\n-------------------------------------------------------------------------------\n")
cat("TABLE 2: Highest Education Level by Age Cohort (Column Percentages)\n")
cat("-------------------------------------------------------------------------------\n\n")

table2 <- df %>%
  filter(!is.na(age_cohort), !is.na(edu_level_label)) %>%
  count(age_cohort, edu_level_label, .drop = FALSE) %>%
  group_by(age_cohort) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

table2_wide <- table2 %>%
  mutate(cell = sprintf("%.1f%%", pct)) %>%
  select(edu_level_label, age_cohort, cell) %>%
  pivot_wider(names_from = age_cohort, values_from = cell, values_fill = "0.0%") %>%
  rename(`Education Level` = edu_level_label)

print(table2_wide, n = 12)

# Column Ns
cat("\nColumn N:\n")
col_n <- df %>%
  filter(!is.na(age_cohort), !is.na(edu_level_label)) %>%
  count(age_cohort, name = "N")
print(col_n)

# ============================================
# Summary statistics
# ============================================
cat("\n\n-------------------------------------------------------------------------------\n")
cat("SUMMARY STATISTICS\n")
cat("-------------------------------------------------------------------------------\n\n")

cat("By Age Cohort:\n")
summary_stats <- df %>%
  filter(!is.na(edu_years)) %>%
  group_by(age_cohort) %>%
  summarise(
    n = n(),
    mean = round(mean(edu_years), 1),
    sd = round(sd(edu_years), 1),
    median = median(edu_years),
    .groups = "drop"
  )
print(summary_stats)

cat("\nBy Gender:\n")
df %>%
  filter(!is.na(edu_years), !is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    mean = round(mean(edu_years), 1),
    sd = round(sd(edu_years), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nBy Location:\n")
df %>%
  filter(!is.na(edu_years), !is.na(urban_rural)) %>%
  group_by(urban_rural) %>%
  summarise(
    n = n(),
    mean = round(mean(edu_years), 1),
    sd = round(sd(edu_years), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\n===============================================================================\n")
cat("  END OF ANALYSIS\n")
cat("===============================================================================\n")
