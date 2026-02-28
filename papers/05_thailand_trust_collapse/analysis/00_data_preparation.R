# 00_data_preparation.R
# Thailand Trust Collapse — Data Preparation
#
# Loads ABS harmonized data, filters to the three countries with W1-W6 coverage
# (Thailand, Philippines, Taiwan), creates derived variables, and saves
# thailand_panel.rds for downstream analysis.
#
# Usage: Rscript papers/05_thailand_trust_collapse/analysis/00_data_preparation.R

library(tidyverse)

# ── Load source data ──────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
analysis_dir <- file.path(project_root, "papers/05_thailand_trust_collapse/analysis")

source(file.path(project_root, "_data_config.R"))
abs_source <- readRDS(abs_harmonized_path)

cat("Source: abs_harmonized.rds\n")
cat("Rows:", format(nrow(abs_source), big.mark = ","), "\n")
cat("Columns:", ncol(abs_source), "\n")

# ── Filter to W1-W6 countries ────────────────────────────────────────────────
# Thailand (8), Philippines (6), Taiwan (7) — the three with all six waves

w1_countries <- c(6, 7, 8)

country_lookup <- tribble(
  ~country, ~country_name,
  6, "Philippines",
  7, "Taiwan",
  8, "Thailand"
)

d <- abs_source %>%
  filter(country %in% w1_countries) %>%
  left_join(country_lookup, by = "country")

cat("\nFiltered to W1-W6 countries:", format(nrow(d), big.mark = ","), "obs\n")

# ── Select variables ─────────────────────────────────────────────────────────

vars_to_select <- c(
  # Identifiers
  "country", "country_name", "wave", "idnumber",

  # Trust (6 primary institutions + NGOs + local govt for breadth analysis)
  "trust_national_government", "trust_parliament", "trust_military",
  "trust_courts", "trust_police", "trust_political_parties",
  "trust_ngos", "trust_local_government",

  # Democratic attitudes
  "democracy_satisfaction", "dem_always_preferable", "dem_vs_econ",

  # Authoritarian preferences
  "strongman_rule", "expert_rule", "military_rule", "single_party_rule",

  # Economic perceptions
  "econ_national_now", "econ_outlook_1yr",

  # Controls
  "age", "gender", "education_years", "urban_rural",

  # Survey weights
  "weight"
)

available <- intersect(vars_to_select, names(d))
missing <- setdiff(vars_to_select, names(d))

cat("Variables:", length(available), "available,", length(missing), "missing\n")
if (length(missing) > 0) cat("Missing:", paste(missing, collapse = ", "), "\n")

d <- d %>% select(all_of(available))

# ── Derived variables ────────────────────────────────────────────────────────

d <- d %>%
  mutate(
    # Wave as integer (source data uses numeric 1-6)
    wave_num = as.integer(wave),
    survey_year = case_when(
      wave_num == 1 ~ 2002, wave_num == 2 ~ 2006, wave_num == 3 ~ 2011,
      wave_num == 4 ~ 2015, wave_num == 5 ~ 2019, wave_num == 6 ~ 2022
    ),

    # Demographics
    female = if_else(gender == 2, 1L, 0L),
    is_urban = if_else(urban_rural == 1, 1L, 0L),
    age_centered = age - mean(age, na.rm = TRUE),
    education_z = (education_years - mean(education_years, na.rm = TRUE)) /
                  sd(education_years, na.rm = TRUE),

    # Time indicators
    pre_covid = if_else(wave_num %in% 1:4, 1L, 0L),
    post_covid = if_else(wave_num == 6, 1L, 0L),

    # Country factor
    country_name = factor(country_name,
                          levels = c("Thailand", "Philippines", "Taiwan")),

    # Wave labels
    wave_label = factor(wave_num,
      levels = 1:6,
      labels = c("W1\n(2001-03)", "W2\n(2005-08)", "W3\n(2010-12)",
                 "W4\n(2014-16)", "W5\n(2018-20)", "W6\n(2020-22)")
    )
  )

# ── Survey weights ───────────────────────────────────────────────────────────
# Weights available W3-W6; set to 1 (unweighted) for W1-W2

d <- d %>%
  mutate(weight = if_else(is.na(weight), 1, weight))

cat("\nWeight coverage:\n")
d %>%
  group_by(wave) %>%
  summarise(pct_wt1 = round(mean(weight == 1) * 100, 1),
            mean_wt = round(mean(weight), 3),
            sd_wt = round(sd(weight), 3),
            .groups = "drop") %>%
  print()

# ── Trust composites ─────────────────────────────────────────────────────────

d <- d %>%
  rowwise() %>%
  mutate(
    trust_political = mean(c(trust_national_government, trust_parliament,
                             trust_political_parties), na.rm = TRUE),
    trust_nonpolitical = mean(c(trust_courts, trust_military, trust_police),
                              na.rm = TRUE),
    trust_all = mean(c(trust_national_government, trust_parliament,
                       trust_courts, trust_military, trust_police,
                       trust_political_parties), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(c(trust_political, trust_nonpolitical, trust_all),
                ~if_else(is.nan(.), NA_real_, .)))

# ── Save ─────────────────────────────────────────────────────────────────────

saveRDS(d, file.path(analysis_dir, "thailand_panel.rds"))

cat("\n=== DATASET SUMMARY ===\n")
cat("Saved:", file.path(analysis_dir, "thailand_panel.rds"), "\n")
cat("Rows:", format(nrow(d), big.mark = ","), "\n")
cat("Columns:", ncol(d), "\n")
cat("Countries:", paste(levels(d$country_name), collapse = ", "), "\n")
cat("Waves:", paste(sort(unique(d$wave)), collapse = ", "), "\n")

# Validate by country x wave
cat("\n=== SAMPLE SIZE BY COUNTRY × WAVE ===\n")
print(table(d$country_name, d$wave))

# Key trust means for Thailand
cat("\n=== THAILAND TRUST BY WAVE ===\n")
d %>%
  filter(country_name == "Thailand") %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    trust_govt = round(mean(trust_national_government, na.rm = TRUE), 2),
    trust_mil = round(mean(trust_military, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()
