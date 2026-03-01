## ──────────────────────────────────────────────────────────────────────────────
## 07 — Sensitivity Analysis: corrupt_witnessed Alternative Codings
##
## The harmonized binary uses the broadest coding for W4 (categories 1-3 of
## the 5-category q120 item = "witnessed"). This script tests two stricter
## alternatives that affect W4's coding while keeping all other waves at the
## main harmonized value:
##
##   main  : current harmonized (q120 ∈ {1,2,3} = witnessed)
##   alt1  : personal experience only (q120 == 1; strict)
##   alt2  : personal + family member (q120 ∈ {1,2}; intermediate)
##
## For each coding the script:
##   1. Reconstructs the long-format ABS Korea dataset
##   2. Re-estimates the OLS wave × type interaction model (m_factor)
##   3. Runs the W4→W5 divergence contrast (avg_comparisons + hypotheses)
##   4. Compares wave-level % witnessed and DiD estimates across codings
##
## Output: results/sensitivity_corrupt_witnessed.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(haven)
library(marginaleffects)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/07_south_korea_accountability_gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

raw_dir <- "/Users/jeffreystark/Development/Research/survey-data-prep/data/abs/raw"

# ── 1. Load harmonized ABS (Korea) ───────────────────────────────────────────
abs_harm <- readRDS(abs_harmonized_path)
kor_harm <- abs_harm |>
  filter(country == 3) |>
  mutate(wave = as.integer(wave))

cat("Korea harmonized: n =", nrow(kor_harm),
    "| waves:", paste(sort(unique(kor_harm$wave)), collapse = " "), "\n")

# ── 2. Load raw W4 to extract original q120 categories ───────────────────────
w4_raw <- read_sav(file.path(raw_dir, "wave4/W4_v15_merged20250609_release.sav"))
kor4_raw <- w4_raw |>
  filter(country == 3) |>
  transmute(
    # q120: 1=personally witnessed, 2=family, 3=friend, 4=personally never,
    #        7/8/9 = DK/refuse
    q120 = as.integer(q120)
  )

cat("\nW4 Korea raw q120 distribution:\n")
print(table(kor4_raw$q120, useNA = "ifany"))
cat("  1=personally witnessed, 2=family told, 3=friend told, 4=never witnessed\n")

# Verify row counts align with harmonized W4
n_harm_w4 <- sum(kor_harm$wave == 4)
n_raw_w4  <- nrow(kor4_raw)
cat(sprintf("Harmonized W4: %d obs | Raw W4: %d obs\n", n_harm_w4, n_raw_w4))
# The harmonized dataset may have slightly different n due to variable filtering;
# we derive alternative codings from the raw proportions and apply them
# at the wave level rather than merging row-by-row (no shared respondent ID).

# ── 3. Build three wave-level corruption_witnessed series ────────────────────
# For each alternative coding we compute the W4 proportion, then substitute
# it into the per-respondent harmonized variable for W4 rows.
# NOTE: We cannot merge at the respondent level (no shared ID across files),
# so we sample-proportion-match: recode the W4 harmonized column so that
# exactly the alt-coding proportion of W4 respondents are coded as witnessed.
# The ordering within W4 is irrelevant for the regression since we use wave
# means; the re-draw matches marginal proportions exactly.

# Better approach: rebuild from raw proportions using the valid n
valid4  <- kor4_raw |> filter(q120 %in% 1:4)  # exclude DK/refuse
n4      <- nrow(valid4)

prop_main <- mean(valid4$q120 %in% 1:3)   # current: 1,2,3 = witnessed
prop_alt1 <- mean(valid4$q120 == 1)        # strict: personal only
prop_alt2 <- mean(valid4$q120 %in% 1:2)   # intermediate: personal + family

cat(sprintf("\nW4 witnessed proportions under alternative codings:\n"))
cat(sprintf("  main  (1-3=1): %.1f%% (%d / %d)\n",
            100 * prop_main, sum(valid4$q120 %in% 1:3), n4))
cat(sprintf("  alt1  (1=1)  : %.1f%% (%d / %d)\n",
            100 * prop_alt1, sum(valid4$q120 == 1), n4))
cat(sprintf("  alt2  (1-2=1): %.1f%% (%d / %d)\n",
            100 * prop_alt2, sum(valid4$q120 %in% 1:2), n4))

# ── 4. Helper: build long-format dataset under a given W4 coding ──────────────
build_long <- function(kor_df, w4_cw_values) {
  # w4_cw_values: a 0/1 vector of length = number of Korea W4 rows, with
  # the alternative corrupt_witnessed coding for W4.
  df <- kor_df
  w4_rows <- which(df$wave == 4)
  stopifnot(length(w4_rows) == length(w4_cw_values))
  df$corrupt_witnessed[w4_rows] <- w4_cw_values

  df |>
    filter(!is.na(corrupt_national_govt), !is.na(corrupt_local_govt)) |>
    mutate(
      institutional = ((corrupt_national_govt + corrupt_local_govt) / 2 - 1) / 3,
      experiential  = as.double(corrupt_witnessed)
    ) |>
    select(wave, institutional, experiential) |>
    pivot_longer(cols = c(experiential, institutional),
                 names_to = "type", values_to = "corruption") |>
    filter(!is.na(corruption)) |>
    mutate(
      wave_num = as.numeric(wave),
      wave_f   = factor(paste0("w", wave_num), levels = paste0("w", 1:6)),
      type     = factor(type, levels = c("institutional", "experiential"))
    )
}

# Generate W4 recodes: draw random 0/1 vectors matching the target proportions
# reproducibly, preserving the total valid W4 n (respondents with non-missing
# corrupt_national_govt AND corrupt_local_govt).
kor_w4_valid <- kor_harm |>
  filter(wave == 4, !is.na(corrupt_national_govt), !is.na(corrupt_local_govt))
n4v <- nrow(kor_w4_valid)

set.seed(2025)
w4_main <- kor_harm |> filter(wave == 4) |>
  pull(corrupt_witnessed) |> as.double()

# For alt1 and alt2, recode at the valid-row level using raw proportions
make_w4_coding <- function(prop, n) {
  v <- rep(0L, n)
  n1 <- round(prop * n)
  v[sample(n, n1)] <- 1L
  v
}
# Full W4 vector (including rows that may be dropped by filter)
n4_full <- sum(kor_harm$wave == 4)
w4_alt1_vec <- make_w4_coding(prop_alt1, n4_full)
w4_alt2_vec <- make_w4_coding(prop_alt2, n4_full)

# Build three long-format datasets
long_main <- build_long(kor_harm, w4_main)
long_alt1 <- build_long(kor_harm, w4_alt1_vec)
long_alt2 <- build_long(kor_harm, w4_alt2_vec)

# ── 5. Wave-level % for each coding ──────────────────────────────────────────
summarise_waves <- function(long_df, label) {
  long_df |>
    filter(type == "experiential") |>
    group_by(wave_num) |>
    summarise(pct = mean(corruption, na.rm = TRUE),
              n   = sum(!is.na(corruption)), .groups = "drop") |>
    mutate(coding = label)
}

wave_summary <- bind_rows(
  summarise_waves(long_main, "main (1-3=1)"),
  summarise_waves(long_alt1, "alt1 (1=1 only)"),
  summarise_waves(long_alt2, "alt2 (1-2=1)")
) |>
  mutate(wave_label = paste0("W", wave_num, " (", c(2001,2003,2008,2015,2019,2022)[wave_num], ")"))

cat("\n=== Wave-level % witnessed under each coding ===\n")
wave_summary |>
  select(coding, wave_label, pct, n) |>
  mutate(pct = scales::percent(pct, accuracy = 0.1)) |>
  pivot_wider(names_from = coding, values_from = c(pct, n),
              names_glue = "{coding}: {.value}") |>
  print(n = 6)

# ── 6. OLS wave × type model for each coding ─────────────────────────────────
fit_model <- function(long_df) {
  lm(corruption ~ wave_f * type, data = long_df)
}

m_main <- fit_model(long_main)
m_alt1 <- fit_model(long_alt1)
m_alt2 <- fit_model(long_alt2)

# ── 7. W4→W5 DiD contrast: avg_comparisons + hypotheses ─────────────────────
run_contrast <- function(model, label) {
  contrast_raw <- avg_comparisons(
    model,
    variables = "type",
    by        = "wave_f"
  )
  # Rows: b1=w1, b2=w2, b3=w3, b4=w4, b5=w5, b6=w6
  result <- hypotheses(contrast_raw, hypothesis = "b5 - b4 = 0") |>
    as_tibble() |>
    transmute(
      coding   = label,
      estimate = round(estimate, 4),
      std.error = round(std.error, 4),
      statistic = round(statistic, 3),
      p.value   = round(p.value, 4)
    )
  result
}

contrast_summary <- bind_rows(
  run_contrast(m_main, "main (1-3=1)"),
  run_contrast(m_alt1, "alt1 (1=1 only)"),
  run_contrast(m_alt2, "alt2 (1-2=1)")
)

cat("\n=== W4→W5 DiD contrast under each coding ===\n")
cat("(Positive estimate: experiential rises MORE than institutional W4→W5)\n")
cat("(Negative estimate: experiential falls MORE than institutional W4→W5)\n\n")
print(contrast_summary)

# ── 8. Direction check ───────────────────────────────────────────────────────
# Is the W4→W5 direction consistent across codings?
cat("\n=== Direction of W4→W5 change (experiential % witnessed) ===\n")
wave_summary |>
  filter(wave_num %in% 4:5) |>
  select(coding, wave_num, pct) |>
  pivot_wider(names_from = wave_num, values_from = pct,
              names_prefix = "W") |>
  mutate(
    direction = ifelse(W5 > W4, "INCREASE", "DECREASE"),
    change    = scales::percent(W5 - W4, accuracy = 0.1)
  ) |>
  print()

# ── 9. Save ───────────────────────────────────────────────────────────────────
sensitivity_results <- list(
  wave_summary     = wave_summary,
  contrast_summary = contrast_summary,
  w4_proportions   = tibble(
    coding      = c("main (1-3=1)", "alt1 (1=1 only)", "alt2 (1-2=1)"),
    prop_w4     = c(prop_main, prop_alt1, prop_alt2),
    n_witnessed = c(sum(valid4$q120 %in% 1:3),
                    sum(valid4$q120 == 1),
                    sum(valid4$q120 %in% 1:2)),
    n_total     = n4
  )
)

saveRDS(sensitivity_results,
        file.path(results_dir, "sensitivity_corrupt_witnessed.rds"))
cat("\nSensitivity results saved to", results_dir, "\n")
