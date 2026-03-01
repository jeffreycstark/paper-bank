# =============================================================================
# Script 15: Thailand Political Interest Sensitivity Check
# Goal: Does adding political_interest as a control change the Thailand-specific
#       loser AMEs on procedural items? Replicates the data prep pipeline for
#       Thailand only, adding political_interest from the harmonized ABS.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(nnet)
  library(marginaleffects)
})

here::i_am("papers/02_meaning_of_democracy_revision/manuscript/md-manuscript.qmd")
results_path <- here("papers", "02_meaning_of_democracy_revision", "analysis", "revised", "results")

# --- 1. Item metadata (mirrors 01_data_preparation_mlogit.qmd) --------------
set1_items <- tibble(
  value = 1:4,
  item_label = c("Reduce gap rich/poor", "Free elections", "No waste", "Free expression"),
  item_type  = c("substantive", "procedural", "governance", "procedural")
)
set2_items <- tibble(
  value = 1:4,
  item_label = c("Legislature oversight", "Basic necessities", "Organize groups", "Quality services"),
  item_type  = c("procedural", "substantive", "procedural", "governance")
)
set3_items <- tibble(
  value = 1:4,
  item_label = c("Law and order", "Media freedom", "Jobs for all", "Party competition"),
  item_type  = c("governance", "procedural", "substantive", "procedural")
)
set4_items <- tibble(
  value = 1:4,
  item_label = c("Protest freedom", "Clean politics", "Court protection", "Unemployment aid"),
  item_type  = c("procedural", "governance", "procedural", "substantive")
)

all_items <- bind_rows(
  set1_items %>% mutate(set = "Set1"),
  set2_items %>% mutate(set = "Set2"),
  set3_items %>% mutate(set = "Set3"),
  set4_items %>% mutate(set = "Set4")
)

proc_labels <- all_items %>% filter(item_type == "procedural") %>% pull(item_label)

# --- 2. Load and filter harmonized ABS (mirrors 01_data_preparation_mlogit.qmd) ---
source(here("_data_config.R"))
abs_full <- readRDS(abs_harmonized_path)

thai <- abs_full %>%
  filter(
    country == 8,           # Thailand
    wave %in% c(3, 4, 6),
    voted_last_election == 1,
    !is.na(electoral_status)
  ) %>%
  mutate(
    wave_label = paste0("W", wave),
    loser      = if_else(electoral_status == 2, 1L, 0L),
    # Demographic controls (mirrors data prep)
    age       = as.numeric(age),
    female    = if_else(gender == 2, 1L, 0L),
    education = as.numeric(education_level),
    urban     = if_else(urban_rural == 1, 1L, 0L),
    # Factor choice variables
    set1_valid  = dem_meaning_set1 %in% 1:4,
    set1_choice = if_else(set1_valid,
                          factor(dem_meaning_set1, levels = 1:4, labels = set1_items$item_label),
                          NA_character_) %>% factor(levels = set1_items$item_label),
    set2_valid  = dem_meaning_set2 %in% 1:4,
    set2_choice = if_else(set2_valid,
                          factor(dem_meaning_set2, levels = 1:4, labels = set2_items$item_label),
                          NA_character_) %>% factor(levels = set2_items$item_label),
    set3_valid  = dem_meaning_set3 %in% 1:4,
    set3_choice = if_else(set3_valid,
                          factor(dem_meaning_set3, levels = 1:4, labels = set3_items$item_label),
                          NA_character_) %>% factor(levels = set3_items$item_label),
    set4_valid  = dem_meaning_set4 %in% 1:4,
    set4_choice = if_else(set4_valid,
                          factor(dem_meaning_set4, levels = 1:4, labels = set4_items$item_label),
                          NA_character_) %>% factor(levels = set4_items$item_label)
  )

cat(sprintf("Thailand observations (W3/4/6, voters): %d\n", nrow(thai)))
cat(sprintf("political_interest non-missing: %d (%.1f%%)\n",
            sum(!is.na(thai$political_interest)),
            mean(!is.na(thai$political_interest)) * 100))
cat("\nCoverage by wave:\n")
thai %>%
  group_by(wave) %>%
  summarise(n = n(), n_pi = sum(!is.na(political_interest)),
            pct_pi = mean(!is.na(political_interest)) * 100, .groups = "drop") %>%
  print()

# --- 3. Model runner --------------------------------------------------------
run_models <- function(data, choice_col, valid_col, set_name) {
  d_set <- data %>%
    filter(!!sym(valid_col) == TRUE,
           !is.na(age), !is.na(female), !is.na(education), !is.na(urban))

  n_base <- nrow(d_set)
  d_aug  <- d_set %>% filter(!is.na(political_interest))
  n_aug  <- nrow(d_aug)

  base_f <- as.formula(paste0(
    choice_col, " ~ loser + factor(wave) + age + female + education + urban"
  ))
  aug_f  <- as.formula(paste0(
    choice_col, " ~ loser + factor(wave) + age + female + education + urban + political_interest"
  ))

  run_one <- function(formula, data, spec_label) {
    tryCatch({
      m <- multinom(formula, data = data, trace = FALSE)
      avg_slopes(m, variables = "loser") %>%
        as_tibble() %>%
        mutate(set = set_name, spec = spec_label, n = nrow(data))
    }, error = function(e) {
      message("  Error (", spec_label, "): ", e$message)
      NULL
    })
  }

  cat(sprintf("\n--- %s: base N=%d | augmented N=%d ---\n", set_name, n_base, n_aug))

  # Run base on the augmented sample for fair comparison
  base_res <- run_one(base_f, d_aug, "base")
  aug_res  <- run_one(aug_f,  d_aug, "augmented")

  list(base = base_res, augmented = aug_res)
}

sets <- list(
  list(choice = "set1_choice", valid = "set1_valid", name = "Set1"),
  list(choice = "set2_choice", valid = "set2_valid", name = "Set2"),
  list(choice = "set3_choice", valid = "set3_valid", name = "Set3"),
  list(choice = "set4_choice", valid = "set4_valid", name = "Set4")
)

all_results <- map(sets, ~ run_models(thai, .x$choice, .x$valid, .x$name))

# --- 4. Summarize procedural AMEs ------------------------------------------
extract_proc_ames <- function(res_list) {
  bind_rows(
    res_list$base,
    res_list$augmented
  ) %>%
    rename(item_label = group) %>%
    left_join(all_items %>% select(item_label, item_type), by = "item_label") %>%
    filter(item_type == "procedural")
}

proc_ames <- map_dfr(all_results, extract_proc_ames)

summary_tbl <- proc_ames %>%
  group_by(spec) %>%
  summarise(
    mean_proc_ame_pp = mean(estimate, na.rm = TRUE) * 100,
    n_items          = n(),
    .groups = "drop"
  )

cat("\n=== PROCEDURAL AME COMPARISON (mean across all procedural items) ===\n")
print(summary_tbl)

base_ame <- summary_tbl %>% filter(spec == "base")       %>% pull(mean_proc_ame_pp)
aug_ame  <- summary_tbl %>% filter(spec == "augmented")  %>% pull(mean_proc_ame_pp)
diff_pp  <- aug_ame - base_ame

cat(sprintf("\nBase (no political interest):  %.2f pp\n", base_ame))
cat(sprintf("Augmented (+ political interest): %.2f pp\n", aug_ame))
cat(sprintf("Difference:                    %.2f pp\n", diff_pp))

# Item-level comparison
item_comparison <- proc_ames %>%
  select(set, item_label, spec, estimate) %>%
  mutate(ame_pp = estimate * 100) %>%
  select(-estimate) %>%
  pivot_wider(names_from = spec, values_from = ame_pp) %>%
  mutate(diff_pp = augmented - base) %>%
  arrange(set, item_label)

cat("\n=== Item-level AMEs (procedural items only) ===\n")
print(item_comparison)

# --- 5. Decision -----------------------------------------------------------
threshold <- 0.5  # pp
cat(sprintf("\n=== DECISION ===\n"))
if (abs(diff_pp) < threshold) {
  cat(sprintf(
    "Difference (%.2f pp) is below %.1f pp threshold.\n",
    diff_pp, threshold
  ))
  cat("VERDICT: Results barely move — safe to add sentence to manuscript.\n\n")
  cat("Suggested sentence:\n")
  cat(sprintf(
    "'Adding political interest as a control changes the mean procedural AME by %.1f pp,",
    abs(diff_pp)
  ))
  cat(" indicating the trajectory is not driven by differential political engagement\nacross coalitions.'\n")
} else {
  cat(sprintf(
    "Difference (%.2f pp) EXCEEDS %.1f pp threshold.\n",
    diff_pp, threshold
  ))
  cat("VERDICT: Results move substantially — flag for discussion, do NOT insert sentence.\n")
}

# --- 6. Save results -------------------------------------------------------
polinterest_sensitivity <- list(
  summary_tbl      = summary_tbl,
  item_comparison  = item_comparison,
  base_ame_pp      = base_ame,
  aug_ame_pp       = aug_ame,
  diff_pp          = diff_pp,
  threshold        = threshold,
  verdict          = if (abs(diff_pp) < threshold) "safe_to_insert" else "flag_for_discussion"
)
saveRDS(polinterest_sensitivity,
        file.path(results_path, "polinterest_sensitivity.rds"))

cat("\nDone. Results saved.\n")
