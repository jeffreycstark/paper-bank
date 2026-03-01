# =============================================================================
# Script 11: Within-Loser Unfairness Heterogeneity (Revision Package Section 7)
# Goal: Among losers only, does perceiving elections as unfair predict
#       stronger protective-procedural prioritization?
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(nnet)
  library(marginaleffects)
})

here::i_am("papers/02_meaning_of_democracy_revision/manuscript/md-manuscript.qmd")
results_path <- here("papers", "02_meaning_of_democracy_revision", "analysis", "revised", "results")

# --- 1. Load data -------------------------------------------------------
res <- readRDS(file.path(results_path, "mlogit_results.rds"))

# Use set1 data (has election_unfair variable)
d1 <- res$set1_data
d2 <- res$set2_data
d3 <- res$set3_data
d4 <- res$set4_data

# Item metadata
meta <- tibble(
  set       = c(rep("Set1",4), rep("Set2",4), rep("Set3",4), rep("Set4",4)),
  choice_val= rep(1:4, 4),
  item_label= c(
    "Free elections","No waste","Free expression","Reduce gap rich/poor",
    "Legislature oversight","Quality services","Organize groups","Basic necessities",
    "Media freedom","Law and order","Party competition","Jobs for all",
    "Court protection","Clean politics","Protest freedom","Unemployment aid"
  ),
  item_type = c(
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive"
  ),
  item_subtype = c(
    "electoral","quality","liberal","redistribution",
    "oversight","quality","liberal","welfare",
    "liberal","order","electoral","welfare",
    "liberal","quality","liberal","welfare"
  )
)

# Classify protective vs participatory
meta <- meta %>%
  mutate(
    proc_class = case_when(
      item_subtype %in% c("liberal", "oversight") ~ "protective",
      item_subtype %in% c("electoral")             ~ "participatory",
      TRUE                                          ~ item_type
    )
  )

# --- 2. Run within-loser models for each set ----------------------------
run_within_loser <- function(data, set_name, choice_col, valid_col) {
  # Restrict to losers
  d_losers <- data %>%
    filter(loser == 1, !!sym(valid_col) == 1, !is.na(election_unfair))

  n_losers <- nrow(d_losers)
  pct_unfair <- mean(d_losers$election_unfair, na.rm = TRUE) * 100

  # Multinomial logit: choice ~ unfair + country FE + wave FE + demographics
  f <- as.formula(paste0(
    choice_col, " ~ election_unfair + factor(country) + factor(wave) + ",
    "age + female + education + urban"
  ))

  tryCatch({
    m <- multinom(f, data = d_losers, trace = FALSE)

    # Average marginal effects of election_unfair
    ame_tbl <- avg_slopes(m, variables = "election_unfair") %>%
      as_tibble() %>%
      mutate(set = set_name, n_losers = n_losers, pct_unfair = pct_unfair)

    return(ame_tbl)
  }, error = function(e) {
    message("Error in set ", set_name, ": ", e$message)
    return(NULL)
  })
}

results_list <- list(
  run_within_loser(d1, "Set1", "set1_choice", "set1_valid"),
  run_within_loser(d2, "Set2", "set2_choice", "set2_valid"),
  run_within_loser(d3, "Set3", "set3_choice", "set3_valid"),
  run_within_loser(d4, "Set4", "set4_choice", "set4_valid")
)

results_all <- bind_rows(results_list)

# The `group` column in marginaleffects contains the response level label (item name)
# Join with metadata by item label
results_merged <- results_all %>%
  rename(item_label = group) %>%
  left_join(meta %>% select(item_label, item_type, item_subtype, proc_class),
            by = "item_label")

# --- 3. Summarize by domain ---------------------------------------------
domain_summary <- results_merged %>%
  group_by(proc_class) %>%
  summarise(
    mean_ame   = mean(estimate, na.rm = TRUE) * 100,
    n_items    = n(),
    n_positive = sum(estimate > 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Within-Loser Unfairness AMEs by Domain ===\n")
print(domain_summary)

# Print item-level results
cat("\n=== Item-level results ===\n")
item_results <- results_merged %>%
  filter(!is.na(item_type)) %>%
  select(item_label, item_type, item_subtype, proc_class,
         estimate, std.error, statistic, p.value) %>%
  mutate(
    ame_pp = estimate * 100,
    se_pp  = std.error * 100
  ) %>%
  arrange(proc_class, item_label)
print(item_results %>% select(item_label, proc_class, ame_pp, se_pp, p.value))

# --- 4. Count unfair losers --------------------------------------------
# Get % unfair losers across all sets
pct_unfair_all <- d1 %>%
  filter(loser == 1, !is.na(election_unfair)) %>%
  summarise(pct = mean(election_unfair) * 100) %>%
  pull(pct)
cat(sprintf("\nPct of losers perceiving elections as unfair: %.1f%%\n", pct_unfair_all))

# --- 5. Save results ---------------------------------------------------
within_loser_stats <- list(
  domain_summary  = domain_summary,
  item_results    = item_results,
  pct_unfair_losers = pct_unfair_all
)
saveRDS(within_loser_stats, file.path(results_path, "within_loser_unfairness.rds"))
write_csv(item_results, file.path(results_path, "within_loser_unfairness_items.csv"))

cat("\nDone. Results saved.\n")
