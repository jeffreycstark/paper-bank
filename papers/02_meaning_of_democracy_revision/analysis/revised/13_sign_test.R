# =============================================================================
# Script 13: Sign Test for Directional Consistency (Revision Package Section 9)
# Goal: Formal binomial test that the directional pattern across 20 items
#       is unlikely to arise by chance.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

here::i_am("papers/02_meaning_of_democracy_revision/manuscript/md-manuscript.qmd")
results_path <- here("papers", "02_meaning_of_democracy_revision", "analysis", "revised", "results")

# --- 1. Load pooled AME results ----------------------------------------
res    <- readRDS(file.path(results_path, "mlogit_results.rds"))
pooled <- res$all_results

cat("All items:\n")
print(pooled %>% select(set, item_label, item_type, ame) %>%
        mutate(ame_pp = round(ame * 100, 1)) %>%
        arrange(item_type, ame_pp))

# --- 2. Sign counts ----------------------------------------------------
proc_items <- pooled %>% filter(item_type == "procedural")
sub_items  <- pooled %>% filter(item_type == "substantive")

n_proc_pos   <- sum(proc_items$ame > 0)
n_proc_total <- nrow(proc_items)

n_sub_neg    <- sum(sub_items$ame < 0)
n_sub_total  <- nrow(sub_items)

n_all_correct    <- n_proc_pos + n_sub_neg
n_all_total      <- n_proc_total + n_sub_total

cat(sprintf("\nProcedural: %d of %d positive\n", n_proc_pos, n_proc_total))
cat(sprintf("Substantive: %d of %d negative\n", n_sub_neg, n_sub_total))
cat(sprintf("Combined: %d of %d in predicted direction\n", n_all_correct, n_all_total))

# --- 3. Binomial tests -------------------------------------------------
test_proc <- binom.test(n_proc_pos,   n_proc_total, p = 0.5, alternative = "greater")
test_sub  <- binom.test(n_sub_neg,    n_sub_total,  p = 0.5, alternative = "greater")
test_all  <- binom.test(n_all_correct, n_all_total,  p = 0.5, alternative = "greater")

cat("\n=== Sign Tests ===\n")
cat(sprintf("Procedural items in positive direction: %d/%d, p = %.4f\n",
            n_proc_pos, n_proc_total, test_proc$p.value))
cat(sprintf("Substantive items in negative direction: %d/%d, p = %.4f\n",
            n_sub_neg, n_sub_total, test_sub$p.value))
cat(sprintf("All items (proc+sub) in predicted direction: %d/%d, p = %.4f\n",
            n_all_correct, n_all_total, test_all$p.value))

# --- 4. Save results for inline text ------------------------------------
sign_test_stats <- list(
  n_proc_pos    = n_proc_pos,
  n_proc_total  = n_proc_total,
  p_proc        = test_proc$p.value,
  n_sub_neg     = n_sub_neg,
  n_sub_total   = n_sub_total,
  p_sub         = test_sub$p.value,
  n_all_correct = n_all_correct,
  n_all_total   = n_all_total,
  p_all         = test_all$p.value
)
saveRDS(sign_test_stats, file.path(results_path, "sign_test_stats.rds"))

# --- 5. Prose shell (fill in numbers) ----------------------------------
p_fmt <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)

cat(sprintf(
  paste0(
    "\nProse shell:\n",
    "Under the null hypothesis that loser status is unrelated to democratic conceptions,",
    " each item has a 50%% probability of showing a positive (or negative) loser effect.",
    " The observed pattern---%d of %d procedural items positive,",
    " %d of %d substantive items negative---yields p %s on a one-sided binomial test",
    " for the combined directional consistency (%d of %d items in predicted direction),",
    " confirming that the directional consistency is unlikely to reflect chance.\n"
  ),
  n_proc_pos, n_proc_total,
  n_sub_neg,  n_sub_total,
  p_fmt(test_all$p.value),
  n_all_correct, n_all_total
))

cat("Done.\n")
