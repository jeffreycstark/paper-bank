###############################################################################
# Critical Citizens Decomposition by Democratic Conception
# Appendix G addition: Cross-tabulating dem_preference × dem_satisfaction
# correlation by democratic conception type (procedural vs substantive)
###############################################################################

library(tidyverse)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

analysis_dir <- get_script_dir()

load(file.path(analysis_dir, "results", "prepared_data.RData"))

hk5_analysis <- hk5 |> filter(period %in% c("Protest", "Post-NSL"))

# ── 1. Main decomposition: period × conception type ──────────────────────────

cat("================================================================\n")
cat("CRITICAL CITIZENS DECOMPOSITION BY DEMOCRATIC CONCEPTION\n")
cat("================================================================\n\n")

# Labels for dem_procedural_vs_substantive
conception_labels <- c("0" = "Procedural", "1" = "Substantive")

results <- list()

for (p in c("Protest", "Post-NSL")) {
  for (c_val in c(0, 1)) {
    sub <- hk5_analysis |> 
      filter(period == p, dem_procedural_vs_substantive == c_val)
    n <- sum(!is.na(sub$dem_always_preferable) & !is.na(sub$democracy_satisfaction))
    
    if (n >= 20) {
      ct <- cor.test(sub$dem_always_preferable, sub$democracy_satisfaction, 
                     use = "complete.obs")
      results[[length(results) + 1]] <- tibble(
        period = p,
        conception = conception_labels[as.character(c_val)],
        r = ct$estimate,
        ci_lower = ct$conf.int[1],
        ci_upper = ct$conf.int[2],
        p_value = ct$p.value,
        n = n
      )
    }
  }
}

decomposition_table <- bind_rows(results)
print(decomposition_table)

# ── 2. Finer decomposition by dem_essential_harmonized ───────────────────────

cat("\n\nFINER DECOMPOSITION BY ESSENTIAL ELEMENT\n")
cat("(1=Free expression, 2=Free elections, 4=Basic needs, 5=Clean governance)\n\n")

essential_labels <- c(
  "1" = "Free expression",
  "2" = "Free elections", 
  "4" = "Basic necessities",
  "5" = "Clean governance"
)

results_fine <- list()

for (p in c("Protest", "Post-NSL")) {
  for (e_val in c(1, 2, 4, 5)) {
    sub <- hk5_analysis |> 
      filter(period == p, dem_essential_harmonized == e_val)
    n <- sum(!is.na(sub$dem_always_preferable) & !is.na(sub$democracy_satisfaction))
    
    if (n >= 20) {
      ct <- cor.test(sub$dem_always_preferable, sub$democracy_satisfaction, 
                     use = "complete.obs")
      results_fine[[length(results_fine) + 1]] <- tibble(
        period = p,
        essential_element = essential_labels[as.character(e_val)],
        r = ct$estimate,
        ci_lower = ct$conf.int[1],
        ci_upper = ct$conf.int[2],
        p_value = ct$p.value,
        n = n
      )
    }
  }
}

fine_table <- bind_rows(results_fine)
print(fine_table, n = 20)

# ── 3. Fisher z-test for difference between procedural and substantive ───────

cat("\n\nFISHER Z-TEST: Procedural vs Substantive in Post-NSL period\n")

r_proc <- decomposition_table |> 
  filter(period == "Post-NSL", conception == "Procedural") |> pull(r)
n_proc <- decomposition_table |> 
  filter(period == "Post-NSL", conception == "Procedural") |> pull(n)
r_subst <- decomposition_table |> 
  filter(period == "Post-NSL", conception == "Substantive") |> pull(r)
n_subst <- decomposition_table |> 
  filter(period == "Post-NSL", conception == "Substantive") |> pull(n)

# Fisher z transformation
z_proc <- atanh(r_proc)
z_subst <- atanh(r_subst)
se_diff <- sqrt(1/(n_proc - 3) + 1/(n_subst - 3))
z_stat <- (z_subst - z_proc) / se_diff
p_diff <- 2 * pnorm(-abs(z_stat))

cat("r_procedural =", round(r_proc, 3), "(N =", n_proc, ")\n")
cat("r_substantive =", round(r_subst, 3), "(N =", n_subst, ")\n")
cat("Fisher z =", round(z_stat, 3), "\n")
cat("p =", format.pval(p_diff, 3), "\n")


# ── 4. Item non-response table ───────────────────────────────────────────────

cat("\n\n================================================================\n")
cat("DIFFERENTIAL ITEM NON-RESPONSE TABLE\n")
cat("================================================================\n\n")

key_vars <- c(
  # Trust items (expected: stable response rates)
  "trust_police", "trust_national_government", "trust_president",
  "trust_parliament", "trust_courts", "trust_civil_service",
  # Democracy items (expected: declining response rates)
  "dem_always_preferable", "democracy_satisfaction", 
  "dem_extent_current", "democracy_suitability",
  "system_deserves_support", "dem_country_present_govt",
  # Governance items
  "gov_free_to_organize", "dem_free_speech", "election_free_fair",
  # Control items (expected: stable)
  "political_interest", "rich_poor_treated_equally", 
  "system_needs_change", "nat_willing_emigrate"
)

# Categorize
item_categories <- c(
  trust_police = "Trust", trust_national_government = "Trust", 
  trust_president = "Trust", trust_parliament = "Trust",
  trust_courts = "Trust", trust_civil_service = "Trust",
  dem_always_preferable = "Democracy (normative)", 
  democracy_satisfaction = "Democracy (normative)",
  dem_extent_current = "Democracy (evaluative)",
  democracy_suitability = "Democracy (evaluative)",
  system_deserves_support = "System support",
  dem_country_present_govt = "Democracy (evaluative)",
  gov_free_to_organize = "Governance", dem_free_speech = "Governance",
  election_free_fair = "Governance",
  political_interest = "Control", rich_poor_treated_equally = "Control",
  system_needs_change = "System support", nat_willing_emigrate = "Control"
)

item_labels <- c(
  trust_police = "Trust in police",
  trust_national_government = "Trust in national government",
  trust_president = "Trust in president/CE",
  trust_parliament = "Trust in parliament",
  trust_courts = "Trust in courts",
  trust_civil_service = "Trust in civil service",
  dem_always_preferable = "Democracy always preferable",
  democracy_satisfaction = "Democratic satisfaction",
  dem_extent_current = "Extent of current democracy",
  democracy_suitability = "Democracy suitability",
  system_deserves_support = "System deserves support",
  dem_country_present_govt = "Rate govt as democratic",
  gov_free_to_organize = "Freedom to organize",
  dem_free_speech = "Free to speak without fear",
  election_free_fair = "Elections free and fair",
  political_interest = "Political interest",
  rich_poor_treated_equally = "Rich/poor treated equally",
  system_needs_change = "System needs major change",
  nat_willing_emigrate = "Willing to emigrate"
)

nonresponse_results <- map_dfr(key_vars, function(v) {
  if (!v %in% names(hk5_analysis)) return(NULL)
  
  protest <- hk5_analysis |> filter(period == "Protest")
  postnsl <- hk5_analysis |> filter(period == "Post-NSL")
  
  protest_valid <- sum(!is.na(protest[[v]]))
  postnsl_valid <- sum(!is.na(postnsl[[v]]))
  protest_rate <- protest_valid / nrow(protest)
  postnsl_rate <- postnsl_valid / nrow(postnsl)
  
  # Fisher exact test for proportion difference
  mat <- matrix(c(
    protest_valid, nrow(protest) - protest_valid,
    postnsl_valid, nrow(postnsl) - postnsl_valid
  ), nrow = 2, byrow = TRUE)
  ft <- fisher.test(mat)
  
  tibble(
    variable = v,
    label = item_labels[v],
    category = item_categories[v],
    protest_rate = round(100 * protest_rate, 1),
    postnsl_rate = round(100 * postnsl_rate, 1),
    delta_pp = round(100 * (postnsl_rate - protest_rate), 1),
    p_value = ft$p.value
  )
})

nonresponse_results |> 
  arrange(category, delta_pp) |>
  print(n = 25, width = 120)

# Summary by category
cat("\n=== Mean response rate change by item category ===\n")
nonresponse_results |>
  group_by(category) |>
  summarise(
    n_items = n(),
    mean_protest = round(mean(protest_rate), 1),
    mean_postnsl = round(mean(postnsl_rate), 1),
    mean_delta = round(mean(delta_pp), 1),
    .groups = "drop"
  ) |>
  print()


# ── 5. Non-response by age group for democracy items ─────────────────────────

cat("\n=== Non-response on dem_always_preferable by age × period ===\n")
age_nonresp <- hk5_analysis |>
  filter(!is.na(age_group)) |>
  group_by(period, age_group) |>
  summarise(
    n_total = n(),
    n_valid_dempref = sum(!is.na(dem_always_preferable)),
    rate_dempref = round(100 * n_valid_dempref / n_total, 1),
    n_valid_trustpol = sum(!is.na(trust_police)),
    rate_trustpol = round(100 * n_valid_trustpol / n_total, 1),
    .groups = "drop"
  )

print(age_nonresp, n = 20)


# ── 6. Save results ──────────────────────────────────────────────────────────

critical_citizens_decomposition <- decomposition_table
critical_citizens_fine <- fine_table
fisher_z_result <- tibble(
  r_procedural = r_proc, n_procedural = n_proc,
  r_substantive = r_subst, n_substantive = n_subst,
  z_stat = z_stat, p_value = p_diff
)

save(
  critical_citizens_decomposition,
  critical_citizens_fine,
  fisher_z_result,
  nonresponse_results,
  age_nonresp,
  file = file.path(analysis_dir, "results", "revision_results.RData")
)

cat("\n\nResults saved to revision_results.RData\n")
