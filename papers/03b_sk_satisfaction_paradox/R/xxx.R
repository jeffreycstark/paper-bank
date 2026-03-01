# =============================================================================
# TAIWAN DIAGNOSTIC: Does the Korea–Taiwan divergence reflect
# substantive normative depth or just different identity politics?
#
# Core question: Is the Taiwan "critical citizens" pattern confined to the
# abstract "democracy is always preferable" item (which could be tribal/
# indoctrination), or does it extend to substantive democratic content
# (authoritarian rejection, liberal-democratic values, system legitimacy)?
#
# If it extends → structural difference, keep Taiwan story
# If it's confined → may just be different identity signaling, rethink
#
# Run after: 00_data_preparation.qmd (needs analysis_data.rds)
# Requires: merge from abs_harmonized.rds for auth detachment vars
# =============================================================================

library(tidyverse)
library(broom)

# --- Setup ---
project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03_south_korea_decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")
source(file.path(project_root, "_data_config.R"))

normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

controls <- "age_n + gender + edu_n + urban_rural + polint_n"

# =============================================================================
# STEP 0: Load data and merge any missing variables
# =============================================================================
dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# Variables we need for this diagnostic
auth_vars <- c("strongman_rule", "military_rule", "expert_rule", "single_party_rule")
extra_vars <- c("dem_free_speech", "gov_free_to_organize",
                "democracy_suitability", "democracy_efficacy",
                "dem_best_form", "dem_vs_econ")

needed_vars <- c(auth_vars, extra_vars)
missing_vars <- setdiff(needed_vars, names(dat))

if (length(missing_vars) > 0) {
  cat("Merging missing variables from harmonized ABS:", 
      paste(missing_vars, collapse = ", "), "\n")
  abs_all <- readRDS(abs_harmonized_path)
  
  # Only pull what exists in the harmonized data
  available <- intersect(missing_vars, names(abs_all))
  unavailable <- setdiff(missing_vars, names(abs_all))
  if (length(unavailable) > 0) {
    cat("  NOT in harmonized data:", paste(unavailable, collapse = ", "), "\n")
  }
  
  if (length(available) > 0) {
    merge_cols <- c("wave", "country")
    if ("row_id" %in% names(dat) & "row_id" %in% names(abs_all)) {
      merge_cols <- c(merge_cols, "row_id")
    } else if ("idnumber" %in% names(dat) & "idnumber" %in% names(abs_all)) {
      merge_cols <- c(merge_cols, "idnumber")
    }
    
    auth_merge <- abs_all |>
      filter(country %in% c(3, 7)) |>
      select(all_of(c(merge_cols, available)))
    
    dat <- dat |> left_join(auth_merge, by = merge_cols)
    cat("  Merged", length(available), "variables.\n")
  }
}

# =============================================================================
# STEP 1: Construct normalized variables
# =============================================================================
dat <- dat |>
  group_by(country_label) |>
  mutate(
    # Authoritarian detachment (higher = MORE rejection of auth alternatives)
    # These are typically scaled 1-4 where higher = more support for auth
    # So we REVERSE them: rejection = max - value
    strongman_reject_n   = normalize_01(max(strongman_rule, na.rm = TRUE) + 1 - strongman_rule),
    military_reject_n    = normalize_01(max(military_rule, na.rm = TRUE) + 1 - military_rule),
    expert_reject_n      = normalize_01(max(expert_rule, na.rm = TRUE) + 1 - expert_rule),
    singleparty_reject_n = normalize_01(max(single_party_rule, na.rm = TRUE) + 1 - single_party_rule),
  ) |>
  ungroup() |>
  rowwise() |>
  mutate(
    auth_reject_index = mean(c_across(c(strongman_reject_n, military_reject_n,
                                         expert_reject_n, singleparty_reject_n)),
                             na.rm = TRUE)
  ) |>
  ungroup()

# Also normalize any available liberal-dem items
if ("dem_free_speech" %in% names(dat)) {
  dat <- dat |> group_by(country_label) |>
    mutate(free_speech_n = normalize_01(dem_free_speech)) |> ungroup()
}
if ("gov_free_to_organize" %in% names(dat)) {
  dat <- dat |> group_by(country_label) |>
    mutate(free_organize_n = normalize_01(gov_free_to_organize)) |> ungroup()
}
if ("democracy_suitability" %in% names(dat)) {
  dat <- dat |> group_by(country_label) |>
    mutate(dem_suitable_n = normalize_01(democracy_suitability)) |> ungroup()
}
if ("democracy_efficacy" %in% names(dat)) {
  dat <- dat |> group_by(country_label) |>
    mutate(dem_efficacy_n = normalize_01(democracy_efficacy)) |> ungroup()
}

# =============================================================================
# DIAGNOSTIC 1: Authoritarian Detachment
# 
# Q: Does the Korea-Taiwan divergence replicate on rejection of auth alternatives?
# If Taiwan shows negative econ → auth_rejection (economically comfortable
# Taiwanese MORE rejecting of authoritarianism) while Korea shows null,
# that's evidence of structural depth beyond cheerleading.
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  DIAGNOSTIC 1: AUTHORITARIAN DETACHMENT                        ║\n")
cat("║  DV = rejection of strongman/military/expert/single-party rule  ║\n")
cat("║  Key Q: Does econ → auth rejection differ Korea vs Taiwan?      ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

# Coverage check
cat("--- Coverage ---\n")
dat |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    strongman_ok = sum(!is.na(strongman_rule)),
    auth_index_ok = sum(!is.na(auth_reject_index)),
    .groups = "drop"
  ) |> print(n = 20)

# 1a. Pooled model: econ → auth_reject_index
cat("\n--- 1a. Pooled: econ → auth rejection index ---\n")
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(auth_reject_index))
  f <- as.formula(paste("auth_reject_index ~ econ_index + factor(wave) +", controls))
  m <- lm(f, data = sub)
  
  coef <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")
  cat(sprintf("  %s: β = %.3f (SE = %.3f), p = %.4f, n = %d %s\n",
              cntry, coef$estimate, coef$std.error, coef$p.value, nobs(m),
              ifelse(coef$p.value < 0.05, " *", "")))
}

# 1b. Individual auth items
cat("\n--- 1b. Individual authoritarian rejection items (pooled) ---\n")
auth_items <- c(
  "strongman_reject_n"   = "Reject strongman",
  "military_reject_n"    = "Reject military rule",
  "expert_reject_n"      = "Reject expert rule",
  "singleparty_reject_n" = "Reject single-party"
)

for (cntry in c("Korea", "Taiwan")) {
  cat(sprintf("\n  %s:\n", cntry))
  for (i in seq_along(auth_items)) {
    varname <- names(auth_items)[i]
    label <- auth_items[i]
    sub <- dat |> filter(country_label == cntry, !is.na(.data[[varname]]))
    if (nrow(sub) < 100) { cat(sprintf("    %s: insufficient data\n", label)); next }
    
    f <- as.formula(paste(varname, "~ econ_index + factor(wave) +", controls))
    m <- lm(f, data = sub)
    coef <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")
    cat(sprintf("    %s: β = %.3f (p = %.4f) %s\n",
                label, coef$estimate, coef$p.value,
                ifelse(coef$p.value < 0.05, "*", "")))
  }
}

# 1c. Cross-country interaction test
cat("\n--- 1c. Cross-country interaction: econ × Korea → auth rejection ---\n")
both <- dat |>
  filter(!is.na(auth_reject_index)) |>
  mutate(korea = as.integer(country_label == "Korea"))

f_int <- as.formula(paste("auth_reject_index ~ econ_index * korea + factor(wave) +", controls))
m_int <- lm(f_int, data = both)

key_terms <- tidy(m_int, conf.int = TRUE) |>
  filter(term %in% c("econ_index", "korea", "econ_index:korea"))
print(key_terms |> select(term, estimate, std.error, p.value))

cat("\n  INTERPRETATION:\n")
cat("  If econ_index:korea interaction is significant and positive,\n")
cat("  Taiwan's econ→auth rejection relationship is more negative than Korea's.\n")
cat("  That would mean the Taiwan pattern extends beyond cheerleading.\n")

# =============================================================================
# DIAGNOSTIC 2: INDIVIDUAL AUTH ITEMS WAVE-BY-WAVE
# 
# Same structure as the paper's main analysis, but with auth rejection as DV.
# Shows whether the pattern is temporally stable.
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  DIAGNOSTIC 2: WAVE-BY-WAVE AUTH REJECTION                     ║\n")
cat("║  Same structure as main analysis, auth_reject_index as DV       ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

wave_auth <- list()
for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w,
                          !is.na(auth_reject_index))
    if (nrow(sub) < 100) next
    
    f <- as.formula(paste("auth_reject_index ~ econ_index +", controls))
    m <- lm(f, data = sub)
    coef <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")
    
    wave_auth[[paste(cntry, w)]] <- coef |>
      mutate(country = cntry, wave = w, n = nobs(m),
             r_sq = summary(m)$r.squared)
  }
}

wave_auth_df <- bind_rows(wave_auth)
cat("--- Wave-by-wave: econ → auth rejection index ---\n")
wave_auth_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                          p.value < 0.05 ~ "*", TRUE ~ "")) |>
  select(country, wave, estimate, std.error, p.value, sig, n) |>
  arrange(country, wave) |>
  print(n = 20)

# =============================================================================
# DIAGNOSTIC 3: MEAN-LEVEL DIFFERENCES
# 
# Q: Do Taiwanese score HIGHER on auth rejection and normative items
#    than Koreans? If yes, that's consistent with deeper commitment.
#    If similar or Korean higher, the "depth" story weakens.
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  DIAGNOSTIC 3: MEAN-LEVEL COMPARISONS                          ║\n")
cat("║  Are Taiwanese more rejecting of authoritarianism than Koreans? ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

compare_vars <- c("auth_reject_index", "strongman_reject_n", "military_reject_n",
                   "expert_reject_n", "singleparty_reject_n",
                   "qual_pref_dem_n", "qual_extent_n",
                   "sat_democracy_n", "sat_govt_n")

cat("--- Mean (SD) by country, pooled waves ---\n")
for (v in compare_vars) {
  if (!v %in% names(dat)) next
  means <- dat |>
    group_by(country_label) |>
    summarise(m = mean(.data[[v]], na.rm = TRUE),
              s = sd(.data[[v]], na.rm = TRUE),
              n = sum(!is.na(.data[[v]])),
              .groups = "drop")
  
  kr <- means |> filter(country_label == "Korea")
  tw <- means |> filter(country_label == "Taiwan")
  
  # Quick t-test
  t_res <- t.test(dat[[v]][dat$country_label == "Korea"],
                  dat[[v]][dat$country_label == "Taiwan"])
  
  cat(sprintf("  %-25s  Korea: %.3f (%.3f, n=%d)  Taiwan: %.3f (%.3f, n=%d)  diff p=%.4f %s\n",
              v,
              kr$m, kr$s, kr$n,
              tw$m, tw$s, tw$n,
              t_res$p.value,
              ifelse(t_res$p.value < 0.05, "*", "")))
}

# Also by wave
cat("\n--- Auth rejection index by country and wave ---\n")
dat |>
  group_by(country_label, wave) |>
  summarise(
    mean_auth_reject = mean(auth_reject_index, na.rm = TRUE),
    mean_pref_dem    = mean(qual_pref_dem_n, na.rm = TRUE),
    n = sum(!is.na(auth_reject_index)),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = country_label,
              values_from = c(mean_auth_reject, mean_pref_dem, n)) |>
  print(n = 12)


# =============================================================================
# DIAGNOSTIC 4: THE "CHEERLEADING VS. SUBSTANCE" TEST
#
# Core test: Run econ → DV for EVERY available normative/quality item
# in both countries. Classify each as "cheerleading" (abstract endorsement)
# vs "substantive" (specific democratic content).
#
# If Korea-Taiwan divergence shows up ONLY on cheerleading items
#   → identity politics / indoctrination story
# If it shows up on substantive items too
#   → genuine structural difference in normative depth
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  DIAGNOSTIC 4: CHEERLEADING VS. SUBSTANCE                      ║\n")
cat("║  Does the divergence extend to substantive democratic items?    ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

# All candidate DVs and their classification
all_dvs <- tribble(
  ~varname,              ~label,                          ~type,
  "qual_pref_dem_n",     "Democracy always preferable",   "CHEERLEADING",
  "auth_reject_index",   "Auth rejection index",          "SUBSTANTIVE",
  "strongman_reject_n",  "Reject strongman rule",         "SUBSTANTIVE",
  "military_reject_n",   "Reject military rule",          "SUBSTANTIVE",
  "singleparty_reject_n","Reject single-party rule",      "SUBSTANTIVE",
  "expert_reject_n",     "Reject expert rule",            "SUBSTANTIVE",
  "qual_extent_n",       "Dem extent (current)",          "EVALUATIVE",
  "qual_sys_support_n",  "System deserves support",       "EVALUATIVE",
  "qual_sys_change_n",   "No major change needed",        "EVALUATIVE",
  "sys_proud_n",         "System pride",                  "EVALUATIVE",
  "sat_democracy_n",     "Satisfaction w/ democracy",     "SATISFACTION",
  "sat_govt_n",          "Satisfaction w/ government",     "SATISFACTION",
)

# Add conditional pro-dem items if available
if ("dem_suitable_n" %in% names(dat)) {
  all_dvs <- bind_rows(all_dvs,
    tibble(varname = "dem_suitable_n", label = "Democracy suitable", type = "CONDITIONAL"))
}
if ("dem_efficacy_n" %in% names(dat)) {
  all_dvs <- bind_rows(all_dvs,
    tibble(varname = "dem_efficacy_n", label = "Democracy can solve problems", type = "CONDITIONAL"))
}

cat("--- Pooled econ → DV for all items, by country ---\n\n")
cat(sprintf("%-30s  %-14s  %-10s  %-10s  %-6s  %-10s  %-6s\n",
            "DV", "Type",
            "Korea β", "Korea p", "K sig",
            "Taiwan β", "T sig"))
cat(paste(rep("-", 100), collapse = ""), "\n")

diagnostic_results <- list()

for (i in 1:nrow(all_dvs)) {
  v <- all_dvs$varname[i]
  if (!v %in% names(dat)) next
  
  kr_b <- tw_b <- kr_p <- tw_p <- NA_real_
  
  for (cntry in c("Korea", "Taiwan")) {
    sub <- dat |> filter(country_label == cntry, !is.na(.data[[v]]))
    if (nrow(sub) < 200) next
    
    f <- as.formula(paste(v, "~ econ_index + factor(wave) +", controls))
    m <- lm(f, data = sub)
    coef <- tidy(m) |> filter(term == "econ_index")
    
    if (cntry == "Korea") { kr_b <- coef$estimate; kr_p <- coef$p.value }
    else { tw_b <- coef$estimate; tw_p <- coef$p.value }
    
    diagnostic_results[[paste(cntry, v)]] <- tibble(
      country = cntry, varname = v, label = all_dvs$label[i],
      type = all_dvs$type[i],
      beta = coef$estimate, se = coef$std.error, p = coef$p.value)
  }
  
  kr_sig <- ifelse(is.na(kr_p), "", ifelse(kr_p < 0.001, "***",
            ifelse(kr_p < 0.01, "**", ifelse(kr_p < 0.05, "*", ""))))
  tw_sig <- ifelse(is.na(tw_p), "", ifelse(tw_p < 0.001, "***",
            ifelse(tw_p < 0.01, "**", ifelse(tw_p < 0.05, "*", ""))))
  
  cat(sprintf("%-30s  %-14s  %8.3f  %10.4f  %-6s  %8.3f  %-6s\n",
              all_dvs$label[i], all_dvs$type[i],
              kr_b, kr_p, kr_sig, tw_b, tw_sig))
}

diag_df <- bind_rows(diagnostic_results)


# =============================================================================
# DIAGNOSTIC 5: THE VERDICT
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  DIAGNOSTIC SUMMARY: WHAT TO LOOK FOR                          ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

cat("SCENARIO A: Keep Taiwan (structural difference confirmed)\n")
cat("  → Taiwan shows significant NEGATIVE econ→auth rejection\n")
cat("     (economically comfortable Taiwanese reject authoritarianism more)\n")
cat("  → Korea shows null or positive on same items\n")
cat("  → Divergence extends to substantive items, not just cheerleading\n\n")

cat("SCENARIO B: Rethink Taiwan (divergence is shallow)\n")
cat("  → Taiwan's critical citizens pattern confined to 'always preferable'\n")
cat("  → Auth rejection items show similar patterns in both countries\n")
cat("  → Mean-level differences are small\n")
cat("  → Divergence may reflect identity signaling, not normative depth\n\n")

cat("SCENARIO C: Mixed (partial support)\n")
cat("  → Some substantive items diverge, others don't\n")
cat("  → Consider: which specific items diverge tells you something\n")
cat("  → May support a more modest Taiwan comparison section\n\n")

# Save results
saveRDS(diag_df, file.path(results_dir, "taiwan_diagnostic_results.rds"))
cat("✓ Saved diagnostic results to taiwan_diagnostic_results.rds\n")
cat("\nDone. Review the output above to decide the Taiwan question.\n")