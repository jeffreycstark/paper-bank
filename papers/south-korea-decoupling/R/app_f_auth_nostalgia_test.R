# =============================================================================
# App F alternative: Authoritarian nostalgia as inverse proxy for identity-fusion
#
# Logic: If democratic identity-fusion is society-wide in Taiwan, then even
# citizens with authoritarian sympathies should still show the critical citizens
# pattern (econ → lower quality evaluation). In Korea, where identity-fusion is
# absent, authoritarian sympathy should predict weaker democratic commitment
# but NOT interact with econ evaluations (because the decoupling is structural).
#
# Variables from ABS harmonized data (all waves, 1–4 scale):
#   - strongman_rule
#   - military_rule
#   - expert_rule
#   - single_party_rule
#
# These need to be pulled into analysis_data.rds first (Step 0 below).
# =============================================================================

library(tidyverse)
library(broom)

# --- Setup ---
project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/south-korea-decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")

source(file.path(project_root, "_data_config.R"))

controls <- "age_n + gender + edu_n + urban_rural + polint_n"

normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

extract_econ_coef <- function(model) {
  tidy(model, conf.int = TRUE) |>
    filter(term == "econ_index") |>
    select(estimate, std.error, statistic, p.value, conf.low, conf.high)
}

# =============================================================================
# Step 0: Pull authoritarian variables into analysis data
# =============================================================================
dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# Check if auth vars already exist
auth_vars_raw <- c("strongman_rule", "military_rule", "expert_rule", "single_party_rule")
missing_vars <- setdiff(auth_vars_raw, names(dat))

if (length(missing_vars) > 0) {
  cat("Merging authoritarian variables from harmonized ABS...\n")
  abs_all <- readRDS(abs_harmonized_path)

  auth_merge <- abs_all |>
    filter(country %in% c(3, 7)) |>
    select(wave, country, row_id, all_of(auth_vars_raw))

  # Merge on wave + country + row_id (or whatever unique ID exists)
  # If row_id isn't unique, try wave + country + idnumber
  if ("row_id" %in% names(dat)) {
    dat <- dat |> left_join(auth_merge, by = c("wave", "country", "row_id"))
  } else if ("idnumber" %in% names(dat) & "idnumber" %in% names(abs_all)) {
    auth_merge2 <- abs_all |>
      filter(country %in% c(3, 7)) |>
      select(wave, country, idnumber, all_of(auth_vars_raw))
    dat <- dat |> left_join(auth_merge2, by = c("wave", "country", "idnumber"))
  } else {
    stop("Cannot find a merge key. Check row_id or idnumber in both datasets.")
  }
  cat("  Done.\n")
} else {
  cat("Authoritarian variables already present.\n")
}

# Normalize and construct index
dat <- dat |>
  group_by(country_label) |>
  mutate(
    strongman_n    = normalize_01(strongman_rule),
    military_n     = normalize_01(military_rule),
    expert_n       = normalize_01(expert_rule),
    singleparty_n  = normalize_01(single_party_rule)
  ) |>
  ungroup() |>
  rowwise() |>
  mutate(
    auth_index = mean(c_across(c(strongman_n, military_n, expert_n, singleparty_n)),
                      na.rm = TRUE)
  ) |>
  ungroup()

# Coverage check
cat("\n=== Authoritarian variable coverage ===\n")
dat |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    strongman_ok = sum(!is.na(strongman_rule)),
    military_ok  = sum(!is.na(military_rule)),
    expert_ok    = sum(!is.na(expert_rule)),
    single_ok    = sum(!is.na(single_party_rule)),
    auth_idx_ok  = sum(!is.na(auth_index)),
    .groups = "drop"
  ) |>
  print(n = 20)

# =============================================================================
# 1. Authoritarian index as moderator (continuous interaction)
#    econ_index × auth_index → quality DVs
# =============================================================================
cat("\n=== 1. Continuous interaction: econ × auth_index ===\n")
auth_interaction <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(auth_index))
  cat(sprintf("%s: n = %d\n", cntry, nrow(sub)))

  for (dv_pair in list(
    c("qual_index",      "Quality index"),
    c("qual_pref_dem_n", "Democracy always preferable"),
    c("qual_extent_n",   "Extent democratic"),
    c("sat_index",       "Satisfaction index")
  )) {
    f <- as.formula(paste(dv_pair[1], "~ econ_index * auth_index + factor(wave) +",
                          controls))
    m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
    if (is.null(m)) next

    auth_interaction[[paste(cntry, dv_pair[1], sep = "_")]] <-
      tidy(m, conf.int = TRUE) |>
      filter(term %in% c("econ_index", "auth_index", "econ_index:auth_index")) |>
      mutate(country = cntry, dv = dv_pair[2], n = nobs(m))
  }
}

bind_rows(auth_interaction) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv, term, estimate, std.error, p.value, sig, n) |>
  print(n = 40)

# =============================================================================
# 2. Median-split: high vs low authoritarian sympathy
# =============================================================================
cat("\n=== 2. Median-split subgroups: high vs low auth sympathy ===\n")
auth_subgroup <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(auth_index))
  med <- median(sub$auth_index, na.rm = TRUE)
  sub <- sub |> mutate(
    auth_group = if_else(auth_index > med, "High auth sympathy", "Low auth sympathy")
  )

  cat(sprintf("%s median auth_index = %.3f\n", cntry, med))
  cat(sprintf("  High: n = %d, Low: n = %d\n",
              sum(sub$auth_group == "High auth sympathy"),
              sum(sub$auth_group == "Low auth sympathy")))

  for (grp in c("High auth sympathy", "Low auth sympathy")) {
    grp_sub <- sub |> filter(auth_group == grp)
    if (nrow(grp_sub) < 150) next

    for (dv_pair in list(
      c("qual_index",      "Quality index"),
      c("qual_pref_dem_n", "Democracy always preferable"),
      c("qual_extent_n",   "Extent democratic")
    )) {
      f <- as.formula(paste(dv_pair[1], "~ econ_index + factor(wave) +", controls))
      m <- tryCatch(lm(f, data = grp_sub), error = function(e) NULL)
      if (is.null(m)) next

      auth_subgroup[[paste(cntry, grp, dv_pair[1], sep = "_")]] <-
        extract_econ_coef(m) |>
        mutate(country = cntry, group = grp, dv = dv_pair[2], n = nobs(m))
    }
  }
}

bind_rows(auth_subgroup) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, group, dv, estimate, std.error, p.value, sig, n) |>
  arrange(country, dv, group) |>
  print(n = 30)

# =============================================================================
# 3. Individual authoritarian items as moderators (dem_pref only)
#    Which specific authoritarian alternative drives the pattern?
# =============================================================================
cat("\n=== 3. Individual auth items × econ → dem_pref ===\n")
indiv_auth <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)

  for (mod_info in list(
    c("strongman_n",   "Strongman rule"),
    c("military_n",    "Military rule"),
    c("expert_n",      "Expert rule"),
    c("singleparty_n", "Single-party rule")
  )) {
    sub_ok <- sub |> filter(!is.na(.data[[mod_info[1]]]))
    if (nrow(sub_ok) < 200) next

    f <- as.formula(paste("qual_pref_dem_n ~ econ_index *", mod_info[1],
                          "+ factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub_ok), error = function(e) NULL)
    if (is.null(m)) next

    int_term <- paste0("econ_index:", mod_info[1])
    indiv_auth[[paste(cntry, mod_info[1], sep = "_")]] <-
      tidy(m, conf.int = TRUE) |>
      filter(term == int_term) |>
      mutate(country = cntry, moderator = mod_info[2], n = nobs(m))
  }
}

bind_rows(indiv_auth) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, moderator, estimate, std.error, p.value, sig, n) |>
  print()

# =============================================================================
# 4. Triple comparison: auth_index vs nat_proud_n vs sys_proud_n
#    (on same restricted sample for fair comparison)
# =============================================================================
cat("\n=== 4. Three moderators compared: interaction → dem_pref ===\n")
triple <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry,
                       !is.na(auth_index),
                       !is.na(nat_proud_n),
                       !is.na(sys_proud_n))
  cat(sprintf("%s common sample: n = %d\n", cntry, nrow(sub)))

  for (mod_info in list(
    c("auth_index",  "Auth nostalgia (index)"),
    c("nat_proud_n", "National pride (generic)"),
    c("sys_proud_n", "System pride")
  )) {
    f <- as.formula(paste("qual_pref_dem_n ~ econ_index *", mod_info[1],
                          "+ factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
    if (is.null(m)) next

    int_term <- paste0("econ_index:", mod_info[1])
    triple[[paste(cntry, mod_info[1], sep = "_")]] <-
      tidy(m, conf.int = TRUE) |>
      filter(term == int_term) |>
      mutate(country = cntry, moderator = mod_info[2], n = nobs(m))
  }
}

triple_df <- bind_rows(triple)

triple_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, moderator, estimate, std.error, p.value, sig, n) |>
  print()

# =============================================================================
# 5. Save results for manuscript wiring
# =============================================================================
auth_results <- list(
  interaction  = bind_rows(auth_interaction),
  subgroup     = bind_rows(auth_subgroup),
  indiv_items  = bind_rows(indiv_auth),
  triple_comp  = triple_df
)

saveRDS(auth_results, file.path(results_dir, "auth_nostalgia_results.rds"))
cat("\n✓ Saved to", file.path(results_dir, "auth_nostalgia_results.rds"), "\n")
