# =============================================================================
# Wire up hard-coded values: inspect saved objects + create expanded results
#
# This script:
# 1. Inspects model_results.rds to understand current structure
# 2. Identifies what's hard-coded in the manuscript
# 3. Runs the missing models and saves results for inline R
# 4. Produces a single expanded model_results.rds with everything wired up
#
# Run with: Rscript R/wire_hardcoded_values.R
# =============================================================================

library(tidyverse)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")

source(file.path(project_root, "_data_config.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))
mr  <- readRDS(file.path(results_dir, "model_results.rds"))

controls     <- "age_n + gender + edu_n + urban_rural + polint_n"
controls_vec <- c("age_n", "gender", "edu_n", "urban_rural", "polint_n")

normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# =============================================================================
# STEP 0: Diagnostic — what's currently saved?
# =============================================================================
cat("=== Current model_results.rds structure ===\n")
cat("Top-level names:", paste(names(mr), collapse = ", "), "\n\n")

cat("wave_by_wave columns:", paste(names(mr$wave_by_wave), collapse = ", "), "\n")
cat("wave_by_wave unique DVs:", paste(unique(mr$wave_by_wave$dv), collapse = ", "), "\n")
cat("wave_by_wave unique countries:", paste(unique(mr$wave_by_wave$country), collapse = ", "), "\n\n")

cat("indiv_dvs:\n")
print(mr$indiv_dvs)

cat("\ncross_country:\n")
print(mr$cross_country)

cat("\nsubgroups_full unique splits:", paste(unique(mr$subgroups_full$split), collapse = ", "), "\n")
cat("subgroups_full unique DVs:", paste(unique(mr$subgroups_full$dv), collapse = ", "), "\n")
cat("subgroups_full unique countries:", paste(unique(mr$subgroups_full$country), collapse = ", "), "\n\n")

# =============================================================================
# STEP 1: Wave-by-wave individual-item models (Korea + Taiwan)
#
# Currently hard-coded in manuscript:
# - Korea sat_dem: β = 0.262 (W2) to 0.455 (W4)
# - Korea sat_govt: β = 0.473 to 0.823
# - Korea dem_pref: β = -0.246 (W1), -0.121 (W2), 0.067 (W3), etc.
# - Taiwan sat_dem: β = 0.273 to 0.561
# - Taiwan sat_govt: β = 0.491 to 0.841
# - Taiwan dem_pref: β = -0.287 (W1), -0.306 (W2), 0.065 (W3), etc.
# - Taiwan dem_extent pooled: β = -0.114
# =============================================================================
cat("=== STEP 1: Wave-by-wave individual-item models ===\n")

indiv_dvs <- c(
  "sat_democracy_n", "sat_govt_n",
  "qual_pref_dem_n", "qual_extent_n",
  "qual_sys_support_n", "qual_sys_change_n"
)

indiv_dv_labels <- c(
  "sat_democracy_n"    = "Satisfaction with democracy",
  "sat_govt_n"         = "Satisfaction with government",
  "qual_pref_dem_n"    = "Democracy always preferable",
  "qual_extent_n"      = "Democratic extent",
  "qual_sys_support_n" = "System deserves support",
  "qual_sys_change_n"  = "No major change needed"
)

wave_indiv_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100) next

    for (dv in indiv_dvs) {
      if (all(is.na(sub[[dv]]))) next
      if (sum(!is.na(sub[[dv]])) < 50) next

      f <- as.formula(paste(dv, "~ econ_index +", controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      coefs <- tidy(m, conf.int = TRUE)
      econ_row <- coefs |> filter(term == "econ_index")

      wave_indiv_results[[paste(cntry, w, dv, sep = "_")]] <- tibble(
        country  = cntry,
        wave     = w,
        dv       = dv,
        dv_label = indiv_dv_labels[dv],
        estimate = econ_row$estimate,
        std.error = econ_row$std.error,
        statistic = econ_row$statistic,
        p.value  = econ_row$p.value,
        conf.low = econ_row$conf.low,
        conf.high = econ_row$conf.high,
        n        = nobs(m),
        r_sq     = summary(m)$r.squared,
        adj_r_sq = summary(m)$adj.r.squared
      )
    }
  }
}

wave_indiv_df <- bind_rows(wave_indiv_results)

cat("\n--- Korea wave-by-wave individual items ---\n")
wave_indiv_df |>
  filter(country == "Korea") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv_label, wave, estimate, p.value, sig, n) |>
  arrange(dv_label, wave) |>
  print(n = 60)

cat("\n--- Taiwan wave-by-wave individual items ---\n")
wave_indiv_df |>
  filter(country == "Taiwan") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv_label, wave, estimate, p.value, sig, n) |>
  arrange(dv_label, wave) |>
  print(n = 60)

# =============================================================================
# STEP 2: Pooled individual-item models (both countries)
#
# Currently mr$indiv_dvs has Korea only. Need Taiwan too.
# =============================================================================
cat("\n=== STEP 2: Pooled individual-item models (both countries) ===\n")

pooled_indiv_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)

  for (dv in indiv_dvs) {
    sub_ok <- sub |> filter(!is.na(.data[[dv]]))
    if (nrow(sub_ok) < 200) next

    f <- as.formula(paste(dv, "~ econ_index + factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub_ok), error = function(e) NULL)
    if (is.null(m)) next

    coefs <- tidy(m, conf.int = TRUE)
    econ_row <- coefs |> filter(term == "econ_index")

    pooled_indiv_results[[paste(cntry, dv, sep = "_")]] <- tibble(
      country  = cntry,
      dv       = dv,
      dv_label = indiv_dv_labels[dv],
      estimate = econ_row$estimate,
      std.error = econ_row$std.error,
      statistic = econ_row$statistic,
      p.value  = econ_row$p.value,
      conf.low = econ_row$conf.low,
      conf.high = econ_row$conf.high,
      n        = nobs(m),
      r_sq     = summary(m)$r.squared,
      adj_r_sq = summary(m)$adj.r.squared
    )
  }
}

pooled_indiv_df <- bind_rows(pooled_indiv_results)

cat("\nPooled individual-item results:\n")
pooled_indiv_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv_label, estimate, p.value, sig, n, r_sq) |>
  arrange(dv_label, country) |>
  print(n = 20)

# =============================================================================
# STEP 3: Cross-country interaction on individual items
#
# Currently hard-coded: interaction β = 0.217 for dem_pref
# Need: all individual items
# =============================================================================
cat("\n=== STEP 3: Cross-country interaction (individual items) ===\n")

xc_indiv_results <- list()

for (dv in indiv_dvs) {
  both <- dat |>
    filter(!is.na(.data[[dv]])) |>
    mutate(is_korea = as.numeric(country_label == "Korea"))

  if (nrow(both) < 500) next

  f <- as.formula(paste(dv, "~ econ_index * is_korea + factor(wave) +", controls))
  m <- tryCatch(lm(f, data = both), error = function(e) NULL)
  if (is.null(m)) next

  coefs <- tidy(m, conf.int = TRUE)

  xc_indiv_results[[dv]] <- coefs |>
    filter(term %in% c("econ_index", "is_korea", "econ_index:is_korea")) |>
    mutate(dv = dv, dv_label = indiv_dv_labels[dv], n = nobs(m))
}

xc_indiv_df <- bind_rows(xc_indiv_results)

cat("\nCross-country interaction (individual items):\n")
xc_indiv_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv_label, term, estimate, p.value, sig, n) |>
  print(n = 30)

# =============================================================================
# STEP 4: Political interest subgroup — individual items
#
# Currently hard-coded: β = -0.071 (high polint), 0.009 (low polint)
# for qual_pref_dem_n. Need these from saved results.
# =============================================================================
cat("\n=== STEP 4: Political interest subgroup (individual items) ===\n")

polint_indiv_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(polint_group))

  for (grp in c("High interest", "Low interest")) {
    grp_sub <- sub |> filter(polint_group == grp)
    if (nrow(grp_sub) < 200) next

    for (dv in c("qual_pref_dem_n", "qual_extent_n", "sat_democracy_n")) {
      grp_ok <- grp_sub |> filter(!is.na(.data[[dv]]))
      if (nrow(grp_ok) < 100) next

      f <- as.formula(paste(dv, "~ econ_index + factor(wave) +", controls))
      m <- tryCatch(lm(f, data = grp_ok), error = function(e) NULL)
      if (is.null(m)) next

      coefs <- tidy(m, conf.int = TRUE)
      econ_row <- coefs |> filter(term == "econ_index")

      polint_indiv_results[[paste(cntry, grp, dv, sep = "_")]] <- tibble(
        country  = cntry,
        group    = grp,
        split    = "polint_group",
        dv       = dv,
        dv_label = indiv_dv_labels[dv],
        estimate = econ_row$estimate,
        std.error = econ_row$std.error,
        p.value  = econ_row$p.value,
        n        = nobs(m)
      )
    }
  }
}

polint_indiv_df <- bind_rows(polint_indiv_results)

cat("\nPolitical interest subgroup (individual items):\n")
polint_indiv_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, group, dv_label, estimate, p.value, sig, n) |>
  arrange(country, dv_label, group) |>
  print(n = 20)

# =============================================================================
# STEP 5: Ordered logit (Korea + Taiwan, dem_pref)
#
# Currently hard-coded: Korea β = -0.003, Taiwan β = -1.201
# =============================================================================
cat("\n=== STEP 5: Ordered logit ===\n")

library(MASS)

ologit_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |>
    filter(country_label == cntry, !is.na(dem_always_preferable)) |>
    mutate(dem_pref_factor = factor(dem_always_preferable, ordered = TRUE))

  f <- as.formula(paste("dem_pref_factor ~ econ_index + factor(wave) +", controls))
  m <- tryCatch(polr(f, data = sub, Hess = TRUE), error = function(e) NULL)
  if (is.null(m)) next

  coefs <- tidy(m)
  econ_row <- coefs |> filter(term == "econ_index")

  # For polr, need to compute p-value from t-value
  econ_row <- econ_row |>
    mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))

  ologit_results[[cntry]] <- tibble(
    country   = cntry,
    dv        = "dem_always_preferable",
    method    = "ordered_logit",
    estimate  = econ_row$estimate,
    std.error = econ_row$std.error,
    statistic = econ_row$statistic,
    p.value   = econ_row$p.value,
    n         = nobs(m)
  )
}

ologit_df <- bind_rows(ologit_results)

cat("\nOrdered logit (dem_always_preferable):\n")
ologit_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  dplyr::select(country, estimate, std.error, p.value, sig, n) |>
  print()

# =============================================================================
# STEP 6: System pride mechanism probe results
#
# Hard-coded in revised sections. Need to save these.
# =============================================================================
cat("\n=== STEP 6: System pride mechanism probe ===\n")

# Check if system pride variables exist
if (!"sys_proud_n" %in% names(dat)) {
  cat("sys_proud_n not in dat — skipping system pride models.\n")
  syspride_results <- NULL
} else {
  syspride_interaction <- list()
  syspride_subgroup    <- list()

  for (cntry in c("Korea", "Taiwan")) {
    sub <- dat |> filter(country_label == cntry, !is.na(sys_proud_n))
    cat(sprintf("%s sys_proud: n = %d\n", cntry, nrow(sub)))

    # Continuous interaction
    for (dv_pair in list(
      c("qual_index",      "Quality index"),
      c("qual_pref_dem_n", "Democracy always preferable"),
      c("qual_extent_n",   "Extent democratic")
    )) {
      f <- as.formula(paste(dv_pair[1], "~ econ_index * sys_proud_n + factor(wave) +",
                            controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      syspride_interaction[[paste(cntry, dv_pair[1], sep = "_")]] <-
        tidy(m, conf.int = TRUE) |>
        filter(term %in% c("econ_index", "sys_proud_n", "econ_index:sys_proud_n")) |>
        mutate(country = cntry, dv = dv_pair[2], n = nobs(m))
    }

    # Median-split subgroups
    med <- median(sub$sys_proud_n, na.rm = TRUE)
    sub <- sub |> mutate(
      syspride_group = if_else(sys_proud_n > med, "High system pride", "Low system pride")
    )

    for (grp in c("High system pride", "Low system pride")) {
      grp_sub <- sub |> filter(syspride_group == grp)
      if (nrow(grp_sub) < 150) next

      for (dv_pair in list(
        c("qual_index",      "Quality index"),
        c("qual_pref_dem_n", "Democracy always preferable"),
        c("qual_extent_n",   "Extent democratic")
      )) {
        f <- as.formula(paste(dv_pair[1], "~ econ_index + factor(wave) +", controls))
        m <- tryCatch(lm(f, data = grp_sub), error = function(e) NULL)
        if (is.null(m)) next

        econ_row <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")

        syspride_subgroup[[paste(cntry, grp, dv_pair[1], sep = "_")]] <-
          econ_row |>
          mutate(country = cntry, group = grp, dv = dv_pair[2], n = nobs(m))
      }
    }
  }

  # National pride comparison (for side-by-side)
  natpride_interaction <- list()
  for (cntry in c("Korea", "Taiwan")) {
    sub <- dat |> filter(country_label == cntry,
                         !is.na(sys_proud_n), !is.na(nat_proud_n))

    for (mod_info in list(
      c("nat_proud_n",  "National pride"),
      c("sys_proud_n",  "System pride")
    )) {
      f <- as.formula(paste("qual_pref_dem_n ~ econ_index *", mod_info[1],
                            "+ factor(wave) +", controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      int_term <- paste0("econ_index:", mod_info[1])
      natpride_interaction[[paste(cntry, mod_info[1], sep = "_")]] <-
        tidy(m, conf.int = TRUE) |>
        filter(term == int_term) |>
        mutate(country = cntry, moderator = mod_info[2], n = nobs(m))
    }
  }

  syspride_results <- list(
    interaction = bind_rows(syspride_interaction),
    subgroup    = bind_rows(syspride_subgroup),
    comparison  = bind_rows(natpride_interaction)
  )

  cat("\nSystem pride interaction results:\n")
  syspride_results$interaction |>
    mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                           p.value < 0.05  ~ "*",   TRUE ~ "")) |>
    dplyr::select(country, dv, term, estimate, p.value, sig, n) |>
    print(n = 30)

  cat("\nSystem pride subgroup results:\n")
  syspride_results$subgroup |>
    mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                           p.value < 0.05  ~ "*",   TRUE ~ "")) |>
    dplyr::select(country, group, dv, estimate, p.value, sig, n) |>
    print(n = 20)

  cat("\nNat pride vs sys pride comparison:\n")
  syspride_results$comparison |>
    mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                           p.value < 0.05  ~ "*",   TRUE ~ "")) |>
    dplyr::select(country, moderator, estimate, p.value, sig, n) |>
    print()
}

# =============================================================================
# STEP 7: National pride mechanism (original, for backward compat)
# =============================================================================
cat("\n=== STEP 7: National pride mechanism (original) ===\n")

natpride_subgroup <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(nat_proud_high))

  for (grp in c("High pride", "Low pride")) {
    grp_sub <- sub |> filter(nat_proud_high == grp)
    if (nrow(grp_sub) < 150) next

    f <- as.formula(paste("qual_pref_dem_n ~ econ_index + factor(wave) +", controls))
    m <- tryCatch(lm(f, data = grp_sub), error = function(e) NULL)
    if (is.null(m)) next

    econ_row <- tidy(m, conf.int = TRUE) |> filter(term == "econ_index")

    natpride_subgroup[[paste(cntry, grp, sep = "_")]] <-
      econ_row |>
      mutate(country = cntry, group = grp, dv = "qual_pref_dem_n", n = nobs(m))
  }
}

# National pride interaction term
natpride_int <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(nat_proud_n))

  f <- as.formula(paste("qual_pref_dem_n ~ econ_index * nat_proud_n + factor(wave) +",
                        controls))
  m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
  if (is.null(m)) next

  natpride_int[[cntry]] <-
    tidy(m, conf.int = TRUE) |>
    filter(term %in% c("econ_index", "nat_proud_n", "econ_index:nat_proud_n")) |>
    mutate(country = cntry, n = nobs(m))
}

natpride_results <- list(
  subgroup    = bind_rows(natpride_subgroup),
  interaction = bind_rows(natpride_int)
)

cat("\nNational pride subgroup:\n")
natpride_results$subgroup |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  dplyr::select(country, group, estimate, p.value, sig, n) |>
  print()

cat("\nNational pride interaction:\n")
natpride_results$interaction |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  filter(term == "econ_index:nat_proud_n") |>
  dplyr::select(country, term, estimate, p.value, sig, n) |>
  print()

# =============================================================================
# STEP 8: Save expanded model_results.rds
# =============================================================================
cat("\n=== STEP 8: Saving expanded results ===\n")

# Preserve existing structure, add new components
mr_expanded <- mr

# Add new components
mr_expanded$wave_indiv        <- wave_indiv_df
mr_expanded$pooled_indiv      <- pooled_indiv_df
mr_expanded$xc_indiv          <- xc_indiv_df
mr_expanded$polint_indiv      <- polint_indiv_df
mr_expanded$ologit            <- ologit_df
mr_expanded$syspride          <- syspride_results
mr_expanded$natpride          <- natpride_results

# Back up old file
file.copy(
  file.path(results_dir, "model_results.rds"),
  file.path(results_dir, "model_results_backup.rds"),
  overwrite = TRUE
)

saveRDS(mr_expanded, file.path(results_dir, "model_results.rds"))

cat("\n✓ Saved expanded model_results.rds\n")
cat("  New components: wave_indiv, pooled_indiv, xc_indiv, polint_indiv,\n")
cat("                  ologit, syspride, natpride\n")
cat("  Backup: model_results_backup.rds\n")

# =============================================================================
# STEP 9: Print manuscript lookup table
#
# This maps hard-coded values in the manuscript to their R accessor paths
# for use in inline R code.
# =============================================================================
cat("\n\n=== MANUSCRIPT WIRING REFERENCE ===\n")
cat("Use these accessors in inline R code:\n\n")

cat("# Setup code for manuscript:\n")
cat("# mr <- readRDS('analysis/results/model_results.rds')\n\n")

cat("# --- Wave-by-wave individual items ---\n")
cat("# mr$wave_indiv |> filter(country=='Korea', dv=='sat_democracy_n', wave==2)\n")
cat("# mr$wave_indiv |> filter(country=='Taiwan', dv=='qual_pref_dem_n', wave==6)\n\n")

cat("# --- Pooled individual items ---\n")
cat("# Korea pooled dem_pref: mr$pooled_indiv |> filter(country=='Korea', dv=='qual_pref_dem_n')\n")
cat("# Taiwan pooled dem_pref: mr$pooled_indiv |> filter(country=='Taiwan', dv=='qual_pref_dem_n')\n")
cat("# Taiwan pooled dem_extent: mr$pooled_indiv |> filter(country=='Taiwan', dv=='qual_extent_n')\n\n")

cat("# --- Cross-country interaction (individual items) ---\n")
cat("# dem_pref interaction: mr$xc_indiv |> filter(dv=='qual_pref_dem_n', term=='econ_index:is_korea')\n\n")

cat("# --- Political interest subgroup ---\n")
cat("# Korea high polint dem_pref: mr$polint_indiv |> filter(country=='Korea', group=='High interest', dv=='qual_pref_dem_n')\n")
cat("# Korea low polint dem_pref: mr$polint_indiv |> filter(country=='Korea', group=='Low interest', dv=='qual_pref_dem_n')\n\n")

cat("# --- Ordered logit ---\n")
cat("# Korea: mr$ologit |> filter(country=='Korea')\n")
cat("# Taiwan: mr$ologit |> filter(country=='Taiwan')\n\n")

cat("# --- System pride mechanism ---\n")
cat("# Interaction: mr$syspride$interaction |> filter(country=='Taiwan', dv=='Democracy always preferable', term=='econ_index:sys_proud_n')\n")
cat("# Subgroup: mr$syspride$subgroup |> filter(country=='Taiwan', dv=='Democracy always preferable')\n")
cat("# Comparison: mr$syspride$comparison |> filter(country=='Taiwan')\n\n")

cat("# --- National pride mechanism ---\n")
cat("# Subgroup: mr$natpride$subgroup |> filter(country=='Taiwan')\n")
cat("# Interaction: mr$natpride$interaction |> filter(country=='Taiwan', term=='econ_index:nat_proud_n')\n\n")

cat("=== Done ===\n")
