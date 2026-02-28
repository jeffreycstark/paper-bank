# =============================================================================
# App F.1 (modified): System pride × Econ → Quality (both countries)
#
# Replaces nat_proud_n with sys_proud_n as the identity-fusion moderator.
# system_proud ("I am proud of our system of government") is a more targeted
# proxy for democratic-national identity fusion than generic national pride.
#
# Available: waves 3–6 only (both countries).
# Expects: dat from analysis_data.rds with sys_proud_n already constructed.
# =============================================================================

library(tidyverse)
library(broom)

# --- Setup (adjust paths if needed) ---
project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03_south_korea_decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

controls <- "age_n + gender + edu_n + urban_rural + polint_n"

extract_econ_coef <- function(model) {
  tidy(model, conf.int = TRUE) |>
    filter(term == "econ_index") |>
    select(estimate, std.error, statistic, p.value, conf.low, conf.high)
}

# =============================================================================
# 1. Continuous interaction: econ_index × sys_proud_n
# =============================================================================
syspride_interaction <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(sys_proud_n))
  cat(sprintf("%s: n = %d (waves %s)\n", cntry, nrow(sub),
              paste(sort(unique(sub$wave)), collapse = ", ")))

  if (nrow(sub) < 200) next

  # Quality index
  f <- as.formula(paste("qual_index ~ econ_index * sys_proud_n + factor(wave) +", controls))
  m <- lm(f, data = sub)

  syspride_interaction[[paste(cntry, "qual_index", sep = "_")]] <-
    tidy(m, conf.int = TRUE) |>
    filter(term %in% c("econ_index", "sys_proud_n", "econ_index:sys_proud_n")) |>
    mutate(country = cntry, moderator = "System pride", dv = "Quality index",
           n = nobs(m))

  # Individual normative items
  for (dv_pair in list(
    c("qual_pref_dem_n", "Democracy always preferable"),
    c("qual_extent_n",   "Extent democratic")
  )) {
    f2 <- as.formula(paste(dv_pair[1], "~ econ_index * sys_proud_n + factor(wave) +", controls))
    m2 <- tryCatch(lm(f2, data = sub), error = function(e) NULL)
    if (is.null(m2)) next

    syspride_interaction[[paste(cntry, dv_pair[1], sep = "_")]] <-
      tidy(m2, conf.int = TRUE) |>
      filter(term %in% c("econ_index", "sys_proud_n", "econ_index:sys_proud_n")) |>
      mutate(country = cntry, moderator = "System pride", dv = dv_pair[2],
             n = nobs(m2))
  }
}

cat("\n=== F.1 (modified): System pride continuous interaction ===\n")
bind_rows(syspride_interaction) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv, term, estimate, std.error, p.value, sig, n) |>
  print(n = 30)

# =============================================================================
# 2. Median-split subgroups (parallels manuscript Section 5.5)
# =============================================================================
syspride_subgroup <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(sys_proud_n))
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

      syspride_subgroup[[paste(cntry, grp, dv_pair[1], sep = "_")]] <-
        extract_econ_coef(m) |>
        mutate(country = cntry, group = grp, dv = dv_pair[2], n = nobs(m))
    }
  }
}

cat("\n=== F.1 (modified): System pride median-split subgroups ===\n")
bind_rows(syspride_subgroup) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, group, dv, estimate, std.error, p.value, sig, n) |>
  arrange(country, dv, group) |>
  print(n = 30)

# =============================================================================
# 3. Comparison: nat_proud_n vs sys_proud_n side by side
#    (Useful for deciding which to report)
# =============================================================================
cat("\n=== Side-by-side: interaction term (econ × moderator) for dem_pref ===\n")
comparison <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry,
                       !is.na(sys_proud_n), !is.na(nat_proud_n))

  for (mod_info in list(
    c("nat_proud_n",  "National pride (generic)"),
    c("sys_proud_n",  "System pride")
  )) {
    f <- as.formula(paste("qual_pref_dem_n ~ econ_index *", mod_info[1],
                          "+ factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
    if (is.null(m)) next

    int_term <- paste0("econ_index:", mod_info[1])
    comparison[[paste(cntry, mod_info[1], sep = "_")]] <-
      tidy(m, conf.int = TRUE) |>
      filter(term == int_term) |>
      mutate(country = cntry, moderator = mod_info[2], n = nobs(m))
  }
}

bind_rows(comparison) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, moderator, estimate, std.error, p.value, sig, n) |>
  print()
