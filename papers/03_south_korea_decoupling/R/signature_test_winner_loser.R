# =============================================================================
# Signature test: Winner/loser moderation of econ → democratic attitudes
#
# Theory:
#   If Korea's decoupling is structural (performance-derived legitimation),
#   it should hold for BOTH winners and losers. Winners might show slightly
#   higher satisfaction, but neither group should link econ to normative
#   commitment. If the decoupling were merely a sore-loser effect, losers
#   would drive it — and winners would look more like Taiwan.
#
#   In Taiwan (identity-fused), the critical citizens pattern should appear
#   in BOTH groups because normative commitment transcends partisanship.
#   The winner/loser gap should appear on satisfaction (universal finding)
#   but NOT on normative commitment (because the identity anchor is
#   independent of partisan fortunes).
#
# Variable: electoral_status (W2–6, 1=Winner, 2=Loser)
# =============================================================================

library(tidyverse)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/south-korea-decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")

source(file.path(project_root, "_data_config.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

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
# Step 0: Merge electoral_status if not already present
# =============================================================================
if (!"electoral_status" %in% names(dat)) {
  cat("Merging electoral_status from harmonized ABS...\n")
  abs_all <- readRDS(abs_harmonized_path)

  es_merge <- abs_all |>
    filter(country %in% c(3, 7)) |>
    select(wave, country, row_id, electoral_status)

  if ("row_id" %in% names(dat)) {
    dat <- dat |> left_join(es_merge, by = c("wave", "country", "row_id"))
  } else {
    es_merge2 <- abs_all |>
      filter(country %in% c(3, 7)) |>
      select(wave, country, idnumber, electoral_status)
    dat <- dat |> left_join(es_merge2, by = c("wave", "country", "idnumber"))
  }
  cat("  Done.\n")
} else {
  cat("electoral_status already present.\n")
}

# Create labeled factor
dat <- dat |>
  mutate(
    wl_status = case_when(
      electoral_status == 1 ~ "Winner",
      electoral_status == 2 ~ "Loser",
      TRUE ~ NA_character_
    )
  )

# =============================================================================
# Step 1: Coverage check
# =============================================================================
cat("\n=== Winner/loser coverage ===\n")
dat |>
  filter(!is.na(wl_status)) |>
  group_by(country_label, wave, wl_status) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = wl_status, values_from = n, values_fill = 0) |>
  mutate(total = Winner + Loser,
         pct_winner = round(100 * Winner / total, 1)) |>
  print(n = 20)

cat("\nTotal by country:\n")
dat |>
  filter(!is.na(wl_status)) |>
  count(country_label, wl_status) |>
  pivot_wider(names_from = wl_status, values_from = n) |>
  print()

# =============================================================================
# Step 2: Continuous interaction — econ × winner/loser
#
# Model: DV ~ econ_index * is_winner + factor(wave) + controls
# Key coefficient: econ_index:is_winner
#
# Predictions:
#   Satisfaction: significant positive interaction (winners more satisfied,
#     universal finding) — BOTH countries
#   Dem_pref Korea: null interaction (decoupling holds for both groups)
#   Dem_pref Taiwan: null interaction (critical citizens in both groups)
# =============================================================================
cat("\n=== 2. Continuous interaction: econ × winner ===\n")

dat <- dat |>
  mutate(is_winner = as.numeric(wl_status == "Winner"))

wl_interaction <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry, !is.na(wl_status))
  cat(sprintf("%s: n = %d (winners = %d, losers = %d)\n",
              cntry, nrow(sub),
              sum(sub$is_winner == 1), sum(sub$is_winner == 0)))

  for (dv_pair in list(
    c("sat_democracy_n", "Satisfaction with democracy"),
    c("sat_govt_n",      "Satisfaction with government"),
    c("qual_pref_dem_n", "Democracy always preferable"),
    c("qual_extent_n",   "Extent democratic"),
    c("qual_index",      "Quality index"),
    c("sat_index",       "Satisfaction index")
  )) {
    f <- as.formula(paste(dv_pair[1],
                          "~ econ_index * is_winner + factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
    if (is.null(m)) next

    wl_interaction[[paste(cntry, dv_pair[1], sep = "_")]] <-
      tidy(m, conf.int = TRUE) |>
      filter(term %in% c("econ_index", "is_winner", "econ_index:is_winner")) |>
      mutate(country = cntry, dv = dv_pair[2], n = nobs(m))
  }
}

wl_int_df <- bind_rows(wl_interaction)

cat("\n--- Interaction results ---\n")
wl_int_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv, term, estimate, std.error, p.value, sig, n) |>
  print(n = 50)

# Focus: the key interaction term
cat("\n--- Key interaction (econ × winner) summary ---\n")
wl_int_df |>
  filter(term == "econ_index:is_winner") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv, estimate, p.value, sig, n) |>
  print(n = 20)

# =============================================================================
# Step 3: Subgroup split — winners vs losers separately
#
# This is the money table: econ → dem_pref for winners and losers in each
# country. The signature prediction:
#   Korea winners:  ~0 (decoupled)
#   Korea losers:   ~0 (decoupled)
#   Taiwan winners: negative (critical citizens)
#   Taiwan losers:  negative (critical citizens)
# =============================================================================
cat("\n=== 3. Subgroup split: winners vs losers ===\n")

wl_subgroup <- list()

for (cntry in c("Korea", "Taiwan")) {
  for (grp in c("Winner", "Loser")) {
    sub <- dat |> filter(country_label == cntry, wl_status == grp)
    if (nrow(sub) < 150) {
      cat(sprintf("  SKIP %s %s: n = %d\n", cntry, grp, nrow(sub)))
      next
    }

    for (dv_pair in list(
      c("sat_democracy_n", "Satisfaction with democracy"),
      c("sat_govt_n",      "Satisfaction with government"),
      c("qual_pref_dem_n", "Democracy always preferable"),
      c("qual_extent_n",   "Extent democratic")
    )) {
      f <- as.formula(paste(dv_pair[1], "~ econ_index + factor(wave) +", controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      wl_subgroup[[paste(cntry, grp, dv_pair[1], sep = "_")]] <-
        extract_econ_coef(m) |>
        mutate(country = cntry, group = grp, dv = dv_pair[2], n = nobs(m))
    }
  }
}

wl_sub_df <- bind_rows(wl_subgroup)

cat("\n--- Winner/loser subgroup results ---\n")
wl_sub_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, group, dv, estimate, p.value, sig, n) |>
  arrange(dv, country, group) |>
  print(n = 30)

# The signature table
cat("\n=== SIGNATURE TABLE: econ → dem_pref by winner/loser ===\n")
wl_sub_df |>
  filter(dv == "Democracy always preferable") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ ""),
         result = sprintf("β = %.3f (%s)", estimate, 
                          ifelse(sig == "", sprintf("p = %.3f", p.value), sig))) |>
  select(country, group, result, n) |>
  print()

cat("\n=== COMPARISON: econ → satisfaction by winner/loser ===\n")
wl_sub_df |>
  filter(dv == "Satisfaction with democracy") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ ""),
         result = sprintf("β = %.3f (%s)", estimate,
                          ifelse(sig == "", sprintf("p = %.3f", p.value), sig))) |>
  select(country, group, result, n) |>
  print()

# =============================================================================
# Step 4: Wave-by-wave winner/loser (dem_pref only)
#
# Shows stability of the pattern across time
# =============================================================================
cat("\n=== 4. Wave-by-wave: econ → dem_pref by winner/loser ===\n")

wl_wave <- list()

for (cntry in c("Korea", "Taiwan")) {
  for (w in 2:6) {  # electoral_status starts W2
    for (grp in c("Winner", "Loser")) {
      sub <- dat |> filter(country_label == cntry, wave == w, wl_status == grp)
      if (nrow(sub) < 80) next

      f <- as.formula(paste("qual_pref_dem_n ~ econ_index +", controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      wl_wave[[paste(cntry, w, grp, sep = "_")]] <-
        extract_econ_coef(m) |>
        mutate(country = cntry, wave = w, group = grp, n = nobs(m))
    }
  }
}

wl_wave_df <- bind_rows(wl_wave)

wl_wave_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, wave, group, estimate, p.value, sig, n) |>
  arrange(country, wave, group) |>
  print(n = 30)

# =============================================================================
# Step 5: Three-way interaction — econ × winner × Korea
#
# Tests whether the winner/loser moderation differs across countries
# =============================================================================
cat("\n=== 5. Three-way: econ × winner × Korea ===\n")

both <- dat |>
  filter(!is.na(wl_status), !is.na(qual_pref_dem_n)) |>
  mutate(is_korea = as.numeric(country_label == "Korea"))

f3 <- as.formula(paste("qual_pref_dem_n ~ econ_index * is_winner * is_korea",
                       "+ factor(wave) +", controls))
m3 <- lm(f3, data = both)

cat("\nThree-way interaction (dem_pref):\n")
tidy(m3, conf.int = TRUE) |>
  filter(str_detect(term, "econ_index|is_winner|is_korea")) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(term, estimate, std.error, p.value, sig) |>
  print()

# Same for satisfaction
f3s <- as.formula(paste("sat_democracy_n ~ econ_index * is_winner * is_korea",
                        "+ factor(wave) +", controls))
m3s <- lm(f3s, data = both |> filter(!is.na(sat_democracy_n)))

cat("\nThree-way interaction (satisfaction):\n")
tidy(m3s, conf.int = TRUE) |>
  filter(str_detect(term, "econ_index|is_winner|is_korea")) |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(term, estimate, std.error, p.value, sig) |>
  print()

# =============================================================================
# Step 6: Save results
# =============================================================================
wl_results <- list(
  interaction   = wl_int_df,
  subgroup      = wl_sub_df,
  wave_by_wave  = wl_wave_df,
  three_way_pref = tidy(m3, conf.int = TRUE),
  three_way_sat  = tidy(m3s, conf.int = TRUE)
)

saveRDS(wl_results, file.path(results_dir, "winner_loser_results.rds"))
cat("\n✓ Saved to", file.path(results_dir, "winner_loser_results.rds"), "\n")

# =============================================================================
# Step 7: Print summary for manuscript
# =============================================================================
cat("\n\n")
cat("================================================================\n")
cat("  MANUSCRIPT SUMMARY: Winner/Loser Signature Test\n")
cat("================================================================\n\n")

cat("SATISFACTION (econ → sat_dem):\n")
wl_sub_df |>
  filter(dv == "Satisfaction with democracy") |>
  mutate(result = sprintf("  %s %-7s: β = %+.3f (p %s)",
                          country, group, estimate,
                          ifelse(p.value < 0.001, "< 0.001",
                                 sprintf("= %.3f", p.value)))) |>
  pull(result) |>
  cat(sep = "\n")

cat("\n\nNORMATIVE COMMITMENT (econ → dem_pref):\n")
wl_sub_df |>
  filter(dv == "Democracy always preferable") |>
  mutate(result = sprintf("  %s %-7s: β = %+.3f (p %s)",
                          country, group, estimate,
                          ifelse(p.value < 0.001, "< 0.001",
                                 sprintf("= %.3f", p.value)))) |>
  pull(result) |>
  cat(sep = "\n")

cat("\n\nINTERPRETATION:\n")
cat("  If Korea shows null for BOTH winners and losers on dem_pref:\n")
cat("    → Decoupling is structural, not a sore-loser effect\n")
cat("  If Taiwan shows negative for BOTH winners and losers:\n")
cat("    → Critical citizens pattern transcends partisanship\n")
cat("  If satisfaction shows winner > loser in both countries:\n")
cat("    → Winner/loser gap operates on performance track only\n")
cat("================================================================\n")
