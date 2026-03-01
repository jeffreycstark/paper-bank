## ──────────────────────────────────────────────────────────────────────────────
## 08 — KAMOS Economic Satisfaction Moderation Test
##
## Tests whether the intermediary trust collapse (Accountability Gap) persists
## among respondents with positive economic evaluations, ruling out economic
## malaise as a confound.
##
## Specification:
##   Pooled weighted OLS (as in 06_kamos_pooled_ols.R) extended with:
##   (a) Continuous: trust ~ wave_f * inst_type * econ_z + controls
##   (b) Tercile:    trust ~ wave_f * inst_type * econ_tercile + controls
##   Both econ_national and econ_family as moderators.
##
## Also produces:
##   - Descriptive table of output-legitimacy variables (2016 vs 2019)
##     showing the "satisfaction paradox": econ vars rose, trust fell.
##
## Output:
##   results/08_econ_moderation.rds
##   results/08_output_legitimacy_table.csv
##   figures/fig_econ_moderation_*.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(sandwich)
library(lmtest)
library(marginaleffects)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/07_south_korea_accountability_gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")

# ── 1. Load & prepare ─────────────────────────────────────────────────────────
kamos <- readRDS(kamos_harmonized_path) |>
  mutate(
    resp_id           = row_number(),
    trust_legislative = (trust_national_assembly + trust_legislature) / 2,
    wave_f            = factor(wave, levels = c(1, 4), labels = c("W1", "W4")),
    # Standardize econ vars (pooled across waves for comparability)
    econ_national_z   = as.numeric(scale(econ_national)),
    econ_family_z     = as.numeric(scale(econ_family)),
    econ_composite    = (econ_national + econ_family) / 2,
    econ_composite_z  = as.numeric(scale(econ_composite)),
    # Terciles (pooled)
    econ_nat_tercile  = ntile(econ_national,  3),
    econ_fam_tercile  = ntile(econ_family,    3),
    econ_comp_tercile = ntile(econ_composite, 3)
  ) |>
  mutate(
    econ_nat_tercile  = factor(econ_nat_tercile,  labels = c("Low", "Mid", "High")),
    econ_fam_tercile  = factor(econ_fam_tercile,  labels = c("Low", "Mid", "High")),
    econ_comp_tercile = factor(econ_comp_tercile, labels = c("Low", "Mid", "High"))
  )

cat("KAMOS n =", nrow(kamos), "| waves:", paste(sort(unique(kamos$wave)), collapse = " "), "\n")
cat("econ_national range:", range(kamos$econ_national, na.rm=TRUE), "\n")
cat("econ_family   range:", range(kamos$econ_family,   na.rm=TRUE), "\n")

# ── 2. Descriptive table: output-legitimacy paradox ───────────────────────────
# Variables that ROSE while trust FELL — the output legitimacy paradox
output_vars <- c(
  "econ_national", "econ_family", "pol_satisfaction",
  "national_pride", "social_mobility", "social_mobility_next_gen"
)
trust_vars <- c(
  "trust_central_govt", "trust_local_govt",
  "trust_national_assembly", "trust_legislature",
  "trust_media", "trust_ngo"
)
all_desc_vars <- c(output_vars, trust_vars)

desc_table <- kamos |>
  pivot_longer(cols = all_of(all_desc_vars), names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(variable, wave_f) |>
  summarise(
    mean = weighted.mean(value, w = weight, na.rm = TRUE),
    sd   = sqrt(Hmisc::wtd.var(value, weights = weight, na.rm = TRUE)),
    n    = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from  = wave_f,
    values_from = c(mean, sd, n),
    names_glue  = "{wave_f}_{.value}"
  ) |>
  mutate(
    change   = round(W4_mean - W1_mean, 3),
    W1_mean  = round(W1_mean, 3),
    W4_mean  = round(W4_mean, 3),
    W1_sd    = round(W1_sd, 3),
    W4_sd    = round(W4_sd, 3),
    domain   = case_when(
      variable %in% output_vars ~ "Output legitimacy",
      TRUE                       ~ "Institutional trust"
    )
  ) |>
  arrange(domain, variable) |>
  select(domain, variable, W1_mean, W1_sd, W4_mean, W4_sd, change, W1_n, W4_n)

cat("\n=== Output Legitimacy Paradox: 2016 vs 2019 ===\n")
print(desc_table, n = 30)

write.csv(desc_table,
          file.path(results_dir, "08_output_legitimacy_table.csv"),
          row.names = FALSE)
cat("Descriptive table saved.\n")

# ── 3. Reshape to long (3-type classification) ────────────────────────────────
kamos_long <- kamos |>
  pivot_longer(
    cols      = c(trust_central_govt, trust_local_govt,
                  trust_legislative, trust_media, trust_ngo),
    names_to  = "institution",
    values_to = "trust"
  ) |>
  mutate(
    inst_type = case_when(
      institution %in% c("trust_central_govt", "trust_local_govt") ~ "Executive",
      institution == "trust_legislative"                            ~ "Horizontal",
      institution %in% c("trust_media", "trust_ngo")               ~ "Societal"
    ),
    inst_type = factor(inst_type, levels = c("Executive", "Horizontal", "Societal"))
  ) |>
  filter(!is.na(trust), !is.na(weight),
         !is.na(econ_national_z), !is.na(econ_family_z))

cat("\nLong rows:", nrow(kamos_long),
    "| unique resp_id:", n_distinct(kamos_long$resp_id), "\n")

# ── 4a. Three-way model — econ_national continuous ────────────────────────────
m_econ_cont <- lm(
  trust ~ wave_f * inst_type * econ_national_z +
    age + gender + education + income + ideology + party_id,
  data    = kamos_long,
  weights = weight
)
vcov_cont <- vcovCL(m_econ_cont, cluster = ~resp_id, type = "HC1")
coef_cont <- coeftest(m_econ_cont, vcov = vcov_cont)

cat("\n=== Three-way model: continuous econ_national (standardized) ===\n")
# Print only wave and interaction terms
rows_of_interest <- grep("wave_fW4", rownames(coef_cont), value = TRUE)
print(coef_cont[rows_of_interest, ])

# ── 4b. Three-way model — econ composite tercile ─────────────────────────────
m_econ_terc <- lm(
  trust ~ wave_f * inst_type * econ_comp_tercile +
    age + gender + education + income + ideology + party_id,
  data    = kamos_long |> filter(!is.na(econ_comp_tercile)),
  weights = weight
)
vcov_terc <- vcovCL(m_econ_terc, cluster = ~resp_id, type = "HC1")
coef_terc <- coeftest(m_econ_terc, vcov = vcov_terc)

cat("\n=== Three-way model: composite econ tercile ===\n")
rows_terc <- grep("wave_fW4", rownames(coef_terc), value = TRUE)
print(coef_terc[rows_terc, ])

# ── 5. Key result: does the 4:1 ratio survive conditioning on high econ sat? ──
# W1→W4 change by inst_type, within each econ tercile
did_by_tercile <- avg_comparisons(
  m_econ_terc,
  variables = "wave_f",
  by        = c("inst_type", "econ_comp_tercile"),
  vcov      = vcov_terc
)

cat("\n=== W1→W4 trust change by institution type × econ satisfaction tercile ===\n")
did_by_tercile |>
  as_tibble() |>
  select(inst_type, econ_comp_tercile, estimate, std.error, p.value) |>
  mutate(across(c(estimate, std.error), \(x) round(x, 3)),
         p.value = round(p.value, 4)) |>
  arrange(econ_comp_tercile, inst_type) |>
  print(n = 30)

# Among HIGH economic satisfaction respondents: is collapse still 4:1?
high_econ <- did_by_tercile |>
  as_tibble() |>
  filter(econ_comp_tercile == "High") |>
  select(inst_type, estimate) |>
  mutate(estimate = abs(estimate))

exec_hi  <- high_econ$estimate[high_econ$inst_type == "Executive"]
horiz_hi <- high_econ$estimate[high_econ$inst_type == "Horizontal"]
soc_hi   <- high_econ$estimate[high_econ$inst_type == "Societal"]
ratio_hi <- round(mean(c(horiz_hi, soc_hi)) / exec_hi, 1)

cat(sprintf("\nAmong HIGH econ-satisfaction respondents:\n"))
cat(sprintf("  Executive decline:   %.3f\n", exec_hi))
cat(sprintf("  Horizontal decline:  %.3f\n", horiz_hi))
cat(sprintf("  Societal decline:    %.3f\n", soc_hi))
cat(sprintf("  Intermediary/executive ratio: %.1f : 1\n", ratio_hi))

# ── 6. Predicted margins for figure ──────────────────────────────────────────
preds_terc <- predictions(
  m_econ_terc,
  newdata = datagrid(
    wave_f            = c("W1", "W4"),
    inst_type         = c("Executive", "Horizontal", "Societal"),
    econ_comp_tercile = c("Low", "Mid", "High")
  ),
  vcov = vcov_terc
)

# ── 7. Figure ──────────────────────────────────────────────────────────────────
fig_econ <- preds_terc |>
  as_tibble() |>
  mutate(
    wave_label = recode(wave_f, "W1" = "2016", "W4" = "2019"),
    inst_label = recode(inst_type,
      "Executive"  = "Executive",
      "Horizontal" = "Horizontal\n(Legislature)",
      "Societal"   = "Societal\n(Media + NGO)"
    ),
    econ_label = paste0("Economic satisfaction:\n", econ_comp_tercile)
  ) |>
  ggplot(aes(x = wave_label, y = estimate,
             color = inst_label, group = inst_label,
             ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = inst_label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.4) +
  facet_wrap(~econ_label, nrow = 1) +
  scale_color_manual(values = c(
    "Executive"           = "#0072B2",
    "Horizontal\n(Legislature)" = "#D55E00",
    "Societal\n(Media + NGO)"   = "#009E73"
  )) +
  scale_fill_manual(values = c(
    "Executive"           = "#0072B2",
    "Horizontal\n(Legislature)" = "#D55E00",
    "Societal\n(Media + NGO)"   = "#009E73"
  )) +
  scale_y_continuous(limits = c(1, 8), breaks = seq(2, 8, 2)) +
  labs(
    x        = NULL,
    y        = "Predicted trust (0-10)",
    color    = NULL, fill = NULL,
    title    = "Accountability gap by economic satisfaction tercile, 2016-2019",
    subtitle = "Pooled weighted OLS, cluster-robust SEs. Three-way Wave x InstitutionType x EconSat interaction.",
    caption  = "Predicted margins. 95% CIs. Source: KAMOS W1 (2016) / W4 (2019)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 9, face = "bold"),
    panel.spacing   = unit(1, "lines")
  )

ggsave(file.path(fig_dir, "fig_econ_moderation_tercile.pdf"),
       fig_econ, width = 9, height = 5)
ggsave(file.path(fig_dir, "fig_econ_moderation_tercile.png"),
       fig_econ, width = 9, height = 5, dpi = 300)
cat("Figure saved.\n")

# ── 8. Save all results ───────────────────────────────────────────────────────
econ_results <- list(
  coef_continuous    = coef_cont,
  coef_tercile       = coef_terc,
  did_by_tercile     = did_by_tercile,
  preds_tercile      = preds_terc,
  ratio_high_econ    = ratio_hi,
  exec_hi            = exec_hi,
  horiz_hi           = horiz_hi,
  soc_hi             = soc_hi
)

saveRDS(econ_results, file.path(results_dir, "08_econ_moderation.rds"))
cat("\nAll results saved to", results_dir, "\n")
