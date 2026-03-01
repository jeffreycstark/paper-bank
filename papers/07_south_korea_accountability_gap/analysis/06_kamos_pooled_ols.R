## ──────────────────────────────────────────────────────────────────────────────
## 06 — KAMOS Pooled Weighted OLS (replaces LMM in 05_kamos_trust_threeway.R)
##
## Respecification: pooled cross-sectional OLS with survey weights,
## wave × inst_type interactions, cluster-robust SEs on respondent ID.
##
## Two models:
##   m_pooled   : 3-type composite (Executive / Horizontal / Societal)
##   m_split    : 6 individual institutions (both legislative items separate)
##
## Output: results/kamos_pooled_*.rds, figures/fig_trust_pooled.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(sandwich)
library(lmtest)
library(marginaleffects)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/07_south_korea_accountability_gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")

# ── 1. Load & prepare ─────────────────────────────────────────────────────────
kamos <- readRDS(kamos_harmonized_path) |>
  mutate(
    resp_id          = row_number(),
    trust_legislative = (trust_national_assembly + trust_legislature) / 2,
    wave_f            = factor(wave, levels = c(1, 4), labels = c("W1", "W4"))
  )

cat("KAMOS: n =", nrow(kamos), "| waves:", paste(sort(unique(kamos$wave)), collapse = " "),
    "| weight range:", round(range(kamos$weight, na.rm = TRUE), 3), "\n")

# ── 2a. 3-TYPE model — composite legislative item ────────────────────────────
kamos_3type <- kamos |>
  pivot_longer(
    cols      = c(trust_central_govt, trust_local_govt,
                  trust_legislative,
                  trust_media, trust_ngo),
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
  filter(!is.na(trust), !is.na(weight))

cat("3-type long rows:", nrow(kamos_3type),
    "| unique resp_id:", n_distinct(kamos_3type$resp_id), "\n")

# Pooled weighted OLS — Executive is reference, W1 is reference
m_pooled <- lm(
  trust ~ wave_f * inst_type +
    age + gender + education + income + ideology + party_id,
  data    = kamos_3type,
  weights = weight
)

# Cluster-robust SEs (cluster on respondent: 5 obs per person × 2 waves)
vcov_pooled   <- vcovCL(m_pooled, cluster = ~resp_id, type = "HC1")
coef_pooled   <- coeftest(m_pooled, vcov = vcov_pooled)

cat("\n=== Pooled OLS (3 types), cluster-robust SEs ===\n")
print(coef_pooled)

# ── 2b. 6-INSTITUTION model — both legislative items separate ────────────────
kamos_6inst <- kamos |>
  pivot_longer(
    cols      = c(trust_central_govt, trust_local_govt,
                  trust_national_assembly, trust_legislature,
                  trust_media, trust_ngo),
    names_to  = "institution",
    values_to = "trust"
  ) |>
  mutate(
    inst_type = case_when(
      institution %in% c("trust_central_govt", "trust_local_govt") ~ "Executive",
      institution == "trust_national_assembly"                       ~ "Natl_Assembly",
      institution == "trust_legislature"                             ~ "Legislature",
      institution %in% c("trust_media", "trust_ngo")               ~ "Societal"
    ),
    inst_type = factor(inst_type,
                       levels = c("Executive", "Natl_Assembly", "Legislature", "Societal"))
  ) |>
  filter(!is.na(trust), !is.na(weight))

m_split <- lm(
  trust ~ wave_f * inst_type +
    age + gender + education + income + ideology + party_id,
  data    = kamos_6inst,
  weights = weight
)

vcov_split <- vcovCL(m_split, cluster = ~resp_id, type = "HC1")
coef_split <- coeftest(m_split, vcov = vcov_split)

cat("\n=== Pooled OLS (6 institutions), cluster-robust SEs ===\n")
print(coef_split)

# ── 3. Predicted margins (3-type) ────────────────────────────────────────────
preds_pooled <- predictions(
  m_pooled,
  newdata = datagrid(
    wave_f    = c("W1", "W4"),
    inst_type = c("Executive", "Horizontal", "Societal")
  ),
  vcov = vcov_pooled
)

cat("\nPredicted trust margins (3-type model):\n")
preds_pooled |>
  as_tibble() |>
  select(wave_f, inst_type, estimate, conf.low, conf.high) |>
  arrange(inst_type, wave_f) |>
  print(n = 12)

# ── 4. Predicted margins (6-institution) ────────────────────────────────────
preds_split <- predictions(
  m_split,
  newdata = datagrid(
    wave_f    = c("W1", "W4"),
    inst_type = c("Executive", "Natl_Assembly", "Legislature", "Societal")
  ),
  vcov = vcov_split
)

cat("\nPredicted trust margins (6-institution model):\n")
preds_split |>
  as_tibble() |>
  select(wave_f, inst_type, estimate, conf.low, conf.high) |>
  arrange(inst_type, wave_f) |>
  print(n = 16)

# ── 5. Wave change estimates for inline text ─────────────────────────────────
# Difference-in-differences: W4 - W1 by inst_type (cluster-robust)
did_pooled <- avg_comparisons(
  m_pooled,
  variables = "wave_f",
  by        = "inst_type",
  vcov      = vcov_pooled
)

cat("\nW1→W4 changes by institution type (pooled model):\n")
print(did_pooled)

did_split <- avg_comparisons(
  m_split,
  variables = "wave_f",
  by        = "inst_type",
  vcov      = vcov_split
)

cat("\nW1→W4 changes by institution type (6-institution model):\n")
print(did_split)

# ── 6. Figure — 3-type ───────────────────────────────────────────────────────
fig_pooled <- preds_pooled |>
  as_tibble() |>
  mutate(
    wave_label = recode(wave_f, "W1" = "2016", "W4" = "2019"),
    inst_label = recode(inst_type,
      "Executive"  = "Executive\n(Central + Local govt)",
      "Horizontal" = "Horizontal\n(National Assembly composite)",
      "Societal"   = "Societal\n(Media + NGO)"
    )
  ) |>
  ggplot(aes(x = wave_label, y = estimate,
             color = inst_label, group = inst_label,
             ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = inst_label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(values = c(
    "Executive\n(Central + Local govt)"         = "#0072B2",
    "Horizontal\n(National Assembly composite)" = "#D55E00",
    "Societal\n(Media + NGO)"                   = "#009E73"
  )) +
  scale_fill_manual(values = c(
    "Executive\n(Central + Local govt)"         = "#0072B2",
    "Horizontal\n(National Assembly composite)" = "#D55E00",
    "Societal\n(Media + NGO)"                   = "#009E73"
  )) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(
    x        = NULL,
    y        = "Predicted trust (0–10)",
    color    = NULL, fill = NULL,
    title    = "Differential trust collapse by accountability function, 2016–2019",
    subtitle = "Pooled weighted OLS, cluster-robust SEs. Controlled for age, gender, education, income, ideology, party ID.",
    caption  = "Predicted margins at covariate means. 95% CIs.\nSource: KAMOS W1 (2016) / W4 (2019). N ≈ 3,500."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 9))

ggsave(file.path(fig_dir,     "fig_trust_pooled.pdf"), fig_pooled, width = 6.5, height = 5.5)
ggsave(file.path(fig_dir,     "fig_trust_pooled.png"), fig_pooled, width = 6.5, height = 5.5, dpi = 300)
cat("Figure saved.\n")

# ── 7. Figure — 6-institution ────────────────────────────────────────────────
inst_labels_6 <- c(
  "Executive"     = "Executive\n(Central govt)",
  "Natl_Assembly" = "National Assembly",
  "Legislature"   = "Legislature",
  "Societal"      = "Societal\n(Media + NGO)"
)

fig_split <- preds_split |>
  as_tibble() |>
  mutate(
    wave_label = recode(wave_f, "W1" = "2016", "W4" = "2019"),
    inst_label = factor(recode(inst_type, !!!inst_labels_6),
                        levels = inst_labels_6)
  ) |>
  ggplot(aes(x = wave_label, y = estimate,
             color = inst_label, group = inst_label,
             ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = inst_label), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(
    x       = NULL,
    y       = "Predicted trust (0–10)",
    color   = NULL, fill = NULL,
    title   = "Trust collapse by institution, 2016–2019 (legislative items disaggregated)",
    subtitle = "Pooled weighted OLS, cluster-robust SEs.",
    caption = "Predicted margins at covariate means. 95% CIs.\nSource: KAMOS W1 (2016) / W4 (2019)."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 9))

ggsave(file.path(fig_dir, "fig_trust_split.pdf"), fig_split, width = 6.5, height = 5.5)
ggsave(file.path(fig_dir, "fig_trust_split.png"), fig_split, width = 6.5, height = 5.5, dpi = 300)
cat("Split-institution figure saved.\n")

# ── 8. Save all results ───────────────────────────────────────────────────────
# Correlation between the two legislative items (unchanged)
assembly_leg_cor <- cor(kamos$trust_national_assembly, kamos$trust_legislature,
                        use = "complete.obs")
cat(sprintf("\ntrust_national_assembly × trust_legislature  r = %.3f\n", assembly_leg_cor))

saveRDS(coef_pooled,     file.path(results_dir, "kamos_pooled_coeftest.rds"))
saveRDS(coef_split,      file.path(results_dir, "kamos_split_coeftest.rds"))
saveRDS(preds_pooled,    file.path(results_dir, "kamos_pooled_predicted_margins.rds"))
saveRDS(preds_split,     file.path(results_dir, "kamos_split_predicted_margins.rds"))
saveRDS(did_pooled,      file.path(results_dir, "kamos_pooled_did.rds"))
saveRDS(did_split,       file.path(results_dir, "kamos_split_did.rds"))
saveRDS(assembly_leg_cor, file.path(results_dir, "trust3_assembly_leg_cor.rds"))

cat("\nAll results saved to", results_dir, "\n")
