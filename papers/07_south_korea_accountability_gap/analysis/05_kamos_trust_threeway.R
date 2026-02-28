library(tidyverse)
library(lme4)
library(marginaleffects)
library(broom.mixed)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/south-korea-accountability-gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")

# ── 1. Load & prepare ─────────────────────────────────────────────────────────
# NOTE: variable is party_id (not partyid) in the harmonized file
kamos <- readRDS(kamos_harmonized_path) |>
  mutate(
    # KAMOS has no respondent ID column; create one from row position so
    # the long-format random intercept (1 | resp_id) can cluster the 5
    # institution observations that belong to the same individual.
    resp_id = row_number(),
    trust_legislative = (trust_national_assembly + trust_legislature) / 2,
    wave = factor(wave, levels = c(1, 4), labels = c("W1", "W4"))
    # wave is integer in harmonized file; levels c(1,4) is correct
  )

# ── 2. Reshape to long, apply three-way classification ────────────────────────
kamos_long <- kamos |>
  pivot_longer(
    cols      = c(trust_central_govt, trust_local_govt,
                  trust_legislative,
                  trust_media, trust_ngo),
    names_to  = "institution",
    values_to = "trust"
  ) |>
  mutate(
    inst_type = case_when(
      institution %in% c("trust_central_govt", "trust_local_govt") ~ "A_Executive",
      institution == "trust_legislative"                            ~ "B_Horizontal",
      institution %in% c("trust_media", "trust_ngo")               ~ "C_Societal"
    ),
    inst_type = factor(inst_type,
                       levels = c("A_Executive", "B_Horizontal", "C_Societal"),
                       labels = c("Executive", "Horizontal", "Societal"))
  )

cat("Long-format rows:", nrow(kamos_long),
    "| unique resp_id:", n_distinct(kamos_long$resp_id), "\n")

# ── 3. Three-way controlled model ─────────────────────────────────────────────
# Executive = reference category for inst_type
# Random intercept by respondent (repeated measures: 5 institutions × 2 waves)
m_trust3 <- lmer(
  trust ~ wave * inst_type +
    age + gender + education + income + ideology + party_id +
    (1 | resp_id),
  data  = kamos_long,
  REML  = FALSE
)

summary(m_trust3)

# ── 4. Predicted margins ──────────────────────────────────────────────────────
preds <- predictions(
  m_trust3,
  newdata = datagrid(
    wave      = c("W1", "W4"),
    inst_type = c("Executive", "Horizontal", "Societal")
  )
)

# ── 5. Figure ─────────────────────────────────────────────────────────────────
fig_trust3 <- preds |>
  as_tibble() |>
  mutate(
    wave_label = recode(wave, "W1" = "2016", "W4" = "2019"),
    inst_label = recode(inst_type,
      "Executive"   = "Executive\n(Central + Local govt)",
      "Horizontal"  = "Horizontal\n(National Assembly)",
      "Societal"    = "Societal\n(Media + NGO)"
    )
  ) |>
  ggplot(aes(x = wave_label, y = estimate,
             color = inst_label, group = inst_label,
             ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = inst_label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(values = c(
    "Executive\n(Central + Local govt)" = "#0072B2",
    "Horizontal\n(National Assembly)"   = "#D55E00",
    "Societal\n(Media + NGO)"           = "#009E73"
  )) +
  scale_fill_manual(values = c(
    "Executive\n(Central + Local govt)" = "#0072B2",
    "Horizontal\n(National Assembly)"   = "#D55E00",
    "Societal\n(Media + NGO)"           = "#009E73"
  )) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(
    x        = NULL,
    y        = "Predicted trust (0–10)",
    color    = NULL, fill = NULL,
    title    = "Differential trust collapse by accountability function, 2016–2019",
    subtitle = "Controlled for age, gender, education, income, ideology, party ID",
    caption  = "Predicted margins from mixed model (REML=FALSE). 95% CIs.\nSource: KAMOS W1 (2016) / W4 (2019). N ≈ 3,500."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 9))

ggsave(file.path(fig_dir,     "fig_trust_threeway.pdf"), fig_trust3,
       width = 6.5, height = 5.5)
ggsave(file.path(fig_dir,     "fig_trust_threeway.png"), fig_trust3,
       width = 6.5, height = 5.5, dpi = 300)
ggsave(file.path(results_dir, "fig_trust_threeway.pdf"), fig_trust3,
       width = 6.5, height = 5.5)

cat("Figure saved.\n")

# ── 6. Extract interaction terms for inline reporting ─────────────────────────
interaction_terms <- tidy(m_trust3, conf.int = TRUE) |>
  filter(str_detect(term, "wave"))

cat("\nInteraction terms (wave × inst_type):\n")
print(interaction_terms)

# Wave-specific slopes by institution type (for appendix table)
slopes_by_type <- avg_slopes(
  m_trust3,
  variables = "wave",
  by        = "inst_type"
)

cat("\nAverage slopes by inst_type:\n")
print(slopes_by_type)

# ── 7. Save results ───────────────────────────────────────────────────────────
# ── 8. Correlation between the two legislative items (for footnote) ───────────
assembly_leg_cor <- cor(
  kamos$trust_national_assembly,
  kamos$trust_legislature,
  use = "complete.obs"
)
cat(sprintf("\ntrust_national_assembly × trust_legislature  r = %.2f\n",
            assembly_leg_cor))

saveRDS(interaction_terms,  file.path(results_dir, "trust3_interaction_terms.rds"))
saveRDS(slopes_by_type,     file.path(results_dir, "trust3_slopes_by_type.rds"))
saveRDS(preds,              file.path(results_dir, "trust3_predicted_margins.rds"))
saveRDS(assembly_leg_cor,   file.path(results_dir, "trust3_assembly_leg_cor.rds"))

cat("Results saved to", results_dir, "\n")
