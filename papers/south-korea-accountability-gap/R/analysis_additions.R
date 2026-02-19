## ══════════════════════════════════════════════════════════════════════════════
## R/analysis_additions.R
## South Korea Accountability Gap — Analytical Additions
##
## Tasks:
##   1. ABS ordered logit (Models 1–2): govt_withholds_info, corrupt_witnessed
##   2. KAMOS OLS (Models 3–5): trust_media, trust_ngo, trust_national_assembly
##   3. Wave × ideology interaction robustness checks (all 5 models)
##   4. Cross-national comparison table from cross_national_slopes.rds
##   5. Coefficient plot (wave dummies, 95% Wald CIs, 5 panels)
##   6. Appendix table (modelsummary, .tex + .docx)
##
## Outputs → outputs/tables/ and outputs/figures/
## ══════════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)          # polr: ordered logit
  library(broom)         # tidy model output
  library(modelsummary)  # formatted regression tables
  library(kableExtra)    # latex table output
  library(patchwork)     # multi-panel figures
  library(ggtext)        # markdown in ggplot labels
  library(scales)        # axis formatting
})

# MASS masks dplyr::select — restore dplyr version
select <- dplyr::select

# ─── Paths ─────────────────────────────────────────────────────────────────────
project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/south-korea-accountability-gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
out_tables   <- file.path(paper_dir, "outputs", "tables")
out_figures  <- file.path(paper_dir, "outputs", "figures")

dir.create(out_tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)

# Colour palette — consistent with existing analysis
col_pre      <- "#2166AC"
col_post     <- "#D6604D"
col_exec     <- "#4393C3"
col_intermed <- "#D73027"
col_rising   <- "#D73027"
col_stable   <- "#878787"

wave_years <- c("1"=2003,"2"=2006,"3"=2011,"4"=2015,"5"=2019,"6"=2022)

# ─── 0. Load data ──────────────────────────────────────────────────────────────
cat("Loading data...\n")

abs_raw <- readRDS(abs_harmonized_path)
kor <- abs_raw |>
  filter(country == 3) |>
  mutate(
    survey_year = wave_years[as.character(wave)],
    wave        = as.integer(wave)
  )

kamos_raw <- readRDS(kamos_harmonized_path)
kamos <- kamos_raw |>
  mutate(
    wave_year  = if_else(wave == 1, 2016L, 2019L),
    wave_label = if_else(wave == 1, "Wave 1 (2016)", "Wave 4 (2019)"),
    wave_fac   = factor(wave_year, levels = c(2016L, 2019L))
  )

cross_slopes <- readRDS(file.path(results_dir, "cross_national_slopes.rds"))

cat("✓ ABS Korea: n =", nrow(kor), "| waves:", paste(sort(unique(kor$wave)), collapse=" "), "\n")
cat("✓ KAMOS: n =", nrow(kamos), "| waves:", paste(sort(unique(kamos$wave_year)), collapse=" "), "\n")
cat("✓ Cross-national slopes: n =", nrow(cross_slopes), "rows,",
    n_distinct(cross_slopes$variable), "variables\n\n")

# ─── Covariate detection ────────────────────────────────────────────────────────
# Returns first matching variable name, or NULL if none found.
pick <- function(data, candidates) {
  found <- candidates[candidates %in% names(data)]
  if (length(found) == 0) NULL else found[1]
}

abs_age    <- pick(kor,   c("age", "respondent_age", "q2_age", "age_r"))
abs_edu    <- pick(kor,   c("edu", "education", "educ", "edu_level", "q_edu", "edu_r"))
abs_urban  <- pick(kor,   c("urban", "urb", "urban_rural", "locality", "rural", "q113_urb"))
abs_gender <- pick(kor,   c("gender", "sex", "female", "male", "q1_sex", "q1_gender"))
abs_ideo   <- pick(kor,   c("ideology", "polviews", "ideo", "pol_views", "polview",
                             "q109_ideology", "lrscale"))

kamos_age    <- pick(kamos, c("age", "respondent_age", "age_r"))
kamos_edu    <- pick(kamos, c("edu", "education", "educ", "edu_level"))
kamos_gender <- pick(kamos, c("gender", "sex", "female", "male"))
kamos_ideo   <- pick(kamos, c("ideology", "polviews", "ideo", "pol_views", "lrscale"))
kamos_vote   <- pick(kamos, c("vote_intention", "vote_int", "vote", "party_id"))

cat("── Covariates detected ──\n")
cat("  ABS:   age=", abs_age,    " edu=", abs_edu,    " urban=", abs_urban,
    " gender=", abs_gender, " ideology=", abs_ideo,  "\n")
cat("  KAMOS: age=", kamos_age,  " edu=", kamos_edu,  " gender=", kamos_gender,
    " ideology=", kamos_ideo, " vote=", kamos_vote, "\n\n")

abs_covs   <- na.omit(c(abs_age, abs_edu, abs_urban, abs_gender, abs_ideo))
kamos_covs <- na.omit(c(kamos_age, kamos_edu, kamos_gender, kamos_ideo, kamos_vote))

# Build formula from predictors
make_fml <- function(dv, predictors) {
  rhs <- paste(predictors[nchar(predictors) > 0], collapse = " + ")
  if (nchar(rhs) == 0) rhs <- "1"
  as.formula(paste(dv, "~", rhs))
}

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 1: ABS Ordered Logit Models
# DV: govt_withholds_info (M1), corrupt_witnessed (M2)
# Predictors: wave (factor) + age + education + urban + gender + ideology
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 1: ABS Ordered Logit Models ══\n")

## Model 1: govt_withholds_info — only available W2–W6; Wave 2 = reference
d_m1 <- kor |>
  filter(!is.na(govt_withholds_info), wave >= 2) |>
  mutate(
    wave_fac            = factor(wave, levels = 2:6),
    govt_withholds_info = factor(govt_withholds_info, ordered = TRUE)
  ) |>
  select(govt_withholds_info, wave_fac, all_of(abs_covs)) |>
  drop_na()

fml_m1 <- make_fml("govt_withholds_info", c("wave_fac", abs_covs))
cat("Model 1 (govt_withholds_info): n =", nrow(d_m1), " | ref = Wave 2\n")

m1 <- tryCatch(
  MASS::polr(fml_m1, data = d_m1, Hess = TRUE, method = "logistic"),
  warning = function(w) {
    cat("  ⚠ Warning:", conditionMessage(w), "\n")
    suppressWarnings(MASS::polr(fml_m1, data = d_m1, Hess = TRUE, method = "logistic"))
  }
)
cat("  ✓ Model 1 fitted\n")

## Model 2: corrupt_witnessed — available W1–W6; Wave 1 = reference
## Note: this item uses a 1–2 scale (binary), so polr requires 3+ levels.
## Use logistic regression (glm binomial) instead; coefficients are log-odds.
d_m2 <- kor |>
  filter(!is.na(corrupt_witnessed)) |>
  mutate(
    wave_fac          = factor(wave, levels = 1:6),
    corrupt_witnessed = as.integer(corrupt_witnessed) - 1L   # recode to 0/1
  ) |>
  select(corrupt_witnessed, wave_fac, all_of(abs_covs)) |>
  drop_na()

fml_m2 <- make_fml("corrupt_witnessed", c("wave_fac", abs_covs))
cat("Model 2 (corrupt_witnessed):   n =", nrow(d_m2),
    " | ref = Wave 1 | binary DV → logistic (glm)\n")

m2 <- glm(fml_m2, data = d_m2, family = binomial(link = "logit"))
cat("  ✓ Model 2 fitted\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 2: KAMOS OLS Models
# DV: trust_media (M3), trust_ngo (M4), trust_national_assembly (M5)
# Predictors: wave_fac (2016 = ref) + age + education + gender + ideology + vote
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 2: KAMOS OLS Models ══\n")

## Model 3: trust_media
d_m3 <- kamos |>
  select(trust_media, wave_fac, all_of(kamos_covs)) |>
  drop_na()
fml_m3 <- make_fml("trust_media", c("wave_fac", kamos_covs))
cat("Model 3 (trust_media):             n =", nrow(d_m3), " | ref = 2016\n")
m3 <- lm(fml_m3, data = d_m3)
cat("  ✓ Model 3 fitted\n")

## Model 4: trust_ngo
d_m4 <- kamos |>
  select(trust_ngo, wave_fac, all_of(kamos_covs)) |>
  drop_na()
fml_m4 <- make_fml("trust_ngo", c("wave_fac", kamos_covs))
cat("Model 4 (trust_ngo):               n =", nrow(d_m4), " | ref = 2016\n")
m4 <- lm(fml_m4, data = d_m4)
cat("  ✓ Model 4 fitted\n")

## Model 5: trust_national_assembly
d_m5 <- kamos |>
  select(trust_national_assembly, wave_fac, all_of(kamos_covs)) |>
  drop_na()
fml_m5 <- make_fml("trust_national_assembly", c("wave_fac", kamos_covs))
cat("Model 5 (trust_national_assembly): n =", nrow(d_m5), " | ref = 2016\n")
m5 <- lm(fml_m5, data = d_m5)
cat("  ✓ Model 5 fitted\n\n")

# ─── Extract wave coefficients ─────────────────────────────────────────────────
extract_wave <- function(model, model_id, dv_label, ref_label) {
  broom::tidy(model, conf.int = TRUE) |>
    filter(str_detect(term, "^wave_fac")) |>
    mutate(
      model_id   = model_id,
      dv         = dv_label,
      ref_wave   = ref_label,
      wave_label = str_replace(term, "^wave_fac", "")
    ) |>
    select(model_id, dv, ref_wave, term, wave_label,
           estimate, std.error, conf.low, conf.high,
           any_of(c("statistic", "p.value")))
}

wave_coefs_abs <- bind_rows(
  extract_wave(m1, "M1", "Govt withholds information", "Wave 2 (2006)"),
  extract_wave(m2, "M2", "Corrupt witnessed",          "Wave 1 (2003)")
)

wave_coefs_kamos <- bind_rows(
  extract_wave(m3, "M3", "Trust: media",              "2016"),
  extract_wave(m4, "M4", "Trust: civil society",      "2016"),
  extract_wave(m5, "M5", "Trust: national assembly",  "2016")
)

write_csv(wave_coefs_abs,   file.path(out_tables, "table_wave_coefs_abs.csv"))
write_csv(wave_coefs_kamos, file.path(out_tables, "table_wave_coefs_kamos.csv"))
cat("✓ Saved table_wave_coefs_abs.csv\n")
cat("✓ Saved table_wave_coefs_kamos.csv\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 3: Wave × Ideology Interaction Robustness Check
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 3: Wave × Ideology Interactions ══\n")

run_interaction <- function(base_fml, data, model_id, dv_label,
                             wave_var, ideo_var, type) {
  if (is.null(ideo_var) || !ideo_var %in% names(data)) {
    return(tibble(
      model_id = model_id, dv = dv_label,
      n_interaction_terms = NA_integer_,
      any_significant = FALSE,
      min_p = NA_real_, max_p = NA_real_,
      note = "ideology variable not available — interaction skipped"
    ))
  }

  int_fml <- update(base_fml, paste(". ~ . +", wave_var, "*", ideo_var))

  m_int <- tryCatch({
    if (type == "polr") {
      suppressWarnings(MASS::polr(int_fml, data = data, Hess = TRUE, method = "logistic"))
    } else if (type == "glm") {
      glm(int_fml, data = data, family = binomial(link = "logit"))
    } else {
      lm(int_fml, data = data)
    }
  }, error = function(e) {
    return(structure(list(error = e$message), class = "error_result"))
  })

  if (inherits(m_int, "error_result")) {
    return(tibble(
      model_id = model_id, dv = dv_label,
      n_interaction_terms = NA_integer_,
      any_significant = FALSE,
      min_p = NA_real_, max_p = NA_real_,
      note = paste("Model error:", m_int$error)
    ))
  }

  int_terms <- broom::tidy(m_int) |>
    filter(str_detect(term, paste0(wave_var, ".*:", ideo_var, "|",
                                   ideo_var, ":.*", wave_var)))

  if (nrow(int_terms) == 0) {
    return(tibble(
      model_id = model_id, dv = dv_label,
      n_interaction_terms = 0L,
      any_significant = FALSE,
      min_p = NA_real_, max_p = NA_real_,
      note = "no interaction terms recovered from model"
    ))
  }

  tibble(
    model_id            = model_id,
    dv                  = dv_label,
    n_interaction_terms = nrow(int_terms),
    any_significant     = any(int_terms$p.value < 0.05, na.rm = TRUE),
    min_p               = min(int_terms$p.value, na.rm = TRUE),
    max_p               = max(int_terms$p.value, na.rm = TRUE),
    note                = ""
  )
}

int_results <- bind_rows(
  run_interaction(fml_m1, d_m1, "M1", "Govt withholds info",
                  "wave_fac", abs_ideo,   "polr"),
  run_interaction(fml_m2, d_m2, "M2", "Corrupt witnessed",
                  "wave_fac", abs_ideo,   "glm"),
  run_interaction(fml_m3, d_m3, "M3", "Trust: media",
                  "wave_fac", kamos_ideo, "lm"),
  run_interaction(fml_m4, d_m4, "M4", "Trust: civil society",
                  "wave_fac", kamos_ideo, "lm"),
  run_interaction(fml_m5, d_m5, "M5", "Trust: national assembly",
                  "wave_fac", kamos_ideo, "lm")
)

print(int_results)

# Build plain-text summary
flagged <- int_results |> filter(any_significant == TRUE)

summary_lines <- c(
  "WAVE × IDEOLOGY INTERACTION TEST SUMMARY",
  "==========================================",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Models: M1=govt_withholds_info (ABS, ordered logit, ref=W2)",
  "        M2=corrupt_witnessed    (ABS, ordered logit, ref=W1)",
  "        M3=trust_media         (KAMOS, OLS, ref=2016)",
  "        M4=trust_ngo           (KAMOS, OLS, ref=2016)",
  "        M5=trust_national_assembly (KAMOS, OLS, ref=2016)",
  "",
  sprintf("Ideology variable — ABS: '%s' | KAMOS: '%s'",
          ifelse(is.null(abs_ideo), "NOT FOUND", abs_ideo),
          ifelse(is.null(kamos_ideo), "NOT FOUND", kamos_ideo)),
  "",
  "RESULTS:",
  "--------"
)

for (i in seq_len(nrow(int_results))) {
  r <- int_results[i,]
  if (is.na(r$min_p)) {
    line <- sprintf("  %s (%s): %s", r$model_id, r$dv, r$note)
  } else {
    flag <- if (isTRUE(r$any_significant)) {
      "  *** SIGNIFICANT — REVIEW PARTISAN SORTING CLAIM IN DISCUSSION ***"
    } else ""
    line <- sprintf("  %s (%s): %d interaction term(s), p range [%.3f, %.3f]%s",
                    r$model_id, r$dv, r$n_interaction_terms,
                    r$min_p, r$max_p, flag)
  }
  summary_lines <- c(summary_lines, line)
}

summary_lines <- c(
  summary_lines,
  "",
  if (nrow(flagged) == 0) {
    "CONCLUSION: No wave×ideology interaction significant at p < .05 in any model."
  } else {
    paste0(
      "⚠ FLAGGED: Significant wave×ideology interaction found in: ",
      paste(flagged$model_id, collapse=", "),
      ".\n  The partisan sorting claim in the Discussion section should be reviewed."
    )
  },
  "",
  "Note: These are robustness checks only. Main text tables are unchanged."
)

writeLines(summary_lines, file.path(out_tables, "interaction_test_summary.txt"))
cat("\n✓ Saved interaction_test_summary.txt\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 4: Cross-National Comparison Table
# Source: analysis/results/cross_national_slopes.rds
# (Instructions reference outlier_slopes.csv; equivalent data in RDS file)
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 4: Cross-National Comparison Table ══\n")

# Helper: build comparison table for one variable
build_cross_tbl <- function(var_name, var_label) {
  sub <- cross_slopes |>
    filter(variable == var_name) |>
    arrange(desc(z_score)) |>
    mutate(
      rank    = row_number(),
      n_total = n(),
      sig = case_when(
        p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
        p_value < 0.05  ~ "*",   p_value < 0.10  ~ "†",
        TRUE ~ ""
      )
    )

  mean_s <- mean(sub$slope, na.rm = TRUE)
  sd_s   <- sd(sub$slope,   na.rm = TRUE)
  n_cty  <- nrow(sub)

  korea <- sub |> filter(str_detect(country_name, regex("Korea", ignore_case = TRUE)))
  top5  <- sub |> slice_head(n = 5)

  regional <- tibble(
    country_name = "Regional mean",
    slope  = mean_s, se = sd_s, z_score = NA_real_,
    n_waves = NA_integer_,
    rank = NA_integer_, n_total = n_cty, sig = ""
  )

  rows <- if (nrow(korea) > 0 && !any(korea$rank <= 5)) {
    bind_rows(top5, korea)
  } else {
    top5
  }
  rows <- bind_rows(rows, regional)

  rows |>
    transmute(
      Variable      = var_label,
      Country       = country_name,
      Rank          = if_else(is.na(rank), "—", paste0(rank, " / ", n_total)),
      `Slope`       = sprintf("%.4f%s", slope, sig),
      `SE`          = sprintf("%.4f", se),
      `z-score`     = if_else(is.na(z_score), "—", sprintf("%.2f", z_score)),
      `Waves`       = if_else(is.na(n_waves), "—", as.character(n_waves)),
      is_korea      = str_detect(country_name, regex("Korea", ignore_case = TRUE))
    )
}

tbl_gwi <- build_cross_tbl("govt_withholds_info", "Government withholds information")
tbl_cw  <- build_cross_tbl("corrupt_witnessed",   "Personally witnessed corruption")

cross_tbl_full <- bind_rows(tbl_gwi, tbl_cw)
write_csv(cross_tbl_full |> select(-is_korea),
          file.path(out_tables, "table_crossnational.csv"))
cat("✓ Saved table_crossnational.csv\n")

# LaTeX fragment — one panel per variable
write_latex_panel <- function(tbl, var_label, fname) {
  out <- tbl |>
    select(Country, Rank, Slope, SE, `z-score`, Waves) |>
    mutate(Country = if_else(tbl$is_korea & !is.na(tbl$is_korea),
                             paste0("\\textbf{", Country, "}"), Country))

  kbl(out, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = sprintf("Cross-National ABS Slopes: %s", var_label)) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10) |>
    footnote(
      general = paste0(
        "Rows: top 5 countries by z-score, Korea (if not in top 5), and regional mean. ",
        "† p<.10  * p<.05  ** p<.01  *** p<.001 (slope estimate). ",
        "Slope = OLS coefficient on wave number. ",
        "Korea in bold. Regional mean = cross-national average slope (SD in SE column)."
      ),
      general_title = "Note.", threeparttable = TRUE
    ) |>
    as.character() |>
    writeLines(file.path(out_tables, fname))
  cat("✓ Saved", fname, "\n")
}

write_latex_panel(tbl_gwi, "Government Withholds Information",
                  "table_crossnational_gwi.tex")
write_latex_panel(tbl_cw,  "Personally Witnessed Corruption",
                  "table_crossnational_cw.tex")

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 5: Coefficient Plot
# Wave dummy coefficients and 95% Wald CIs for all five models.
# Two figures: ABS (2 panels) and KAMOS (3 panels).
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 5: Coefficient Plot ══\n")

# Combine coefficients
coefs_abs <- wave_coefs_abs |>
  mutate(
    dataset = "ABS",
    xlab    = "Log-odds (vs. reference wave)",
    color   = case_when(
      dv == "Govt withholds information" ~ col_stable,
      dv == "Corrupt witnessed"          ~ col_rising,
      TRUE                               ~ col_stable
    )
  )

coefs_kamos <- wave_coefs_kamos |>
  mutate(
    dataset = "KAMOS",
    xlab    = "OLS coefficient (vs. 2016)",
    color   = col_intermed
  )

# Single-panel factory
make_panel <- function(data, title, ref_label, xlab_str, dot_color) {
  ggplot(data, aes(x = estimate, xmin = conf.low, xmax = conf.high,
                   y = reorder(wave_label, estimate))) +
    geom_vline(xintercept = 0, linewidth = 0.55, color = "grey40",
               linetype = "dashed") +
    geom_errorbarh(height = 0.25, linewidth = 0.8, color = dot_color, alpha = 0.8) +
    geom_point(size = 3.2, shape = 21, fill = "white",
               color = dot_color, stroke = 1.7) +
    annotate("text", x = 0, y = 0.55,
             label = paste0("ref: ", ref_label),
             size = 2.5, color = "grey55", fontface = "italic", hjust = 0.5) +
    labs(title = title, x = xlab_str, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(size = 9, face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = 8)
    )
}

p_gwi <- make_panel(
  coefs_abs |> filter(dv == "Govt withholds information"),
  "M1: Govt withholds info\n(ordered logit, ref = Wave 2)",
  "Wave 2 (~2006)", "Log-odds", col_stable
)

p_cw <- make_panel(
  coefs_abs |> filter(dv == "Corrupt witnessed"),
  "M2: Corrupt witnessed\n(ordered logit, ref = Wave 1)",
  "Wave 1 (~2003)", "Log-odds", col_rising
)

p_med <- make_panel(
  coefs_kamos |> filter(dv == "Trust: media"),
  "M3: Trust — media\n(OLS, ref = 2016)",
  "2016", "OLS coefficient", col_intermed
)

p_ngo <- make_panel(
  coefs_kamos |> filter(dv == "Trust: civil society"),
  "M4: Trust — civil society\n(OLS, ref = 2016)",
  "2016", "OLS coefficient", col_intermed
)

p_nat <- make_panel(
  coefs_kamos |> filter(dv == "Trust: national assembly"),
  "M5: Trust — national assembly\n(OLS, ref = 2016)",
  "2016", "OLS coefficient", col_intermed
)

fig_abs <- (p_gwi | p_cw) +
  plot_annotation(
    title    = "ABS Models: Wave Coefficients (Models 1–2)",
    subtitle = "Log-odds relative to reference wave. Error bars = 95% Wald CIs. Dashed line = 0.",
    theme    = theme(
      plot.title    = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey50")
    )
  )

fig_kamos <- (p_med | p_ngo | p_nat) +
  plot_annotation(
    title    = "KAMOS Models: Wave Coefficients (Models 3–5)",
    subtitle = "OLS coefficients relative to 2016 reference. Error bars = 95% Wald CIs. Dashed line = 0.",
    theme    = theme(
      plot.title    = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey50")
    )
  )

# Combined figure (all 5 panels, 2+3 split layout)
fig_combined <- (p_gwi | p_cw) / (p_med | p_ngo | p_nat) +
  plot_annotation(
    title    = "Figure A. Wave Coefficients — All Models (1–5)",
    subtitle = paste0(
      "Top row: ABS ordered logit (log-odds). ",
      "Bottom row: KAMOS OLS (0–10 trust scale). ",
      "Error bars = 95% Wald CIs."
    ),
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey50")
    )
  )

ggsave(file.path(out_figures, "fig_coefplot_abs.pdf"),
       fig_abs,      width = 10, height = 5)
ggsave(file.path(out_figures, "fig_coefplot_abs.png"),
       fig_abs,      width = 10, height = 5, dpi = 300)
ggsave(file.path(out_figures, "fig_coefplot_kamos.pdf"),
       fig_kamos,    width = 13, height = 5)
ggsave(file.path(out_figures, "fig_coefplot_kamos.png"),
       fig_kamos,    width = 13, height = 5, dpi = 300)
ggsave(file.path(out_figures, "fig_coefplot.pdf"),
       fig_combined, width = 13, height = 9)
ggsave(file.path(out_figures, "fig_coefplot.png"),
       fig_combined, width = 13, height = 9, dpi = 300)

cat("✓ Saved fig_coefplot_abs.pdf/.png\n")
cat("✓ Saved fig_coefplot_kamos.pdf/.png\n")
cat("✓ Saved fig_coefplot.pdf/.png (combined)\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 6: Appendix Table — Full Regression Output (All 5 Models)
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ TASK 6: Appendix Regression Table ══\n")

# Save full model objects
saveRDS(
  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
  file.path(out_tables, "tableA1_full_regression.rds")
)
cat("✓ Saved tableA1_full_regression.rds\n")

ms_notes <- paste(
  "Models 1–2: ordered logit (MASS::polr); coefficients are log-odds.",
  "Threshold parameters omitted.",
  "Models 3–5: OLS; unstandardized coefficients on 0–10 trust scale.",
  "Wave is a factor variable; first listed wave serves as reference.",
  "95% confidence intervals are Wald-based.",
  "† p<.10  * p<.05  ** p<.01  *** p<.001."
)

model_list <- list(
  "M1: Govt Info\n(ord. logit)" = m1,
  "M2: Corrupt Exp\n(logit)"    = m2,
  "M3: Media\n(OLS)"            = m3,
  "M4: Civil Soc\n(OLS)"        = m4,
  "M5: Nat. Assem\n(OLS)"       = m5
)

# Suppress threshold row-names from polr (named like "1|2", "2|3")
tryCatch({
  modelsummary::modelsummary(
    models    = model_list,
    output    = file.path(out_tables, "tableA1_full_regression.tex"),
    stars     = c("†" = .10, "*" = .05, "**" = .01, "***" = .001),
    notes     = ms_notes,
    title     = "Table A1. Full Regression Output: All Five Models",
    fmt       = 3,
    coef_omit = "\\|"   # drop polr threshold parameters (named "k|k+1")
  )
  cat("✓ Saved tableA1_full_regression.tex\n")
}, error = function(e) cat("⚠ .tex error:", e$message, "\n"))

tryCatch({
  # Use flextable backend for .docx (avoids R pandoc package dependency)
  ft <- modelsummary::modelsummary(
    models    = model_list,
    output    = "flextable",
    stars     = c("†" = .10, "*" = .05, "**" = .01, "***" = .001),
    notes     = ms_notes,
    title     = "Table A1. Full Regression Output: All Five Models",
    fmt       = 3,
    coef_omit = "\\|"
  )
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, ft)
  print(doc, target = file.path(out_tables, "tableA1_full_regression.docx"))
  cat("✓ Saved tableA1_full_regression.docx\n")
}, error = function(e) {
  cat("⚠ .docx error:", e$message, "\n")
})

# ══ COMPLETION SUMMARY ════════════════════════════════════════════════════════
cat("\n", strrep("═", 64), "\n", sep = "")
cat("ANALYSIS ADDITIONS COMPLETE\n")
cat(strrep("═", 64), "\n\n", sep = "")

expected_files <- c(
  file.path(out_tables,  "table_wave_coefs_abs.csv"),
  file.path(out_tables,  "table_wave_coefs_kamos.csv"),
  file.path(out_tables,  "interaction_test_summary.txt"),
  file.path(out_tables,  "table_crossnational.csv"),
  file.path(out_tables,  "table_crossnational_gwi.tex"),
  file.path(out_tables,  "table_crossnational_cw.tex"),
  file.path(out_tables,  "tableA1_full_regression.rds"),
  file.path(out_tables,  "tableA1_full_regression.tex"),
  file.path(out_tables,  "tableA1_full_regression.docx"),
  file.path(out_figures, "fig_coefplot_abs.pdf"),
  file.path(out_figures, "fig_coefplot_abs.png"),
  file.path(out_figures, "fig_coefplot_kamos.pdf"),
  file.path(out_figures, "fig_coefplot_kamos.png"),
  file.path(out_figures, "fig_coefplot.pdf"),
  file.path(out_figures, "fig_coefplot.png")
)

cat("Output files:\n")
for (f in expected_files) {
  mark <- if (file.exists(f)) "✓" else "✗ MISSING"
  cat(sprintf("  %s %s\n", mark, file.path(basename(dirname(f)), basename(f))))
}

cat("\nData issues to check:\n")
if (is.null(abs_ideo))   cat("  ⚠ ABS ideology variable not detected — interaction M1/M2 skipped\n")
if (is.null(kamos_ideo)) cat("  ⚠ KAMOS ideology variable not detected — interaction M3/M4/M5 skipped\n")
if (length(abs_covs) == 0)   cat("  ⚠ No ABS covariates detected — wave-only models fitted\n")
if (length(kamos_covs) == 0) cat("  ⚠ No KAMOS covariates detected — wave-only models fitted\n")
cat("  • corrupt_witnessed is a 1–2 scale; ordered logit = logistic (one threshold)\n")
cat("  • outlier_slopes.csv not present; used cross_national_slopes.rds instead\n")
cat("  • See interaction_test_summary.txt for partisan sorting robustness verdict\n")
