# R/helpers.R — Shared utilities for cambodia-fairy-tale analysis scripts
# Source after library(tidyverse) and library(broom) are loaded.

# ── Normalization ──────────────────────────────────────────────────────────────
normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ── Model helpers ──────────────────────────────────────────────────────────────
controls <- "age_n + gender + edu_n + urban_rural + polint_n"

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# ── Survey year lookups ────────────────────────────────────────────────────────
# ABS waves available for Cambodia: W2, W3, W4, W6
survey_years_khm <- c(2008, 2012, 2015, 2022)

survey_years_lookup <- tibble::tibble(
  wave = c(2, 3, 4, 6),
  year = survey_years_khm
)

# ── Trend slope helper ─────────────────────────────────────────────────────────
calc_slope <- function(data, yvar, year_var = "year") {
  d <- data[!is.na(data[[yvar]]) & !is.na(data[[year_var]]), ]
  if (nrow(d) < 3) {
    return(tibble::tibble(term = year_var, estimate = NA_real_,
                          std.error = NA_real_, p.value = NA_real_,
                          per_decade = NA_real_))
  }
  m <- lm(as.formula(paste(yvar, "~", year_var)), data = d)
  broom::tidy(m) |>
    dplyr::filter(term == year_var) |>
    dplyr::mutate(per_decade = estimate * 10)
}

# ── Publication theme ──────────────────────────────────────────────────────────
theme_pub <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(size = 11, face = "bold",
                                               margin = ggplot2::margin(b = 6)),
    plot.subtitle      = ggplot2::element_text(size = 9, color = "grey30",
                                               margin = ggplot2::margin(b = 10)),
    plot.caption       = ggplot2::element_text(size = 7.5, color = "grey50",
                                               hjust = 0,
                                               margin = ggplot2::margin(t = 8)),
    legend.position    = "bottom",
    legend.text        = ggplot2::element_text(size = 9),
    legend.title       = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text         = ggplot2::element_text(face = "bold", size = 10),
    axis.title         = ggplot2::element_text(size = 9),
    axis.text          = ggplot2::element_text(size = 8.5)
  )

# ── Colour palettes ────────────────────────────────────────────────────────────
pal_main <- c("#2166AC", "#B2182B", "#4DAF4A")
