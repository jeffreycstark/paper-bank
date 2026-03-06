# R/helpers.R — Shared utilities for 00-model analysis scripts
# Source after library(tidyverse) is loaded.

# ── Normalization ──────────────────────────────────────────────────────────────
normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ── Country filter helper ──────────────────────────────────────────────────────
# Usage: filter_countries(dat, c(3, 7), c("Korea", "Taiwan"))
filter_countries <- function(data, codes, labels) {
  stopifnot(length(codes) == length(labels))
  lkp <- setNames(labels, as.character(codes))
  data |>
    dplyr::filter(country %in% codes) |>
    dplyr::mutate(
      country_label = factor(lkp[as.character(country)], levels = labels)
    )
}

# ── Model helpers ──────────────────────────────────────────────────────────────
sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# Extract a named coefficient row from a tidy model
extract_coef <- function(model, term_name) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == term_name) |>
    dplyr::select(estimate, std.error, statistic, p.value, conf.low, conf.high)
}

# ── Wave-year labeling ─────────────────────────────────────────────────────────
# TODO: update with the correct survey years for this paper's countries
# survey_years_lookup <- tibble::tibble(
#   wave    = rep(1:6, 2),
#   country = rep(c("Country A", "Country B"), each = 6),
#   year    = c(YEAR_A1, YEAR_A2, ..., YEAR_B1, YEAR_B2, ...)
# )

# ── Trend slope helper ─────────────────────────────────────────────────────────
# Returns tidy tibble: term, estimate, std.error, p.value, per_decade
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

# ── Publication ggplot theme ───────────────────────────────────────────────────
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
# TODO: update with country/variable names for this paper
# pal_country <- c("Country A" = "#2166AC", "Country B" = "#D55E00")
# pal_dv      <- c("DV1" = "#2166AC", "DV2" = "#B2182B")
