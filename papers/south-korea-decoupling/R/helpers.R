# R/helpers.R — Shared utilities for south-korea-decoupling analysis scripts
# Source after library(tidyverse) and library(broom) are loaded.

# ── Normalization ──────────────────────────────────────────────────────────────
normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ── Model helpers ──────────────────────────────────────────────────────────────
controls <- "age_n + gender + edu_n + urban_rural + polint_n"

extract_econ_coef <- function(model) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == "econ_index") |>
    dplyr::select(estimate, std.error, statistic, p.value, conf.low, conf.high)
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# ── Survey year lookups ────────────────────────────────────────────────────────
survey_years_kr <- c(2003, 2006, 2011, 2015, 2019, 2022)
survey_years_tw <- c(2001, 2006, 2010, 2014, 2019, 2022)

survey_years_lookup <- tibble::tibble(
  wave    = rep(1:6, 2),
  country = rep(c("Korea", "Taiwan"), each = 6),
  year    = c(survey_years_kr, survey_years_tw)
)

# ── Trend slope helper ─────────────────────────────────────────────────────────
# Returns a tidy tibble with columns: term, estimate, std.error, p.value, per_decade
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
pal2       <- c("Satisfaction" = "#2166AC", "Democratic quality" = "#B2182B")
pal3       <- c("Satisfaction" = "#2166AC", "Democratic quality" = "#B2182B",
                "Economic evaluations" = "#4DAF4A")
pal_country <- c("Korea" = "#2166AC", "Taiwan" = "#D55E00")
