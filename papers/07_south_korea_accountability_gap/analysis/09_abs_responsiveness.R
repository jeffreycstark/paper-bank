## ──────────────────────────────────────────────────────────────────────────────
## 09 — ABS Responsiveness (govt_responds_people) + Trust Battery Descriptives
##
## Task 2: govt_responds_people longitudinal analysis
##   - Korea wave means (W2–W6; W1 not available)
##   - Cross-national slope comparison: Korea vs. 13 ABS countries (W2)
##   - Trend figure showing Korea's responsiveness trajectory vs. ABS median
##
## Task 3: ABS institutional trust battery descriptive table
##   - trust_parliament, trust_political_parties, trust_courts,
##     trust_national_government, trust_military
##   - Wave means W1–W6 for Korea
##   - Documents W5 dip and W6 recovery pattern
##
## Output:
##   results/09_abs_responsiveness.rds
##   results/09_trust_battery_table.csv
##   figures/fig_responsiveness_*.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(marginaleffects)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/07_south_korea_accountability_gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")
fig_dir      <- file.path(analysis_dir, "figures")

# ── 1. Load ABS harmonized ────────────────────────────────────────────────────
abs_harm <- readRDS(abs_harmonized_path)

cat("ABS harmonized: n =", nrow(abs_harm),
    "| countries:", n_distinct(abs_harm$country), "\n")

# ── 2. Identify govt_responds_people availability ─────────────────────────────
cat("\ngov_responds_people availability by wave (all countries):\n")
abs_harm |>
  group_by(wave) |>
  summarise(
    n_total   = n(),
    n_nonmiss = sum(!is.na(govt_responds_people)),
    pct       = round(100 * n_nonmiss / n_total, 1),
    .groups   = "drop"
  ) |>
  print()

cat("\ngov_responds_people availability by wave (Korea only):\n")
abs_harm |>
  filter(country == 3) |>
  group_by(wave) |>
  summarise(
    n_total   = n(),
    n_nonmiss = sum(!is.na(govt_responds_people)),
    pct       = round(100 * n_nonmiss / n_total, 1),
    .groups   = "drop"
  ) |>
  print()

# ── 3. Korea responsiveness wave means ───────────────────────────────────────
kor <- abs_harm |>
  filter(country == 3) |>
  mutate(wave = as.integer(wave))

kor_resp <- kor |>
  filter(!is.na(govt_responds_people), !is.na(weight)) |>
  group_by(wave) |>
  summarise(
    mean  = weighted.mean(govt_responds_people, w = weight, na.rm = TRUE),
    se    = sd(govt_responds_people, na.rm = TRUE) / sqrt(n()),
    n     = n(),
    .groups = "drop"
  ) |>
  mutate(
    conf.low  = mean - 1.96 * se,
    conf.high = mean + 1.96 * se,
    year      = c(2001, 2003, 2008, 2015, 2019, 2022)[wave]
  )

cat("\n=== Korea: govt_responds_people by wave ===\n")
print(kor_resp)

# ── 4. Cross-national wave-level means (waves with Korea data: W2–W6) ─────────
wave_labels <- c(1, 2, 3, 4, 5, 6)
year_map    <- c(`1` = 2001, `2` = 2003, `3` = 2008,
                 `4` = 2015, `5` = 2019, `6` = 2022)

abs_resp <- abs_harm |>
  filter(!is.na(govt_responds_people)) |>
  mutate(wave = as.integer(wave)) |>
  group_by(country, wave) |>
  summarise(
    mean = weighted.mean(govt_responds_people, w = weight, na.rm = TRUE),
    n    = sum(!is.na(govt_responds_people)),
    .groups = "drop"
  ) |>
  mutate(year = year_map[as.character(wave)])

# Countries present in W2 (earliest wave with Korea data)
countries_w2 <- abs_resp |> filter(wave == 2) |> pull(country) |> unique()
cat("\nCountries in W2:", length(countries_w2), "\n")
cat("Country codes:", paste(sort(countries_w2), collapse = " "), "\n")

# ── 5. Country-level trend slopes (across all available waves) ────────────────
# OLS slope per country: govt_responds_people ~ wave_num
country_slopes <- abs_resp |>
  filter(n >= 50) |>   # at least 50 obs per wave-country cell
  group_by(country) |>
  filter(n() >= 2) |>  # at least 2 waves
  summarise(
    slope    = coef(lm(mean ~ wave))[2],
    n_waves  = n(),
    wave_min = min(wave),
    wave_max = max(wave),
    .groups  = "drop"
  ) |>
  arrange(slope) |>
  mutate(
    is_korea   = country == 3,
    country_label = case_when(
      country == 1  ~ "Japan",
      country == 2  ~ "Hong Kong",
      country == 3  ~ "Korea",
      country == 4  ~ "China",
      country == 5  ~ "Mongolia",
      country == 6  ~ "Philippines",
      country == 7  ~ "Taiwan",
      country == 8  ~ "Thailand",
      country == 9  ~ "Indonesia",
      country == 10 ~ "Singapore",
      country == 11 ~ "Vietnam",
      country == 12 ~ "Cambodia",
      country == 13 ~ "Malaysia",
      country == 14 ~ "Myanmar",
      country == 15 ~ "Australia",
      country == 18 ~ "India",
      TRUE          ~ paste0("Country ", country)
    )
  )

cat("\n=== Country-level trend slopes (govt_responds_people ~ wave) ===\n")
print(country_slopes |> select(country_label, slope, n_waves, wave_min, wave_max), n = 20)

kor_slope <- country_slopes |> filter(country == 3) |> pull(slope)
kor_rank  <- which(sort(country_slopes$slope) == kor_slope)
cat(sprintf("\nKorea slope: %.4f | rank: %d of %d countries\n",
            kor_slope, kor_rank, nrow(country_slopes)))

# ── 6. ABS median trajectory (W2–W6) ─────────────────────────────────────────
abs_median_traj <- abs_resp |>
  filter(wave >= 2) |>
  group_by(wave) |>
  summarise(
    median_mean = median(mean, na.rm = TRUE),
    n_countries = n(),
    year        = first(year_map[as.character(wave)]),
    .groups     = "drop"
  )

cat("\n=== ABS median responsiveness by wave ===\n")
print(abs_median_traj)

# ── 7. Figure: Korea responsiveness vs. ABS median ───────────────────────────
fig_resp_data <- bind_rows(
  kor_resp |>
    filter(wave >= 2) |>
    transmute(wave, year, mean, conf.low, conf.high, series = "Korea"),
  abs_median_traj |>
    transmute(wave, year,
              mean      = median_mean,
              conf.low  = NA_real_,
              conf.high = NA_real_,
              series    = "ABS median")
)

fig_resp <- fig_resp_data |>
  mutate(
    series = factor(series, levels = c("Korea", "ABS median")),
    year_f = factor(year)
  ) |>
  ggplot(aes(x = year_f, y = mean, color = series, group = series)) +
  geom_ribbon(
    data = ~ filter(.x, series == "Korea"),
    aes(ymin = conf.low, ymax = conf.high, fill = series),
    alpha = 0.15, color = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Korea" = "#D55E00", "ABS median" = "#999999")) +
  scale_fill_manual(values  = c("Korea" = "#D55E00", "ABS median" = "#999999")) +
  scale_y_continuous(limits = c(1, 4), breaks = 1:4,
                     labels = c("1\n(Never)", "2\n(Rarely)", "3\n(Sometimes)", "4\n(Often)")) +
  labs(
    x        = NULL,
    y        = "Government responds to people's demands",
    color    = NULL, fill = NULL,
    title    = "Perceived governmental responsiveness, Korea vs. ABS median (2003–2022)",
    subtitle = "ABS Q: How often does the government respond to what people demand? 1 = Never, 4 = Often.",
    caption  = "Weighted wave means. 95% CIs for Korea. ABS median across all countries with data.\nSource: Asian Barometer Survey W2–W6, Korea (country = 3)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.y     = element_text(size = 8)
  )

ggsave(file.path(fig_dir, "fig_responsiveness_trajectory.pdf"),
       fig_resp, width = 7, height = 4.5)
ggsave(file.path(fig_dir, "fig_responsiveness_trajectory.png"),
       fig_resp, width = 7, height = 4.5, dpi = 300)
cat("Responsiveness trajectory figure saved.\n")

# ── 8. Cross-national slope figure ───────────────────────────────────────────
fig_slope <- country_slopes |>
  arrange(slope) |>
  mutate(country_label = factor(country_label, levels = country_label)) |>
  ggplot(aes(x = country_label, y = slope,
             fill = ifelse(is_korea, "Korea", "Other"))) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  scale_fill_manual(values = c("Korea" = "#D55E00", "Other" = "#AAAAAA")) +
  coord_flip() +
  labs(
    x       = NULL,
    y       = "Trend slope (wave units per wave)",
    fill    = NULL,
    title   = "Trend in perceived governmental responsiveness by country",
    subtitle = "OLS slope of wave mean on wave number, across all available ABS waves.",
    caption  = "Positive = rising responsiveness. Negative = declining."
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "fig_responsiveness_slopes.pdf"),
       fig_slope, width = 6, height = 5)
ggsave(file.path(fig_dir, "fig_responsiveness_slopes.png"),
       fig_slope, width = 6, height = 5, dpi = 300)
cat("Cross-national slope figure saved.\n")

# ── 9. Task 3: ABS trust battery descriptive table (Korea W1–W6) ─────────────
trust_vars <- c(
  "trust_national_government",
  "trust_parliament",
  "trust_political_parties",
  "trust_courts",
  "trust_military"
)

# Check availability
cat("\n=== ABS trust battery availability (Korea) ===\n")
kor |>
  select(wave, all_of(trust_vars)) |>
  group_by(wave) |>
  summarise(across(all_of(trust_vars), ~ sum(!is.na(.))), .groups = "drop") |>
  print()

# Wave means
trust_table <- kor |>
  select(wave, weight, all_of(trust_vars)) |>
  pivot_longer(cols = all_of(trust_vars), names_to = "variable", values_to = "trust") |>
  filter(!is.na(trust)) |>
  group_by(variable, wave) |>
  summarise(
    mean = if (any(!is.na(weight[!is.na(trust)])))
             weighted.mean(trust, w = weight, na.rm = TRUE)
           else
             mean(trust, na.rm = TRUE),
    n    = sum(!is.na(trust)),
    .groups = "drop"
  ) |>
  mutate(
    mean  = round(mean, 3),
    year  = year_map[as.character(wave)],
    wave_label = paste0("W", wave, " (", year, ")")
  ) |>
  select(variable, wave_label, mean, n) |>
  pivot_wider(names_from = wave_label, values_from = c(mean, n),
              names_glue = "{wave_label}_{.value}") |>
  mutate(
    var_label = case_when(
      variable == "trust_national_government" ~ "National government",
      variable == "trust_parliament"          ~ "Parliament",
      variable == "trust_political_parties"   ~ "Political parties",
      variable == "trust_courts"              ~ "Courts",
      variable == "trust_military"            ~ "Military"
    ),
    domain = case_when(
      variable %in% c("trust_national_government") ~ "Executive",
      variable %in% c("trust_parliament", "trust_political_parties") ~ "Horizontal",
      variable %in% c("trust_courts", "trust_military") ~ "Non-political"
    )
  ) |>
  arrange(domain, variable) |>
  select(domain, var_label, everything(), -variable)

cat("\n=== ABS institutional trust battery: Korea W1–W6 ===\n")
print(trust_table, width = 120)

write.csv(trust_table,
          file.path(results_dir, "09_trust_battery_table.csv"),
          row.names = FALSE)
cat("Trust battery table saved.\n")

# Also: Wide format just showing means for quick scanning
cat("\n=== ABS trust battery means (wide) ===\n")
kor |>
  select(wave, weight, all_of(trust_vars)) |>
  pivot_longer(cols = all_of(trust_vars), names_to = "variable", values_to = "trust") |>
  filter(!is.na(trust)) |>
  group_by(variable, wave) |>
  summarise(
    mean = round(mean(trust, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = wave, values_from = mean, names_prefix = "W") |>
  mutate(
    W5_to_W6_change = round(W6 - W5, 3),
    W1_to_W6_change = round(W6 - W1, 3)
  ) |>
  print()

# ── 10. Figure: ABS trust battery trajectories ───────────────────────────────
trust_traj <- kor |>
  select(wave, weight, all_of(trust_vars)) |>
  pivot_longer(cols = all_of(trust_vars), names_to = "variable", values_to = "trust") |>
  filter(!is.na(trust)) |>
  group_by(variable, wave) |>
  summarise(
    mean    = mean(trust, na.rm = TRUE),
    se      = sd(trust, na.rm = TRUE) / sqrt(sum(!is.na(trust))),
    .groups = "drop"
  ) |>
  mutate(
    conf.low  = mean - 1.96 * se,
    conf.high = mean + 1.96 * se,
    year      = year_map[as.character(wave)],
    var_label = case_when(
      variable == "trust_national_government" ~ "National government",
      variable == "trust_parliament"          ~ "Parliament",
      variable == "trust_political_parties"   ~ "Political parties",
      variable == "trust_courts"              ~ "Courts",
      variable == "trust_military"            ~ "Military"
    ),
    var_label = factor(var_label, levels = c(
      "National government", "Parliament", "Political parties",
      "Courts", "Military"
    ))
  )

fig_trust_abs <- trust_traj |>
  ggplot(aes(x = year, y = mean, color = var_label, group = var_label,
             ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = var_label), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(2001, 2003, 2008, 2015, 2019, 2022)) +
  scale_y_continuous(limits = c(1, 4), breaks = 1:4,
                     labels = c("1\nNot at all", "2", "3", "4\nA great deal")) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette  = "Dark2") +
  labs(
    x        = NULL,
    y        = "Trust (1–4 scale)",
    color    = NULL, fill = NULL,
    title    = "ABS institutional trust battery, Korea, 2001–2022",
    subtitle = "Weighted wave means with 95% CIs. ABS scale: 1 = Not at all, 4 = A great deal.",
    caption  = "Source: Asian Barometer Survey W1–W6, Korea (country = 3)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.y     = element_text(size = 8)
  )

ggsave(file.path(fig_dir, "fig_abs_trust_battery.pdf"),
       fig_trust_abs, width = 7.5, height = 5)
ggsave(file.path(fig_dir, "fig_abs_trust_battery.png"),
       fig_trust_abs, width = 7.5, height = 5, dpi = 300)
cat("ABS trust battery figure saved.\n")

# ── 11. Save all results ──────────────────────────────────────────────────────
resp_results <- list(
  kor_responsiveness   = kor_resp,
  abs_resp_by_country  = abs_resp,
  country_slopes       = country_slopes,
  abs_median_traj      = abs_median_traj,
  trust_battery_wide   = trust_table,
  trust_battery_traj   = trust_traj,
  kor_slope            = kor_slope,
  kor_slope_rank       = kor_rank,
  n_countries          = nrow(country_slopes)
)

saveRDS(resp_results, file.path(results_dir, "09_abs_responsiveness.rds"))
cat("\nAll results saved to", results_dir, "\n")
