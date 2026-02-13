# NEW RESULTS + ROBUSTNESS SECTIONS — Draft for revised manuscript
# This replaces everything from "# Results" through "## Robustness" in manuscript.qmd.
#
# Key structural changes from v1:
# 1. Results section now has three subsections: Baseline (H1), Trajectories (H2/H3), Mechanism Test
# 2. Fairness interaction promoted from Appendix I to Results §4c
# 3. Free Elections anomaly addressed directly in §4a
# 4. Wave 2 compressed and folded into robustness
# 5. Robustness section compressed to ~600 words
#
# All inline R code uses existing variables from the setup chunk.
# New R chunks needed for fairness interaction data loading (marked with comments).

# Results

## The Baseline Loser Effect

```{r pooled-summary}
#| include: false
# Summary stats for inline text
proc_items <- pooled %>% filter(item_type == "procedural")
sub_items <- pooled %>% filter(item_type == "substantive")
gov_items <- pooled %>% filter(item_type == "governance")

n_proc_pos <- sum(proc_items$ame > 0)
n_proc_total <- nrow(proc_items)
n_proc_sig <- sum(proc_items$p < 0.05)

n_sub_neg <- sum(sub_items$ame < 0)
n_sub_total <- nrow(sub_items)
n_sub_sig <- sum(sub_items$p < 0.05)

proc_range_lo <- fmt_pct(min(proc_items$ame[proc_items$ame > 0]))
proc_range_hi <- fmt_pct(max(proc_items$ame))

sub_range_lo <- fmt_pct(min(sub_items$ame))
sub_range_hi <- fmt_pct(max(sub_items$ame[sub_items$ame < 0]))

# Largest effects
largest_proc <- proc_items %>% filter(ame == max(ame))
largest_sub <- sub_items %>% filter(ame == min(ame))
largest_gov <- gov_items %>% filter(ame == min(ame))

# Free elections item
free_elections <- proc_items %>% filter(item_label == "Free elections")
```

Table 1 reports the average marginal effect of loser status on the probability of selecting each of the twenty items across the pooled sample. The pattern is consistent with H1: `r n_proc_pos` of `r n_proc_total` procedural items show positive effects (losers more likely to select), while all `r n_sub_total` substantive items show negative effects (winners more likely to select). Procedural effects range from `r proc_range_lo` to `r proc_range_hi` percentage points, with `r largest_proc$item_label` showing the largest effect (`r fmt_pp(largest_proc$ame)` pp, $p$ `r ifelse(largest_proc$p < 0.001, "< 0.001", sprintf("= %.3f", largest_proc$p))`). Substantive effects range from `r sub_range_lo` to `r sub_range_hi` pp, with `r largest_sub$item_label` as the largest (`r fmt_pp(largest_sub$ame)` pp, $p$ `r ifelse(largest_sub$p < 0.001, "< 0.001", sprintf("= %.3f", largest_sub$p))`).

```{r pooled-table}
#| output: asis

# Build the main results table
tbl <- pooled %>%
  arrange(desc(ame)) %>%
  mutate(
    Type = str_to_title(item_type),
    Item = item_label,
    `AME (pp)` = sprintf("%+.1f", ame * 100),
    SE = sprintf("%.1f", se * 100),
    Sig = sig
  ) %>%
  select(Item, Type, `AME (pp)`, SE, Sig)

if (knitr::is_latex_output()) {
  tbl_out <- tbl %>%
    kbl(
      booktabs = TRUE,
      longtable = FALSE,
      linesep = "",
      caption = "Average Marginal Effect of Loser Status on Item Selection"
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 10,
      full_width = TRUE
    ) %>%
    footnote(
      general = "Average marginal effects from multinomial logit with country and wave fixed effects and demographic controls (age, gender, education, urban residence). Positive values indicate losers are more likely to select the item. Bootstrap SEs clustered at the country level.",
      symbol = c("$\\\\dagger$ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"),
      general_title = "Note: ",
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  # Make footnote rows wrap and single-space
  tbl_str <- as.character(tbl_out)
  tbl_str <- gsub("\\multicolumn{5}{l}{", "\\multicolumn{5}{p{\\linewidth}}{\\singlespacing ", tbl_str, fixed = TRUE)
  cat(tbl_str)
} else {
  tbl %>%
    kable(format = "pipe", align = c("l", "l", "l", "r", "r", "c"))
}
```

The individual effects are modest---typically two to four percentage points---but the consistency of the pattern across twenty items from five separate batteries is notable. This is not a result driven by one or two items. Winners and losers differ systematically in how they conceptualize democracy, with losers oriented toward the rules governing political competition and winners oriented toward the material benefits government provides.

One item warrants specific attention. Free elections---the most canonical procedural item---shows a *negative* AME (`r fmt_pp(free_elections$ame)` pp), meaning losers are slightly *less* likely than winners to identify free elections as the most important feature of democracy. This runs counter to H1's general prediction, but the pattern becomes interpretable in light of the remaining procedural results. The items that losers *do* favor are the liberal components of democratic governance: free expression (`r fmt_pp(proc_items %>% filter(item_label == "Free expression") %>% pull(ame))` pp), media freedom (`r fmt_pp(proc_items %>% filter(item_label == "Media freedom") %>% pull(ame))` pp), the right to organize groups (`r fmt_pp(proc_items %>% filter(item_label == "Organize groups") %>% pull(ame))` pp), and party competition (`r fmt_pp(proc_items %>% filter(item_label == "Party competition") %>% pull(ame))` pp). Losers who have experienced electoral manipulation or whose preferred parties have been dissolved by courts may have reason to distrust the electoral mechanism itself while valuing the broader liberal ecosystem---media, expression, association, judicial accountability---that enables political contestation outside formal elections. The loser effect, in other words, is not an elections effect but a *liberal-democratic* effect: losers gravitate toward the institutional protections that sustain opposition rather than the specific electoral procedure that has failed to deliver victory.

The governance items reveal an additional pattern. Winners are consistently more likely to select governance items emphasizing order and state capacity, with `r largest_gov$item_label` showing the largest effect (`r fmt_pp(largest_gov$ame)` pp, $p$ `r ifelse(largest_gov$p < 0.001, "< 0.001", sprintf("= %.3f", largest_gov$p))`). Winners conceive of democracy as a framework for stability and effective administration; losers conceive of it as a framework for contestation and the protection of rights. This order-versus-contestation tension is explored further in the discussion.

## Democratic Erosion and Institutional Resilience

```{r trajectory-stats}
#| include: false
# Key trajectory stats
# Thailand
thai_w3_pp <- round(thai_w3_gap * 100, 1)
thai_w4_pp <- round(thai_w4_gap * 100, 1)
thai_w6_pp <- round(thai_w6_gap * 100, 1)
thai_w6_ci <- thai %>% filter(wave == 6)

# South Korea
kor <- cw_gap %>% filter(country_name == "South Korea") %>% arrange(wave)
kor_w3_pp <- round(kor %>% filter(wave == 3) %>% pull(proc_sub_gap) * 100, 1)
kor_w4_pp <- round(kor %>% filter(wave == 4) %>% pull(proc_sub_gap) * 100, 1)
kor_w6_pp <- round(kor %>% filter(wave == 6) %>% pull(proc_sub_gap) * 100, 1)

# Japan
jpn <- cw_gap %>% filter(country_name == "Japan") %>% arrange(wave)

# Cambodia
khm <- cw_gap %>% filter(country_name == "Cambodia") %>% arrange(wave)
```

Figure 1 plots the procedural--substantive gap for each country across survey waves. A positive value indicates that losers disproportionately favor procedural conceptions. The within-country trajectories provide the analysis's most compelling evidence for positional updating.

```{r fig-trajectories, fig.width=10, fig.height=6}
#| label: fig-trajectories
#| fig-cap: "Loser Effect Trajectories: Procedural--Substantive Gap by Country and Wave"
#| fig-alt: "Line plot showing procedural-substantive gap trajectories across three survey waves for eleven Asian countries. Thailand shows dramatic increase from near zero to over 20 percentage points. South Korea remains near zero throughout. Cambodia maintains a high gap around 13 points. Other countries shown in gray."

# Plot countries with 2+ waves
plot_data <- cw_gap %>%
  group_by(country_name) %>%
  filter(n() >= 2) %>%
  ungroup()

highlight <- c("Thailand", "South Korea", "Japan", "Cambodia")

plot_data %>%
  mutate(
    highlighted = country_name %in% highlight,
    country_label = if_else(highlighted, country_name, "Other")
  ) %>%
  ggplot(aes(x = wave, y = proc_sub_gap * 100, group = country_name)) +
  geom_line(
    data = . %>% filter(!highlighted),
    color = "gray75", linewidth = 0.5, alpha = 0.7
  ) +
  geom_point(
    data = . %>% filter(!highlighted),
    color = "gray75", size = 1.5, alpha = 0.7
  ) +
  geom_ribbon(
    data = . %>% filter(highlighted),
    aes(ymin = proc_sub_gap_ci_low * 100, ymax = proc_sub_gap_ci_high * 100,
        fill = country_name),
    alpha = 0.15, color = NA
  ) +
  geom_line(
    data = . %>% filter(highlighted),
    aes(color = country_name), linewidth = 1.2
  ) +
  geom_point(
    data = . %>% filter(highlighted),
    aes(color = country_name), size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(
    breaks = c(3, 4, 6),
    labels = c("W3\n(2010-12)", "W4\n(2014-16)", "W6\n(2019-22)")
  ) +
  scale_color_manual(values = c(
    "Thailand" = "#D62728", "Cambodia" = "#FF7F0E",
    "South Korea" = "#2CA02C", "Japan" = "#1F77B4"
  )) +
  scale_fill_manual(values = c(
    "Thailand" = "#D62728", "Cambodia" = "#FF7F0E",
    "South Korea" = "#2CA02C", "Japan" = "#1F77B4"
  )) +
  labs(
    x = "Survey Wave",
    y = "Procedural − Substantive Gap\n(percentage points)",
    color = NULL, fill = NULL,
    caption = "Note: Gray lines show remaining countries. Shaded bands show 95% CIs for highlighted countries.\nPositive values = losers favor procedural items relative to substantive items."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
```

Thailand provides the most dramatic support for H3. In Wave 3 (2010--2012), during the Democrat Party government of Abhisit Vejjajiva, the procedural--substantive gap was negligible (`r thai_w3_pp` pp, not significant). By Wave 4 (2014--2016), conducted around the time of the May 2014 coup that removed Prime Minister Yingluck Shinawatra, the gap had widened to `r thai_w4_pp` pp ($p < 0.05$). By Wave 6 (2019--2022), after six years of military or military-backed governance, the gap reached `r thai_w6_pp` pp ($p < 0.001$, 95% CI: \[`r round(thai_w6_ci$proc_sub_gap_ci_low * 100, 1)`, `r round(thai_w6_ci$proc_sub_gap_ci_high * 100, 1)`\]). This trajectory---from zero to over twenty percentage points in a decade---tracks Thailand's democratic erosion with remarkable precision [@Kuhonta2014-qk; @Kongkirati2020-fl]. As coups and judicial interventions repeatedly overturned electoral outcomes, supporters of the excluded Thaksin-aligned faction increasingly defined democracy in procedural terms, while supporters of the military-backed order emphasized substantive outcomes.

South Korea presents the mirror image. Despite experiencing its own political upheaval during this period---the impeachment and removal of President Park Geun-hye in 2016--2017 and the subsequent election of progressive Moon Jae-in---the procedural--substantive gap remained modest throughout, fluctuating between `r kor_w3_pp` pp in Wave 3 and `r kor_w4_pp` pp in Wave 4, never exceeding five percentage points. The critical difference is institutional: South Korea's crisis was resolved through constitutional procedures rather than military intervention. The impeachment demonstrated that accountability mechanisms functioned, and the 2017 election delivered genuine alternation. Under these conditions, the positional logic predicts convergence, and convergence is what the data show.

Cambodia shows a persistently large gap (approximately 13 pp across all three waves) that, notably, did not grow following the dissolution of the main opposition party (CNRP) in 2017. In a competitive authoritarian context, the stakes of losing were already effectively maximized---losers' procedural orientation had little room to intensify further. Among the remaining countries (@tbl-country-gaps), Malaysia shows a large gap that narrows across waves, potentially reflecting the political opening that culminated in the 2018 alternation. Myanmar's single observation captures a strikingly large gap consistent with the high stakes of its fragile democratic transition. Taiwan's trajectory is distinctive, reversing from positive to negative by Wave 6---a pattern that may reflect the unusual dynamics of cross-strait identity politics. Japan provides the expected baseline for a consolidated democracy: modest and stable across waves.

```{r tbl-country-gaps}
#| label: tbl-country-gaps
#| tbl-cap: "Procedural--Substantive Gap by Country and Wave"

gap_tbl <- cw_gap %>%
  mutate(
    gap_label = sprintf("%+.1f [%+.1f, %+.1f]",
                        proc_sub_gap * 100,
                        proc_sub_gap_ci_low * 100,
                        proc_sub_gap_ci_high * 100)
  ) %>%
  select(country_name, wave_label, gap_label) %>%
  pivot_wider(names_from = wave_label, values_from = gap_label, values_fill = "---") %>%
  rename(Country = country_name) %>%
  select(Country, W3, W4, W6)

if (knitr::is_latex_output()) {
  gap_tbl %>%
    kbl(
      booktabs = TRUE,
      longtable = FALSE,
      linesep = ""
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 10
    ) %>%
    column_spec(1, width = "3cm") %>%
    column_spec(2, width = "3.5cm") %>%
    column_spec(3, width = "3.5cm") %>%
    column_spec(4, width = "3.5cm") %>%
    footnote(
      general = "Gap = (Mean Procedural AME) - (Mean Substantive AME), in percentage points, with 95% bootstrap CIs. Positive values indicate losers favor procedural items relative to substantive items. Dashes indicate country not surveyed in that wave.",
      general_title = "Note: ",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )
} else {
  gap_tbl %>%
    kable(format = "pipe", align = c("l", "c", "c", "c"))
}
```

## Mechanism Test: Perceived Electoral Fairness

```{r fairness-data}
#| include: false
# Load fairness interaction results
fairness <- read_csv(file.path(results_path, "robustness_fairness_interaction.csv"), show_col_types = FALSE)

# Key interaction effects
court_int <- fairness %>% filter(item == "Court protection") %>% pull(interaction)
media_int <- fairness %>% filter(item == "Media freedom") %>% pull(interaction)
expression_int <- fairness %>% filter(item == "Free expression") %>% pull(interaction)
elections_int <- fairness %>% filter(item == "Free elections") %>% pull(interaction)

# Count positive interactions among procedural items
proc_fairness <- fairness %>% filter(item_type == "procedural")
n_proc_pos_int <- sum(proc_fairness$interaction > 0)
n_proc_int_total <- nrow(proc_fairness)
```

The positional updating account predicts that the loser effect should intensify among losers who perceive elections as unfair, since these citizens face the most acute procedural threat. If instead the loser effect reflects stable normative commitments to procedural democracy, it should appear regardless of whether losers perceive the process as fair---committed proceduralists would prioritize procedures as a matter of principle, not instrumental concern. To test this, I interact loser status with a binary indicator of whether respondents perceived the most recent election as unfair, following @Mauk2022-xx.

```{r fig-fairness}
#| label: fig-fairness
#| fig-cap: "Fairness Interaction: Loser Effect by Perceived Electoral Fairness"
#| fig-alt: "Dot plot showing loser effects separately for respondents perceiving fair vs. unfair elections, across sixteen items. Liberal procedural items show substantially larger loser effects among those perceiving unfair elections."
#| fig-width: 8
#| fig-height: 6

# Plot interaction effects
fairness %>%
  mutate(
    item = fct_reorder(item, interaction),
    item_type = str_to_title(item_type)
  ) %>%
  pivot_longer(
    cols = c(loser_effect_fair, loser_effect_unfair),
    names_to = "condition",
    values_to = "loser_effect"
  ) %>%
  mutate(
    condition = if_else(condition == "loser_effect_fair", "Fair election", "Unfair election")
  ) %>%
  ggplot(aes(x = loser_effect * 100, y = item, color = condition, shape = condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  facet_wrap(~ item_type, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Fair election" = "#1F77B4", "Unfair election" = "#D62728")) +
  labs(
    x = "Loser Effect (percentage points)",
    y = NULL,
    color = NULL, shape = NULL,
    caption = "Note: Loser AMEs from multinomial logit interacting loser status with perceived electoral fairness.\nCountry and wave fixed effects with demographic controls."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
```

The results support the positional account. Among `r n_proc_int_total` procedural items, `r n_proc_pos_int` show a positive interaction---meaning the loser effect is larger among those perceiving elections as unfair. The amplification is especially pronounced for the liberal procedural items: court protection shows an interaction of `r sprintf("%+.1f", court_int * 100)` percentage points, media freedom `r sprintf("%+.1f", media_int * 100)` pp, and free expression `r sprintf("%+.1f", expression_int * 100)` pp. Losers who perceive the electoral process as compromised do not simply withdraw from procedural commitments; they double down on precisely the institutional safeguards---judicial protection, press freedom, expressive liberty---that would constrain those who benefit from unfair elections. The free elections item, by contrast, shows the opposite pattern (interaction: `r sprintf("%+.1f", elections_int * 100)` pp), reinforcing the interpretation from the pooled results: losers who perceive elections as unfair are *less* inclined to identify elections themselves as democracy's essential feature, even as they intensify their commitment to the liberal infrastructure surrounding elections.

These interaction patterns are difficult to reconcile with a stable-commitments account. If the loser effect reflected fixed normative orientations, it should not vary systematically with perceptions of electoral fairness. That it does---and that the amplification concentrates on liberal protections rather than elections per se---suggests that democratic conceptions respond to the perceived severity of procedural threat, consistent with the positional updating mechanism.

# Robustness

Several additional tests assess the sensitivity of the main findings. First, a weighted least squares (WLS) estimation that aggregates the data to the country-wave level sidesteps the clustering problem entirely: nine of ten procedural items show positive loser effects (six significant), all six substantive items show negative effects (five significant), with mean effect sizes comparable to the individual-level estimates (+2.2 pp procedural, -2.4 pp substantive; Appendix G).

Second, Wave 2 of the ABS, which employed a different forced-choice instrument, provides an independent replication. Both procedural items show positive AMEs and both substantive items show negative AMEs, with directional consistency that increases confidence the loser effect is not an artifact of the Wave 3--6 battery design. Open-ended responses from Wave 2 confirm convergent validity: respondents selecting procedural items were significantly more likely to offer procedural definitions spontaneously ($r = 0.28$, $p < .001$; Appendix F).[^1]

[^1]: Following established conceptual frameworks, open-ended responses were coded as procedural, substantive, or excluded. A respondent-level procedural proportion score (0--1) based on up to three codeable responses correlates at $r = 0.28$ with the closed-ended item. Given the noise inherent in open-ended coding, this represents meaningful convergent validity.

Third, Wave 6 fieldwork coincided with the COVID-19 pandemic, which may have compressed the procedural-substantive gap by increasing the salience of state capacity for all citizens. Re-estimating Wave 6 models with controls for pandemic attitudes yields essentially unchanged results (mean procedural AME: `r fmt_pp(covid_proc_base)` → `r fmt_pp(covid_proc_covid)` pp; mean substantive AME: `r fmt_pp(covid_sub_base)` → `r fmt_pp(covid_sub_covid)` pp). Thailand's gap persists through the pandemic, consistent with democratic erosion overwhelming the homogenizing pull of COVID-19.

Fourth, non-voters resemble winners rather than losers in their procedural-substantive gap (14.3 pp vs. 13.9 pp for winners and 17.3 pp for losers), confirming that restricting the sample to voters does not inflate the estimated loser effect (Appendix H). Fifth, a three-way decomposition distinguishing key opposition supporters from other losers yields no clear differentiation across the three countries where it was feasible, likely reflecting limited coverage and additional missingness (Appendix J). The binary winner-loser distinction provides broader coverage and cleaner identification.

Finally, the use of repeated cross-sections rather than panel data means within-country trajectories cannot definitively rule out compositional change. The Thai case offers the strongest counterargument: party loyalties in Thailand's color-coded politics are notably stable, and the gap's tracking of documented political events---rather than gradual demographic shifts---favors the positional interpretation.
