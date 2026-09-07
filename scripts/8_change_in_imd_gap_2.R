# ==============================================================================
# NOx EMISSIONS INEQUALITY BY DEPRIVATION — DATA PREPARATION
# ==============================================================================
# Purpose:
#   Join model outputs with deprivation/geography data, calculate heat-pump-
#   driven NOx emission reductions, and produce the summary datasets used in
#   the thesis figures:
#     - plots/paper_plots/inequality_figure.png
#     - plots/paper_plots/absolute_and_rel_inequality.png
#     - plots/paper_plots/absolute_and_rel_inequality_two_scenarios.png
#
#   This script only prepares and saves data — plotting code lives separately.
#
# Pipeline:
#   1. Config
#   2. Load raw inputs
#   3. Join + compute baseline / heat-pump-adjusted emissions -> data_joined
#   4. Quintile-level summary stats (mean, median, IQR, SE) by year/scenario
#   5. Q1 vs Q5 gap metrics
#        (a) median-based, normalised to 2025 baseline -> gap_metrics
#        (b) mean-based, with Gini/slope/CV + propagated 95% CI -> inequality_metrics
#   6. Save processed datasets
# ==============================================================================

library(tidyverse)
library(sf)
library(ineq)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. CONFIG
# ------------------------------------------------------------------------------

# Assumed NOx (grams) avoided per heat pump per year, replacing a gas boiler
nox_per_boiler_per_year <- 644

# Human-readable scenario labels, used consistently across all outputs/plots
scenario_labels <- c(
  present_day_scenario    = "Current trends persist",
  suitability_probability = "Suitability-driven uptake"
)

# Colours used for the two headline scenarios in plots
scenario_colours <- c(
  "Current trends persist"    = "#6F6B73",
  "Suitability-driven uptake" = "#D95AA3"
)

# Colours/labels used for the four-scenario comparison plots (raw model_run values)
model_run_colours <- c(
  "present_day_scenario"    = "#7F7B82",
  "suitability_probability" = "#DC6BAD",
  "ECO_only"                 = "#6969B3",
  "BUS_only"                 = "#B4CEB3"
)
model_run_labels <- c(
  "present_day_scenario"    = "Present Day Scenario",
  "suitability_probability" = "Suitability Probability",
  "ECO_only"                 = "Energy Company Obligation",
  "BUS_only"                 = "Boiler Upgrade Scheme"
)

# Years reported in summary tables
summary_years <- as.Date(c(
  "2023-01-01", "2025-01-01", "2030-01-01",
  "2035-01-01", "2040-01-01", "2045-01-01", "2050-01-01"
))

# Baseline year against which % change is measured
baseline_year <- as.Date("2025-01-01")

# ------------------------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------------------------

model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")

avg_non_ind_nox_per_pc_2023 <- read_csv("data/processed_data/avg_non_ind_nox_per_pc_2023.csv")

pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")

# Kept for constituencies that need mapping elsewhere; not used by the three
# final figures below, but retained here so data_joined stays a single
# source of truth for anything spatial you add later.
parliamentary_boundaries <- read_sf(
  "data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp"
) |>
  dplyr::select(PCON24CD, geometry)

# ------------------------------------------------------------------------------
# 3. JOIN + COMPUTE EMISSIONS -> data_joined
# ------------------------------------------------------------------------------
# NOTE: We use population-weighted NOx concentrations to reflect human
# exposure. Heat pump emission reductions are calculated as a percentage of
# baseline emissions to stay consistent with that population-weighted frame.

data_joined <- model_results_per_pc |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |> 
  left_join(avg_non_ind_nox_per_pc_2023, join_by(PCON25CD == PCON24CD)) |>
  left_join(pc_combined_dataset, join_by(PCON25CD == PCON25CD)) |>
  left_join(parliamentary_boundaries, join_by(PCON25CD == PCON24CD)) |>
  mutate(
    # ---- Baseline non-industrial combustion emissions (2023) ----
    baseline_exposure_per_km2 = mean_nox_emission_per_km2,      # tonnes NOx / km^2
    total_baseline_emissions_tonnes = total_nox,                # tonnes NOx / year
    
    # ---- Heat pump emission reductions ----
    emission_saving_total_tonnes = cumulative_heat_pump_number *
      nox_per_boiler_per_year / 1e6,
    
    reduction_fraction = emission_saving_total_tonnes / total_baseline_emissions_tonnes,
    reduction_fraction = pmin(pmax(reduction_fraction, 0), 1),  # clamp to [0, 1]
    
    # ---- Updated emissions with heat pumps applied ----
    updated_exposure_per_km2 = baseline_exposure_per_km2 * (1 - reduction_fraction),
    updated_total_emissions_tonnes = total_baseline_emissions_tonnes * (1 - reduction_fraction),
    emission_saving_uniform_per_km2 = emission_saving_total_tonnes / area_km2
  ) |>
  dplyr::select(
    PCON25CD, PCON25NM = PCON25NM.x, year, model_run,
    total_nox, new_ranking, new_ranking_quintile_deprivation, median_imd_decile,
    area_km2,
    baseline_exposure_per_km2, total_baseline_emissions_tonnes,
    heat_pump_number, cumulative_heat_pump_number, heat_pump_years,
    emission_saving_total_tonnes, reduction_fraction,
    updated_exposure_per_km2, updated_total_emissions_tonnes,
    nox_conc_quintile
  )

# ------------------------------------------------------------------------------
# 4. QUINTILE-LEVEL SUMMARY STATS (all 5 quintiles, year x scenario)
# ------------------------------------------------------------------------------
# Computed once and reused below, instead of being recalculated per figure.

quintile_summary_stats <- data_joined |>
  group_by(year, model_run, new_ranking_quintile_deprivation) |>
  summarise(
    mean_emission   = mean(updated_exposure_per_km2, na.rm = TRUE),
    median_emission = median(updated_exposure_per_km2, na.rm = TRUE),
    p25             = quantile(updated_exposure_per_km2, 0.25, na.rm = TRUE),
    p75             = quantile(updated_exposure_per_km2, 0.75, na.rm = TRUE),
    sd              = sd(updated_exposure_per_km2, na.rm = TRUE),
    n               = n(),
    se              = sd / sqrt(n),
    .groups = "drop"
  )

# Q1 (most deprived) vs Q5 (least deprived) only, used by both gap metrics below
quintile_q1_q5 <- quintile_summary_stats |>
  filter(new_ranking_quintile_deprivation %in% c("1", "5"))

# ------------------------------------------------------------------------------
# 5a. GAP METRICS — median-based, normalised to 2025 baseline
#     -> feeds plots/paper_plots/inequality_figure.png
# ------------------------------------------------------------------------------

quintile_q1_q5_normalised <- quintile_q1_q5 |>
  group_by(model_run, new_ranking_quintile_deprivation) |>
  mutate(
    baseline_median = median_emission[year == baseline_year],
    baseline_p25    = p25[year == baseline_year],
    baseline_p75    = p75[year == baseline_year],
    pct_median = (median_emission - baseline_median) / baseline_median * 100,
    pct_p25    = (p25 - baseline_p25) / baseline_p25 * 100,
    pct_p75    = (p75 - baseline_p75) / baseline_p75 * 100
  ) |>
  ungroup() |>
  mutate(
    model_run   = recode(model_run, !!!scenario_labels),
    deprivation = recode(new_ranking_quintile_deprivation,
                         "1" = "Q1 (most deprived)", "5" = "Q5 (least deprived)"
    )
  )

gap_metrics <- quintile_q1_q5 |>
  mutate(model_run = recode(model_run, !!!scenario_labels)) |>
  dplyr::select(year, model_run, new_ranking_quintile_deprivation,
                mean_emission, median_emission, p25, p75) |>
  pivot_wider(
    names_from  = new_ranking_quintile_deprivation,
    values_from = c(mean_emission, median_emission, p25, p75),
    names_sep   = "_Q"
  ) |>
  mutate(
    # Absolute gap: Q1 (most deprived) minus Q5 (least deprived)
    abs_median = median_emission_Q1 - median_emission_Q5,
    abs_p25    = p25_Q1 - p25_Q5,
    abs_p75    = p75_Q1 - p75_Q5,
    # Relative gap: ratio of Q1 to Q5
    rel_median = median_emission_Q1 / median_emission_Q5,
    rel_p25    = p25_Q1 / p25_Q5,
    rel_p75    = p75_Q1 / p75_Q5
  )

# ------------------------------------------------------------------------------
# 5b. INEQUALITY METRICS — mean-based, with Gini/slope/CV + propagated 95% CI
#     -> feeds plots/paper_plots/absolute_and_rel_inequality.png
#        plots/paper_plots/absolute_and_rel_inequality_two_scenarios.png
# ------------------------------------------------------------------------------

q1_q5_means_se <- quintile_q1_q5 |>
  dplyr::select(year, model_run, new_ranking_quintile_deprivation, mean_emission, se) |>
  pivot_wider(
    names_from  = new_ranking_quintile_deprivation,
    values_from = c(mean_emission, se),
    names_glue  = "{.value}_Q{new_ranking_quintile_deprivation}"
  )

inequality_metrics <- quintile_summary_stats |>
  group_by(year, model_run) |>
  summarise(
    gini  = Gini(mean_emission),
    slope = coef(lm(mean_emission ~ new_ranking_quintile_deprivation))[2],
    cv    = sd(mean_emission) / mean(mean_emission),
    .groups = "drop"
  ) |>
  left_join(q1_q5_means_se, by = c("year", "model_run")) |>
  mutate(
    # Point estimates
    q1_q5_ratio  = mean_emission_Q1 / mean_emission_Q5,
    absolute_gap = mean_emission_Q1 - mean_emission_Q5,
    
    # Propagated standard errors
    # Ratio: SE(A/B) ~= (A/B) * sqrt((SE_A/A)^2 + (SE_B/B)^2)
    se_ratio = q1_q5_ratio * sqrt(
      (se_Q1 / mean_emission_Q1)^2 + (se_Q5 / mean_emission_Q5)^2
    ),
    # Difference: SE(A-B) = sqrt(SE_A^2 + SE_B^2)
    se_gap = sqrt(se_Q1^2 + se_Q5^2),
    
    # 95% confidence intervals
    ratio_ci_lower = q1_q5_ratio - 1.96 * se_ratio,
    ratio_ci_upper = q1_q5_ratio + 1.96 * se_ratio,
    gap_ci_lower   = absolute_gap - 1.96 * se_gap,
    gap_ci_upper   = absolute_gap + 1.96 * se_gap
  )

# Summary table for key years (present day + suitability scenarios only)
inequality_summary <- inequality_metrics |>
  filter(
    model_run %in% c("present_day_scenario", "suitability_probability"),
    year %in% summary_years
  ) |>
  dplyr::select(
    model_run, year,
    mean_emission_Q1, mean_emission_Q5, absolute_gap, q1_q5_ratio
  ) |>
  mutate(
    year = format(year, "%Y"),
    across(where(is.numeric), ~ round(.x, 3))
  )

# ------------------------------------------------------------------------------
# 6. SAVE PROCESSED DATASETS
# ------------------------------------------------------------------------------

write_csv(data_joined, "data/processed_data/data_joined.csv")
write_csv(gap_metrics, "data/processed_data/gap_metrics.csv")
write_csv(quintile_q1_q5_normalised, "data/processed_data/quintile_q1_q5_normalised.csv")
write_csv(inequality_metrics, "data/processed_data/inequality_metrics.csv")
write_csv(inequality_summary, "data/processed_data/inequality_metrics_summary.csv")


# 7. PLOTS -> the three paper figures
# ==============================================================================

# ------------------------------------------------------------------------------
# Shared theme
# ------------------------------------------------------------------------------

theme_inequality <- function() {
  theme_minimal(base_size = 14) +
    theme(
      legend.position      = "bottom",
      legend.justification = "left",
      legend.key.width     = unit(2.5, "cm"),
      axis.line            = element_line(colour = "black", linewidth = 0.3),
      axis.ticks           = element_line(colour = "black")
    )
}

# ------------------------------------------------------------------------------
# 7a. inequality_figure.png
#     Absolute gap (Q1 - Q5) and relative gap (Q1 / Q5), median-based,
#     built from gap_metrics (already scenario-labelled in section 5a)
# ------------------------------------------------------------------------------

plot_absolute <- ggplot(gap_metrics, aes(colour = model_run, fill = model_run)) +
  geom_point(aes(x = year, y = abs_median), size = 0.6) +
  geom_line(aes(x = year, y = abs_median), linewidth = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(name = "", values = scenario_colours) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name   = expression("Absolute gap (Q1 " ~ "-" ~ " Q5) (tonnes km"^{-2} * ")"),
    guide  = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = 0.1)
  ) +
  guides(colour = "none", fill = "none") +
  theme_inequality()

plot_relative <- ggplot(gap_metrics, aes(colour = model_run, fill = model_run)) +
  geom_point(aes(x = year, y = rel_median), size = 0.6) +
  geom_line(aes(x = year, y = rel_median), linewidth = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(name = "", values = scenario_colours) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name   = "Relative gap (Q1 / Q5 ratio)",
    guide  = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = 0.1)
  ) +
  theme_inequality()

(plot_absolute + plot_relative +   plot_annotation(tag_levels = "a", tag_suffix = ")") +
    plot_layout(guides = "collect") &
    theme(
      legend.position      = "bottom",
      plot.tag.position    = c(0, 1)
    ) 
  )

ggsave("plots/paper_plots/inequality_figure.png", dpi = 600, height = 130, width = 230, units = "mm")


# ------------------------------------------------------------------------------
# 7c. absolute_and_rel_inequality_two_scenarios.png
#     Same as above, restricted to the two headline scenarios only
# ------------------------------------------------------------------------------

inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  dplyr::select(year, gini, absolute_gap, q1_q5_ratio, model_run) |>
  pivot_longer(cols = c(absolute_gap, q1_q5_ratio), names_to = "metric", values_to = "value") |>
  ggplot(aes(x = year, y = value, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_x_date(name = "Year", guide = guide_axis(minor.ticks = TRUE)) +
  scale_colour_manual(
    name   = "Model Run",
    values = model_run_colours,
    labels = c(
      "present_day_scenario"    = "Current Trends Persist",
      "suitability_probability" = "Suitability Probability"
    )
  ) +
  facet_wrap(~metric, scales = "free_y", ncol = 2, labeller = labeller(
    metric = c(
      absolute_gap = "Absolute gap Q1 - Q5 (tonnes/km\u00b2)",
      q1_q5_ratio  = "Relative gap (Q1 / Q5 ratio)"
    )
  )) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position         = "top",
    legend.justification    = "left",
    legend.title.position  = "top",
    legend.text             = element_text(size = 12),
    axis.line               = element_line(),
    axis.ticks              = element_line(),
    axis.title.y            = element_blank(),
    panel.background        = element_blank(),
    plot.background         = element_blank()
  )

ggsave(
  "plots/paper_plots/absolute_and_rel_inequality_two_scenarios.png",
  width = 20, height = 12, units = "cm", dpi = 600
)
