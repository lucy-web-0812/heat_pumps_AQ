library(tidyverse)



model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")


pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv") |> 
  select(-PCON25NM)



nox_savings_per_boiler_per_year <- (0.056 * 11500) / 1000000000 # Convert from grams to kilotonnnes


pc_stats_with_bounds <- model_results_per_pc |> 
  left_join(pc_combined_dataset) |> 
  filter(model_run %in% c("suitability_probability", "present_day_scenario")) |> 
  mutate(nox_saving_per_year = cumulative_heat_pump_number * nox_savings_per_boiler_per_year,
         nox_saving_per_year_low = cumulative_heat_pump_number_lower_bound * nox_savings_per_boiler_per_year,
         nox_saving_per_year_high = cumulative_heat_pump_number_upper_bound * nox_savings_per_boiler_per_year) |> 
  group_by(westminster_parliamentary_constituency, model_run) |> 
  arrange(year, .by_group = TRUE) |> 
  mutate(cum_nox_saving = cumsum(nox_saving_per_year), 
         cum_nox_saving_low = cumsum(nox_saving_per_year_low), 
         cum_nox_saving_high = cumsum(nox_saving_per_year_high)) 




nox_by_conc_quintile <- pc_stats_with_bounds |> 
  filter(year == "2050-01-01") |> 
  group_by(nox_conc_quintile, model_run) |> 
  summarise(
    total_nox = sum(cum_nox_saving), 
    total_nox_low = sum(cum_nox_saving_low), 
    total_nox_high = sum(cum_nox_saving_high),
    .groups = "drop"
  )

# Pivot wider so Q1 (least polluted) and Q5 (most polluted) sit side by side
nox_conc_wide <- nox_by_conc_quintile |> 
  filter(nox_conc_quintile %in% c(1, 5)) |> 
  pivot_wider(
    names_from = nox_conc_quintile,
    values_from = c(total_nox, total_nox_low, total_nox_high),
    names_glue = "{.value}_q{nox_conc_quintile}"
  )

# % difference: Q1 (least polluted) relative to Q5 (most polluted), using true
# interval arithmetic (independent bounds, not matched low-to-low/high-to-high).
# Positive value = Q1 savings are X% greater than Q5 savings.
nox_conc_pct_diff <- nox_conc_wide |> 
  mutate(
    pct_diff_central = (total_nox_q1 - total_nox_q5) / total_nox_q5 * 100,
    pct_diff_low     = (total_nox_low_q1 - total_nox_high_q5) / total_nox_high_q5 * 100,
    pct_diff_high    = (total_nox_high_q1 - total_nox_low_q5) / total_nox_low_q5 * 100
  ) |> 
  select(model_run, pct_diff_central, pct_diff_low, pct_diff_high)

nox_conc_pct_diff

# Rounded to 2 s.f. for reporting
nox_conc_pct_diff |> 
  mutate(across(where(is.numeric), ~ signif(.x, 3)))



# BY DEPRIVATION
nox_by_deprivation_quintile <- pc_stats_with_bounds |> 
  filter(year == "2050-01-01") |> 
  group_by(new_ranking_quintile_deprivation, model_run) |> 
  summarise(
    total_nox = sum(cum_nox_saving), 
    total_nox_low = sum(cum_nox_saving_low), 
    total_nox_high = sum(cum_nox_saving_high),
    .groups = "drop"
  )

# Pivot wider so Q1 and Q5 (or any pair) sit side by side per scenario
nox_wide <- nox_by_deprivation_quintile |> 
  filter(new_ranking_quintile_deprivation %in% c(1, 5)) |> 
  pivot_wider(
    names_from = new_ranking_quintile_deprivation,
    values_from = c(total_nox, total_nox_low, total_nox_high),
    names_glue = "{.value}_q{new_ranking_quintile_deprivation}"
  )

# Calculate % difference: Q5 relative to Q1, using true interval arithmetic.
# Central estimate uses the central values for both quintiles.
# The LOWEST plausible disparity pairs the smallest Q5 with the largest Q1
# (i.e. low_Q5 vs high_Q1) - the scenario least favourable to a large gap.
# The HIGHEST plausible disparity pairs the largest Q5 with the smallest Q1
# (i.e. high_Q5 vs low_Q1) - the scenario most favourable to a large gap.
# This treats the two quintiles' uncertainty as independent, giving wider,
# more conservative bounds than matching low-to-low/high-to-high.
nox_quintile_pct_diff <- nox_wide |> 
  mutate(
    pct_diff_central = (total_nox_q5 - total_nox_q1) / total_nox_q1 * 100,
    pct_diff_low     = (total_nox_low_q5 - total_nox_high_q1) / total_nox_high_q1 * 100,
    pct_diff_high    = (total_nox_high_q5 - total_nox_low_q1) / total_nox_low_q1 * 100
  ) |> 
  select(model_run, pct_diff_central, pct_diff_low, pct_diff_high)

nox_quintile_pct_diff

# Rounded to 2 s.f. for reporting
nox_quintile_pct_diff |> 
  mutate(across(where(is.numeric), ~ signif(.x, 2)))
