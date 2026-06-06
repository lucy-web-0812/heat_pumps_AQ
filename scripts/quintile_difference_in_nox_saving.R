# Looking at the difference between quintiles for NOx saving.... 

library(tidyverse)



model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")


pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")


nox_per_boiler_per_year <- 644


df <- model_results_per_pc |> 
  left_join(pc_combined_dataset, join_by(PCON25CD)) |> 
  filter(model_run %in% c("suitability_probability", "present_day_scenario"))  |> 
  select(PCON25CD, year, model_run, nox_conc_quintile, cumulative_heat_pump_number) |> 
  mutate(emission_saving_total_tonnes = cumulative_heat_pump_number * 
           nox_per_boiler_per_year / 1000000) |> 
  group_by(model_run, year, nox_conc_quintile) |> 
  summarise(total_nox_saving = sum(emission_saving_total_tonnes))



ggplot(df) +
  geom_point(aes(x = year, y = total_nox_saving, colour = nox_conc_quintile)) +
  facet_wrap(~model_run)



quintile_diff <- df |> 
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  pivot_wider(names_from = nox_conc_quintile, values_from = total_nox_saving) |> 
  mutate(quintile_diff = `1` - `5`) 


ggplot(quintile_diff) +
  geom_point(aes(x = year, y = quintile_diff, colour = model_run)) +
  geom_line(aes(x = year, y = quintile_diff, colour = model_run))








ggplot(quintile_diff) +
  geom_area(aes(x = year, y = `1`, fill = model_run))

            

df |> 
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  ggplot() +
  geom_point(aes(x = year, y = total_nox_saving, colour = factor(nox_conc_quintile))) +
  geom_line(aes(x = year, y = total_nox_saving, colour = factor(nox_conc_quintile))) +
  facet_wrap(~model_run)







df <- model_results_per_pc |> 
  left_join(pc_combined_dataset, join_by(PCON25CD)) |> 
  filter(model_run %in% c("suitability_probability", "present_day_scenario"))  |> 
    select(PCON25CD, year, model_run, new_ranking_quintile_deprivation, cumulative_heat_pump_number) |> 
  mutate(emission_saving_total_tonnes = cumulative_heat_pump_number * 
           nox_per_boiler_per_year / 1000000) |> 
  group_by(model_run, year, new_ranking_quintile_deprivation) |> 
  summarise(total_nox_saving = sum(emission_saving_total_tonnes))



ggplot(df) +
  geom_point(aes(x = year, y = total_nox_saving, colour = new_ranking_quintile_deprivation)) +
  facet_wrap(~model_run)



quintile_diff <- df |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  pivot_wider(names_from = new_ranking_quintile_deprivation, values_from = total_nox_saving) |> 
  mutate(quintile_diff = `1` - `5`) 


ggplot(quintile_diff) +
  geom_point(aes(x = year, y = quintile_diff, colour = model_run)) +
  geom_line(aes(x = year, y = quintile_diff, colour = model_run))








ggplot(quintile_diff) +
  geom_area(aes(x = year, y = `1`, fill = model_run))



df |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  ggplot() +
  geom_point(aes(x = year, y = total_nox_saving, colour = factor(new_ranking_quintile_deprivation))) +
  geom_line(aes(x = year, y = total_nox_saving, colour = factor(new_ranking_quintile_deprivation))) +
  facet_wrap(~model_run)


ggplot(df, aes(x = year, y = total_nox_saving, fill = factor(new_ranking_quintile_deprivation))) +
  geom_area(position = "fill") +
  facet_wrap(~model_run) +
  scale_y_continuous(labels = scales::percent)

