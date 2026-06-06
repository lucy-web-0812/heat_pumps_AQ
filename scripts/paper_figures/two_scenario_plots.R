# TWO SCENARIOS LINE PLOTS...!


library(tidyverse)
library(patchwork)
library(sf)
library(paletteer)


# defining labels to use on plots 

nice_labels <- c(
  present_day_scenario    = "Current trends continue",
  suitability_probability = "Suitability-driven uptake"
)



metric_labels <- c(
  hp_installed = "Heat pumps installed\n(thousands)",
  cumulative_savings_per_quintile = "Cumulative NOx savings\n(kilotonnes)",
  cumulative_damage_cost_avoided_per_quintile = "Cumulative damage \ncost avoided\n(£ millions)",
  present_day_scenario    = "Current trends continue",
  suitability_probability = "Suitability-driven uptake"
)



# And constants to be used to calculate NOx savings



emission_limit <- 0.056 # grams per kwh
ofgem_useage <- 11500 # kwh


nox_per_boiler_per_year <- emission_limit * ofgem_useage # grams



# Producing plots for the different policy scenarios for 2050, separated by quintiles of deprivation and quintiles of NOx concentration. 

model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")

pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")

total_damage_costs <- read_csv("data/processed_data/total_damage_costs_by_model_and_pc.csv")



# Combine the data 


pc_dep_model_results <- model_results_per_pc |> 
  filter(model_run %in% c("suitability_probability", "present_day_scenario"))  |> 
  left_join(pc_combined_dataset, join_by(PCON25CD)) |> 
  left_join(total_damage_costs) |> 
  filter(!year %in% as.Date(c("2023-01-01", "2024-01-01")))|> 
  mutate(emission_saving_total_tonnes = cumulative_heat_pump_number * 
           nox_per_boiler_per_year / 1000000) |> 
  arrange( model_run, PCON25CD, year) |> 
  group_by(model_run, PCON25CD) |> 
  mutate(cumulative_emission_saving_tonnes = cumsum(emission_saving_total_tonnes))




# And what is the total nox saved per quintile? 

nox_savings_by_dep_quintile <- pc_dep_model_results |> 
  group_by(new_ranking_quintile_deprivation, model_run, year) |> 
  summarise(nox_savings_per_quintile = sum(emission_saving_total_tonnes),
            hp_installed = sum(heat_pump_number),
            hp_installed_cumulative = sum(cumulative_heat_pump_number), 
            count = n(), 
            cumulative_savings_per_quintile = sum(cumulative_emission_saving_tonnes), 
            damage_cost_avoided_per_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_per_quintile = sum(cumulative_damage_cost_avoided)) 



ggplot(nox_savings_by_dep_quintile) +
  geom_line(aes(x = year, y = hp_installed_cumulative, colour = new_ranking_quintile_deprivation, group = new_ranking_quintile_deprivation)) +
  geom_line(aes(x = year, y = hp_installed, colour = new_ranking_quintile_deprivation, group = new_ranking_quintile_deprivation)) +
  facet_wrap(~model_run)




nox_savings_by_nox_quintile <- pc_dep_model_results |> 
  group_by(nox_conc_quintile, model_run, year) |> 
  summarise(nox_savings_per_quintile = sum(emission_saving_total_tonnes),
            hp_installed = sum(heat_pump_number),
            count = n(), 
            cumulative_savings_per_quintile = sum(cumulative_emission_saving_tonnes), 
            damage_cost_avoided_per_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_per_quintile = sum(cumulative_damage_cost_avoided)) 


# Overall total NOx savings... 

total_nox_savings <- pc_dep_model_results |> 
  filter(year == "2050-01-01") |> 
  filter(model_run == "suitability_probability") |> 
  pull(cumulative_emission_saving_tonnes) |> 
  sum(na.rm = TRUE)





# Part 2: Line graphs -----------------------------------------------------

# And now the line plots...


# And what about the changes through time.....



# Section A: Annual values ------------------------------------------------

# Creating the df of the difference... 

difference_df_dep <- nox_savings_by_dep_quintile |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  select(year, model_run, new_ranking_quintile_deprivation, nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile) |>
  pivot_wider(
    id_cols = c(year, model_run), 
    names_from = new_ranking_quintile_deprivation,
    values_from = c(nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile),
    names_glue = "{.value}_q{new_ranking_quintile_deprivation}") |> 
  mutate(annual_savings_nox_tonnes_diff = nox_savings_per_quintile_q5 - nox_savings_per_quintile_q1, 
         cumulative_savings_nox_tonnes_diff = cumulative_savings_per_quintile_q5 - cumulative_savings_per_quintile_q1, 
         annual_installations_diff = hp_installed_q5 - hp_installed_q1, 
         annual_damage_cost_avoided_diff = damage_cost_avoided_per_quintile_q5 - damage_cost_avoided_per_quintile_q1, 
         cumulative_damage_cost_avoided_diff = cumulative_damage_cost_avoided_per_quintile_q5 - cumulative_damage_cost_avoided_per_quintile_q1) |> 
  pivot_longer(
    cols = ends_with("_diff"),
    names_to = "metric_q5_minus_q1",
    values_to = "value"
  ) |> 
  select(year, model_run, metric_q5_minus_q1,value)|> 
  mutate(value = case_when(
    metric_q5_minus_q1 == "annual_installations_diff" ~ value / 1000,
    metric_q5_minus_q1 == "annual_savings_nox_tonnes_diff" ~ value /1000,
    metric_q5_minus_q1 == "cumulative_savings_nox_tonnes_diff" ~ value /1000, 
    metric_q5_minus_q1 == "annual_damage_cost_avoided_diff" ~ value / 1e6,
    metric_q5_minus_q1 == "cumulative_damage_cost_avoided_diff" ~ value / 1e6,
    TRUE ~ value
  )) |> 
  mutate(metric_q5_minus_q1 = factor(
    metric_q5_minus_q1,
    levels = c(
      "annual_installations_diff",
      "annual_savings_nox_tonnes_diff",
      "cumulative_savings_nox_tonnes_diff",
      "annual_damage_cost_avoided_diff",
      "cumulative_damage_cost_avoided_diff"
    )
  ))






difference_df_dep |>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = model_run)) +
  scale_y_continuous() +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.6, linetype = "dashed") +
  facet_wrap( ~ metric_q5_minus_q1,
              scales = "free_y", 
              axes = "all") +
  theme_minimal(base_size = 16) +
  theme(
    axis.ticks = element_line(),
    strip.placement = "outside",
    legend.position = "top",
    legend.justification.top = "left",
    legend.text = element_text(size = 16),
    axis.line = element_line(),
    axis.title.y = element_blank(),
    panel.spacing = unit(2, "lines")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 10, linewidth = 2))) 



long_data <- nox_savings_by_dep_quintile |> 
 filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  select(year, model_run, new_ranking_quintile_deprivation, nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile) |>
  pivot_longer(
    cols = c(nox_savings_per_quintile, 
             hp_installed,
             cumulative_savings_per_quintile, 
             damage_cost_avoided_per_quintile,
             cumulative_damage_cost_avoided_per_quintile),
    names_to = "metric",
    values_to = "value"
  ) |> 
  mutate(value = case_when(
    metric == "hp_installed" ~ value / 1000,
    metric == "nox_savings_per_quintile" ~ value /1000,
    metric == "cumulative_savings_per_quintile" ~ value /1000, 
    metric == "damage_cost_avoided_per_quintile" ~ value / 1e6,
    metric == "cumulative_damage_cost_avoided_per_quintile" ~ value / 1e6,
    TRUE ~ value
  )) |> 
  mutate(metric = factor(
    metric,
    levels = c(
      "hp_installed",
      "nox_savings_per_quintile",
      "cumulative_savings_per_quintile",
      "damage_cost_avoided_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    )
  ))


fill_df <- long_data |>
  filter(
    metric %in% c(
      "hp_installed",
      "cumulative_savings_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    ),
    new_ranking_quintile_deprivation %in% c(1, 5)
  ) |>
  mutate(new_ranking_quintile_deprivation = factor(new_ranking_quintile_deprivation)) |>
  select(metric, model_run, year, new_ranking_quintile_deprivation, value) |>
  pivot_wider(
    names_from = new_ranking_quintile_deprivation,
    values_from = value
  )




q <- long_data |>
  filter(
    metric %in% c(
      "hp_installed",
      "cumulative_savings_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    )
  ) |>
  ggplot() +
  geom_ribbon(data = fill_df, aes(x = year, ymin = `1`, ymax = `5`), fill = "grey", alpha = 0.2)+
  scale_x_date(name = "Year") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA)) +
  scale_colour_manual(values = c("#260C3F", "#DCA1C2"), name = "Deprivation Quintile", labels = c(`1`= "1 Most Deprived", `5` = "5 - Least Deprived")) +
  geom_line(aes(
    x = year,
    y = value,
    colour = factor(new_ranking_quintile_deprivation),
    linewidth = factor(new_ranking_quintile_deprivation),
    group = interaction(model_run, new_ranking_quintile_deprivation)
  ),
  linewidth = 1.2) +
  facet_grid(rows = vars(metric), 
             cols = vars(model_run),
             scales = "free_y",
             switch = "y",
             axes = "all",
             labeller = as_labeller(metric_labels)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.ticks = element_line(),
    strip.placement = "outside",
    strip.text = element_text(size = 16), 
    legend.position = "top",
    legend.justification.top = "left",
    legend.text = element_text(size = 16),
    axis.line = element_line(),
    axis.title.y = element_blank(), 
    panel.spacing = unit(2, "lines")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 10, linewidth = 2))) 

plotly::ggplotly(q)

ggsave("plots/paper_plots/deprivation_quintiles_timeseries.png", height = 12, width = 12)


# NOx quintiles -----------------------------------------------------------




difference_df_nox <- nox_savings_by_nox_quintile |> 
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  select(year, model_run, nox_conc_quintile, nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile) |>
  pivot_wider(
    id_cols = c(year, model_run), 
    names_from = nox_conc_quintile,
    values_from = c(nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile),
    names_glue = "{.value}_q{nox_conc_quintile}") |> 
  mutate(annual_savings_nox_tonnes_diff = nox_savings_per_quintile_q5 - nox_savings_per_quintile_q1, 
         cumulative_savings_nox_tonnes_diff = cumulative_savings_per_quintile_q5 - cumulative_savings_per_quintile_q1, 
         annual_installations_diff = hp_installed_q5 - hp_installed_q1, 
         annual_damage_cost_avoided_diff = damage_cost_avoided_per_quintile_q5 - damage_cost_avoided_per_quintile_q1, 
         cumulative_damage_cost_avoided_diff = cumulative_damage_cost_avoided_per_quintile_q5 - cumulative_damage_cost_avoided_per_quintile_q1) |> 
  pivot_longer(
    cols = ends_with("_diff"),
    names_to = "metric_q5_minus_q1",
    values_to = "value"
  ) |> 
  select(year, model_run, metric_q5_minus_q1,value)



difference_df_nox |> 
  ggplot(aes(x=year, y=value, colour = model_run)) +
  geom_line() +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  facet_wrap(~metric_q5_minus_q1, scales = "free_y")







long_data_nox_quntile <- nox_savings_by_nox_quintile |> 
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  select(year, model_run, nox_conc_quintile, nox_savings_per_quintile, hp_installed, cumulative_savings_per_quintile, damage_cost_avoided_per_quintile, cumulative_damage_cost_avoided_per_quintile) |>
  pivot_longer(
    cols = c(nox_savings_per_quintile, hp_installed,cumulative_savings_per_quintile, cumulative_savings_per_quintile,damage_cost_avoided_per_quintile,cumulative_damage_cost_avoided_per_quintile),
    names_to = "metric",
    values_to = "value"
  )|> 
  mutate(value = case_when(
    metric == "hp_installed" ~ value / 1000,
    metric == "nox_savings_per_quintile" ~ value /1000,
    metric == "cumulative_savings_per_quintile" ~ value /1000, 
    metric == "damage_cost_avoided_per_quintile" ~ value / 1e6,
    metric == "cumulative_damage_cost_avoided_per_quintile" ~ value / 1e6,
    TRUE ~ value
  ))  |> 
  mutate(metric = factor(
    metric,
    levels = c(
      "hp_installed",
      "nox_savings_per_quintile",
      "cumulative_savings_per_quintile",
      "damage_cost_avoided_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    )
  ))



fill_df <- long_data_nox_quntile |>
  filter(
    metric %in% c(
      "hp_installed",
      "cumulative_savings_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    ),
    nox_conc_quintile %in% c(1, 5)
  ) |>
  mutate(nox_conc_quintile = factor(nox_conc_quintile)) |>
  select(metric, model_run, year, nox_conc_quintile, value) |>
  pivot_wider(
    names_from = nox_conc_quintile,
    values_from = value
  )






p <- long_data_nox_quntile |>
  filter(
    metric %in% c(
      "hp_installed",
      "cumulative_savings_per_quintile",
      "cumulative_damage_cost_avoided_per_quintile"
    )
  ) |>
  ggplot() +
  geom_ribbon(data = fill_df, aes(x = year, ymin = `1`, ymax = `5`), fill = "grey", alpha = 0.2) +
  geom_line(aes(
    x = year,
    y = value,
    colour = factor(nox_conc_quintile),
    group = nox_conc_quintile
  )) +
  scale_x_date(name = "Year", limits = as_date(c("2025-01-01", "2050-01-01"))) +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
  scale_colour_manual(values = c("#607345FF", "#6C568CFF"), name = "NOx Concentration Quintile", labels = c(`1`= "1 Least Polluted", `5` = "5 - Most Polluted"))  +#, labels = c(`1`= "1 L
  facet_grid(
    rows = vars(metric),
    cols = vars(model_run),
    scales = "free",
    switch = "y",
    axes = "all",
    labeller = as_labeller(metric_labels)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.ticks = element_line(),
    strip.placement = "outside",
    legend.position = "top",
    legend.justification.top = "left",
    legend.text = element_text(size = 16),
    axis.line = element_line(),
    axis.title.y = element_blank(), 
    panel.spacing = unit(2, "lines")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 10, linewidth = 2))) 

plotly::ggplotly(p)

ggsave("plots/paper_plots/nox_quintiles_timeseries.png")


