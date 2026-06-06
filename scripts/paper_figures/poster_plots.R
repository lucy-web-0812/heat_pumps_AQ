# Now look at heat pump years and deprivation....


# POSTER PLOTS........

# Producing plots for the different policy scenarios for 2050, separated by quintiles of deprivation and quintiles of NOx concentration. 

library(tidyverse)
library(sf)
library(paletteer)


model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")

pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv") |> 
  select(-PCON25NM)


nox_savings_per_boiler_per_year <- 672 / 1000000000 # Convert from grams to kilotonnnes





pc_dep_model_results_all <- model_results_per_pc |> 
  left_join(pc_combined_dataset, join_by(PCON25CD)) |> 
  mutate(median_imd_decile = as.factor(median_imd_decile)) |> 
  mutate(nox_saving = heat_pump_years * nox_savings_per_boiler_per_year) |> # cumulative emissions savings in kilotonnes
  mutate(model_run_label = case_when(
    model_run == "all_three_factors" ~ "BUS, ECO and Suitability", 
    model_run == "BUS_only" ~ "Boiler Upgrade Scheme \n(non-means tested)", 
    model_run == "ECO_only" ~ "Energy Company Obligation\n (means tested)", 
    model_run == "present_day_scenario" ~ "Present day situation", 
    model_run == "suitability_probability" ~ "NESTA suitability", 
  ))

pc_dep_model_results <- pc_dep_model_results_all |> 
  filter(year == "2050-01-01") 


pc_dep_model_results |> 
  filter(model_run == "present_day_scenario") |> 
  mutate(heat_pumps_per_pop = cumulative_heat_pump_number/total_population_PC * 10000) |> 
  ggplot(aes(x = heat_pumps_per_pop, y = pw_mean_nox)) +
  geom_point() +
  geom_smooth(method = "lm") 





# Just to see the mapping from using median imd decile to the new quintiles





nice_labels <- c(
  present_day_scenario    = "Current trends continue",
  suitability_probability = "Suitability-driven uptake", 
  BUS_only = "Boiler Upgrade Scheme only", 
  ECO_only = "Energy Company Obligation only"
)




# And what is the total nox saved per quintile? 

pct_savings_by_quintile <- pc_dep_model_results |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |> 
  group_by(new_ranking_quintile_deprivation, model_run) |> 
  summarise(nox_savings_per_quintile = sum(nox_saving), count = n()) |> 
  group_by(model_run) |> 
  mutate(percentage = nox_savings_per_quintile / sum(nox_savings_per_quintile) * 100)


ggplot(pct_savings_by_quintile) +
  geom_col(aes(x = new_ranking_quintile_deprivation, y = percentage, fill = as.factor(new_ranking_quintile_deprivation)), alpha = 0.8) +
  geom_text(
    aes(x = new_ranking_quintile_deprivation, 
        y = percentage,
        label = paste0(round(percentage, 1), "%")), 
    hjust = -0.1,  
    size = 4
  ) +
  coord_flip() +
  facet_wrap(~factor(model_run, levels = c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")), scales = "free", labeller = as_labeller(nice_labels))+
  scale_x_continuous(name = "Relative Deprivation Quintile", limits = c(0.5,5.5)) +
  scale_y_continuous(name = "NOx Emissions Savings (%)", limits = c(0,30), expand = c(0,0), breaks = seq(0,30,5)) +
  #khroma::scale_fill_mediumcontrast(name = "Relative Deprivation Quintile")+
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  theme_classic(base_size = 16, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "none", 
        legend.justification = "left", 
        axis.text = element_text(size = 12)) 





# And also absolute.... 

ggplot(pct_savings_by_quintile) +
  geom_col(aes(x = new_ranking_quintile_deprivation, y = nox_savings_per_quintile, fill = as.factor(new_ranking_quintile_deprivation)), alpha = 0.9) +
  geom_text(
    aes(x = new_ranking_quintile_deprivation, 
        y = nox_savings_per_quintile,
        label = paste0(round(nox_savings_per_quintile, 1), "kt")), 
    hjust = -0.1,  
    size = 6
  ) +
  coord_flip() +  
  facet_wrap(~factor(model_run, levels = c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")), scales = "free", labeller = as_labeller(nice_labels))+
  scale_x_continuous(name = "Relative Deprivation Quintile") +
  scale_y_continuous(name = "NOx Emissions Savings (kt) over the period 2025-2050", limits = c(0,59), expand = c(0,0)) +
  #khroma::scale_fill_mediumcontrast(name = "Relative Deprivation Quintile")+
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  theme_classic(base_size = 20, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 24, hjust = 0),
        legend.position = "none", 
        legend.justification = "left", 
        axis.text = element_text(size = 12), 
        plot.background = element_blank(), 
        panel.background = element_blank()) 


ggsave("plots/poster_plots/nox_savings_vs_imd.png", width = 12.209302, height = 6, dpi = 600)




# By pollution levels too.... # based on the concentration of nox as no2


by_nox_conc_quintiles <- pc_dep_model_results |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  group_by(nox_conc_quintile, model_run) |> 
  summarise(nox_savings_per_conc_quintile = sum(nox_saving), count = n()) |> 
  group_by(model_run) |> 
  mutate(percentage = nox_savings_per_conc_quintile / sum(nox_savings_per_conc_quintile) * 100)  


# And absolute values

ggplot(by_nox_conc_quintiles) +
  geom_col(aes(x = nox_conc_quintile, y = nox_savings_per_conc_quintile, fill = as.factor(nox_conc_quintile)), alpha = 0.8) +
  geom_text(
    aes(x = nox_conc_quintile, 
        y = nox_savings_per_conc_quintile,
        label = paste0(round(nox_savings_per_conc_quintile, 1), "kt")), 
    hjust = -0.1,  
    size = 6    
  ) +
  coord_flip() +
  facet_wrap(~model_run, scales = "free", labeller = as_labeller(nice_labels))+
  scale_x_continuous(name = "NOx Concentration Quintile") +
  scale_y_continuous(name = "NOx Emissions Savings (kt) over the period 2025-2050", expand = c(0,0), limits = c(0,69), breaks = seq(0,69,10)) +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  #scale_fill_viridis_d(name = "NOx Concentration Quintile") +
  theme_classic(base_size = 20, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 24, hjust = 0),
        legend.position = "none", 
        legend.justification = "left", 
        axis.text = element_text(size = 12), 
        plot.background = element_blank(), 
        panel.background = element_blank()) 

ggsave("plots/poster_plots/nox_savings_vs_nox_conc_quintile.png", width = 12.209302, height = 6, dpi = 600)





pc_dep_model_results |> 
  filter(model_run == "present_day_scenario") |> 
  ggplot(aes(x= new_ranking, y = heat_pump_years)) +
  geom_point() +
  geom_smooth(method = "lm")




# Produce map plots too... 

pc_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp")

pc_combined_dataset |> 
  select(-geometry) |> 
  left_join(pc_boundaries, join_by(PCON25CD == PCON24CD)) |> 
  ggplot() +
  geom_sf(aes(fill =factor(new_ranking_quintile_deprivation), geometry = geometry), colour = "grey") +
  scico::scale_fill_scico_d(name = "Relative Deprivation Quintile", palette = "acton") +
  ggthemes::theme_map() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14), 
        legend.background = element_blank())

ggsave("plots/poster_plots/dep_quintile_map.png", width = 6.209302, height = 6.662791, dpi = 600)


pc_combined_dataset |> 
  select(-geometry) |> 
  left_join(pc_boundaries, join_by(PCON25CD == PCON24CD)) |> 
  ggplot() +
  geom_sf(aes(fill =factor(nox_conc_quintile), geometry = geometry), colour = "grey") +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  ggthemes::theme_map() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14),
        legend.background = element_blank())


ggsave("plots/poster_plots/nox_quintile_map.png", width = 6.209302, height = 6.662791, dpi = 600)




# Inequality metrics 



avg_non_ind_nox_per_pc_2023 <- read_csv("data/processed_data/avg_non_ind_nox_per_pc_2023.csv") 




nox_per_boiler_per_year <- 672 #(assuming 672g NOx per boiler per year)

data_joined <- pc_dep_model_results_all |>
  left_join(avg_non_ind_nox_per_pc_2023, join_by(PCON25CD == PCON24CD))


# Calculate quintile-level means for inequality metrics
quintile_means <- data_joined |>
  mutate(
    # ===== BASELINE NON INDUSTRIAL COMBUSTION EMISSIONS (2023) =====
    # Population-weighted exposure intensity (tonnes NOx per km²)
    baseline_exposure_per_km2 = mean_nox_emission_per_km2,
    
    # Total annual emissions in constituency (tonnes NOx per year)
    total_baseline_emissions_tonnes = total_nox,
    
    # ===== HEAT PUMP EMISSION REDUCTIONS =====
    # Total emission savings from heat pump installations (tonnes per year)
    emission_saving_total_tonnes = cumulative_heat_pump_number * 
      nox_per_boiler_per_year / 1000000,
    
    # Calculate proportional reduction
    # This represents the fraction of total emissions eliminated by heat pumps
    reduction_fraction = emission_saving_total_tonnes / total_baseline_emissions_tonnes,
    reduction_fraction = pmin(reduction_fraction, 1.0),  # Cap at 100%
    reduction_fraction = pmax(reduction_fraction, 0.0),  # Ensure non-negative
    
    # ===== UPDATED EMISSIONS WITH HEAT PUMPS =====
    # Apply proportional reduction to population-weighted exposure
    updated_exposure_per_km2 = baseline_exposure_per_km2 * (1 - reduction_fraction),
    
    # Updated total emissions
    updated_total_emissions_tonnes = total_baseline_emissions_tonnes * (1 - reduction_fraction),
    
    # Emission savings spread uniformly (for comparison only)
    emission_saving_uniform_per_km2 = emission_saving_total_tonnes / area_km2
  ) |> 
  group_by(year, model_run, new_ranking_quintile_deprivation) |>
  summarise(
    mean_emission = mean(updated_exposure_per_km2, na.rm = TRUE), # This is the mean of the population weighted averages.... 
    median_emission = median(updated_exposure_per_km2, na.rm = TRUE),
    quantile_25 = quantile(updated_exposure_per_km2, 0.25,na.rm = TRUE),
    quantile_75 = quantile(updated_exposure_per_km2, 0.75,na.rm = TRUE),
    sd = sd(updated_exposure_per_km2, na.rm = T),
    .groups = "drop"
  )


q1_q5_values <- quintile_means |>
  filter(new_ranking_quintile_deprivation %in% c(1, 5)) |>
  dplyr::select(year, model_run, new_ranking_quintile_deprivation, mean_emission) |>
  pivot_wider(
    names_from  = new_ranking_quintile_deprivation,
    values_from = mean_emission,
    names_prefix = "Q"
  )



# Calculate inequality metrics
inequality_metrics <- quintile_means |>
  group_by(year, model_run) |>
  summarise(
    # Gini coefficient (0 = perfect equality, 1 = perfect inequality)
    gini = ineq::Gini(mean_emission),
    # Slope: emissions increase per quintile
    slope = coef(lm(mean_emission ~ new_ranking_quintile_deprivation))[2],
    # Coefficient of variation
    cv = sd(mean_emission) / mean(mean_emission),
    .groups = "drop"
  ) |> 
  left_join(q1_q5_values, by = c("year", "model_run")) |>
  mutate(
    # Ratio: most deprived to least deprived
    q1_q5_ratio = Q1 / Q5,
    
    # Absolute gap between most and least deprived
    absolute_gap = Q1 - Q5
  )
# And a version with just the present dat and suitaility 


inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  dplyr::select(year,gini, absolute_gap, q1_q5_ratio, model_run) |>
  pivot_longer(cols = c(absolute_gap, q1_q5_ratio), 
               names_to = "metric", values_to = "value") |>
  ggplot(aes(x = year, y = value, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_date(name = "Year", guide = guide_axis(minor.ticks = TRUE)) +
  scale_colour_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", "suitability_probability" = "#DC6BAD","ECO_only"= "#6969B3", "BUS_only" = "#B4CEB3"),
    labels = c("present_day_scenario" = "Current trends continue",
               "suitability_probability" ="Suitability-driven uptake", 
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" ="Boiler Upgrade Scheme"
    )) +
  facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = labeller(
    metric = c( 
      absolute_gap = "Absolute gap (Q1 - Q5, tonnes/km²)",
      q1_q5_ratio  = "Relative gap (Q1/Q5)"
    )
  )) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "top",
        legend.justification = "left", 
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.line = element_line(), 
        axis.ticks = element_line(),
        axis.title.y = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank())

ggsave("plots/poster_plots/absolute_and_rel_inequality_two_scenarios.png", width = 18.32, height = 26, units = "cm")

# Now also if getting error bars....
