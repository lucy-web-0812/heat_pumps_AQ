# Script to assign the correct damage cost category to the parlimentary constituencies 
# Do this by looking at the built up areas...


library(sf)
library(tidyverse)
library(plotly)


source("scripts/functions/removing_spiel_function.R")

model_results <- read_csv("data/processed_data/model_results_per_pc.csv")


# We also need to add in the discount factor of 1.5% per annum 



discount_factors <- tibble(year = unique(model_results$year)[-c(1,2)]) |> 
  mutate(discount_factor = 0.985^(row_number() - 1))




# Just use the RUC classifications for the PCs... 



ruc_by_pc <- read_csv("data/raw_data/parliamentary_constituencies/Rural_Urban_Classification_(2021)_of_Westminster_Parliamentary_Constituencies_(2024)_in_EW.csv")  |> 
  mutate(prop_urban = ((100 - `Proportion_of_population_in_rur`) / 100)) |> 
  mutate(prop_rural = Proportion_of_population_in_rur/100)


# Now looking at the damage costs...

damage_costs_by_ruc_type <- read_csv("data/raw_data/damage_costs/damage_cost_by_ruc_type.csv")



# Categorise each FORGE row
damage_costs_categorised <- damage_costs_by_ruc_type |>
  mutate(broad_category = case_when(
    FORGE_type %in% c( "Inner London", "Outer London") ~ "london",
    FORGE_type == "Rural"                                                ~ "rural",
    TRUE                                                                 ~ "urban"   # inner/outer conurbation, urban big/large/medium/small
  ))



urban_costs <- damage_costs_categorised |>
  filter(broad_category == "urban") |>
  summarise(
    central  = mean(`Central damage cost (£/t)`),
    low      = mean(`Damage cost sensitivity range (£/t): low`),
    high     = mean(`Damage cost sensitivity range (£/t): high`)
  )


rural_costs <- damage_costs_categorised |>
  filter(broad_category == "rural") |>
  summarise(
    central  = mean(`Central damage cost (£/t)`),
    low      = mean(`Damage cost sensitivity range (£/t): low`),
    high     = mean(`Damage cost sensitivity range (£/t): high`)
  )


london_costs <- damage_costs_categorised |>
  filter(broad_category == "london") |>
  summarise(
    central  = mean(`Central damage cost (£/t)`),
    low      = mean(`Damage cost sensitivity range (£/t): low`),
    high     = mean(`Damage cost sensitivity range (£/t): high`)
  )





london_pcs <- read_csv("data/raw_data/parliamentary_constituencies/london_constituencies.csv") |> 
  mutate(london_flag = TRUE)


# Add in the London flag.... 

ruc_proportions <- ruc_by_pc |>  
  left_join(london_pcs, join_by(PCON24NM)) |> 
  mutate(london_flag = ifelse(is.na(london_flag), F, london_flag))



damage_costs_by_constituency <- ruc_proportions |> 
  mutate(damage_cost_central = case_when(
    london_flag ~ prop_urban *london_costs$central+ prop_rural * rural_costs$central,
    TRUE      ~ prop_urban * urban_costs$central + prop_rural * rural_costs$central
  ),damage_cost_low = case_when(
    london_flag ~ london_costs$low,
    TRUE      ~ prop_urban * urban_costs$low + prop_rural * rural_costs$low
  ),
  damage_cost_high = case_when(
    london_flag ~ london_costs$high,
    TRUE      ~ prop_urban * urban_costs$high + prop_rural * rural_costs$high
  )) |> 
  dplyr::select(PCON24CD, damage_cost_central, damage_cost_low, damage_cost_high)





write_csv(
  damage_costs_by_constituency,
  "data/processed_data/damage_costs_by_parliamentary_constituency.csv"
)



# Now use the updated damage costs and tie into the model run.... 

nox_from_boiler <- 0.056 # 0.056 g kwh-1
kw_hours_per_year <- 12000 # Taken from Ofgem
grams_to_tonnes_factor <- 10^-6



damage_costs_by_constituency <- read_csv("data/processed_data/damage_costs_by_parliamentary_constituency.csv")



# Join to the model_results.... 


total_damage_costs <- model_results |> 
  left_join(discount_factors) |> 
  left_join(damage_costs_by_constituency, join_by(PCON25CD == PCON24CD)) |> 
  mutate(annual_damage_cost = cumulative_heat_pump_number * kw_hours_per_year * nox_from_boiler * grams_to_tonnes_factor * damage_cost_central * discount_factor) |>
  group_by(PCON25CD, model_run, year) |> 
  summarise(
    total_damage_cost_avoided = sum(annual_damage_cost),
    .groups = "drop"
  ) |>
  filter(is.na(total_damage_cost_avoided) == F) |> 
  arrange(PCON25CD, model_run, year) |>
  group_by(PCON25CD, model_run) |>
  mutate(
    cumulative_damage_cost_avoided = cumsum(total_damage_cost_avoided)
  )



write_csv(total_damage_costs, "data/processed_data/total_damage_costs_by_model_and_pc.csv")


# And if we want to separate by deprivation or pollution levels....


pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")

pc_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp")


df_joined <- total_damage_costs |> 
  left_join(pc_combined_dataset) 


summary_by_quintile <- df_joined |> 
  group_by(model_run,new_ranking_quintile_deprivation, year) |> 
  summarise(total_dca_by_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_by_quintile = sum(cumulative_damage_cost_avoided))




summary_by_quintile |> 
  ggplot() +
  geom_line(aes(x = year, y = cumulative_damage_cost_avoided_by_quintile, colour = new_ranking_quintile_deprivation, group = new_ranking_quintile_deprivation)) +
  facet_wrap(~model_run) +
  scale_x_date(limits = as_date(c("2025-01-01", "2040-01-01"))) +
  scale_y_continuous(limits = c(0,500000000)) +
  scico::scale_colour_scico(palette = "acton") +
  theme_minimal()




key_dates <- summary_by_quintile |> 
  filter(year %in% c("2030-01-01","2040-01-01", "2050-01-01"))


nice_labels <- c(
  present_day_scenario    = "Current trends \n persist",
  suitability_probability = "Suitability-driven uptake", 
  BUS_only = "Boiler Upgrade \nScheme only", 
  ECO_only = "Energy Company \nObligation only"
)



ggplot(key_dates) +
  geom_col(
    aes(
      x = factor(new_ranking_quintile_deprivation),
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      fill = factor(new_ranking_quintile_deprivation)
    )
  ) +
  geom_text(
    aes(
      x = factor(new_ranking_quintile_deprivation),
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      label = paste0("£", round(cumulative_damage_cost_avoided_by_quintile / 1000000, 0), "M"), 
      hjust = -0.2
    )
  ) +
  facet_grid(rows = vars(year), cols = vars(model_run), scales = "free_x",   labeller = labeller(model_run = nice_labels, year = c("2030-01-01" = "2030", "2040-01-01"= "2040", "2050-01-01"="2050")),) +
  #facet_wrap(~model_run) +
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  coord_flip() +
  scale_y_continuous(limits = c(0,1300), name = "Damage Cost Avoided (£ Millions) since 2025") +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 12))



ggplot(key_dates) +
  geom_col(
    aes(
      x = factor(model_run),
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      fill = factor(new_ranking_quintile_deprivation)
    ),
    colour = "lightgrey",
    position = position_dodge(reverse = TRUE)
  ) +
  scale_y_continuous(limits = c(0,1200), name = "Damage Cost Avoided (£ Millions) since 2025", expand = c(0,0)) +
  scale_x_discrete(name = "", labels = nice_labels) +
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  facet_wrap(~year, nrow = 2, labeller = labeller(model_run = nice_labels, year = c("2030-01-01" = "2030", "2040-01-01"= "2040", "2050-01-01"="2050"))) +
  coord_flip()+
  theme_minimal() +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 12))


# Lets do a plot of the constituency maps.... 


spatial_damage_cost <- total_damage_costs |> 
  filter(year %in% c("2040-01-01")) |>  #, "2050-01-01")) |> 
  left_join(pc_boundaries, join_by(PCON25CD == PCON24CD))

ggplotly(
  ggplot(spatial_damage_cost) +
    geom_sf(aes(fill = cumulative_damage_cost_avoided/1000000, geometry = geometry), colour = NA) +
    facet_wrap(~model_run, nrow = 2) +
    scale_fill_gradient(low = "purple", high = "white")+
    ggthemes::theme_map() +
    theme(legend.position = "top")
)



# And do for NOX


summary_by_quintile <- df_joined |> 
  group_by(model_run,nox_conc_quintile, year) |> 
  summarise(total_dca_by_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_by_quintile = sum(cumulative_damage_cost_avoided))




summary_by_quintile |> 
  ggplot() +
  geom_line(aes(x = year, y = cumulative_damage_cost_avoided_by_quintile, colour = nox_conc_quintile, group = nox_conc_quintile)) +
  facet_wrap(~model_run) +
  scale_x_date(limits = as_date(c("2025-01-01", "2040-01-01"))) +
  scale_y_continuous(limits = c(0,500000000)) +
  scale_colour_viridis_c() +
  theme_minimal()




key_dates <- summary_by_quintile |> 
  filter(year %in% c("2030-01-01","2040-01-01", "2050-01-01"))


nice_labels <- c(
  present_day_scenario    = "Current trends \n persist",
  suitability_probability = "Suitability-driven uptake", 
  BUS_only = "Boiler Upgrade \nScheme only", 
  ECO_only = "Energy Company \nObligation only"
)



ggplot(key_dates) +
  geom_col(
    aes(
      x = factor(nox_conc_quintile),
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      fill = factor(nox_conc_quintile)
    )
  ) +
  geom_text(
    aes(
      x = factor(nox_conc_quintile),
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      label = paste0("£", round(cumulative_damage_cost_avoided_by_quintile / 1000000, 0), "M"), 
      hjust = -0.2
    )
  ) +
  facet_grid(rows = vars(year), cols = vars(model_run), scales = "free_x",   labeller = labeller(model_run = nice_labels, year = c("2030-01-01" = "2030", "2040-01-01"= "2040", "2050-01-01"="2050")),) +
  #facet_wrap(~model_run) +
  scale_fill_viridis_d(name = "NOx Concentration Quintile") +
  coord_flip() +
  scale_y_continuous(limits = c(0,1750), name = "Damage Cost Avoided (£ Millions) since 2025") +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 12))



# How many of these nox conc 5 quintiles are in London? 

london_count_per_quintile <- pc_combined_dataset |> 
  left_join(london_pcs, join_by(PCON25NM == PCON24NM)) |> 
  mutate(london_flag = ifelse(is.na(london_flag), FALSE, TRUE)) |> 
  group_by(london_flag, nox_conc_quintile) |> 
  summarise(count = n())



ggplot(london_count_per_quintile) +
  geom_col(aes(x = nox_conc_quintile, y = count, fill = london_flag)) +
  scale_fill_viridis_d(name = "London vs Rest of England and Wales")
