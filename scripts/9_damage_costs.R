# Script to assign the correct damage cost category to the parlimentary constituencies 
# Do this by looking at the built up areas...


library(sf)
library(tidyverse)
library(plotly)
library(patchwork)


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
kw_hours_per_year <- 11500 # Taken from Ofgem
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
  ) |>   
  mutate(model_run_label = case_when(
    model_run == "all_three_factors" ~ "BUS, ECO and Suitability", 
    model_run == "BUS_only" ~ "Boiler Upgrade Scheme \n(non-means tested)", 
    model_run == "ECO_only" ~ "Energy Company Obligation\n (means tested)", 
    model_run == "present_day_scenario" ~ "Current trends continue", 
    model_run == "suitability_probability" ~ "Suitability-driven uptake", 
  ))



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


ribbon_df <- summary_by_quintile |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  mutate(model_run_label = case_when(
    model_run == "present_day_scenario" ~ "Current trends continue", 
    model_run == "suitability_probability" ~ "Suitability-driven uptake", 
  )) |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  select(year, model_run, new_ranking_quintile_deprivation, cumulative_damage_cost_avoided_by_quintile, model_run_label) |>
  pivot_wider(
    names_from = new_ranking_quintile_deprivation,
    values_from = cumulative_damage_cost_avoided_by_quintile
  ) |>
  rename(
    least_deprived = `5`,
    most_deprived= `1`
  ) |> 
  mutate(difference = least_deprived - most_deprived)



p1 <- summary_by_quintile |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  mutate(model_run_label = case_when(
    model_run == "present_day_scenario" ~ "Current trends continue", 
    model_run == "suitability_probability" ~ "Suitability-driven uptake", 
  )) |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |>
  ggplot() +
  geom_ribbon(data = ribbon_df, aes(x = year, ymin = least_deprived/ 1000000, ymax = most_deprived/ 1000000), fill = "grey", alpha = 0.2) +
  geom_line(
    aes(
      x = year,
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      colour = factor(new_ranking_quintile_deprivation),
      group = new_ranking_quintile_deprivation
    ),
    linewidth = 1.2
  ) +
  facet_wrap(~ model_run_label) +
  scale_y_continuous(name = "Cumulative damage \ncost avoided (£Millions)", expand = c(0, 0)) +
  scale_colour_manual(
    name = "Relative Deprivation Quintile",
    values = c("#DCA1C2", "#260C3F"),
    labels = c(`1` = "1 Most Deprived", `5` = "5 - Least Deprived")
  ) +
  theme_minimal(18) +
  theme(
    legend.position = "top",
    legend.justification.top = "left",
    legend.text = element_text(size = 18),
    axis.line = element_line()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 4, linewidth = 2)))


p2 <- ggplot(ribbon_df) +
  geom_area(
    aes(
      x = year,
      y = difference/1000000,
      colour = model_run,
      fill = model_run
    ),
    fill = "grey",
    alpha = 0.2, 
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_fill_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "Difference in damage \ncost avoided (£Millions)") +
  facet_wrap( ~ model_run_label) +
  ggtitle("Difference between quintiles (Q5 - Q1)") +
  theme_minimal(18) +
  theme(legend.position = "none", 
        axis.line = element_line()) 




p1 /p2 


ggsave("plots/misc_plots/difference_between_quintiles.png")



# AND Now by NOx conc 


summary_by_quintile_nox <- df_joined |> 
  group_by(model_run,nox_conc_quintile, year) |> 
  summarise(total_dca_by_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_by_quintile = sum(cumulative_damage_cost_avoided))


ribbon_df_nox <- summary_by_quintile_nox |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  select(year, model_run, nox_conc_quintile, cumulative_damage_cost_avoided_by_quintile) |>
  pivot_wider(
    names_from = nox_conc_quintile,
    values_from = cumulative_damage_cost_avoided_by_quintile
  ) |>
  rename(
    most_polluted = `5`,
    least_polluted = `1`
  ) |> 
  mutate(difference = most_polluted - least_polluted )



p5 <- summary_by_quintile_nox |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  filter(nox_conc_quintile %in% c("1", "5")) |>
  ggplot() +
  geom_ribbon(data = ribbon_df_nox, aes(x = year, ymin = most_polluted/ 1000000, ymax = least_polluted/ 1000000), fill = "grey", alpha = 0.2) +
  geom_line(
    aes(
      x = year,
      y = cumulative_damage_cost_avoided_by_quintile / 1000000,
      colour = factor(nox_conc_quintile),
      group = nox_conc_quintile
    ),
    linewidth = 1.2
  ) +
  facet_wrap(~ model_run) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "Cumulative damage cost \navoided (£Millions)", expand = c(0, 0), limits = c(0,1600)) +
  scale_colour_manual(
    name = "NOx Concentration Quintile",
    values = c("#607345FF", "#6C568CFF"),
    labels = c(`1` = "1 Least polluted", `5` = "5 - Most polluted")
  ) +
  theme_minimal(18) +
  theme(
    legend.position = "top",
    legend.justification.top = "left",
    legend.text = element_text(size = 18),
    axis.line = element_line()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 4, linewidth = 2)))


p6 <- ggplot(ribbon_df_nox) +
  geom_area(
    aes(
      x = year,
      y = difference/1000000,
      colour = model_run,
      fill = model_run
    ),
    fill = "grey",
    alpha = 0.2, 
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_fill_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "Difference in damage cost \navoided (£Millions)") +
  facet_wrap( ~ model_run) +
  ggtitle("Difference between quintiles (Q5 - Q1)") +
  theme_minimal(18) +
  theme(legend.position = "none", 
        axis.line = element_line()) 




p5 /p6







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
      hjust = ifelse(cumulative_damage_cost_avoided_by_quintile / 1000000 > 600, 1.1, -0.1),
      colour = ifelse(cumulative_damage_cost_avoided_by_quintile / 1000000 > 600 & new_ranking_quintile_deprivation %in% c(1,2,3), "white", "black"),
    ), 
    size = 6
  ) +
  facet_grid(rows = vars(year), cols = vars(model_run), scales = "free_x",   labeller = labeller(model_run = nice_labels, year = c("2030-01-01" = "2030", "2040-01-01"= "2040", "2050-01-01"="2050")),) +
  #facet_wrap(~model_run) +
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  scale_colour_identity()+
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
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16))




ggsave("plots/paper_plots/deprivation_damage_costs.png", width = 14.209302, height = 12.662791, dpi = 600)



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
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16))


# Lets do a plot of the constituency maps.... 


# And do for NOX


summary_by_quintile <- df_joined |> 
  group_by(model_run,nox_conc_quintile, year) |> 
  summarise(total_dca_by_quintile = sum(total_damage_cost_avoided), 
            cumulative_damage_cost_avoided_by_quintile = sum(cumulative_damage_cost_avoided))



ggplot(summary_by_quintile) +
  geom_col(aes(x = year, y = total_dca_by_quintile, fill = factor(nox_conc_quintile) ), position = "fill") +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  facet_wrap(~model_run)




summary_by_quintile |>
  ggplot() +
  geom_area(
    aes(
      x = year,
      y = cumulative_damage_cost_avoided_by_quintile,
      colour = factor(nox_conc_quintile),
      fill = factor(nox_conc_quintile),
      group = nox_conc_quintile
    ),
    linewidth = 1,
    alpha = 0.6
  ) +
  facet_wrap( ~ model_run) +
  scale_x_date(limits = as_date(c("2025-01-01", "2050-01-01"))) +
  scale_y_continuous(limits = c(0, 6000000000)) +
  scale_colour_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
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
      hjust = ifelse(cumulative_damage_cost_avoided_by_quintile / 1000000 > 600, 1.1, -0.1),
      colour = ifelse(cumulative_damage_cost_avoided_by_quintile / 1000000 > 600 & nox_conc_quintile %in% c(5,4,2,1), "white", "black"),
    ), 
    size = 6
  ) +
  facet_grid(rows = vars(year), cols = vars(model_run), scales = "free_x",   labeller = labeller(model_run = nice_labels, year = c("2030-01-01" = "2030", "2040-01-01"= "2040", "2050-01-01"="2050")),) +
  #facet_wrap(~model_run) +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  scale_colour_identity() +
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
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16))


ggsave("plots/paper_plots/nox_quintile_damage_costs.png", width = 14.209302, height = 10.662791, dpi = 600)




# How many of these nox conc 5 quintiles are in London? 

london_count_per_quintile <- pc_combined_dataset |> 
  left_join(london_pcs, join_by(PCON25NM == PCON24NM)) |> 
  mutate(london_flag = ifelse(is.na(london_flag), FALSE, TRUE)) |> 
  group_by(london_flag, nox_conc_quintile) |> 
  summarise(count = n())



ggplot(london_count_per_quintile) +
  geom_col(aes(x = nox_conc_quintile, y = count, fill = london_flag)) +
  scale_fill_viridis_d(name = "London vs Rest of England and Wales")
