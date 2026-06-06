# Now look at heat pump years and deprivation....

# Producing plots for the different policy scenarios for 2050, separated by quintiles of deprivation and quintiles of NOx concentration. 

library(tidyverse)
library(sf)
library(paletteer)
library(patchwork)


model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")

pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv") |> 
  select(-PCON25NM)


nox_savings_per_boiler_per_year <- (0.056 * 11500) / 1000000000 # Convert from grams to kilotonnnes





pc_dep_model_results_all <- model_results_per_pc |> 
  left_join(pc_combined_dataset, join_by(PCON25CD)) |> 
  mutate(median_imd_decile = as.factor(median_imd_decile)) |> 
  mutate(nox_saving = heat_pump_years * nox_savings_per_boiler_per_year) |> # cumulative emissions savings in kilotonnes
  mutate(model_run_label = case_when(
    model_run == "all_three_factors" ~ "BUS, ECO and Suitability", 
    model_run == "BUS_only" ~ "Boiler Upgrade Scheme \n(non-means tested)", 
    model_run == "ECO_only" ~ "Energy Company Obligation\n (means tested)", 
    model_run == "present_day_scenario" ~ "Current trends continue", 
    model_run == "suitability_probability" ~ "Suitability-driven uptake", 
  ))

pc_dep_model_results <- pc_dep_model_results_all |> 
filter(year == "2050-01-01") 


pc_dep_model_results |> 
  filter(model_run == "present_day_scenario") |> 
  mutate(heat_pumps_per_pop = cumulative_heat_pump_number/total_population_PC * 10000) |> 
  ggplot(aes(x = heat_pumps_per_pop, y = pw_mean_nox)) +
  geom_point() +
  geom_smooth(method = "lm") 



p <- pc_dep_model_results_all |> 
  filter(year == "2023-01-01") |> 
  mutate(heat_pumps_per_pop = total_heat_pumps_per_pc /total_population_PC * 10000) |> 
  ggplot(aes(x = heat_pumps_per_pop, y = pw_mean_nox)) +
  geom_pointrange(aes(ymin= min_nox, ymax = max_nox),
                  fill = "white",
                  shape = 22,
                  colour = "#004C1F",
                  size = 0.8
  ) +
  scale_y_continuous(breaks = seq(0, 65, 10),  name = expression("Annual mean modelled NO" [x] * " concentration - " * mu * "g m"^{-3}, limits = c(0,65))
  ) +
  scale_x_continuous(
    name = "Heat Pumps per 10,000 people",
    trans = "log10",
    expand = c(0, 0),
    limits = c(1, 400),
    minor_breaks = rep(1:9, times = 5) * 10^(0:4),
    breaks = c(1, 5, 10, 50, 100, 200, 300, 500)
  ) +
  theme_classic() +
  annotation_logticks(sides = "b") +
  theme(
    panel.grid.minor.x = element_line("lightgrey", linewidth = 0.2),
    panel.grid.major.x = element_line("lightgrey", linewidth = 0.2),
    panel.grid.major.y = element_line("lightgrey")
  ) 



# Make a explanatory graphic
explainer <- ggplot() +
  geom_pointrange(
    aes(x = 1, y = 10, ymin = 8, ymax = 12),
    shape = 22, fill = "white", colour = "#004C1F", size = 2, linewidth = 1.2,
  ) +
  scale_x_continuous(limits = c(0.75,12.5)) +
  geom_segment(aes(x = 2, y = 8, xend = 1.1, yend = 8),  arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 2, y = 10, xend = 1.5, yend = 10),  arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 2, y = 12, xend = 1.1, yend = 12),  arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = 2.15, y = 10, label = "Mean NOx \nacross constituency", hjust = 0, size = 3.5) +
  annotate("text", x = 2.15, y = 12, label = "Max NOx", hjust = 0, size = 3.5) +
  annotate("text", x = 2.15, y = 8, label = "Min NOx", hjust = 0, size = 3.5) +
  theme_void()


cowplot::ggdraw(p) +
  cowplot::draw_plot(explainer, x = 0.82, y = 0.77, width = 0.35, height = 0.21)

ggsave("plots/paper_plots/nox_by_pc.png")





# Just to see the mapping from using median imd decile to the new quintiles

pc_dep_model_results |> 
  group_by(median_imd_decile, new_ranking_quintile_deprivation) |> 
  summarise(count = n()) |> 
  ggplot() +
  geom_tile(aes(x = median_imd_decile, y = new_ranking_quintile_deprivation, fill = count)) +
  geom_text(aes(x = median_imd_decile, y = new_ranking_quintile_deprivation, label = count)) +
  scale_fill_viridis_c()




nice_labels <- c(
  present_day_scenario    = "Current trends persist",
  suitability_probability = "Suitability-driven uptake", 
  BUS_only = "Boiler Upgrade Scheme only", 
  ECO_only = "Energy Company Obligation only"
)




# And what is the total nox saved per quintile? 

pct_savings_by_quintile <- pc_dep_model_results |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")) |> 
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
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  theme_classic(base_size = 16, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        axis.text = element_text(size = 12)) +
  labs(subtitle = "Where quintile 1 represents 20% of the population living in the most-deprived constituencies") +
  ggtitle("Distribution of NOx emissions savings over the period 2025 - 2050 \nfrom installing heat pumps by relative deprivation quintile")



ggsave("plots/paper_plots/nox_savings_vs_imd_percentage.png", width = 12.209302, height = 16.662791, dpi = 600)



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
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "NOx Emissions Savings 2025-2050", limits = c(0,59), expand = c(0,0)) +
  #khroma::scale_fill_mediumcontrast(name = "Relative Deprivation Quintile")+
  scico::scale_fill_scico_d(palette = "acton", name = "Relative Deprivation Quintile") +
  theme_classic(base_size = 20, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 24, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        axis.text = element_text(size = 24)) +
 labs(subtitle = "Where quintile 1 represents 20% of the population living in the most-deprived constituencies") +
  ggtitle("Distribution of NOx emissions savings over the period 2025 - 2050 \nfrom installing heat pumps by relative deprivation quintile")




ggsave("plots/paper_plots/nox_savings_vs_imd.png", width = 12.209302, height = 12.662791, dpi = 600)




# By pollution levels too.... # based on the concentration of nox as no2
  
  
by_nox_conc_quintiles <- pc_dep_model_results |> 
    filter(model_run %in% c("present_day_scenario", "suitability_probability","BUS_only", "ECO_only")) |>
  group_by(nox_conc_quintile, model_run) |> 
  summarise(nox_savings_per_conc_quintile = sum(nox_saving), count = n()) |> 
  group_by(model_run) |> 
  mutate(percentage = nox_savings_per_conc_quintile / sum(nox_savings_per_conc_quintile) * 100)  
  

ggplot(by_nox_conc_quintiles) +
  geom_col(aes(x = nox_conc_quintile, y = percentage, fill = as.factor(nox_conc_quintile)), alpha = 0.8) +
  geom_text(
    aes(x = nox_conc_quintile, 
        y = percentage,
        label = paste0(round(percentage, 1), "%")), 
    hjust = -0.1,  
    size = 3.5    
  ) +
  coord_flip() +
  facet_wrap(~model_run, scales = "free", labeller = as_labeller(nice_labels))+
  scale_x_continuous(name = "Relative Pollution Quintile where 5 are most \npolluted 20% of parliamentary consituencies") +
  scale_y_continuous(name = "Percentage of NOx Emissions Savings", expand = c(0,0), limits = c(0,32), breaks = seq(0,32,5)) +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  #scale_fill_viridis_d(name = "NOx Concentration Quintile") +
  theme_classic(base_size = 16, base_family = "sans") +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        axis.text = element_text(size = 12)) +
  ggtitle("Proportion of NOx emissions savings from installing heat pumps \nby relative NOx concentration quintile")


ggsave("plots/paper_plots/nox_savings_vs_nox_conc_quintile_pct.png", width = 12.209302, height = 12.662791, dpi = 600)

                                # And absolute values

ggplot(by_nox_conc_quintiles) +
  geom_col(aes(x = nox_conc_quintile, y = nox_savings_per_conc_quintile, fill = as.factor(nox_conc_quintile)), alpha = 0.8) +
  geom_text(
    aes(x = nox_conc_quintile, 
        y = nox_savings_per_conc_quintile,
        label = paste0(round(nox_savings_per_conc_quintile, 1), "kt")), 
    hjust = -0.1,  
    size = 5    
  ) +
  coord_flip() +
  facet_wrap(~model_run, scales = "free", labeller = as_labeller(nice_labels))+
  scale_x_continuous(name = "NOx Concentration Quintile") +
  scale_y_continuous(name = "NOx Emissions Savings 2025-2050", expand = c(0,0), limits = c(0,69), breaks = seq(0,69,10)) +
  scale_fill_paletteer_d("calecopal::lupinus", name = "NOx Concentration Quintile", direction = -1) +
  #scale_fill_viridis_d(name = "NOx Concentration Quintile") +
  theme_classic(base_size = 16) +
  theme(panel.spacing = unit(1,"cm"), 
        panel.grid.major.x = element_line(colour = "lightgrey"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left", 
        axis.text = element_text(size = 12)) +
  labs(subtitle = "Where quintile 1 represents 20% of the population living in the least-polluted constituencies") +
  ggtitle("Distribution of NOx emissions savings over the period 2025 - 2050 \nfrom installing heat pumps by relative NOx concentration quintile")



ggsave("plots/paper_plots/nox_savings_vs_nox_conc_quintile.png", width = 12.209302, height = 12.662791, dpi = 600)





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

ggsave("plots/paper_plots/dep_quintile_map.png", width = 6.209302, height = 6.662791, dpi = 600)


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


ggsave("plots/paper_plots/nox_quintile_map.png", width = 6.209302, height = 6.662791, dpi = 600)





# And what about the changes through time.....


timeseries_nox <- pc_dep_model_results_all |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |> 
  group_by(nox_conc_quintile, year,  model_run_label) |> 
  summarise(total_nox = sum(nox_saving)) 


# Creating the df of the difference... 

ribbon_df <- timeseries_nox |> 
  filter(nox_conc_quintile %in% c("1", "5")) |> 
  select(year, model_run_label, nox_conc_quintile, total_nox) |>
  pivot_wider(
    names_from = nox_conc_quintile,
    values_from = total_nox
  ) |>
  rename(
    least_polluted = `1`,
    most_polluted = `5`
  ) |> 
  mutate(difference = least_polluted - most_polluted)


p1 <- timeseries_nox |>
  filter(nox_conc_quintile %in% c("1", "5")) |>
  ggplot() +
  geom_ribbon(
    data = ribbon_df,
    aes(x = year, ymin = most_polluted, ymax = least_polluted),
    fill = "grey",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      x = year,
      y = total_nox,
      colour = factor(nox_conc_quintile),
      group = nox_conc_quintile
    ),
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#607345FF", "#6C568CFF"), name = "NOx Concentration Quintile", labels = c(`1`= "1 Least Polluted", `5` = "5 - Most Polluted")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "NOx (killotonnes)", expand = c(0,0)) +
  
  ggtitle("Cumulative saving by nox concentration quintile") +
  facet_wrap( ~ model_run_label) +
  theme_minimal(18) +
  theme(legend.position = "top", 
        legend.justification.top = "left",
        legend.text = element_text(size = 18),
        axis.line = element_line()) +
  guides(colour = guide_legend(override.aes = list(size = 10, linewidth = 2)))



p2 <- ggplot(ribbon_df) +
  geom_area(
    aes(
      x = year,
      y = difference,
      colour = model_run_label,
      fill = model_run_label
    ),
    fill = "grey",
    alpha = 0.2, 
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_fill_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "Difference in NOx (killotonnes)",
                     expand = c(0, 0),
                     limits = c(NA, 30)) +
  facet_wrap( ~ model_run_label) +
  ggtitle("Difference between quintiles (Q1 - Q5)") +
  theme_minimal(18) +
  theme(legend.position = "none", 
        axis.line = element_line()) 




p1 / p2


ggsave("plots/misc_plots/difference_over_time_nox.png")




timeseries_dep <- pc_dep_model_results_all |> 
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |> 
  group_by(new_ranking_quintile_deprivation, year,  model_run_label) |> 
  summarise(total_nox = sum(nox_saving)) 





ribbon_df_dep <- timeseries_dep |> 
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |> 
  select(year, model_run_label, new_ranking_quintile_deprivation, total_nox) |>
  pivot_wider(
    names_from = new_ranking_quintile_deprivation,
    values_from = total_nox
  ) |>
  rename(
    least_deprived = `5`,
    most_deprived = `1`
  ) |> 
  mutate(difference = least_deprived - most_deprived)



p3 <- timeseries_dep |>
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |>
  ggplot() +
  #geom_point(aes(x = year, y = total_nox, colour = factor(nox_conc_quintile), group = nox_conc_quintile, linetype = factor(nox_conc_quintile))) +
  geom_ribbon(
    data = ribbon_df_dep,
    aes(x = year, ymin = most_deprived, ymax = least_deprived),
    fill = "grey",
    alpha = 0.2
  ) +
  geom_line(
    aes(
      x = year,
      y = total_nox,
      colour = factor(new_ranking_quintile_deprivation),
      group = new_ranking_quintile_deprivation
    ),
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#260C3F", "#DCA1C2"), name = "Deprivation Quintile", labels = c(`1`= "1 Most Deprived", `5` = "5 - Least Deprived")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "NOx savings (killotonnes)", expand = c(0,0),limits = c(0,50)) +
  
  ggtitle("Cumulative saving by deprivation quintile") +
  facet_wrap( ~ model_run_label) +
  theme_minimal(18) +
  theme(legend.position = "top", 
        legend.justification.top = "left",
        legend.text = element_text(size = 18),
        axis.line = element_line()) +
  guides(colour = guide_legend(override.aes = list(size = 10, linewidth = 2)))





p4 <- ggplot(ribbon_df_dep) +
  geom_area(
    aes(
      x = year,
      y = difference,
      colour = model_run_label,
      fill = model_run_label
    ),
    fill = "grey",
    alpha = 0.2, 
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_fill_manual(values = c("#7F7B82", "#DC6BAD")) +
  scale_x_date(name = "Year") +
  scale_y_continuous(name = "Difference in NOx (killotonnes)",
                     expand = c(0, 0),
                     limits = c(NA, 25)) +
  facet_wrap( ~ model_run_label) +
  ggtitle("Difference between quintiles (Q5 - Q1)") +
  theme_minimal(18) +
  theme(legend.position = "none", 
        axis.line = element_line()) 


p3/p4




ggsave("plots/misc_plots/difference_over_time_dep.png")


# And what about just the number of heat pumps per year per model...



model_names <- c("present_day_scenario" = "Current trends continue",
                    "suitability_probability" = "Suitability-driven uptake" )



pc_dep_model_results_all |>
  group_by(nox_conc_quintile, model_run, year) |>
  summarise(
    total_hp_per_quintile = sum(heat_pump_number),
    total_hp_lower = sum(heat_pump_number_lower_bound),
    total_hp_upper = sum(heat_pump_number_upper_bound)
  ) |>
  ungroup() |>
  filter(model_run %in% c("suitability_probability", "present_day_scenario")) |>
  filter(nox_conc_quintile %in% c("1", "5")) |>
  
  ggplot() +
  geom_line(
    aes(
      x = year,
      y = total_hp_per_quintile / 1000,
      colour = factor(nox_conc_quintile),
      group = nox_conc_quintile
    ), 
    linewidth = 1.2
  ) +
  scale_colour_manual(
    name = "NOx Concentration Quintile",
    values = c("#607345FF", "#6C568CFF"),
    labels = c(`1` = "1 Least polluted", `5` = "5 - Most polluted")
  ) +
  scale_fill_manual(
    name = "NOx Concentration Quintile",
    values = c("#607345FF", "#6C568CFF"),
    labels = c(`1` = "1 Least polluted", `5` = "5 - Most polluted")
  ) +
  scale_x_datetime(name = "Year", limits = c(as.POSIXct("2020-01-01"), NA)) +
  scale_y_continuous(name = "Heat pump installations (thousands)",
                     expand = c(0, 0),
                     limits = c(0, 500)) +
  facet_wrap( ~ model_run, labeller = as_labeller(model_names)) +
  theme_minimal(18) +
  theme(legend.position = "top", axis.line = element_line())







pc_dep_model_results_all |>
  group_by(new_ranking_quintile_deprivation, model_run, year) |>
  summarise(
    total_hp_per_quintile = sum(heat_pump_number),
    total_hp_lower = sum(heat_pump_number_lower_bound),
    total_hp_upper = sum(heat_pump_number_upper_bound)
  ) |>
  ungroup() |>
  filter(model_run %in% c("suitability_probability", "present_day_scenario")) |>
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |>
  
  ggplot() +
  # geom_point(
  #   aes(
  #     x = year,
  #     y = total_hp_per_quintile / 1000,
  #     colour = factor(nox_conc_quintile),
  #     group = nox_conc_quintile
  #   ),
  #   size = 0.8
  # ) +
  geom_line(
    aes(
      x = year,
      y = total_hp_per_quintile / 1000,
      colour = factor(new_ranking_quintile_deprivation),
      group = new_ranking_quintile_deprivation
    ), 
    linewidth = 1.2
  ) +
  # geom_ribbon(
  #   aes(
  #     x = year,
  #     ymin = total_hp_lower / 1000,
  #     ymax = total_hp_upper / 1000,
  #     fill = factor(new_ranking_quintile_deprivation)
  #   ),
  #   alpha = 0.3
  # ) +
  scale_colour_manual(
    name = "Relative Deprivation Quintile",
    values = c("#260C3F", "#DCA1C2"),
    labels = c(`1` = "1 Most deprived", `5` = "5 - Least deprived")
  ) +
  scale_fill_manual(
    name = "Relative Deprivation Quintile",
    values = c("#260C3F", "#DCA1C2"),
    labels = c(`1` = "1 Most deprived", `5` = "5 - Least deprived")
  ) +
  scale_x_datetime(name = "Year", limits = c(as.POSIXct("2020-01-01"), NA)) +
  scale_y_continuous(name = "Heat pump installations (thousands)",
                     expand = c(0, 0),
                     limits = c(0, 500)) +
  facet_wrap( ~ model_run, labeller = as_labeller(model_names)) +
  theme_minimal(18) +
  theme(legend.position = "top", axis.line = element_line())

