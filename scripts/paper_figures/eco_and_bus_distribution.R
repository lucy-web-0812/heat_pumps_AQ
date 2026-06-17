
# Plots of just the ECO and BUS distribution


library(tidyverse)
library(readxl)
library(plotly)
library(sf)
library(ggiraph)
library(patchwork)
library(cowplot)



if (!file.exists("data/processed_data/grant_scheme_stats_with_geographies.gpkg")) {

source("scripts/functions/removing_spiel_function.R")

# Nesta stats 
nesta_stats <- read_csv("data/processed_data/nesta_suitability_lsoa.csv")


# BUS stats
bus_stats <- read_csv("data/processed_data/bus_per_pc.csv")

# ECO Stats 
eco_stats <- read_csv("data/processed_data/eco_per_pc.csv")


# LSOA to Parliamentary Constituency look up 

lsoa_to_pc_E_W <- read_csv("data/processed_data/lsoa_2021_to_pc_2024_england_wales.csv") 


# Also worth having the population stats per PC so that we can change the metric to be per 10,000 people etc 


population_stats_per_pc <- read_csv("data/processed_data/pc_population_stats.csv") 


# Geographies needed 

parliamentary_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp") |> 
  select(c(PCON24CD, geometry))


hexmap_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/uk-wpc-hex-constitcode-v5-june-2024.shp") |> 
  select(BCName, CTR_REG, Country, geometry) |> 
  mutate(CTR_REG = ifelse(is.na(CTR_REG) == T, Country, CTR_REG)) |> 
  rename(hexmap_geometry = geometry)


# And combine with scheme stats


grant_scheme_stats_with_geographies <- bus_stats |>
  left_join(eco_stats, join_by(area_codes)) |> 
  rename(heat_pumps_BUS = BUS_heatpumps_per_pc, heat_pumps_ECO = ECO_total_heat_pumps) |> 
  mutate(heat_pumps_BUS_ECO_total = heat_pumps_BUS + heat_pumps_ECO) |> 
  pivot_longer(cols = c(heat_pumps_BUS, heat_pumps_ECO, heat_pumps_BUS_ECO_total), names_to = "scheme", values_to = "heat_pump_installations_per_pc") |> 
  left_join(population_stats_per_pc, join_by(area_codes == PCON25CD)) |> 
  rename(total_population = total) |> 
  mutate(heat_pump_installations_per_pc_per_10000 = heat_pump_installations_per_pc/ total_population * 10000) |> 
  left_join(parliamentary_boundaries, join_by(area_codes == PCON24CD)) |> 
  left_join(hexmap_boundaries, join_by(westminster_parliamentary_constituency == BCName)) |> 
  select(area_codes, westminster_parliamentary_constituency, scheme, heat_pump_installations_per_pc, total_population, heat_pump_installations_per_pc_per_10000, geometry, CTR_REG, Country, hexmap_geometry)


# note that this does not preserve the hexmap geometries... when reading in the gpkg here 
saveRDS(grant_scheme_stats_with_geographies, "data/processed_data/grant_scheme_stats_with_geographies.gpkg")

} else {
  
  grant_scheme_stats_with_geographies <- readRDS("data/processed_data/grant_scheme_stats_with_geographies.gpkg") # Althought a bit of a faff due to the way that st_joins work...
  
}




p <- ggplot(grant_scheme_stats_with_geographies) +
  geom_sf_interactive(aes(fill = heat_pump_installations_per_pc_per_10000, geometry = geometry, 
                          tooltip = paste0(westminster_parliamentary_constituency, 
                                           "<br> Installs per 10,000: ", signif(heat_pump_installations_per_pc_per_10000,3)),  
                          data_id = area_codes), colour = NA) +
  scale_fill_viridis_c(trans = "log10", name = "", labels = c("1", "10", "100"), breaks = c(1,10,100)) +
  facet_wrap(~scheme) +
  theme_void() +
  theme(legend.position = "top") +
  ggtitle("Heat Pump Installations per 10000 people per Parliamentary Constituency")



girafe(ggobj = p)




# If we want to see what areas there are a lot of BUS versus ECO and vice versa.... 


grant_scheme_stats_with_geographies |> 
  select(area_codes, scheme, heat_pump_installations_per_pc_per_10000) |> 
  pivot_wider(names_from = scheme, values_from = heat_pump_installations_per_pc_per_10000) |> 
  ggplot() +
  geom_point(aes(x = heat_pumps_BUS , y = heat_pumps_ECO )) +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p")




# And just getting some summary/stats on comparing the schemes.....


grant_scheme_stats_with_geographies |> 
  select(area_codes, scheme, westminster_parliamentary_constituency, heat_pump_installations_per_pc, heat_pump_installations_per_pc_per_10000) |> 
  arrange(heat_pump_installations_per_pc_per_10000)|> 
  mutate(heat_pump_installations_per_pc_per_10000 = round(heat_pump_installations_per_pc_per_10000, 1)) |> 
  mutate(nation = substr(area_codes,1,1)) |> 
  group_by(scheme, nation) |> # Can toggle on and off the stats for England versus Wales....
  summarise(mean = mean(heat_pump_installations_per_pc), 
            mean_per_10000 = mean(heat_pump_installations_per_pc_per_10000),
            stdev = sd(heat_pump_installations_per_pc),
            stdev_per_10000 = sd(heat_pump_installations_per_pc_per_10000))



grant_scheme_stats_with_geographies |> 
  group_by(scheme) |> 
  summarise(total_hp_installations = sum(heat_pump_installations_per_pc)) 



map_labels <- c(heat_pumps_BUS = "Boiler Upgrade Scheme", heat_pumps_ECO = "Energy Company Obligation")



# Lets add the two schemes together and see what this looks like....


#### ------------- PAPER PLOTS --------------

# BOILER UPGRADE SCHEME 

a <- grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_BUS") |> 
  ggplot() +
  geom_sf(aes(fill = heat_pump_installations_per_pc_per_10000, geometry = geometry), colour = NA) +
  #scale_fill_viridis_c(option = "viridis",
  scico::scale_fill_scico(palette = "oslo", direction = -1,
                          name = "Heat Pump Installations per 10,000 people via the Boiler Upgrade Scheme",
                          #trans = "log1p",
                          breaks = c(0,10,20,30,40,50,60, 70, 80),
                          limits = c(0,80),
                          guide = guide_coloursteps(
                            barwidth = unit(100, "mm"),
                            barheight = unit(5, "mm"),
                            label.position = "bottom",
                            title.position = "top",
                            ticks.colour = "white",
                            ticks.linewidth = 1, #nrow = 1
                          )) +
  theme_void() +
  #ggtitle("Heat Pump Installations per 10,0000 people via the Boiler Upgrade Scheme") +
  theme(legend.position = "top", 
        legend.title = element_text(size = 16, hjust = 0.5),
        legend.text = element_text(size = 14)) 




b <- grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_BUS") |> 
  ggplot() +
  geom_sf(aes(fill = heat_pump_installations_per_pc_per_10000, geometry = hexmap_geometry), colour = NA) +
  #scale_fill_viridis_c(option = "viridis",
  scico::scale_fill_scico(palette = "oslo", direction = -1,
                          name = "Heat Pump Installations per 10,000 people via the Boiler Upgrade Scheme",
                          #trans = "log1p",
                          breaks = c(0,10,20,30,40,50,60, 70, 80),
                          limits = c(0,80),
                          guide = guide_coloursteps(
                            barwidth = unit(100, "mm"),
                            barheight = unit(5, "mm"),
                            label.position = "bottom",
                            title.position = "top",
                            ticks.colour = "white",
                            ticks.linewidth = 1, #nrow = 1,
                          )) +
  theme_void() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 16, hjust = 0.5), 
        legend.text = element_text(size = 14)) 


a + b + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")



ggsave("plots/presentation/BUS_distribution.png", device = "png", dpi = 600)



grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_BUS") |> 
  select(westminster_parliamentary_constituency, heat_pump_installations_per_pc, heat_pump_installations_per_pc_per_10000) |> 
  mutate(heat_pump_installations_per_pc_per_10000 = round(heat_pump_installations_per_pc_per_10000,2)) |> 
  DT::datatable()


# ENERGY COMPANY OBLIGATIOM 

c <- grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_ECO") |> 
  mutate(heat_pump_installations_per_pc_per_10000 = na_if(heat_pump_installations_per_pc_per_10000, 0)) |>
  ggplot() +
  geom_sf(aes(fill = heat_pump_installations_per_pc_per_10000, geometry = geometry), colour = NA) +
  #scale_fill_viridis_c(option = "viridis",
  scico::scale_fill_scico(palette = "acton", direction = -1,
                          name = "Heat Pump Installations per 10,000 people via the Energy Company Obligation",
                          trans = "log1p",
                          na.value = "gray50",
                          limits = c(0.1,275),
                          breaks = c(0.1,5,10,30,50,100,275),
                          labels = c(0.1,5,10,30,50,100,275),
                          guide = guide_coloursteps(
                            barwidth = unit(100, "mm"), 
                            barheight = unit(5, "mm"),
                            label.position = "bottom",
                            title.position = "top",
                            ticks.colour = "white",
                            ticks.linewidth = 1,
                          )) +
  theme_void() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 16, hjust = 0.5), 
        legend.text = element_text(size = 14)) 



d <- grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_ECO") |> 
  mutate(heat_pump_installations_per_pc_per_10000 = na_if(heat_pump_installations_per_pc_per_10000, 0)) |>
  ggplot() +
  geom_sf(aes(fill = heat_pump_installations_per_pc_per_10000, geometry = hexmap_geometry), colour = NA) +
  #scale_fill_viridis_c(option = "viridis",
  scico::scale_fill_scico(palette = "acton", direction = -1,
                          name = "Heat Pump Installations per 10,000 people via the Energy Company Obligation",
                          na.value = "gray50",
                           trans = "log1p",
                          limits = c(0.1,275),
                          breaks = c(0.1,5,10,30,50,100,275),
                          labels = c(0.1,5,10,30,50,100,275),
                          guide = guide_coloursteps(
                            barwidth = unit(100, "mm"), 
                            barheight = unit(5, "mm"),
                            label.position = "bottom",
                            title.position = "top",
                            ticks.colour = "white",
                            ticks.linewidth = 1,
                          )) +
  theme_void() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 16, hjust = 0.5), 
        legend.text = element_text(size = 14)) 



c + d + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")



ggsave("plots/presentation/ECO_distribution.png", device = "png", dpi = 600)


grant_scheme_stats_with_geographies |> 
  filter(scheme == "heat_pumps_ECO") |> 
  select(westminster_parliamentary_constituency, heat_pump_installations_per_pc, heat_pump_installations_per_pc_per_10000) |> 
  mutate(heat_pump_installations_per_pc_per_10000 = round(heat_pump_installations_per_pc_per_10000,2)) |> 
  DT::datatable()


# SUITABILIITY


lsoa_boundaries <- read_sf("data/LSOA_boundaries/boundaries_used_by_NESTA_clean.gpkg")



nesta_stats_with_geom <- nesta_stats |> 
  select(lsoa, ASHP_S_avg_score_weighted, ASHP_N_avg_score_weighted) |> 
  left_join(lsoa_boundaries, join_by(lsoa == lsoacd))

e <- ggplot(nesta_stats_with_geom) +
  geom_sf(aes(geometry = geom, fill = ASHP_S_avg_score_weighted), colour = NA) +
  #scale_fill_viridis_c(name = "",
  scale_fill_gradient(high = "darkred", low = "grey",
                      breaks = c(0.2,0.4, 0.6, 0.8,1), 
                      limits = c(0.2,1),
                      guide = guide_legend(keywidth = unit(10,"mm"), , 
                                           label.position = "bottom")) +
  theme_void()


f <- ggplot(nesta_stats_with_geom) +
  geom_sf(aes(geometry = geom, fill = ASHP_N_avg_score_weighted), colour = NA) +
  #scale_fill_viridis_c(name = "",
  scale_fill_gradient(high = "darkred", low = "grey",
                      breaks = c(0.2,0.4, 0.6, 0.8, 1), 
                      limits = c(0.2,1),
                      guide = guide_legend(keywidth = unit(10,"mm"), 
                                           label.position = "bottom")) +
  theme_void()



e + f + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")




nesta_stats |> 
  pivot_longer(cols = c(ASHP_S_avg_score_weighted, ASHP_N_avg_score_weighted), names_to = "type_of_rating", values_to = "suitability") |> 
  ggplot() +
  ggridges::geom_density_ridges(aes(x = suitability, y= type_of_rating, fill = type_of_rating),  quantile_lines = T) +
  scale_fill_brewer(palette = "Accent")



# STATS on the populations of parliamentary constituencies 

mean_population <- mean(population_stats_per_pc$total_population, na.rm = T)
median_population <- median(population_stats_per_pc$total_population,na.rm = T)
max_population <- max(population_stats_per_pc$total_population)
min_population <- min(population_stats_per_pc$total_population)




parliamentary_constituency_population_plot <- population_stats_per_pc |> 
  ggplot(aes(x = total_population)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "lightgrey", colour = "black")+
  geom_vline(xintercept = mean_population, colour = "darkgreen", linewidth = 1.4, alpha = 0.5, linetype = "dashed")+
  geom_vline(xintercept = median_population, colour = "purple", linewidth = 1.4, alpha = 0.5, linetype = "dashed")+
  geom_density(linewidth = 0.8) +
  scale_y_continuous(name = "Density", expand = c(0,0)) +
  scale_x_continuous(name = "Constituency Population", expand = c(0,0), breaks = seq(60000, 160000, by = 10000)) +
  annotate(geom = "curve", arrow = arrow(length = unit(0.3,"cm")), xend = 106000, x = 110000, yend = 0.00005, y = 0.000049, curvature = -0.2, colour = "darkgreen", linewidth = 0.8) +
  annotate(geom = "curve", arrow = arrow(length = unit(0.3,"cm")), xend = 103000, x = 110000, yend = 0.000042, y = 0.000044, curvature = -0.2, colour = "#9B4FE3", linewidth = 0.8)+
  annotate(geom = "text", label = paste0("Mean population:", round(mean_population, 0)), x = 117000, y = 0.0000495, colour = "darkgreen", size = 5) +
  annotate(geom = "text", label = paste0("Median population:", round(median_population, 0)), x = 117000, y = 0.000044, colour = "#9B4FE3", size = 5) +
  annotate(geom = "text", label = paste0("And Poplar and Limehouse has \nthe largest population -\n", max_population), x = 150000, y = 0.00001, size = 4) +
  annotate(geom = "text", label = paste0("Ynys Môn has \nthe smallest population -\n", min_population), x = 75000, y = 0.00001, size = 4) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
        panel.background = element_rect(fill = "transparent"), 
        axis.text.y = element_blank())


ggsave("plots/potential_paper_outputs/parliamentary_constituency_population.png", device = "png", dpi = 600)



population_stats_per_pc |> 
  group_by(con_code) |> 
  summarise(minimum_population = min(total_population)) |> 
  arrange(minimum_population)



# Now get high definition Parliamentary Boundaries for plots


parliamentary_boundaries_high_definition <- read_sf("data/Parliamentary Boundaries/Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BFC_-1420043245393085943.gpkg")



anglesey_plot <- parliamentary_boundaries_high_definition |> 
  filter(PCON24CD %in% c("W07000112")) |> 
  ggplot() +
  geom_sf(aes(geometry = SHAPE), colour = "darkgrey", fill = "lightgrey") +
  theme_void()



poplar_and_limehouse_plot <- parliamentary_boundaries_high_definition |> 
  filter(PCON24CD == "E14001096") |> 
  ggplot() +
  geom_sf(aes(geometry = SHAPE), colour = "darkgrey", fill = "lightgrey") +
  theme_void()



# and now put these onto the main plot somehow??

parliamentary_constituency_population_plot + patchwork::inset_element(anglesey_plot, left = 0.005, bottom = 0.23, right = 0.15, top = 0.38, on_top = F) +
  patchwork::inset_element(poplar_and_limehouse_plot, left = 0.91, bottom = 0.08, right = 0.93, top = 0.13, on_top = F)



ggsave("plots/potential_paper_outputs/parliamentary_constituency_population.png", device = "png", dpi = 600)

