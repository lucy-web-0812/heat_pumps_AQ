# There are several statistics reported on an LSOA scale that need to be converted to on a parliamentary constituency basis... 

library(tidyverse)
library(sf)
library(terra)



##############################
# 1. Heat pump Suitability 
##############################

nesta_suitability <- read_csv("data/processed_data/nesta_suitability_lsoa.csv")

# Parliamentary constituencies....

parliamentary_constituencies <- read_csv("data/processed_data/lsoa_2021_to_pc_2024_england_wales.csv")


lsoa_populations <- read_csv("data/processed_data/lsoa_population_stats.csv")


# Getting a list of the LSOAs that are in England and Wales (excluding Scotland)

lsoas_of_interest <- unique(parliamentary_constituencies$LSOA21CD) 


LSOA_with_heatpump_suitability  <- nesta_suitability |>  
  filter(lsoa %in% lsoas_of_interest) |> # just filter for the england and wales lsoas
  distinct(lsoa, .keep_all = T) |> # Just ensuring no duplicate entries
  select(lsoa,
         lsoa_name,
         ASHP_N_avg_score_weighted, # Keeping what are interested in 
         GSHP_N_avg_score_weighted) |>
  mutate(combined_heat_pump_suitability = ASHP_N_avg_score_weighted + GSHP_N_avg_score_weighted)  |> # combined scores... equally weighted....
  select(-c(ASHP_N_avg_score_weighted, GSHP_N_avg_score_weighted)) # Get rid of extras


# Should we be doing this as a population weighted mean? 


heatpump_suitability_per_pc <- LSOA_with_heatpump_suitability |> 
  left_join(parliamentary_constituencies, join_by(lsoa == LSOA21CD)) |> 
  left_join(lsoa_populations, join_by(lsoa == LSOA21CD)) |> 
  group_by(PCON25CD, PCON25NM) |> 
  summarise(combined_heat_pump_suitability = weighted.mean(combined_heat_pump_suitability, w = number_of_households, na.rm = T))



write_csv(heatpump_suitability_per_pc, "data/processed_data/nesta_suitability_by_pc.csv")



##############################
# 2. Air Pollutants
##############################

# Here is the data to take the modelled annual concentration estimates from Defra and turn these into estimates for the whole constituency....
# This is functionalised so it is possible to take any csv file of pollutants and turn this into constituency averages, a plot should also be produced.....


parliamentary_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp") |> 
  select(PCON24CD, geometry)


# Taken from.... 

# Carnell, E.; Tomlinson, S.J.; Reis, S. (2025). UK gridded population at 1 km resolution for 2021 based on Census 2021/2022 and Land Cover Map 2021 NERC EDS Environmental Information Data Centre. https://doi.org/10.5285/7beefde9-c520-4ddf-897a-0167e8918595

# Population raster
pop <- rast("data/raw_data/kilometer_square_data/population_per_1km2/data/uk_residential_population_2021.tif") 



# Lets convert this into a function...... 


process_pollutant <- function(file_name,
                              pollutant_name, # What is the pollutant called? 
                              year_measured,
                              boundary_sf = parliamentary_boundaries,
                              crs = 27700, # British national grid is the preset
                              plot = TRUE) {
  
  # 1. Read and clean pollutant grid 
  df <- read_csv(paste0("data/raw_data/air_quality/concs/", file_name)) |>
    rename(
      gridcode = !!pollutant_name,
      x = `...2`,
      y = `...3`,
      value = `...4`
    ) |>
    filter(!if_any(x:y, is.na)) |>
    st_as_sf(coords = c("x","y"), crs = crs)
  
  # 🔹 Extract population (1 km² grid)
  df$population <- terra::extract(pop, vect(df))[, 2]
  
  
  # 2. Look at the intersection between these 
  
  intersected <- st_intersection(df, boundary_sf)
  
  stats <- intersected |>
    group_by(PCON24CD) |>
    summarise(
      !!paste0("max_", pollutant_name) := max(value, na.rm = TRUE),
      !!paste0("min_", pollutant_name) := min(value, na.rm = TRUE),
      !!paste0("mean_", pollutant_name) := mean(value, na.rm = TRUE),
      !!paste0("median_", pollutant_name) := median(value, na.rm = TRUE),
      !!paste0("sd_", pollutant_name) := sd(value, na.rm = TRUE),
      
      !!paste0("pw_mean_", pollutant_name) :=
        sum(value * population, na.rm = TRUE) /
        sum(population, na.rm = TRUE)
    ) |>
    st_drop_geometry() |>
    left_join(boundary_sf, by = "PCON24CD")
  
  # 4. Optional map 
  if (plot) {
    p <- ggplot(stats) +
      geom_sf(aes(fill = !!rlang::sym(paste0("pw_mean_", pollutant_name)), geometry = geometry), colour = NA) +
      scale_fill_viridis_c(name = paste0("Population Weighted Mean ", pollutant_name)) +
      labs(
        title = paste("Mean", toupper(pollutant_name), "concentration by constituency in", year_measured)
      ) +
      theme_void()
    
    print(p)
  }
  
  # 5. Return cleaned stats 
  return(stats)
}



pm10_stats <- process_pollutant(file_name = "mappm102024g.csv", pollutant_name = "pm10", year_measured = "2024")
pm2.5_stats <- process_pollutant(file_name = "mappm252024g.csv", pollutant_name = "pm2.5", year_measured = "2024")
no2_stats <- process_pollutant(file_name = "mapno22024.csv", pollutant_name = "no2", year_measured = "2024")
nox_stats <- process_pollutant(file_name = "mapnox2024.csv", pollutant_name = "nox", year_measured = "2024")

write_csv(pm10_stats, "data/processed_data/pm10_stats_2024_per_pc.csv")
write_csv(pm2.5_stats, "data/processed_data/pm_25_stats_2024_per_pc.csv")
write_csv(no2_stats, "data/processed_data/no2_stats_2024_per_pc.csv")
write_csv(nox_stats, "data/processed_data/nox_stats_2024_per_pc.csv")
