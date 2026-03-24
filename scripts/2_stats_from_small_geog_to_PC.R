# There are several statistics reported on an LSOA scale that need to be converted to on a parliamentary constituency basis... 

library(tidyverse)
library(sf)
library(terra)
library(IMD)



##############################
# 1. Heat pump Suitability 
##############################

nesta_suitability <- read_csv("data/processed_data/nesta_suitability_lsoa.csv")

# Parliamentary constituencies....

parliamentary_constituencies <- read_csv("data/processed_data/lsoa_2021_to_pc_2024_england_wales.csv")


lsoa_populations <- read_csv("data/processed_data/lsoa_population_stats_households.csv")


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
  
  # 4.Map 
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





##############################
# 3. Deprivation
##############################
 

# Deprivation is reported for the 2011 LSOAs, however we can map this to the 2021 geometries and further aggregate to the 2024 Parliamentary Constituency boundaries.
# We need to weight the statistics according to the population..... 

# Use the MySociety IMD 
dep_by_lsoa <- IMD::load_composite_imd(nation = "E")


lsoa_to_pc <- read_csv("data/processed_data/lsoa_2021_to_pc_2024_england_wales.csv")


lsoa_pop <- read_csv("data/processed_data/lsoa_population_stats.csv")


lsoa_2011_to_lsoa_2021 <- read_csv("data/raw_data/LSOAs/LSOA11_LSOA21_LAD22_EW_LU_v2_-9218941858449512980.csv") |> 
  select(LSOA11CD, LSOA21CD)



df_joined <- lsoa_to_pc |> 
  left_join(lsoa_pop) |> 
  left_join(lsoa_2011_to_lsoa_2021, join_by(LSOA21CD)) |> 
  left_join(dep_by_lsoa, join_by(LSOA11CD == lsoa)) |> 
  filter(is.na(LSOA11CD) == F)




non_weighted_PC_stats <- df_joined |> 
  group_by(PCON25CD) |> 
  summarise(total_population_PC = sum(total_population), 
            mean_imd_ranking = mean(UK_IMD_E_rank), 
            median_imd_decile = round(median(E_expanded_decile), 0), 
            total_number_of_LSOAs = n(), 
            number_of_LSOAs_with_decile_equal_1 = sum(E_expanded_decile == 1), 
            pct_decile_equal_1 = (number_of_LSOAs_with_decile_equal_1/total_number_of_LSOAs) * 100,
            number_of_LSOAs_with_decile_equal_10 = sum(E_expanded_decile == 10), 
            pct_decile_equal_10 = (number_of_LSOAs_with_decile_equal_10/total_number_of_LSOAs) * 100,)



# Now need to have the population weighted stats.... 


PC_total_population <- df_joined |> 
  group_by(PCON25CD) |> 
  summarise(total_population_PC = sum(total_population))

df_joined_with_weightings <- df_joined |> 
  left_join(PC_total_population, join_by(PCON25CD)) |> 
  mutate(relative_weighting = total_population/ total_population_PC)


# Now checking that weightings add to roughly 1..... 


qa_check_weighting <- df_joined_with_weightings |> 
  group_by(PCON25CD) |> 
  summarise(total_weighting = sum(relative_weighting))


# Now producing stats weighted by population.... 

pop_weighted_PC_stats <- df_joined_with_weightings |> 
  group_by(PCON25CD, PCON25NM) |> 
  summarise(total_population_PC = sum(total_population), 
            mean_imd_ranking = sum(UK_IMD_E_rank * relative_weighting), 
            mean_imd_ranking_package = stats::weighted.mean(UK_IMD_E_rank, total_population),  # both methods agree ! 
            median_imd_lsoa_ranking = round(matrixStats::weightedMedian(UK_IMD_E_rank, total_population), 0), 
            median_imd_decile = round(matrixStats::weightedMedian(E_expanded_decile, total_population), 0) , #using the MatrixStats package for weighted median
            total_number_of_LSOAs = n(), 
            number_of_LSOAs_with_decile_equal_1 = sum(E_expanded_decile == 1), 
            pct_decile_equal_1 = (number_of_LSOAs_with_decile_equal_1/total_number_of_LSOAs) * 100,
            number_of_LSOAs_with_decile_equal_10 = sum(E_expanded_decile == 10), 
            pct_decile_equal_10 = (number_of_LSOAs_with_decile_equal_10/total_number_of_LSOAs) * 100,)


# Now we have our new ranking..... 

new_ranked_PCs_pop_weighted <- pop_weighted_PC_stats |> 
  arrange(median_imd_lsoa_ranking) |> 
  ungroup() |> 
  mutate(new_ranking = row_number()) |> 
  select(-mean_imd_ranking_package)


write_csv(new_ranked_PCs_pop_weighted, file = "data/processed_data/dep_ranked_PCs_pop_weighted.csv")

