# Now look at the NOx emissions from 02nonindustrial combustion compared to the heat pump uptake....


library(raster)
library(sf)
library(dplyr)
library(terra)
library(exactextractr)
library(tidyverse)




nox_from_non_indust <- raster("data/raw_data/NAEI_data/02nonindustcombnox23_2023.asc") 


# Need to reproject into BRITISH NATIONAL GRID to match parliamentary boundaries 

crs(nox_from_non_indust) <- projectRaster(
  nox_from_non_indust,
  crs = CRS("EPSG:27700")
)


parliamentary_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp") |> 
  dplyr::select(PCON24CD, geometry)



# Taken from.... 

# Carnell, E.; Tomlinson, S.J.; Reis, S. (2025). UK gridded population at 1 km resolution for 2021 based on Census 2021/2022 and Land Cover Map 2021 NERC EDS Environmental Information Data Centre. https://doi.org/10.5285/7beefde9-c520-4ddf-897a-0167e8918595

# Population raster
pop <- rast("data/raw_data/kilometer_square_data/population_per_1km2/data/uk_residential_population_2021.tif") 

pop[is.na(pop[])] <- 0


# Use exact_extract..... 


summary_stats <- exact_extract(
  nox_from_non_indust,
  parliamentary_boundaries,
  weights = pop,
  fun = c("mean", "min", "max", "median", "stdev", "weighted_mean", "weighted_stdev" , "sum")
) |> as_tibble() 


pcs_with_non_ind_nox <- parliamentary_boundaries |> 
  cbind(summary_stats) 


ggplot(summary_stats) +
  geom_point(aes(x = weighted_mean, y = mean)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")



ggplot(pcs_with_non_ind_nox) +
  geom_sf(aes(fill = weighted_mean, geometry = geometry), colour = NA) +
  scale_fill_continuous(trans = "log10")



# Also want to add in the total emissions over the total PC area....


avg_non_ind_nox_per_pc_2023 <- pcs_with_non_ind_nox |> 
  mutate(area_km2 = units::set_units(st_area(geometry), km^2), 
    total_nox_t_per_km2 = sum / area_km2, 
    total_nox = sum) |> 
  st_drop_geometry() |> 
  units::drop_units() |> 
  rename(mean_nox_emission_per_km2 = weighted_mean, 
         min_nox_emission_per_km2 = min, 
         max_nox_emission = max, 
         median_nox_emission_per_km2 = median, 
         sd_nox_emission_per_km2 = weighted_stdev)








write_csv(avg_non_ind_nox_per_pc_2023, "data/processed_data/avg_non_ind_nox_per_pc_2023.csv")






