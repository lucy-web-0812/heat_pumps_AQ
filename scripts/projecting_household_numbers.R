# Household projections 

library(tidyverse)


source("scripts/functions/removing_spiel_function.R")

household_projections_2022_based <- spiel_remover("data/raw_data/household_stats/2022-based HHPs Migration category variant (2023 geog).xlsx", sheet = "Table 406") |> 
  filter(area_name == "England") |> 
  pivot_longer(cols = c(x2022:x2047), names_to = "year", values_to = "household_number") |> 
  mutate(year = as.numeric(substr(year,2,5))) 


# Calculate actual growth rates....

growth_rates <- household_projections_2022_based |>
  mutate(
    growth = (household_number / lag(household_number)) - 1
  )


# And then also get the mean over the last 10 years to be able to extend out to 2050 

mean_growth <- growth_rates |>
  filter(year >= 2037) |>
  summarise(mean_growth = mean(growth, na.rm = TRUE)) |>
  pull(mean_growth)


# future data taking this mean growth...

future_growth <- tibble(
  year = 2048:2050,
  growth = mean_growth
)

# combine the datasets.....

increase_per_year <- growth_rates |>
  select(year, growth) |>
  bind_rows(future_growth) |>
  arrange(year) |> 
  mutate(growth = replace_na(growth, 0), 
         growth_index = cumprod(1 + growth))




# PC-level projections
pc_population_stats <- read_csv("data/processed_data/pc_number_of_households.csv")

household_changes_pc <- pc_population_stats |>
  crossing(increase_per_year) |>
  mutate(
    households_per_pc = round(households_per_pc * growth_index, 0)
  ) |> 
  select(PCON25CD, year, households_per_pc)


write_csv(household_changes_pc, "data/processed_data/household_number_increases_per_pc.csv")



# But lets also save as just whole of England and Wales..... 



household_growth_per_year <- household_changes_pc |> 
  group_by(year) |> 
  summarise(number_of_households = sum(households_per_pc)) |> 
  left_join(increase_per_year) |> 
  select(-growth)


write_csv(household_growth_per_year, "data/processed_data/household_number_increases_total.csv")


