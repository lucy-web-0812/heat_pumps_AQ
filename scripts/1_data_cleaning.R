# Lets have one central data cleaning and processing script for all the stats....

library(tidyverse)
library(sf)
library(readxl)

# LAST UPDATED....

# 19.03.2026 with the January BUS and February ECO statistics


source("scripts/functions/removing_spiel_function.R")


# Boiler Upgrade Scheme Statistics

bus_per_pc <- spiel_remover(
  "data/raw_data/BUS_stats/Boiler_Upgrade_Scheme_BUS_Statistics_January_2026.xlsx",
  sheet = "Q1.3"
) |>
  filter(is.na(westminster_parliamentary_constituency) == FALSE) |>
  mutate(
    westminster_parliamentary_constituency = ifelse(
      westminster_parliamentary_constituency == "Weston-Super-Mare",
      # changing the name of weston-super-mare so that it matches other datasets for later joins..... (although join by ONS code where possible)
      "Weston-super-Mare",
      westminster_parliamentary_constituency
    )
  ) |>
  mutate(BUS_heatpumps_per_pc = as.numeric(heat_pump_technologies_note_24)) |>
  # Getting rid of extra columns not needed...
  select(
    -c(
      country_or_region,
      biomass_boilers,
      heat_pump_technologies_note_24
    )
  )


write_csv(bus_per_pc, "data/processed_data/bus_per_pc.csv")



# Energy Company Obligation Statistics

eco_per_pc_raw  <- spiel_remover("data/raw_data/ECO_stats/Headline_HEE_tables_February_2026.xlsx",
                                 sheet = "T3.7")


eco_per_pc <- eco_per_pc_raw |>
 filter(is.na(air_source_heat_pumps) == F) |> # This just removes few extra rows which contains notes
  select(c(
    area_codes,
    air_source_heat_pumps,
    ground_source_heat_pumps,
    hybrid_heat_pumps
  )) |>
  mutate(across(
    c(
      air_source_heat_pumps,
      ground_source_heat_pumps,
      hybrid_heat_pumps
    ),
    ~ case_when(
      . == "#" ~ 3,
      # <- ACTUALLY ANYWHERE BETWEEM 1-4
      . == "^" ~ 6,
      # ^ <- ABOVE 5
      TRUE ~ as.numeric(.)
    )
  )) |>
  mutate(ECO_total_heat_pumps = air_source_heat_pumps + ground_source_heat_pumps +
           hybrid_heat_pumps)

write_csv(eco_per_pc, "data/processed_data/eco_per_pc.csv")






# ----- NESTA STATISTICS -------



nesta_suitability_lsoa <- read_xlsx(
  "data/raw_data/NESTA_stats/20250319_2023_Q4_EPC_heat_pump_suitability_per_lsoa.xlsx",
  sheet = "Data"
) |> 
  mutate(census_proportion_flats = as.numeric(census_proportion_flats)) |> 
  mutate(combined_heat_pump_suitability = ASHP_N_avg_score_weighted + GSHP_N_avg_score_weighted)


write_csv(nesta_suitability_lsoa, "data/processed_data/nesta_suitability_lsoa.csv")



# LSOAs to Parliamentary Constituencies 


lsoa_2021_to_pc_2024_england_wales <- read_csv(
  "data/raw_data/parliamentary_constituencies/LSOA_(2021)_to_future_Parliamentary_Constituencies_Lookup_in_England_and_Wales.csv"
) |>
  select(LSOA21CD, PCON25CD, PCON25NM)


write_csv(lsoa_2021_to_pc_2024_england_wales, "data/processed_data/lsoa_2021_to_pc_2024_england_wales.csv")




# LSOA populations 


lsoa_population_stats <- read_csv("data/raw_data/LSOAs/household_per_LSOA_estimates.csv") |> # THIS DOES NOT INCLUDE SCOTLAND! Just England and Wales
  select(c(`Lower layer Super Output Areas Code`, Observation)) |>
  rename(LSOA21CD = `Lower layer Super Output Areas Code`, number_of_households = Observation)


write_csv(lsoa_population_stats, "data/processed_data/lsoa_population_stats.csv")




# PC populations 


pc_population_stats <- spiel_remover("data/raw_data/parliamentary_constituencies/pc_pop.xlsx", sheet = "Mid-2022 PCON 2025") |> 
  select(c(pcon_2025_code,pcon_2025_name, total)) |> 
  rename(PCON25CD = pcon_2025_code, PCON25NM = pcon_2025_name )



write_csv(pc_population_stats, "data/processed_data/pc_population_stats.csv")



# PC Number of households 

pc_number_of_households <- read_xlsx("data/raw_data/parliamentary_constituencies/households_census.xlsx", sheet = "Constituency data") |> 
  rename(PCON25CD = ONSConstID, PCON25NM = ConstituencyName ) |> 
  group_by(PCON25CD, PCON25NM) |> 
  summarise(households_per_pc = sum(Con_num, na.rm = T)) |> 
  mutate(country_indicator = substr(PCON25CD, 1,1)) |> 
  filter(country_indicator %in% c("E", "W")) |> 
  select(-country_indicator)



write_csv(pc_number_of_households, "data/processed_data/pc_number_of_households.csv")


