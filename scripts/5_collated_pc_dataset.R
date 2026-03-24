# Building a file that has all the information about a parliamentary constituency... 


library(tidyverse)
library(sf)
library(arrow)


# PC names and codes.....

parliamentary_constituencies <- read_csv(
  "data/raw_data/parliamentary_constituencies/LSOA_(2021)_to_future_Parliamentary_Constituencies_Lookup_in_England_and_Wales.csv"
) |> 
  select(PCON25CD, PCON25NM) |> 
  distinct()


# Geographical boundaries... 

pc_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp") |> 
  select(PCON24CD, geometry)


# And hexmap - from https://automaticknowledge.org/wpc-hex/


hexmap_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/uk-wpc-hex-constitcode-v5-june-2024.shp") |> 
  select(GSScode, CTR_REG, Country, geometry ) |> 
  mutate(CTR_REG = ifelse(is.na(CTR_REG) == T, Country, CTR_REG)) |> 
  rename(hexmap_geometry = geometry)




# RUC from my soceity - https://pages.mysociety.org/uk_ruc/downloads/uk-ruc-pcon-2025-ruc-parquet/latest

# Need the second dataset to link with the codes for the join to combine datasets....

pr_ruc_classification <- read_parquet("data/raw_data/parliamentary_constituencies/pcon_2025_ruc.parquet")
parl_codes <- read_csv("data/raw_data/parliamentary_constituencies/parl_constituencies_2025_codes.csv")

# And join to get the GSS codes....

ruc_with_codes <- pr_ruc_classification |> 
  left_join(parl_codes, join_by(parl25 == short_code)) |> 
  select(highly_rural, rural, urban, label, gss_code, density, electorate)



# Deprivation... 

pc_deprivation <- read_csv("data/processed_data/dep_ranked_PCs_pop_weighted.csv")




# Type of heating.... 
central_heating_by_pc <- read_csv("data/raw_data/parliamentary_constituencies/central_heating_by_pc.csv") |> 
  select(ONSConstID,variables, Con_pc ) |> 
  pivot_wider(names_from = variables, values_from = Con_pc) |> 
  janitor::clean_names()





# Combine together 


pc_combined_dataset_1 <- parliamentary_constituencies |> 
  left_join(pc_boundaries, join_by(PCON25CD == PCON24CD)) |> 
  left_join(hexmap_boundaries, join_by(PCON25CD == GSScode)) |> 
  left_join(ruc_with_codes, join_by(PCON25CD == gss_code)) |> 
  left_join(pc_deprivation) |> 
  left_join(central_heating_by_pc, join_by(PCON25CD == ons_const_id)) |> 
  arrange(new_ranking) |>
  mutate(
    pop_cum = cumsum(total_population_PC),
    pop_share = pop_cum / sum(total_population_PC),
    new_ranking_quintile_deprivation = cut(
      pop_share,
      breaks = seq(0, 1, 0.2),
      labels = 1:5,
      include.lowest = TRUE
    )
  ) |> 
  select(-pop_cum, pop_share)


# Also add in the nox concentrations for each parliamentary constituency.

nox_stats <- read_csv("data/processed_data/nox_stats_2024_per_pc.csv") |> 
  select(-geometry)

pc_combined_dataset_2 <- pc_combined_dataset_1 |> 
  left_join(nox_stats, join_by(PCON25CD == PCON24CD)) |> 
  arrange(pw_mean_nox) |> 
  mutate(pop_cum = cumsum(total_population_PC),
         pop_share = pop_cum / sum(total_population_PC),
         nox_conc_quintile = cut(
           pop_share,
           breaks = seq(0, 1, 0.2),
           labels = 1:5,
           include.lowest = TRUE
         )) |> 
  select(-pop_cum, pop_share)




pc_combined_dataset_2 |> 
  write_csv("data/processed_data/pc_combined_dataset.csv")


