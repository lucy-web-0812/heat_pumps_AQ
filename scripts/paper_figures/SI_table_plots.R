# Producing look up table for constituency on main facts.... 

library(tidyverse)
pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")



table_for_paper <- pc_combined_dataset |> 
  select(PCON25CD, PCON25NM, total_population_PC, median_imd_lsoa_ranking, new_ranking_quintile_deprivation, pw_mean_nox, nox_conc_quintile) |> 
  rename(Population = total_population_PC, `Median IMD LSOA Ranking` = median_imd_lsoa_ranking, `Relative Deprivation Quintile` = new_ranking_quintile_deprivation, 
         `Mean NOx Concentration` = pw_mean_nox, `NOx Concentration Quintile` = nox_conc_quintile) 




pc_combined_dataset |> 
  arrange(pw_mean_nox) |> 
  group_by(nox_conc_quintile) |> 
  summarise(quintile_lower_bound = min(pw_mean_nox), 
            quintile_upper_bound = max(pw_mean_nox))




pc_combined_dataset |> 
  arrange(median_imd_lsoa_ranking) |> 
  group_by(new_ranking_quintile_deprivation) |> 
  summarise(quintile_lower_bound = mean(median_imd_decile), 
            quintile_upper_bound = median(median_imd_decile))
