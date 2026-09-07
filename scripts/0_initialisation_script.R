# Initialisation script....

# Sort out any conflicts...

library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# And the packages required...

library(arrow)
library(cowplot)
library(exactextractr)
library(sf)
library(terra)
library(sf)
library(tidyverse)
library(readxl)
library(paletteer)
library(patchwork)
library(plotly)
library(raster)
library(ineq)
library(janitor)
library(ggthemes)
library(khroma)
library(scico)
library(matrixStats)
library(units)
library(rlang)
library(IMD)
library(scales)



source("scripts/1_data_cleaning.R")
source("scripts/2_stats_from_small_geog_to_PC.R")
source("scripts/3_projecting_household_numbers.R")
source("scripts/4_model_run_per_pc.R")
source("scripts/5_collated_pc_dataset.R")
source("scripts/6_2050_data_versus_imd_and_nox_conc.R")
source("scripts/7_non_industrial_nox_per_pc.R")
source("scripts/8_change_in_imd_gap_2.R")
source("scripts/9_damage_costs.R")
source("scripts/10_nox_savings_by_dep_and_conc.R")

