# Heat Pump Adoption in the UK: Implications for Air Quality Disparities

This repository contains the data processing pipeline, modelling code, and figures for a study examining the geographic distribution of heat pump adoption across parliamentary constituencies in England and Wales, and its implications for air quality and health equity.

The project models NOx emissions savings and avoided damage costs under different heat pump deployment scenarios (continuation of current policy vs. suitability-driven allocation) from 2025 to 2050, and examines how these benefits are distributed across deprivation and pollution quintiles.


## Repository structure
 
```
├── data/
│   ├── raw_data/         # Source datasets (ONS, Defra, DESNZ, etc.)
│   └── processed_data/   # Cleaned/derived datasets produced by the scripts below
├── scripts/
│   ├── 0_initialisation_script.R              # Runs the full pipeline in order (see below)
│   ├── 1_data_cleaning.R
│   ├── 2_stats_from_small_geog_to_PC.R
│   ├── 3_projecting_household_numbers.R
│   ├── 4_model_run_per_pc.R
│   ├── 5_collated_pc_dataset.R
│   ├── 6_2050_data_versus_imd_and_nox_conc.R
│   ├── 7_non_industrial_nox_per_pc.R
│   ├── 8_change_in_imd_gap_2.R
│   ├── 9_damage_costs.R
│   ├── 10_nox_savings_by_dep_and_conc.R
│   ├── functions/                              # Shared helper functions
│   ├── paper_figures/                          # Scripts producing manuscript figures
│   ├── mapping_nox_quintile_to_dep.R
│   └── quintile_difference_in_nox_saving.R
├── plots/
│   ├── paper_plots/           # Final figures used in the manuscript
│   ├── poster_plots/
│   ├── presentation/
│   ├── scrolytelling_plots/
│   └── misc_plots/            # Exploratory plots, not used in outputs
├── index.qmd / index.html              # Scrollytelling web summary of the analysis
├── presentation.qmd / presentation.html  # Conference presentation
└── heat_pumps_AQ.Rproj
```
 
## Getting started
 
1. Clone the repository and open `heat_pumps_AQ.Rproj` in RStudio. Opening via the `.Rproj` file ensures all relative file paths (e.g. `data/processed_data/...`) resolve correctly regardless of where the repository is stored on your machine.
2. Install the required packages (see below).
3. Run `scripts/0_initialisation_script.R`, which sources scripts 1 through 10 in order to reproduce the full pipeline: data cleaning -> geographic aggregation -> household/deployment projections -> Monte Carlo allocation modelling -> NOx and damage cost calculations -> figures.
Each numbered script depends on outputs from earlier scripts in the sequence (typically CSVs written to `data/processed_data/`), so scripts should generally be run in order, or via `0_initialisation_script.R`, rather than individually from a fresh session.
