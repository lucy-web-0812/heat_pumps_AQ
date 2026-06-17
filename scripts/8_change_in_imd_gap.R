
library(tidyverse)
library(sf)
library(ineq)
library(ggridges)
library(paletteer)
library(patchwork)


# ==============================================================================
# 1. DATA LOADING
# ==============================================================================

model_results_per_pc <- read_csv("data/processed_data/model_results_per_pc.csv")

avg_non_ind_nox_per_pc_2023 <- read_csv("data/processed_data/avg_non_ind_nox_per_pc_2023.csv") 

pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")

parliamentary_boundaries <- read_sf("data/raw_data/parliamentary_constituencies/boundaries/PCON_JULY_2024_UK_BUC.shp") |> 
  dplyr::select(PCON24CD, geometry)





# Assumed nox (grams) per boiler per year

nox_per_boiler_per_year <- 644 #(assuming 644g NOx per boiler per year)


# And some labels for later

nice_labels <- c(
  present_day_scenario    = "Current trends persist",
  suitability_probability = "Suitability-driven uptake"
)


# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

# NOTE: We use population-weighted NOx concentrations to reflect human exposure.
# Heat pump emission reductions are calculated as a percentage of baseline
# emissions to maintain consistency with the population-weighted framework.

data_joined <- model_results_per_pc |>
  left_join(avg_non_ind_nox_per_pc_2023, join_by(PCON25CD == PCON24CD)) |>
  left_join(pc_combined_dataset, join_by(PCON25CD == PCON25CD)) |>
  left_join(parliamentary_boundaries, join_by(PCON25CD == PCON24CD)) |>
  mutate(
    # ===== BASELINE NON INDUSTRIAL COMBUSTION EMISSIONS (2023) =====
    # Population-weighted exposure intensity (tonnes NOx per km²)
    baseline_exposure_per_km2 = mean_nox_emission_per_km2,
    
    # Total annual emissions in constituency (tonnes NOx per year)
    total_baseline_emissions_tonnes = total_nox,
    
    # ===== HEAT PUMP EMISSION REDUCTIONS =====
    # Total emission savings from heat pump installations (tonnes per year)
    emission_saving_total_tonnes = cumulative_heat_pump_number * 
      nox_per_boiler_per_year / 1000000,
    
    # Calculate proportional reduction
    # This represents the fraction of total emissions eliminated by heat pumps
    reduction_fraction = emission_saving_total_tonnes / total_baseline_emissions_tonnes,
    reduction_fraction = pmin(reduction_fraction, 1.0),  # Cap at 100%
    reduction_fraction = pmax(reduction_fraction, 0.0),  # Ensure non-negative
    
    # ===== UPDATED EMISSIONS WITH HEAT PUMPS =====
    # Apply proportional reduction to population-weighted exposure
    updated_exposure_per_km2 = baseline_exposure_per_km2 * (1 - reduction_fraction),
    
    # Updated total emissions
    updated_total_emissions_tonnes = total_baseline_emissions_tonnes * (1 - reduction_fraction),

    # Emission savings spread uniformly
    emission_saving_uniform_per_km2 = emission_saving_total_tonnes / area_km2
  ) |>
  dplyr::select(
    PCON25CD, PCON25NM.x, year, model_run,
    total_nox,
    new_ranking, new_ranking_quintile_deprivation, median_imd_decile,
    area_km2, 
    baseline_exposure_per_km2, total_baseline_emissions_tonnes,
    heat_pump_number, cumulative_heat_pump_number, heat_pump_years,
    emission_saving_total_tonnes, reduction_fraction,
    updated_exposure_per_km2, updated_total_emissions_tonnes,
    nox_conc_quintile
  ) |> 
  rename( PCON25NM = PCON25NM.x)



# Brief look at the amount of area that each quintile occupies....

area_occupied_by_deprivation_quintile <- data_joined |> 
  dplyr::select(PCON25CD, new_ranking_quintile_deprivation, area_km2, total_nox) |> 
  unique() |> 
  group_by(new_ranking_quintile_deprivation) |> 
  summarise(total_area = sum(area_km2), 
            total_emission = sum(total_nox)) |> 
  mutate(
    area_as_pct = total_area/sum(total_area) * 100, 
    emissions_as_pct = total_emission/sum(total_emission) * 100) |> 
  pivot_longer(cols = c(area_as_pct, emissions_as_pct), names_to = "metric", values_to = "pct_share")


ggplot(area_occupied_by_deprivation_quintile) +
  geom_col(aes(x = new_ranking_quintile_deprivation, y = pct_share, fill = new_ranking_quintile_deprivation)) +
  scico::scale_fill_scico(palette = "acton") +
  coord_flip() +
  facet_wrap(~metric, scales = "free") +
  theme_minimal()



area_occupied_by_nox_quintile <- data_joined |> 
  dplyr::select(PCON25CD, nox_conc_quintile, area_km2) |> 
  unique() |> 
  group_by(nox_conc_quintile) |> 
  summarise(total_area = sum(area_km2), 
            count =n()) |> 
  mutate(
    area_as_pct = total_area/sum(total_area) * 100) 



# ==============================================================================
# 3. BASELINE INEQUALITY (2023)
# ==============================================================================

# Calculate baseline emissions by deprivation quintile
baseline_quintile_summary <- data_joined |>
  filter(year == "2023-01-01" ) |>
  group_by(new_ranking_quintile_deprivation) |>
  summarise(
    mean_nox = mean(baseline_exposure_per_km2, na.rm = TRUE),
    median_nox = median(baseline_exposure_per_km2, na.rm = TRUE),
    ymin = quantile(baseline_exposure_per_km2, 0.10, na.rm = TRUE),
    lower = quantile(baseline_exposure_per_km2, 0.25, na.rm = TRUE),
    upper = quantile(baseline_exposure_per_km2, 0.75, na.rm = TRUE),
    ymax = quantile(baseline_exposure_per_km2, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Plot baseline inequality
ggplot(baseline_quintile_summary, aes(x = factor(new_ranking_quintile_deprivation))) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = median_nox, upper = upper, ymax = ymax),
    stat = "identity"
  ) +
  scale_y_continuous(
    name = "Non-Industrial Combustion NOx Emissions (tonnes km⁻²)", limits = c(0,NA)
  ) +
  scale_x_discrete(
    name = "Relative Parliamentary Constituency Deprivation\n(1 = 20% most deprived)"
  ) +
  ggtitle("Baseline NOx Emissions from Non-Industrial Combustion (2023)") +
  theme_minimal(base_size = 14)



# Ridge line plot instead.... 

data_joined |> 
  filter(year == "2023-01-01" ) |>
  ggplot(aes(x = baseline_exposure_per_km2, y = factor(new_ranking_quintile_deprivation))) +
  geom_density_ridges(quantile_lines = T) #+
  #scale_x_log10()


# Let just look at the baseline for all grid squares.... 


ggsave("plots/misc_plots/baseline_nox_inequality.png", width = 10, height = 6)










# Alternative way of plotting ------------------------------------------------------------
# ==============================================================================
# 6. INEQUALITY METRICS
# ==============================================================================

# --- 6.1 Constituency-level summary by quintile ---
# Calculate mean emission intensity per deprivation quintile, year and scenario

quintile_means <- data_joined |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  filter(new_ranking_quintile_deprivation %in% c("1", "5")) |>
  group_by(year, model_run, new_ranking_quintile_deprivation) |>
  summarise(
    mean_emission  = mean(updated_exposure_per_km2, na.rm = TRUE),
    median_emission = median(updated_exposure_per_km2, na.rm = TRUE),
    p25 = quantile(updated_exposure_per_km2, 0.25),
    p75 = quantile(updated_exposure_per_km2, 0.75),
    .groups = "drop"
  )

# ==============================================================================
# PREPARE DATA
# ==============================================================================

scenario_colours <- c(
  "Current trends persist"    = "#6F6B73",
  "Suitability-driven uptake" = "#D95AA3"
)

# --- Baseline normalisation for % reduction plot ---
quintile_means_norm <- quintile_means |>
  group_by(model_run, new_ranking_quintile_deprivation) |>
  mutate(
    baseline_median = median_emission[year == as.Date("2025-01-01")],
    baseline_p25    = p25[year == as.Date("2025-01-01")],
    baseline_p75    = p75[year == as.Date("2025-01-01")],
    pct_median = (median_emission - baseline_median) / baseline_median * 100,
    pct_p25    = (p25 - baseline_p25) / baseline_p25 * 100,
    pct_p75    = (p75 - baseline_p75) / baseline_p75 * 100
  ) |>
  ungroup() |>
  mutate(
    model_run = recode(model_run,
                       "present_day_scenario"    = "Current trends persist",
                       "suitability_probability" = "Suitability-driven uptake"
    ),
    deprivation = recode(new_ranking_quintile_deprivation,
                         "1" = "Q1 (most deprived)",
                         "5" = "Q5 (least deprived)"
    )
  )

# --- Absolute and relative gap ---
# Pivot wide so Q1 and Q5 are columns, then compute gap metrics
gap_metrics <- quintile_means |>
  mutate(
    model_run = recode(model_run,
                       "present_day_scenario"    = "Current trends persist",
                       "suitability_probability" = "Suitability-driven uptake"
    )
  ) |>
  pivot_wider(
    names_from  = new_ranking_quintile_deprivation,
    values_from = c(mean_emission, median_emission, p25, p75),
    names_sep   = "_Q"
  ) |>
  mutate(
    # Absolute gap: Q1 (most deprived) minus Q5 (least deprived)
    abs_median = median_emission_Q1 - median_emission_Q5,
    abs_p25    = p25_Q1 - p25_Q5,
    abs_p75    = p75_Q1 - p75_Q5,
    # Relative gap: ratio of Q1 to Q5
    rel_median = median_emission_Q1 / median_emission_Q5,
    rel_p25    = p25_Q1 / p25_Q5,
    rel_p75    = p75_Q1 / p75_Q5
  )

# ==============================================================================
# SHARED THEME
# ==============================================================================

theme_inequality <- function() {
  theme_minimal(base_size = 14) +
    theme(
      legend.position      = "bottom",
      legend.justification = "left",
      legend.key.width     = unit(2.5, "cm"),
      axis.line            = element_line(colour = "black", linewidth = 0.3),
      axis.ticks           = element_line(colour = "black")
    )
}

# ==============================================================================
# PLOT Z: Absolute reduction in emission intensity from 2025 baseline
# ==============================================================================
plot_abs <- ggplot(
  quintile_means_norm,
  aes(colour = model_run, fill = model_run, linetype = deprivation)
) +
  geom_line(aes(x= year, y = median_emission), linewidth = 1.2) +
  scale_colour_manual(name = "", values = scenario_colours) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_linetype_manual(
    name   = "",
    values = c(
      "Q1 (most deprived)"  = "dashed",
      "Q5 (least deprived)" = "solid"
    ),
    labels = c(
      "Q1 (most deprived)"  = expression("Q"[1]~"(most deprived)"),
      "Q5 (least deprived)" = expression("Q"[5]~"(least deprived)")
    )) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name   = expression("Median emissions intensity (tonnes km"^{-2}*")"),
    guide  = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = 0.2)
  )  +
  guides(
    colour   = guide_legend(nrow = 2, byrow = TRUE),
    fill     = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_inequality()


# ==============================================================================
# PLOT A: % reduction in emission intensity from 2025 baseline
# ==============================================================================

plot_pct <- ggplot(
  quintile_means_norm,
  aes(colour = model_run, fill = model_run, linetype = deprivation)
) +
  # geom_ribbon(
  #   aes(x = year, ymin = pct_p25, ymax = pct_p75),
  #   alpha = 0.15, colour = NA
  # ) +
  geom_line(aes(x = year, y = pct_median), linewidth = 1.2) +
  scale_colour_manual(name = "", values = scenario_colours) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_linetype_manual(
    name   = "",
    values = c(
      "Q1 (most deprived)"  = "dashed",
      "Q5 (least deprived)" = "solid"
    ),
    labels = c(
      "Q1 (most deprived)"  = expression("Q"[1]~"(most deprived)"),
      "Q5 (least deprived)" = expression("Q"[5]~"(least deprived)")
    )
  ) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name   = "Reduction in emission intensity (%)",
    labels = scales::label_number(suffix = "%"),
    guide  = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = 0.2)
  ) +
  guides(
    colour   = guide_legend(nrow = 2, byrow = TRUE),
    fill     = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_inequality()

# ==============================================================================
# PLOT B: Absolute gap (Q1 - Q5)
# ==============================================================================

plot_absolute <- ggplot(gap_metrics, aes(colour = model_run, fill = model_run)) +
  # geom_ribbon(
  #   aes(x = year, ymin = abs_p25, ymax = abs_p75),
  #   alpha = 0.15, colour = NA
  # ) +
  geom_point(aes(x = year, y = abs_median), size = 0.6) +
  geom_line(aes(x = year, y = abs_median), linewidth = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(name = "", values = scenario_colours) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name  = expression("Absolute gap (Q1 − Q5) (tonnes km"^{-2}*")"),
    guide = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = 0.1),
    breaks = seq(0,1.6, by = 0.4),
  ) +
  guides(colour = "none", fill = "none")+
  theme_inequality() 

# ==============================================================================
# PLOT C: Relative gap (Q1 / Q5 ratio)
# ==============================================================================

plot_relative <- ggplot(gap_metrics, aes(colour = model_run, fill = model_run)) +
  # geom_ribbon(
  #   aes(x = year, ymin = rel_p25, ymax = rel_p75),
  #   alpha = 0.15, colour = NA
  # ) +
  geom_point(aes(x = year, y = rel_median), size = 0.6) +
  geom_line(aes(x = year, y = rel_median), linewidth = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(name = "", values = scenario_colours, ) +
  scale_fill_manual(name = "", values = scenario_colours) +
  scale_x_date(
    name   = "Year",
    guide  = guide_axis(minor.ticks = TRUE),
    limits = c(as.Date("2025-01-01"), NA)
  ) +
  scale_y_continuous(
    name  = "Relative gap (Q1 / Q5 ratio)",
    guide = guide_axis(minor.ticks = TRUE),
    breaks = seq(1,2.2, by = 0.2),
    expand = expansion(mult = 0.1)
  ) +
 # guides(colour = "none", fill = "none")+
  theme_inequality()


ggplotly(plot_relative)

# ==============================================================================
# COMBINE
# ==============================================================================

library(patchwork)

plot_absolute + plot_relative +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position      = "bottom",
    plot.tag.position    = c(0, 1)
  )

ggsave("plots/paper_plots/inequality_figure.png", dpi = 600)




(plot_absolute + plot_relative + guide_area()) +
  plot_layout(
    design = "
AB
CL
",
    guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position      = "right",
    legend.justification = "left",
    legend.direction = "horizontal",
    plot.tag.position    = c(0, 1)
  )+
  theme(
    legend.key.width = unit(2.5, "cm"),
    legend.spacing.y = unit(0.5, "cm")
  )



ggsave(
  "plots/paper_plots/inequality_figure.png", dpi = 600
)



















# ==============================================================================
# 4. FUTURE PROJECTIONS (2025-2050)
# ==============================================================================

# Filter to key years and present day scenario
future_inequality <- data_joined |>
  filter(model_run %in% c("present_day_scenario")) |>
  filter(
    year %in% c(
      "2025-01-01",
      "2030-01-01",
      "2035-01-01",
      "2040-01-01",
      "2045-01-01",
      "2050-01-01"
    )
  ) |> 
  mutate(emissions_as_pct_of_baseline = (updated_exposure_per_km2/ baseline_exposure_per_km2)*100)
  

# Summarise by quintile and year
future_quintile_summary <- future_inequality |>
  group_by(new_ranking_quintile_deprivation, year, model_run) |>
  summarise(
    mean_nox = mean(updated_exposure_per_km2, na.rm = TRUE),
    median_nox = median(updated_exposure_per_km2, na.rm = TRUE),
    ymin = quantile(updated_exposure_per_km2, 0.10, na.rm = TRUE),
    lower = quantile(updated_exposure_per_km2, 0.25, na.rm = TRUE),
    upper = quantile(updated_exposure_per_km2, 0.75, na.rm = TRUE),
    ymax = quantile(updated_exposure_per_km2, 0.90, na.rm = TRUE),
  ) 

# Plot future projections by year (faceted)
ggplot(future_inequality) +
  geom_boxplot(aes(x = factor(new_ranking_quintile_deprivation), y =updated_exposure_per_km2), outliers = F,
               #outlier.shape = 4, outlier.size = 1) +
  ) +
  # geom_boxplot(
  #   aes(x = factor(new_ranking_quintile_deprivation), 
  #       ymin = ymin, lower = lower, middle = median_nox, upper = upper, ymax = ymax),
  #   stat = "identity"
  # ) +
  facet_wrap(~year) +
  scale_y_continuous(
    name = "Non-Industrial Combustion NOx Emissions (tonnes km⁻²)", 
    limits = c(0,NA), expand = c(0,0),
  ) +
  scale_x_discrete(
    name = "Relative Deprivation (1 = 20% most deprived)"
  ) +
  labs(
    title = "Future Projections of Non-Industrial Combustion NOx Emissions by Deprivation (2025-2050)",
    subtitle = "If the present day trend of heat pump adoption continues"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line())

ggsave("plots/misc_plots/future_nox_projections.png", width = 14, height = 10)




# Plot emissions as percentage of baseline
ggplot(future_inequality, aes(
  x = factor(new_ranking_quintile_deprivation),
  y = emissions_as_pct_of_baseline,
  fill = new_ranking_quintile_deprivation
)) +
  geom_boxplot( alpha = 0.8, outlier.shape = 4, outlier.size = 0.5) +
  facet_wrap(~substr(as.character(year), 1,4), scales = "free") +
 # khroma::scale_fill_mediumcontrast() +
  khroma::scale_fill_acton() +
  scale_y_continuous(
    name = "Emissions as % of 2023 Baseline",
    #limits = c(35, 100)
  ) +
  scale_x_discrete(
    name = "Relative Deprivation (1 = 20% most deprived)"
  ) +
  labs(
    title = "NOx Emission Reductions by Deprivation Quintile",
    subtitle = "Percentage remaining compared to 2023 baseline"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line())

ggsave("plots/misc_plots/future_nox_pct_reduction.png", width = 14, height = 10)

# Overlaid lines showing trends across years
ggplot(future_quintile_summary, aes(x = new_ranking_quintile_deprivation)) +
  geom_smooth(
    aes(y = mean_nox, group = year, linetype = as.factor(year), colour =as.factor(year)),
    method = "lm", fill = NA
  ) +
  # geom_line(
  #   aes(y = mean_nox, group = year, linetype = as.factor(year), colour = as.factor(year))
  # ) +
  facet_wrap(~model_run) +
  scale_y_continuous(
    name = "Non-Industrial Combustion NOx Emissions (tonnes km⁻²)"
  ) +
  scale_x_continuous(
    name = "Relative Deprivation (1 = 20% most deprived)",
    breaks = 1:5
  ) +
  scale_colour_viridis_d(name = "Year") +
  scale_linetype(name = "Year") +
  labs(
    title = "Evolution of Emissions Inequality (2025-2050)",
    subtitle = "Mean emissions by deprivation quintile"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/misc_plots/emissions_inequality_trends.png", width = 10, height = 6)

# ==============================================================================
# 5. CUMULATIVE EMISSIONS (2025-2050)
# ==============================================================================

# Calculate cumulative emissions over the period
cumulative_emissions <- data_joined |>
 # filter(year != "2023-01-01") |>
  group_by(PCON25CD, model_run) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    # Business as usual (no heat pumps)
    cumulative_nox_bau_tonnes_per_km2 = cumsum(baseline_exposure_per_km2),
    cumulative_nox_bau_tonnes = cumsum(total_baseline_emissions_tonnes),
    
    # With heat pump adoption
    cumulative_nox_hp_tonnes_per_km2 = cumsum(updated_exposure_per_km2),
    cumulative_nox_hp_tonnes = cumsum(updated_total_emissions_tonnes),
    
    # Avoided emissions
    avoided_emissions_tonnes_per_km2 = cumulative_nox_bau_tonnes_per_km2 - cumulative_nox_hp_tonnes_per_km2,
    avoided_emissions_tonnes = cumulative_nox_bau_tonnes - cumulative_nox_hp_tonnes
  ) |>
  ungroup()

# Summarize cumulative emissions at 2050 by deprivation
cumulative_2050 <- cumulative_emissions |>
  filter(
    year == "2050-01-01",
    model_run %in% c("present_day_scenario", "suitability_probability")
  ) |>
  group_by(new_ranking_quintile_deprivation, model_run) |>
  summarise(
    mean_nox = mean(cumulative_nox_hp_tonnes_per_km2, na.rm = TRUE),
    median_nox = median(cumulative_nox_hp_tonnes_per_km2, na.rm = TRUE),
    ymin = quantile(cumulative_nox_hp_tonnes_per_km2, 0.1, na.rm = TRUE),
    lower = quantile(cumulative_nox_hp_tonnes_per_km2, 0.25, na.rm = TRUE),
    upper = quantile(cumulative_nox_hp_tonnes_per_km2, 0.75, na.rm = TRUE),
    ymax = quantile(cumulative_nox_hp_tonnes_per_km2, 0.90, na.rm = TRUE),
    .groups = "drop"
  )


# Plot cumulative emissions 2025-2050
ggplot(cumulative_emissions, aes(x = factor(new_ranking_quintile_deprivation), y = cumulative_nox_hp_tonnes_per_km2, fill = new_ranking_quintile_deprivation) ) +
  geom_boxplot(outlier.shape = 4,) +
  khroma::scale_fill_acton()+
  facet_wrap(~model_run) +
  scale_y_continuous(
    name = "Cumulative NOx Emissions 2025-2050 (tonnes km⁻²)",
   # limits = c(0, 200), expand = c(0, 0), breaks = seq(0, 200, 25)
  ) +
  scale_x_discrete(
    name = "Relative Deprivation (1 = 20% most deprived)"
  ) +
  labs(
    title = "Cumulative NOx Emissions by Deprivation (2025-2050)",
    subtitle = "Total emissions over the period with heat pump adoption"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line())

ggsave("plots/misc_plots/cumulative_nox_emissions.png", width = 12, height = 6)

# Summarize avoided emissions (benefit of heat pumps)
avoided_emissions_2050 <- cumulative_emissions |>
  filter(year == "2050-01-01") |>
  group_by(new_ranking_quintile_deprivation, model_run) |>
  summarise(
    mean = mean(avoided_emissions_tonnes_per_km2, na.rm = T),
    ymin = quantile(avoided_emissions_tonnes_per_km2, 0.10, na.rm = TRUE),
    lower = quantile(avoided_emissions_tonnes_per_km2, 0.25, na.rm = TRUE),
    median = median(avoided_emissions_tonnes_per_km2, na.rm = TRUE),
    upper = quantile(avoided_emissions_tonnes_per_km2, 0.75, na.rm = TRUE),
    ymax = quantile(avoided_emissions_tonnes_per_km2, 0.90, na.rm = TRUE),
    .groups = "drop"
  )


# Plot avoided emissions
avoided_emissions_2050 |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  ggplot(aes(x = factor(new_ranking_quintile_deprivation))) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = median, upper = upper, ymax = ymax),
    stat = "identity", fill = "#D0CEBA", alpha = 0.2
  ) +
  geom_point(aes(y = mean),shape = 4) +
  facet_wrap(~model_run,  labeller = as_labeller(nice_labels)) +
  scale_y_continuous(
    name = "Avoided Cumulative Emissions (tonnes km⁻²)",
    limits = c(0, NA), 
    expand = c(0,0)
  ) +
  scale_x_discrete(
    name = "Relative Deprivation (1 = 20% most deprived)"
  ) +
  scale_shape_manual(name = "Mean") +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(), 
        strip.text = element_text(size = 18, hjust = 0),
        legend.position = "top", 
        legend.justification = "left",)

ggsave("plots/misc_plots/avoided_emissions.png", width = 12, height = 6)

# ==============================================================================
# 6. INEQUALITY METRICS
# ==============================================================================

# Calculate quintile-level means for inequality metrics
quintile_means <- data_joined |>
  group_by(year, model_run, new_ranking_quintile_deprivation) |>
  summarise(
    mean_emission = mean(updated_exposure_per_km2, na.rm = TRUE), # This is the mean of the population weighted averages.... 
    median_emission = median(updated_exposure_per_km2, na.rm = TRUE),
    quantile_25 = quantile(updated_exposure_per_km2, 0.25,na.rm = TRUE),
    quantile_75 = quantile(updated_exposure_per_km2, 0.75,na.rm = TRUE),
    sd = sd(updated_exposure_per_km2, na.rm = T),
    .groups = "drop"
  )


q1_q5_values <- quintile_means |>
  filter(new_ranking_quintile_deprivation %in% c(1, 5)) |>
  dplyr::select(year, model_run, new_ranking_quintile_deprivation, mean_emission) |>
  pivot_wider(
    names_from  = new_ranking_quintile_deprivation,
    values_from = mean_emission,
    names_prefix = "Q"
  )



# Calculate inequality metrics
inequality_metrics <- quintile_means |>
  group_by(year, model_run) |>
  summarise(
    # Gini coefficient (0 = perfect equality, 1 = perfect inequality)
    gini = Gini(mean_emission),
    # Slope: emissions increase per quintile
    slope = coef(lm(mean_emission ~ new_ranking_quintile_deprivation))[2],
    # Coefficient of variation
    cv = sd(mean_emission) / mean(mean_emission),
    .groups = "drop"
  ) |> 
  left_join(q1_q5_values, by = c("year", "model_run")) |>
  mutate(
    # Ratio: most deprived to least deprived
    q1_q5_ratio = Q1 / Q5,
    
    # Absolute gap between most and least deprived
    absolute_gap = Q1 - Q5
  )



inequality_metrics_norm <- inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  group_by(model_run) |>
  mutate(
    # 2023 baselines
    q1_2023 = Q1[year == as.Date("2023-01-01")], 
    q5_2023 = Q5[year == as.Date("2023-01-01")]
  ) |>
  mutate(q1_norm = (Q1 - q1_2023)/ q1_2023 * 100 , 
         q5_norm = (Q5 - q5_2023)/ q5_2023 * 100) |> 
  ungroup() |> 
  pivot_longer(cols = c(q1_norm, q5_norm), names_to = "q_norm", values_to = "pct_red")



inequality_metrics_norm |> 
  ggplot(aes(x = year, y = pct_red, colour = interaction(model_run, q_norm))) +
  geom_point()









# Plot Absolute gap over time
inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  ggplot(aes(x = year, y = q1_q5_ratio, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    name = "Ratio between Q1 and Q5 (tonnes km⁻²)") +
  labs(
    title = "Absolute difference in emissions between Q1 and Q5",
    x = "Year",
    colour = "Model Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", 
        legend.justification = "left", 
        axis.line = )

ggsave("plots/misc_plots/gini_coefficient_trends.png", width = 10, height = 6)


write_csv(inequality_metrics, "data/processed_data/inequality_metrics.csv")



# Plot absolute vs relative inequality
inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")) |>
  dplyr::select(year,gini, absolute_gap, q1_q5_ratio, model_run) |>
  pivot_longer(cols = c(absolute_gap, q1_q5_ratio), 
               names_to = "metric", values_to = "value") |>
  ggplot(aes(x = year, y = value, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", "suitability_probability" = "#DC6BAD","ECO_only"= "#6969B3", "BUS_only" = "#B4CEB3"),
   labels = c("present_day_scenario" = "Present Day Scenario",
              "suitability_probability" ="Suitability Probability", 
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" ="Boiler Upgrade Scheme"
                )) +
  facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = labeller(
    metric = c( 
      absolute_gap = "Absolute gap (tonnes/km²)",
      q1_q5_ratio  = "Relative gap (Q1 / Q5 ratio)"
    )
  )) +
  labs(
    title = "Absolute and Relative emissions inequality",
    subtitle = "Most deprived (Q1) compared to least deprived (Q5)",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        legend.justification = "left", 
        axis.line = element_line(), 
        axis.title.y = element_blank())




# Plot absolute vs relative inequality - POSTER VERSION
inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")) |>
  dplyr::select(year,gini, absolute_gap, q1_q5_ratio, model_run) |>
  pivot_longer(cols = c(absolute_gap, q1_q5_ratio), 
               names_to = "metric", values_to = "value") |>
  ggplot(aes(x = year, y = value, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", "suitability_probability" = "#DC6BAD","ECO_only"= "#6969B3", "BUS_only" = "#B4CEB3"),
    labels = c("present_day_scenario" = "Present Day Scenario",
               "suitability_probability" ="Suitability Probability", 
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" ="Boiler Upgrade Scheme"
    )) +
  facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = labeller(
    metric = c( 
      absolute_gap = "Absolute gap (tonnes/km²)",
      q1_q5_ratio  = "Relative gap (Q1 / Q5 ratio)"
    )
  )) +
  labs(
    title = "Absolute and Relative emissions inequality",
    subtitle = "Most deprived (Q1) compared to least deprived (Q5)",
    x = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        legend.justification = "left", 
        legend.text = element_text(size = 16),
        axis.line = element_line(), 
        axis.ticks = element_line(),
        axis.title.y = element_blank()) +
guides(colour=guide_legend(nrow=2,byrow=TRUE))

ggsave("plots/poster_plots/absolute_and_rel_inequality.png", width = 23.79, height = 31.32, units = "cm")
ggsave("plots/paper_plots/absolute_and_rel_inequality.png", width = 8.79, height = 12.32)


# And a version with just the present data and suitability 


inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  dplyr::select(year,gini, absolute_gap, q1_q5_ratio, model_run) |>
  pivot_longer(cols = c(absolute_gap, q1_q5_ratio), 
               names_to = "metric", values_to = "value") |>
  ggplot(aes(x = year, y = value, colour = model_run)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_x_date(name = "Year", guide = guide_axis(minor.ticks = TRUE)) +
  scale_colour_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", "suitability_probability" = "#DC6BAD","ECO_only"= "#6969B3", "BUS_only" = "#B4CEB3"),
    labels = c("present_day_scenario" = "Current Trends Persist",
               "suitability_probability" ="Suitability Probability", 
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" ="Boiler Upgrade Scheme"
    )) +
  facet_wrap(~metric, scales = "free_y", ncol = 2, labeller = labeller(
    metric = c( 
      absolute_gap = "Absolute gap Q1 - Q5 (tonnes/km²)",
      q1_q5_ratio  = "Relative gap (Q1 / Q5 ratio)"
    )
  )) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        legend.justification = "left", 
        legend.title.position = "top",
        legend.text = element_text(size = 12),
        axis.line = element_line(), 
        axis.ticks = element_line(),
        axis.title.y = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank())
  
ggsave("plots/paper_plots/absolute_and_rel_inequality_two_scenarios.png", width = 20, height = 12, units = "cm", dpi = 600)




# Normalised to emissions pct


df_norm <- inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  group_by(model_run) |>
  mutate(
    Q1_pct = (Q1 / Q1[year == min(year)]) * 100,
    Q5_pct = (Q5 / Q5[year == min(year)]) * 100
  )  |>
  ungroup()




ggplot(df_norm, aes(x = year)) +
  geom_line(aes(y = Q1_pct, colour = "Most deprived"), linewidth = 1.2) +
  geom_line(aes(y = Q5_pct, colour = "Least deprived"), linewidth = 1.2)  +
  facet_wrap(~model_run)










# Now also if getting error bars....



# Calculate quintile-level means with standard errors
quintile_means <- data_joined |>
  group_by(year, model_run, new_ranking_quintile_deprivation) |>
  summarise(
    mean_emission = mean(updated_exposure_per_km2, na.rm = TRUE),
    median_emission = median(updated_exposure_per_km2, na.rm = TRUE),
    quantile_25 = quantile(updated_exposure_per_km2, 0.25, na.rm = TRUE),
    quantile_75 = quantile(updated_exposure_per_km2, 0.75, na.rm = TRUE),
    sd = sd(updated_exposure_per_km2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),  # Standard error of the mean
    .groups = "drop"
  )

# Get Q1 and Q5 values with their standard errors
q1_q5_values <- quintile_means |>
  filter(new_ranking_quintile_deprivation %in% c(1, 5)) |>
  dplyr::select(year, model_run, new_ranking_quintile_deprivation, mean_emission, se) |>
  pivot_wider(
    names_from = new_ranking_quintile_deprivation,
    values_from = c(mean_emission, se),
    names_glue = "{.value}_Q{new_ranking_quintile_deprivation}"
  )

# Calculate inequality metrics with propagated uncertainty
inequality_metrics <- quintile_means |>
  group_by(year, model_run) |>
  summarise(
    gini = Gini(mean_emission),
    slope = coef(lm(mean_emission ~ new_ranking_quintile_deprivation))[2],
    cv = sd(mean_emission) / mean(mean_emission),
    .groups = "drop"
  ) |>
  left_join(q1_q5_values, by = c("year", "model_run")) |>
  mutate(
    # Point estimates
    q1_q5_ratio = mean_emission_Q1 / mean_emission_Q5,
    absolute_gap = mean_emission_Q1 - mean_emission_Q5,
    
    # Propagate standard errors
    # For ratio: SE(A/B) ≈ (A/B) * sqrt((SE_A/A)² + (SE_B/B)²)
    se_ratio = q1_q5_ratio * sqrt(
      (se_Q1 / mean_emission_Q1)^2 + (se_Q5 / mean_emission_Q5)^2
    ),
    
    # For difference: SE(A-B) = sqrt(SE_A² + SE_B²)
    se_gap = sqrt(se_Q1^2 + se_Q5^2),
    
    # 95% confidence intervals
    ratio_ci_lower = q1_q5_ratio - 1.96 * se_ratio,
    ratio_ci_upper = q1_q5_ratio + 1.96 * se_ratio,
    gap_ci_lower = absolute_gap - 1.96 * se_gap,
    gap_ci_upper = absolute_gap + 1.96 * se_gap
  )

# Plot with uncertainty bounds
inequality_metrics |>
  filter(model_run %in% c("present_day_scenario", "suitability_probability", "BUS_only", "ECO_only")) |>
  dplyr::select(year, model_run, 
         absolute_gap, gap_ci_lower, gap_ci_upper,
         q1_q5_ratio, ratio_ci_lower, ratio_ci_upper) |>
  pivot_longer(
    cols = c(absolute_gap, q1_q5_ratio),
    names_to = "metric",
    values_to = "value"
  ) |>
  # Add corresponding CI bounds
  mutate(
    ci_lower = ifelse(metric == "absolute_gap", gap_ci_lower, ratio_ci_lower),
    ci_upper = ifelse(metric == "absolute_gap", gap_ci_upper, ratio_ci_upper)
  ) |>
  ggplot(aes(x = year, y = value, colour = model_run, fill = model_run)) +
  
  # Add uncertainty ribbon
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
  
  # Main line and points
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  scale_colour_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", 
               "suitability_probability" = "#DC6BAD",
               "ECO_only" = "#6969B3", 
               "BUS_only" = "#B4CEB3"),
    labels = c("present_day_scenario" = "Present Day Scenario",
               "suitability_probability" = "Suitability Probability",
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" = "Boiler Upgrade Scheme")
  ) +
  scale_fill_manual(
    name = "Model Run",
    values = c("present_day_scenario" = "#7F7B82", 
               "suitability_probability" = "#DC6BAD",
               "ECO_only" = "#6969B3", 
               "BUS_only" = "#B4CEB3"),
    labels = c("present_day_scenario" = "Present Day Scenario",
               "suitability_probability" = "Suitability Probability",
               "ECO_only" = "Energy Company Obligation",
               "BUS_only" = "Boiler Upgrade Scheme")
  ) +
  facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = labeller(
    metric = c( 
      absolute_gap = "Absolute gap (tonnes/km²)",
      q1_q5_ratio  = "Relative gap (Q1 / Q5 ratio)"
    )
  )) +
  labs(
    title = "Absolute and Relative emissions inequality",
    subtitle = "Most deprived (Q1) compared to least deprived (Q5) with 95% CI",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = "left", 
    axis.line = element_line(), 
    axis.title.y = element_blank()
  )



ggsave("plots/misc_plots/inequality_absolute_vs_relative_new_with_errors.png", width = 10, height = 8)

# Summary table of inequality metrics for key years
inequality_summary <- inequality_metrics |>
  filter(
    model_run %in% c("present_day_scenario", "suitability_probability") &  #"BUS_only", "ECO_only") &
    year %in% c("2023-01-01", "2025-01-01", "2030-01-01", "2035-01-01", "2040-01-01", "2045-01-01", "2050-01-01")
  ) |>
  dplyr::select(model_run,year,
         mean_emission_Q1,
         mean_emission_Q5,
         absolute_gap,
         q1_q5_ratio,
         #gini,
         #slope
         ) |>
  mutate(
    year = format(year, "%Y"),
    across(where(is.numeric), ~round(.x, 3))
  )

print(inequality_summary)

write_csv(inequality_summary, "data/processed_data/inequality_metrics_summary.csv")




# And some more plots....





# Looking now at the total emissions burden rather than average exposure... 

future_inequality <- data_joined |>
 filter(model_run %in% c("present_day_scenario", "suitability_probability")) |>
  filter(
    year %in% as.Date(c(
      "2025-01-01", "2030-01-01", "2035-01-01",
      "2040-01-01", "2045-01-01", "2050-01-01"
    ))
  ) |> 
  group_by(new_ranking_quintile_deprivation, year, model_run) |>
  summarise(
    total_nox = sum(updated_exposure_per_km2, na.rm = TRUE),
    .groups = "drop"
  ) 


ggplot(future_inequality,
       aes(
         x = factor(new_ranking_quintile_deprivation),
         y = total_nox,
         fill = model_run
       )) +
  geom_col(position = "dodge") +
  facet_wrap(~ year) +
  scale_y_continuous(
    name = "Total NOx emissions (tonnes)"
  ) +
  scale_x_discrete(
    name = "Relative Deprivation (1 = 20% most deprived)"
  ) +
  labs(
    title = "Total NOx emissions by deprivation quintile",
    subtitle = "Non-industrial combustion, 2025–2050"
  ) +
  theme_minimal(base_size = 14)





