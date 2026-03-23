# PARLIAMENTARY CONSISTUENCY BASIS!!! 


# This could take a while...

# Model Scenarios - Whole country
library(tidyverse)
library(sf)
library(plotly)
library(readxl)
library(scales)




# FIRSTLY PREP THE DATA
# We need the LSOA data, BUS data, ECO data and heat pump suitability - all of these have been cleaned in script data_cleaning

# NESTA suitability per pc ....

heatpump_suitability_per_pc <- read_csv("data/processed_data/nesta_suitability_by_pc.csv")


# Boiler Upgrade Scheme Statistics

bus_per_pc <- read_csv("data/processed_data//bus_per_pc.csv")


# Energy Company Obligation Statistics


eco_per_pc <- read_csv("data/processed_data/eco_per_pc.csv")


# We also need to make sure that if we reach a point where 100% of heatpumps no more are added...

# For this we need the total number of household estimates


pc_number_of_households <- read_csv("data/processed_data/pc_number_of_households.csv") 

pc_population_stats <- read_csv("data/processed_data/pc_population_stats.csv") |> 
  rename(population_per_pc = total)



# Join together the BUS, the ECO and suitability scores....

PC_hp_suitability_installs <- heatpump_suitability_per_pc |>
  left_join(bus_per_pc,
            join_by(PCON25CD == area_codes)) |>
  left_join(eco_per_pc, join_by(PCON25CD == area_codes)) |>
  mutate(total_heat_pumps_per_pc = BUS_heatpumps_per_pc + ECO_total_heat_pumps) 



# And the totals that these need to add up to... based on CCC estimates


# CCC estimations given as % of household so need total across UK
household_total <- pc_number_of_households |>
  summarise(total_households = sum(households_per_pc)) |>
  pull()



# <--- ASSUME HOUSEHOLD NUMBER CHANGING AT CONSTANT RATE-- see script projecting_household_numbers.R
household_changes <- read.csv("data/processed_data/household_number_increases_total.csv") |>
  mutate(year = as_date(as.character(year), format = "%Y"))


# Read in CCC data - taken from Figure 7.2.4 Key indicators for the residential buildings . From the dataset The-Seventh-Carbon-Budget-Charts-and-data-in-the-report
future_heat_pumps_percentages <- read_csv("data/raw_data/CCC_seventh_carbon_budget/ccc_uptake.csv") |>
  rename(year = Year) |>
  select(year, percentage) |>
  left_join(household_changes) |>
  select(-growth_index) |> 
  filter(year >= "2023-01-01" &
           year <= "2050-01-01") # Just filter to get the 2024 to 2040 data




# Estimate 2024 data by linear interpolation .... as this is missed out in the CCC data

heat_pump_number_2024 <- data.frame(
  year = "2024-01-01",
  percentage = (future_heat_pumps_percentages$percentage[1] + future_heat_pumps_percentages$percentage[2]) / 2,
  number_of_households = (future_heat_pumps_percentages$number_of_households[1] + future_heat_pumps_percentages$number_of_households[2]) / 2
)



# Convert from heat pump percentages to actual numbers using the populations


future_heat_pumps_numbers <- future_heat_pumps_percentages |>
  rbind(heat_pump_number_2024) |>
  arrange(year) |>
  mutate(cumulative_heat_pump_number = percentage / 100 * (number_of_households)) |>
  mutate(heat_pump_number = c( # Calculate the heat pump installs per year (apart from first year)
    cumulative_heat_pump_number[1] / 2,
    diff(cumulative_heat_pump_number)
  ))   



# ------- Now need to produce the model!!! -------


# Prep the df
ordered_with_relative_probabilities <- PC_hp_suitability_installs |>
  select(
    -c(
      PCON25NM,
      air_source_heat_pumps,
      ground_source_heat_pumps,
      hybrid_heat_pumps
    )
  ) |>
  left_join(pc_population_stats, by = "PCON25CD") |>
  left_join(pc_number_of_households, by = "PCON25CD") |> 
  rename(initial_number_of_households = households_per_pc) |> # this is the number of households we are starting with...
  ungroup() |> 
  mutate(
    # For where the ECO scheme has 0 heat pumps currently, making this 1 so a probability is assigned.
    ECO_total_heat_pumps = ifelse(ECO_total_heat_pumps == 0, 1, ECO_total_heat_pumps),
    # Check denominators before dividing.... should not get 0 here but would make the next step error
    suitability_sum = sum(combined_heat_pump_suitability, na.rm = TRUE),
    BUS_sum = sum(BUS_heatpumps_per_pc, na.rm = TRUE),
    ECO_sum = sum(ECO_total_heat_pumps, na.rm = TRUE), 
    total_hp_both_scheme_sum = sum(total_heat_pumps_per_pc, na.rm = TRUE),
  ) |>
  mutate(
    suitability_probability = if_else(suitability_sum > 0,combined_heat_pump_suitability / suitability_sum, 0),
    BUS_pc_probability = if_else(BUS_sum > 0, BUS_heatpumps_per_pc / BUS_sum, 0),
    ECO_pc_probability = if_else(ECO_sum > 0, ECO_total_heat_pumps / ECO_sum, 0),
    total_hp_pc_probability = ifelse(total_hp_both_scheme_sum > 0, total_heat_pumps_per_pc/total_hp_both_scheme_sum, 0), 
    # Calculate combined scores
    BUS_only = BUS_pc_probability,
    ECO_only = ECO_pc_probability,
    present_day_scenario = total_hp_pc_probability, 
   #nox_targeted = total_hp_pc_probability * nox_weight / sum(total_hp_pc_probability * nox_weight, na.rm = TRUE), 
  ) |>
  arrange(desc(suitability_probability)) |>
  # Remove unnecessary rows
  select(
    -c(
      combined_heat_pump_suitability,
      BUS_heatpumps_per_pc,
      ECO_total_heat_pumps,
      BUS_pc_probability,
      ECO_pc_probability,
      total_hp_pc_probability,
      suitability_sum,
      BUS_sum,
      ECO_sum,
      total_hp_both_scheme_sum
    )
  ) |>
  # Reshaping to get in tidy format
  pivot_longer(
    cols = c(
      suitability_probability,
      BUS_only,
      ECO_only,
      present_day_scenario
    ),
    names_to = "model_run",
    values_to = "probability"
  ) |>
  filter(!is.na(probability), probability > 0)  # Remove zero probabilities (shouldnt be any....)



# Validation check
cat(
  "Data prepared successfully. Total PCs:",
  n_distinct(ordered_with_relative_probabilities$PCON25CD),
  "\n"
)
cat("Model runs:", paste(
  unique(ordered_with_relative_probabilities$model_run),
  collapse = ", "
), "\n")

# Model runs to loop through
model_runs <- unique(ordered_with_relative_probabilities$model_run)

# Empty list to store results
results <- list()

# Set number of Monte Carlo draws (1 for testing, 10000 for production)
n_draws <- 100  # Change to 10000 (or however many you want) for full model
hard_cap_start <- 0.70
hard_cap_end <- 0.80



# Household growth rate given by.... 

# Estimated household growth rate for England and Wales- see script projecting_household_numbers
household_growth_rate <- household_changes |> 
  select(year, growth_index) |> 
  filter(year != "2022-01-01")



cat(
  "Starting model runs with",
  n_draws,
  "Monte Carlo draws per year...\n
    and a variable annual household growth rate of",
  household_growth_rate$growth_index * 100,
  "%...\n
    with a threshold for market saturation in an PC of",
  (hard_cap_end) * 100,
  "%...\n"
)

# Progress tracking
total_iterations <- length(model_runs) * nrow(future_heat_pumps_numbers)
current_iteration <- 0

for (model in model_runs) {
  cat("Running model:", model, "\n")
  
  df <- ordered_with_relative_probabilities |>
    filter(model_run == model) |>
    mutate(cumulative_heat_pumps = total_heat_pumps_per_pc)
  
  # Validate model data
  if (nrow(df) == 0) {
    warning(paste("No data for model:", model))
    next
  }
  
  # Check probability sum
  prob_sum <- sum(df$probability, na.rm = TRUE)
  if (abs(prob_sum - 1) > 1e-10) {
    warning(paste(
      "Probabilities don't sum to 1 for model",
      model,
      ". Sum =",
      prob_sum
    ))
  }
  
  # Loop over each year
  for (i in 1:nrow(future_heat_pumps_numbers)) {
    current_iteration <- current_iteration + 1
    # To get a progress update every after approx 5%
    if (current_iteration %% 6 == 0) {
      cat("Progress:",
          round(current_iteration / total_iterations * 100, 1),
          "% \n")
    }
    
    year <- future_heat_pumps_numbers$year[i] |>
      substr(1, 4)
    
    target_installations <- future_heat_pumps_numbers$heat_pump_number[i]
    
    growth_rate_household <- household_growth_rate$growth_index[i]
    
    # Skip if no installations planned
    if (target_installations <= 0) {
      df <- df |>
        mutate(
          !!paste0("heat_pump_number_", year) := 0,!!paste0("heat_pump_number_upper_bound_", year) := 0,!!paste0("heat_pump_number_lower_bound_", year) := 0,!!paste0("number_of_households_", year) :=
            round(
              initial_number_of_households * (growth_rate_household)
            )
        )
      next
    }
    
    # Update the data frame for each year's model results
    df <- df |>
      # Calculate household growth (ensure minimum of 1 household)
      mutate(!!paste0("number_of_households_", year) :=
               pmax(1, round(
                 initial_number_of_households * (growth_rate_household),
                 digits = 0
               ))) |>
      # And ensure that the number of households without heat pumps cannot go below 0
      mutate(households_without_heat_pumps =
               pmax(0, .data[[paste0("number_of_households_", year)]] - cumulative_heat_pumps)) |>
      
      # Calculate availability ratio for threshold check
      mutate(household_availability_ratio =
               households_without_heat_pumps / .data[[paste0("number_of_households_", year)]]) |>
      
      # Apply smooth transition instead of hard threshold
      mutate(
        # Calculate current market penetration
        market_penetration = cumulative_heat_pumps / .data[[paste0("number_of_households_", year)]],
        
        # Logistic constraint (slows as penetration increases)
        k_logistic = 8,
        hard_cap_start = hard_cap_start,
        hard_cap_end = hard_cap_end,
        midpoint_logistic = 0.6,
        logistic_factor = 1 / (1 + exp(k_logistic * (market_penetration - midpoint_logistic))),
        
        # Hard cap at high penetration (but smoothed)

        hard_cap_factor = case_when(
          market_penetration <= hard_cap_start ~ 1,
          market_penetration >= hard_cap_end ~ 0,
          TRUE ~ (hard_cap_end - market_penetration) / (hard_cap_end - hard_cap_start)
        ),
        
        # Combine both constraints
        combined_factor = logistic_factor * hard_cap_factor,
        
        # Apply to probability
        heat_pump_probability_household_weighted =
          households_without_heat_pumps * probability * combined_factor
      )
    # Check if any areas are available for installations
    total_weighted_prob <- sum(df$heat_pump_probability_household_weighted, na.rm = TRUE)
    
    if (total_weighted_prob > 0) {
      # Normalise probabilities for multinomial distribution...
      df <- df |>
        mutate(normalised_probability = heat_pump_probability_household_weighted / total_weighted_prob)
      
      # Monte Carlo simulation with error handling
      tryCatch({
        # For production (n_draws = 10000), this will generate proper confidence intervals - keep to smaller number when testing
        
        heat_pump_matrix <- rmultinom(n_draws,
                                      target_installations,
                                      df$normalised_probability)
        
        # Cap based on TOTAL households, not remaining
        max_additional_installs <- pmax(0, 
                                        (df[[paste0("number_of_households_", year)]] * 0.85) - 
                                          df$cumulative_heat_pumps)
        
        heat_pump_matrix_household_capped <- pmin(heat_pump_matrix,
                                                  max_additional_installs)
        

        # Add summary statistics directly
        df <- df |>
          mutate(
            !!paste0("heat_pump_number_", year) := round(rowMeans(
              heat_pump_matrix_household_capped
            ), 0),!!paste0("heat_pump_number_upper_bound_", year) := round(
              apply(heat_pump_matrix_household_capped, 1, quantile, 0.975),
              0
            ),!!paste0("heat_pump_number_lower_bound_", year) := round(
              apply(heat_pump_matrix_household_capped, 1, quantile, 0.025),
              0
            )
          ) |>
          select(-normalised_probability)
        
      }, error = function(e) {
        warning(paste(
          "Error in Monte Carlo simulation for year",
          year,
          ":",
          e$message
        ))
        # Fallback: deterministic allocation - hopefully does not need to be used....
        df <<- df |>
          mutate(
            deterministic_allocation = round(target_installations * normalised_probability),!!paste0("heat_pump_number_", year) := deterministic_allocation,!!paste0("heat_pump_number_upper_bound_", year) := deterministic_allocation,!!paste0("heat_pump_number_lower_bound_", year) := deterministic_allocation
          ) |>
          select(-deterministic_allocation, -normalised_probability)
      })
      
    } else {
      # No installations possible
      warning(paste(
        "No eligible areas for installations in year",
        year,
        "for model",
        model
      ))
      df <- df |>
        mutate(
          !!paste0("heat_pump_number_", year) := 0,!!paste0("heat_pump_number_upper_bound_", year) := 0,!!paste0("heat_pump_number_lower_bound_", year) := 0
        )
    }
    
    # Update cumulative installations and validate
    df <- df |>
      mutate(
        new_installations = .data[[paste0("heat_pump_number_", year)]],
        cumulative_heat_pumps = cumulative_heat_pumps + new_installations,
        # Ensure we don't exceed total households
        cumulative_heat_pumps = pmin(cumulative_heat_pumps, .data[[paste0("number_of_households_", year)]] * 0.85)
      ) |>
      select(-new_installations)
    
    # Clean up temporary columns
    df <- df |>
      select(
        -households_without_heat_pumps,
        -household_availability_ratio,
        -heat_pump_probability_household_weighted,
        -logistic_factor, 
        -combined_factor,
        -hard_cap_factor,
        -k_logistic, 
        -midpoint_logistic, 
        -hard_cap_start, 
        -hard_cap_end, 
        -market_penetration
        
      )
  }
  
  # Store results and feedback validation
  if (nrow(df) > 0) {
    results[[as.character(model)]] <- df
    cat("Model",
        model,
        "completed successfully with",
        nrow(df),
        "PCs\n")
  } else {
    warning(paste("No results for model:", model))
  }
}

# Combine results with error checking
if (length(results) > 0) {
  results_df <- bind_rows(results)
  cat("All models completed. Total rows in results:",
      nrow(results_df),
      "\n")
} else {
  stop("No model results generated. Check input data and parameters.")
}

# Reshaping the data to easier to read and plot form with improved error handling
model_results <- results_df |>
  select(-cumulative_heat_pumps) |> 
  # regex pattern matching
  pivot_longer(
    cols = matches(
      "^(heat_pump_number(_upper_bound|_lower_bound)?|number_of_households)_\\d{4}$"
    ),
    names_to = c("variable", "year"),
    names_pattern = "(.*)_(\\d{4})$",
    values_to = "value"
  ) |>
  # Handle missing values
  filter(!is.na(value)) |>
  pivot_wider(names_from = "variable", values_from = "value") |>
  # Improved date parsing
  mutate(year = lubridate::as_date(paste0(year, "-01-01"))) |>
  group_by(PCON25CD, model_run) |>
  arrange(year) |>
  mutate(
    cumulative_heat_pump_number = cumsum(heat_pump_number) + total_heat_pumps_per_pc,
    # Ensure upper bound doesn't exceed total households
    cumulative_heat_pump_number_upper_bound = pmin(
      cumsum(heat_pump_number_upper_bound) + total_heat_pumps_per_pc,
      number_of_households
    ),
    cumulative_heat_pump_number_lower_bound = cumsum(heat_pump_number_lower_bound) + total_heat_pumps_per_pc,
    # Calculate percentage with bounds checking
    pct_of_households_with_heat_pump = cumulative_heat_pump_number / number_of_households
  ) |>
  ungroup() |> 
  group_by(PCON25CD, model_run) |>
  mutate(heat_pump_years = cumsum(cumulative_heat_pump_number)) |> 
  ungroup() |> 
  select(-PCON25NM.x) |> 
  rename(PCON25NM = PCON25NM.y)


# Final validation
cat(
  "Final results summary:\n - Total PCs:",
  n_distinct(model_results$PCON25CD),
  "\n - Models:",
  paste(unique(model_results$model_run), collapse = ", "),
  "\n - Years:",
  paste(unique(year(model_results$year)), collapse = ", "),
  "\n- Max heat pump percentage:",
  round(
    max(model_results$pct_of_households_with_heat_pump, na.rm = TRUE) * 100,
    2
  ),
  "%\n"
)

# Check for any issues....
na_count <- sum(is.na(model_results$cumulative_heat_pump_number))
if (na_count > 0) {
  warning(paste("Found", na_count, "NA values in cumulative heat pump numbers"))
}

over_100_pct <- sum(model_results$pct_of_households_with_heat_pump > 1.001,
                    na.rm = TRUE)
if (over_100_pct > 0) {
  warning(paste(
    "Found",
    over_100_pct,
    "cases where heat pump percentage exceeds 100%"
  ))
}



# And write to disk for future analysis....

write.csv(
  model_results,
  "data/processed_data/model_results_per_pc.csv"
)






# And then just a few initial plots to check what the data looks like...! This looks promising!

sample_dataset <- sample_n(results_df, 5)


sample_data_for_plot <- model_results |>
  filter(PCON25CD %in% sample_dataset$PCON25CD)



ggplot(sample_data_for_plot) +
  geom_ribbon(
    aes(
      x = year,
      ymax = cumulative_heat_pump_number_upper_bound,
      ymin = cumulative_heat_pump_number_lower_bound,
      fill = model_run
    ),
    alpha = 0.2
  ) +
  geom_point(aes(
    x = year,
    y = cumulative_heat_pump_number,
    colour = model_run,
    group = model_run
  )) +
  geom_line(aes(
    x = year,
    y = cumulative_heat_pump_number,
    colour = model_run,
    group = model_run
  )) +
  geom_line(aes(x = year, y = number_of_households), linetype = "dashed") +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(
    cols = vars(PCON25NM),
    rows = vars(model_run),
    scales = "free"
  )





ggplot(sample_data_for_plot) +
  geom_ribbon(
    aes(
      x = year,
      ymax = heat_pump_number_upper_bound,
      ymin = heat_pump_number_lower_bound,
      fill = model_run
    ),
    alpha = 0.2
  ) +
  geom_point(aes(
    x = year,
    y = heat_pump_number,
    colour = model_run,
    group = model_run
  )) +
  geom_line(aes(
    x = year,
    y = heat_pump_number,
    colour = model_run,
    group = model_run
  )) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(
    cols = vars(PCON25NM),
    rows = vars(model_run),
    scales = "free"
  )





sample_data_for_plot |> 
  ggplot() +
  geom_line(aes(x = year, y = pct_of_households_with_heat_pump * 100, colour = model_run)) +
  geom_point(aes(x = year, y = pct_of_households_with_heat_pump * 100, colour = model_run)) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~PCON25NM)




