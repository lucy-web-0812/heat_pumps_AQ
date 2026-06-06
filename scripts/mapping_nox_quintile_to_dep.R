# Look at how many of each deprivation quintile are in each NOx quintile 


library(tidyverse)
library(paletteer)


pc_combined_dataset <- read_csv("data/processed_data/pc_combined_dataset.csv")



plot_data <- pc_combined_dataset |>
  group_by(
    new_ranking_quintile_deprivation,
    nox_conc_quintile
  ) |>
  summarise(
    total_population_PC = sum(total_population_PC),
    .groups = "drop"
  ) |>
  group_by(new_ranking_quintile_deprivation) |>
  mutate(
    prop = total_population_PC / sum(total_population_PC),
    label = scales::percent(prop, accuracy = 1)
  ) 


ggplot(plot_data) +
  geom_col(
    aes(
      x = new_ranking_quintile_deprivation,
      y = total_population_PC * 100 ,
      fill = fct_rev(factor(nox_conc_quintile)),
      #colour = nox_conc_quintile
    ),
    position = "fill", 
    alpha = 0.8
  ) +
  geom_text(
    aes(
      x = new_ranking_quintile_deprivation,
      y = prop,
      label = label,
      group = forcats::fct_rev(factor(nox_conc_quintile))
    ),
    position = position_fill(vjust = 0.5),
    colour = "black",
    size = 4
  ) +
  scale_fill_paletteer_d(
    "calecopal::lupinus",
    name = "NOx Concentration Quintile",
    direction = 1,
   ) +
  guides(fill = guide_legend(reverse = TRUE)) +
      scale_y_continuous(expand = c(0, 0),
                         name = "Population (%)", 
                         labels = c(0,25,50,75,100)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0.45, 5.55), 
                         name = "Relative Deprivation Quintie", 
                         breaks = c(1:5),
                         labels = c("1 - Most\n Deprived", "2", "3", "4", "5 - Least\n Deprived")) +
  coord_flip() +
      theme_classic() +
    theme(legend.position = "top", 
          panel.grid.major.x = element_line(colour = "lightgrey"),
          panel.grid.minor.x = element_line(colour = "lightgrey")) 



# How about a heat map of where everyone lives? 

plot_data |> 
  ungroup() |> 
  mutate(
    prop = total_population_PC / sum(total_population_PC),
    label = scales::percent(prop, accuracy = 1)
  ) |> 
  mutate(text_colour = ifelse(
    scales::rescale(total_population_PC) > 0.5,
    "white",
    "black"
  )) |> 
  ggplot() +
  geom_tile(
    aes(x = nox_conc_quintile, y = new_ranking_quintile_deprivation, fill = total_population_PC),
    colour = "white"
  ) +
  geom_text(
    aes(
      x = nox_conc_quintile,
      y = new_ranking_quintile_deprivation,
      label = paste0(
        sprintf("%.2f", total_population_PC / 1e6),
        "M"
      ),
      colour = text_colour
    )) +
      scale_x_continuous(expand = c(0, 0), 
                     name = "NOx Concentration Quintie", 
                     breaks = c(1:5),
                     labels = c("1 - Least \nPolluted", "2", "3", "4", "5 - Most\n Polluted")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.45, 5.55), 
                     name = "Relative Deprivation Quintie", 
                     breaks = c(1:5),
                     labels = c("1 - Most\n Deprived", "2", "3", "4", "5 - Least\n Deprived")) +
  scale_fill_distiller(palette = "Greens", direction = 1,
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    name = "Population (millions)"
  ) +
  scale_colour_identity() +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.75, "cm"), 
      frame.colour = "darkgrey",
      ticks.colour = "white"
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    #legend.position = "none",
        legend.justification = "left",
        panel.grid = element_blank())
 



ggsave("plots/paper_plots/nox_quintile_to_deprivation.png")
