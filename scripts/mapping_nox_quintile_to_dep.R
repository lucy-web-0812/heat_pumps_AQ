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
  #group_by(new_ranking_quintile_deprivation) |>
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


totals <- plot_data |>
  group_by(new_ranking_quintile_deprivation) |>
  summarise(
    total_pop = sum(total_population_PC)
  )


totals_nox <- plot_data |>
  group_by(nox_conc_quintile) |>
  summarise(
    total_pop = sum(total_population_PC)
  )




plot_data |> 
  ungroup() |> 
 # group_by(new_ranking_quintile_deprivation) |> 
  mutate(
    prop = total_population_PC / sum(total_population_PC),
    label = scales::percent(prop, accuracy = 0.1)
  ) |> 
  mutate(text_colour = ifelse(
    scales::rescale(total_population_PC) > 0.5,
    "white",
    "black"
  )) |> 
  ggplot() +
  geom_tile(
    aes(x = nox_conc_quintile, y = new_ranking_quintile_deprivation, fill = prop),
    #height= 0.8,
    colour = "white"
  ) +
  geom_text(
    aes(
      x = nox_conc_quintile,
      y = new_ranking_quintile_deprivation,
      label = label,
      colour = text_colour
    )) +
  geom_text(
    data = totals,
    aes(
      y = new_ranking_quintile_deprivation,
      x = 5.6,
      colour = "grey30", 
      label = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.01)(total_pop)
    ),
    inherit.aes = FALSE,
    size = 5,
    hjust = 0
  ) +
  geom_text(
    data = totals_nox,
    aes(
      x = nox_conc_quintile - 0.25,
      #vjust = -0.1,
      y = 0.2,
      colour = "grey30", 
      label = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.01)(total_pop)
    ),
    inherit.aes = FALSE,
    size = 5,
    hjust = 0
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = "NOx Concentration Quintile", 
                     breaks = c(1:5),
                     limits = c(0.45, 6.5), 
                     labels = c("1 - Least \nPolluted", "2", "3", "4", "5 - Most\n Polluted"), 
                     position = "top") +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.75, 5.5), 
                     name = "Relative Deprivation Quintile", 
                     breaks = c(1:5),
                     labels = c("1 - Most\n Deprived", "2", "3", "4", "5 - Least\n Deprived")) +
  scale_fill_distiller(palette = "Greens", direction = 1,
    labels = scales::label_percent(),
    name = "Population"
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
  annotate(
    "text",
    x = 6.4,
    y = 5.4,
    label = "population in deprivation quintile",
    fontface = "italic",
    hjust = 0, 
    angle = -90, 
    size = 4
  ) +
  annotate(
    "text",
    x = 1.25,
    y = -0.15,
    label = "population in NOx quintile", 
    fontface = "italic", 
    size = 4
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.y = element_line(linewidth = 30, colour = "white"),
    legend.position = "none",
        legend.justification = "left",
        panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"))
 



ggsave("plots/paper_plots/nox_quintile_to_deprivation.png")
