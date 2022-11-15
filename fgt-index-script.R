#####################################################################################
#####################################################################################
############################# FGT Poverty Index #####################################
#####################################################################################
#####################################################################################



# Importing packages ----------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(rlang)
library(ggthemes)
library(patchwork)
library(geobr)


# Data and functions ------------------------------------------------------

import_fgt_data <- function(path_name = ""){
  x_df <- geobr::read_state()
  y_df <- readxl::read_xlsx(path = path_name)
  
  fgt_data_joined <- full_join(
    x = x_df,
    y = y_df,
    by = c("name_state" = "uf")
  )
  return(fgt_data_joined)
}

fgt_plot <- function(fgt_data, fill_col, fill_name_lab = ""){
  ggplot(data = {{fgt_data}})+
    geom_sf(data = {{fgt_data}}, aes(fill = {{fill_col}}), color = NA, size = .15)+
    theme_map()+
    theme(
      legend.position = "left",
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.title = element_text(vjust = 0.5),
      legend.box.background = element_rect(color = "black", size = 0.8),
      legend.box.margin = margin(.1, .1, .1, .1),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )+
    geom_sf_text(
      data = {{fgt_data}} %>% 
        mutate(name_state = ifelse(
          name_state == "Distrito Federal", "DF", name_state)),
      aes(label = name_state, 
          hjust = 0.5, 
          vjust = 0.5),
      size = 3,
      check_overlap = TRUE)+
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8,"YlOrRd"), na.value = "#A9A9A9")+
    labs(fill = fill_name_lab)
  
}


# Ploting FGT poverty index by state  -------------------------------------

## Households living below the poverty line R$ 499,00 

fgt_data_pl1 <- import_fgt_data(path_name = "fgt_index_1.xlsx")

fgt_index_per_state_pl1 <- fgt_plot(
  fgt_data = fgt_data_pl1,
  fill_col = fgt_index,
  fill_name_lab = "FGT Index"
)+
  annotate(
    geom = "curve",
    x = -40,
    y = 3,
    xend = -44,
    yend = -2,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  )+
  annotate(
    geom = "text",
    x = -40, y = 3.5,
    label = "Poverty rate: 22.16%",
    fontface = "bold.italic",
    family = "sans",
    size = 3.5
  )

plot(fgt_index_per_state_pl1)

ggsave(
  plot = fgt_index_per_state_pl1,
  filename = "fgt_index_per_state_pl1.pdf",
  width = 10,
  height = 6,
  dpi = 150,
  units = "in"
)


## Households living below the poverty line R$ 1.364,46

fgt_data_pl2 <- import_fgt_data(path_name = "fgt_index_2.xlsx")

fgt_index_per_state_pl2 <- fgt_plot(
  fgt_data = fgt_data_pl2,
  fill_col = fgt_index,
  fill_name_lab = "FGT Index"
  )+
  annotate(
    geom = "curve",
    x = -40,
    y = 3,
    xend = -44,
    yend = -2,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  )+
  annotate(
    geom = "text",
    x = -40, y = 3.5,
    label = "Poverty rate: 48.30%",
    fontface = "bold.italic",
    family = "sans",
    size = 3.5
  )

plot(fgt_index_per_state_pl2)

ggsave(
  plot = fgt_index_per_state_pl2,
  filename = "fgt_index_per_state_pl2.pdf",
  width = 10,
  height = 6,
  dpi = 150,
  units = "in"
)

## Comparing the two maps 

maps_joined <- fgt_index_per_state_pl1 + fgt_index_per_state_pl2 +
  patchwork::plot_annotation(
    title = "Percentage of Households living below Povert Line in Brazilian States in 2019",
    subtitle = "Calculated through Foster, Greer & Thorbecke Poverty Index. Were used two poverty lines: 499 BRL and 1,364.46 BRL",
    caption = "Elaborated by Fabrício Ferreira (@fabr_ferreira) with PNADC microdata."
  )

plot(maps_joined)

ggsave(
  plot = maps_joined,
  filename = "fgt-index-poverty-states-br.pdf",
  width = 12,
  height = 8,
  dpi = 300,
  units = "in"
)
