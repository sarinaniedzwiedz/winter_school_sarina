# Excercise maps 1 - Arctic Maps workflow
# Sarina
# 22.11.2022


# Libraries ---------------------------------------------------------------

# Which libraries should be loaded?
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)

if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

if(!require(ggsn)){install.packages("ggsn")}
library(ggsn)

if(!require(palmerpenguins)){install.packages("palmerpenguins")}
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Call the global data to the environment
map_data_world <- map_data("world")


# Example -----------------------------------------------------------------

# The basic map
map_data_world %>% 
  filter(region == "Germany") %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))


# Exercise 1 --------------------------------------------------------------

# Create maps of four regions and combine
# Use a mix of cropping and direct access 
map_data('world') %>% 
  select(region) %>% 
  distinct() %>% 
  arrange(region) 

map_Norway <- map_data_world %>% 
  filter(region == "Norway" ) %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))
head(map_Norway)
View(map_data_world %>% 
       filter(region == "Norway" & subregion == "Svalbard"))

# getting Svalbard from the global map
plot1 <- ggplot() +
  # The global shape file
  borders(fill = "grey70", colour = "black") + # takes global map and plots all borders, so it does not make sense to plot it in top of sth more zoomed in
  # Equal sizing for lon/lat 
  coord_equal(xlim = c(10, 30), ylim = c(75, 82))
plot1

# Svalbard filtering by subregion
plot_Sval_1 <- map_data_world %>% 
  filter(region == "Norway" & subregion == "Svalbard") %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0))
plot_Sval_1

# filtering for group, which is Spitsbergen
plot_Spitsbergen <- map_data_world %>% 
  filter(region == "Norway" & subregion == "Svalbard" & group == "1094") %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0))
plot_Spitsbergen

# Zooming into Isfjorden
plot_Isfjorden <- plot_Spitsbergen +
  coord_cartesian(xlim = c(13.5, 17.5), ylim = c(77.98, 79)) +
  geom_rect(aes(xmin = 13.5, xmax = 17.5, ymin = 77.98, ymax = 79),
            fill = NA, colour = "red", linewidth = 1) +
  annotate("text", label = "Isfjorden", 
           x = 16.9, y = 78.1, size = 4, fontface = "bold.italic", colour = "white") +
  theme_void()
plot_Isfjorden

# zooming into Kongsfjorden
# if you want more point, maybe it is better to create a dataframe; easier to give a legend
plot_Kongsfjorden <- plot_Spitsbergen +
  coord_cartesian(xlim = c(11, 13), ylim = c(78.7, 79.2)) +
  geom_rect(aes(xmin = 11, xmax = 13, ymin = 78.7, ymax = 79.2),
           fill = NA, colour = "blue", linewidth = 1) +
  annotate("text", label = "Kongsfjorden",
           x = 12.6, y = 78.75, size = 4, fontface = "bold.italic", colour = "white") +
  geom_point(aes(x = 11.878833, y = 79.118583), colour = "blue", size = 4) +
  theme_void()
plot_Kongsfjorden

# inserting red rectangles in Svalbard map
plot_Sval_2 <- plot_Sval_1 +
  geom_rect(aes(xmin = 13.5, xmax = 17.5, ymin = 77.98, ymax = 79),
            fill = NA, colour = "red", linewidth = 1) +
  geom_rect(aes(xmin = 11, xmax = 13, ymin = 78.7, ymax = 79.2),
            fill = NA, colour = "blue", linewidth = 1) 
plot_Sval_2

# inserting Isfjorden and Kongsfjorden subplots
if(!require(ggpmisc)){install.packages("ggpmisc")}
library(ggpmisc)
if(!require(cowplot)){install.packages("cowplot")}
library(cowplot)


fig <- ggdraw() +
  draw_plot(plot_Sval_2, x = 0.2) +
  draw_plot(plot_Isfjorden, x = 0.02, y = 0.2, width = 0.3, height = 0.3) +
  draw_plot(plot_Kongsfjorden, x = 0.02, y = 0.6, width = 0.3, height = 0.3) 
  # draw_plot_label(label = "Isfjorden", x = 0, y = 0.57, hjust = -0.5, vjust = 1.5) +
  # draw_plot_label(label = "Kongsfjorden", x = 0, y = 1)
fig

# 
# ggsave(plot = fig, filename = "figures/fig.png",
#        width = 10, height = 7.5)

# Exercise 2 --------------------------------------------------------------

# Create a map that benefits from a scale bar and add a North arrow
# Hint: use annotate("segment", ...) to accomplish this


# Exercise 3 --------------------------------------------------------------

# Create a meaningful inset map


# BONUS -------------------------------------------------------------------

# Plot maps using Google Maps

