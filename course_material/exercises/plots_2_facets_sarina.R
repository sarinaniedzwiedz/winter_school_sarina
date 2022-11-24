# Facets
# Sarina
# 21.11.2022


# NB: Create these section breaks by pressing: ctrl+shift+r
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# Basic faceted plot
lm_1 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species)
lm_1

# Basic combined plot
ggarrange(lm_1, lm_1)


# Exercise 1 --------------------------------------------------------------

# Create a new plot type and facet by gender
plot_1 <- ggplot(data = penguins, 
       aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(color = species), size = 3, shape = 7) +
  facet_wrap(~year)
plot_1


# Exercise 2 --------------------------------------------------------------

# Create a new plot type and facet by two categories
plot_2 <- ggplot(data = penguins, 
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_boxplot(aes(fill = species)) +
  facet_wrap(sex~year)
plot_2


# Exercise 3 --------------------------------------------------------------

# Combine all of the plots you've created so far
# Save them as a high-res file larger than 2 MB
grid_2 <- ggarrange(plot_1, plot_2,
                    # Set number of rows and columns
                    ncol = 2, nrow = 1,
                    # Label each figure
                    labels = c("a)", "b)"),
                    # Create common legend
                    common.legend = TRUE,
                    # Set legend position
                    legend = "bottom")
grid_2
ggsave(plot = grid_2, filename = "figures/grid_2.png", 
       width = 10, height = 8, dpi = 2000)



# BONUS -------------------------------------------------------------------

# Use a different package to combine plots

par(mfrow=c(2,1))
plot_1
plot_2

library(gridExtra)
grid.arrange(plot_1, plot_2, ncol = 2)
