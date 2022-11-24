# Plot excersise 1
# Sarina
# 21.11.2021


# NB: Create these section breaks by pressing: ctrl+shift+r
  # A prompt will ask you for the section label
  # Type what you want then press enter
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# The basic plot
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = species))


# Exercise 1 --------------------------------------------------------------

# Create a basic plot with different x and y axes
View(penguins)
ggplot(data = penguins, 
       aes(x = sex, y = flipper_length_mm)) +
  geom_point(aes(color = species))


# Exercise 2 --------------------------------------------------------------

# Change the aes() arguments
ggplot(data = penguins, 
       aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_point(aes(color = island, shape = sex)) +
  scale_shape_manual(values=c(14, 15)) 


# Exercise 3 --------------------------------------------------------------

# Change the labels
ggplot(data = penguins, 
       aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_point(aes(color = island)) +
  labs (x = "Bill length (mm)", y = "Flipper length (mm)", color = "Island")


# BONUS -------------------------------------------------------------------

# Create a ridgeplot
if(!require(ggridges)){install.packages("ggridges")}
library(ggridges)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(viridis)){install.packages("viridis")}
library(viridis)
if(!require(hrbrthemes)){install.packages("hrbrthemes")}
library(hrbrthemes)

ggplot(penguins, aes(x = bill_length_mm, y = species, fill = sex)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )








