# Script name
# Author
# Date


# NB: Create these section breaks by pressing: ctrl+shift+r
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# Discrete viridis colour palette
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) +
  scale_colour_viridis_d(option = "A")

# Compare species
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill = species), show.legend = F) +
  stat_compare_means(method = "anova")


# Exercise 1 --------------------------------------------------------------

# Create your own continuous and discrete colour palettes
# Create and combine two figures, each using a different palette
# continuous
plot_1 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) + 
  scale_colour_gradientn(colours = c("#C07742", "#BC913C", "#AEAD45", "#96C762" ,"#75DE90" ,"#4CF3C8"))
##C07742,#BC913C,#AEAD45,#96C762,#75DE90,#4CF3C8

# discreete
plot_2 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) + 
  scale_colour_manual(values = c("#39475D", "#82597B", "#EA7A75"))
##39475D,#5C516F,#82597B,#A96180,#CC6B7D,#EA7A75

# combine both figures
grid_3 <- ggarrange(plot_1, plot_2,
                   # Set number of rows and columns
                   ncol = 2, nrow = 1,
                   # Label each figure
                   labels = c("a)", "b)"),
                   # Create common legend
                   common.legend = FALSE,
                   # Set legend position
                   legend = "bottom")
grid_3


# Exercise 2 --------------------------------------------------------------

# Create two versions of the same figure and combine
# Use a viridis colour palette against a default palette in a way 
# that allows features in the data to be more pronounced
plot_3 <- ggplot(data = penguins,
                 aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) + 
  scale_colour_viridis_c(option = "A")
plot_3
##C07742,#BC913C,#AEAD45,#96C762,#75DE90,#4CF3C8

# discreete
plot_4 <- ggplot(data = penguins,
                 aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) 
plot_4
# combine both figures
grid_4 <- ggarrange(plot_3, plot_4,
                    # Set number of rows and columns
                    ncol = 2, nrow = 1,
                    # Label each figure
                    labels = c("a)", "b)"),
                    # Create common legend
                    common.legend = FALSE,
                    # Set legend position
                    legend = "bottom")
grid_4




# Exercise 3 --------------------------------------------------------------

# Plot and combine t-test and ANOVA stats using sst_NOAA
# See this site for more info on plotting stats:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
 sst_NOAA <- read_csv("course_material/data/sst_NOAA1.csv")


# Or from excel
# sst_NOAA <- read_csv2("course_material/data/sst_NOAA1.csv")

# or from anything; always when we save excel in .csv it deliminates columns by a ;
# sst_NOAA <- read_delim("course_material/data/sst_NOAA1.csv", delim = ";")

# Look at data set
head(sst_NOAA)
tail(sst_NOAA)

# quick summaries
glimpse(sst_NOAA)
summary(sst_NOAA)


sst_monthly <- sst_NOAA %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(site, month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))

ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = NULL, y = "Temperature (°C)")

sst_NOAA$site <- as.factor(sst_NOAA$site)
site_levels <- levels(sst_NOAA$site)
my_comparisons <- list(c(site_levels[1], site_levels[2]), 
                       c(site_levels[2], site_levels[3]),
                       c(site_levels[1], site_levels[3]))
# Then we stack it all together
ggplot(data = sst_NOAA, aes(x = site, y = temp)) +
  geom_boxplot(aes(show.legend = F), fill = "grey40") +
  stat_compare_means(method = "anova", colour = "grey50",
                     label.x = 1.8, label.y = 32) +
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons,
                     # hight on which the p values are
                     label.y = c(32, 34, 36)) +
  # Perform t-tests between each group and the overall mean
  stat_compare_means(label = "p.signif", 
                     method = "t.test",
                     ref.group = ".all.") + 
  # Add horizontal line at base mean of all groups
  geom_hline(yintercept = mean(sst_NOAA$temp, na.rm = T), 
             linetype = 2) + 
  labs(y = "Temperature (°C)", x = "Site") +
  theme_bw()


ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = "", y = "Temperature (°C)") +
  facet_wrap(~site, ncol = 1) # Create panels





# BONUS -------------------------------------------------------------------

# Create a correlogram between the two northern hemisphere sites in sst_NOAA







