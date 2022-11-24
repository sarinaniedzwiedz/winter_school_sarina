# Taming data
# Sarina
# 23.11.2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

if(!require(ggsn)){install.packages("ggsn")}
library(ggsn)
if(!require(cowplot)){install.packages("cowplot")}
library(cowplot)


# Data --------------------------------------------------------------------

# SST time series
sst_NOAA <- read_csv("course_material/data/sst_NOAA1.csv")
View(sst_NOAA)

# Global SST layer for 2022-01-01


# Example -----------------------------------------------------------------

# All five functions at once
sst_NOAA %>% 
  arrange(site, temp) %>% 
  select(t, temp) %>% 
  filter(temp >= 23) %>% 
  mutate(year = year(t)) %>% 
  summarise(mean_year = mean(year))


# Exercise 1 --------------------------------------------------------------

# Filter sst_NOAA to have only data for WA from 2005-2010
# Plot as a line plot
# Combine or inset with a map of Western Australia

sst_NOAA_WA_2005 <- sst_NOAA %>% 
  filter(site == "WA") %>% 
  filter(year(t) >= "2005", year(t) <= "2010")
View(sst_NOAA_WA_2005)

plot_1 <- ggplot() +
  geom_line(data = sst_NOAA_WA_2005, aes(x = t, y = temp))
plot_1

# Map

load("course_material/data/OISST_2022.RData")
map_data_world <- map_data("world")

map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  # Why +2000? => put all the values above 180 into a separate group
  # we can always include this into the mapping, as it won't hurt
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))
View(map_global_fix)

map_Australia <- map_global_fix %>% 
  filter(region == "Australia" ) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group),fill = "grey70", colour = "black") +
  coord_quickmap(xlim = c(110, 130), ylim = c(-36, -10)) 
map_Australia

fig <- ggdraw() +
  draw_plot(map_Australia, x = 0.2) +
  draw_plot(plot_1, x = 0.02, y = 0.4, width = 0.5, height = 0.3) 
fig



# Exercise 2 --------------------------------------------------------------

# Create an informative table of the 10 highest monthly temperature in the Med
# Inset this onto a map of the Med with the sea surface temperature from 'OISST_2022'

sst_NOAA_Med <- filter(sst_NOAA, site == "Med") %>% 
  group_by(year(t), month(t)) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) 
View(sst_NOAA_Med)

sst_NOAA_Med %>% arrange(desc(mean_temp))


# Exercise 3 --------------------------------------------------------------

# Plot the the annual mean temperatures of NW_Atl as a bar plot
# Inset a map of Atlantic Canada into the corner of the bar plot


# BONUS -------------------------------------------------------------------

# Find the mean temperature for 2002 in Med and 2005 in WA in the same code chunk
# Hint: The case_when() function will be useful for this
# In another single code chunk, extract the country shapes for Italy and Australia
# Inset plots of the Med temperatures over Italy, and WA over Australia
# Combine into one figure

