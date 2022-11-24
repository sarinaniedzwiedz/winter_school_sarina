# Domesticating data
# Sarina
# 23.11.2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data --------------------------------------------------------------------

# SST data
sst_NOAA <- read_csv("course_material/data/sst_NOAA1.csv")


# Example -----------------------------------------------------------------

# Whatever we can imagine!
sst_NOAA %>%  
  group_by(site) %>%
  summarise(count = n(), 
            count_15 = sum(temp > 20)) %>% 
  mutate(prop_15 = count_15/count) %>% 
  arrange(prop_15) %>% 
  ungroup


# Exercise 1 --------------------------------------------------------------

# Filter two sites and summarise six different statistics

sst_NOAA_1 <- sst_NOAA %>% 
  mutate(month = month(t)) %>% 
  filter(site == "NW_Atl" | site == "Med") %>% 
  group_by(site, month) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            count = n()) %>% 
  ungroup()
sst_NOAA_1

sst_NOAA_2 <- sst_NOAA %>% 
  mutate(month = month(t), day = day(t)) %>% 
  group_by(site) %>% 
  filter(temp > 15) %>% 
  summarise(count = n(), 
            count_15 = sum(temp < 15)) %>% 
  ungroup()
sst_NOAA_2



# Exercise 2 --------------------------------------------------------------

# Find the maximum temperature and SD per year per site
# Plot this as a bar plot with error bars
# Inset a map of each site over each bar plot

sst_NOAA_max1 <- sst_NOAA %>% 
  mutate(year = year(t)) %>% 
  group_by(site, year) %>% 
  summarise(temp_mean = mean(temp, na.rm = T), 
            temp_sd = sd(temp, na.rm = T), 
            temp_max = max(temp, na.rm = T)) %>% 
  ungroup
sst_NOAA_max1

sst_NOAA_max2 <- sst_NOAA %>% 
  mutate(year = year(t)) %>% 
  group_by(site) %>% 
  summarise(temp_mean = mean(temp, na.rm = T), 
            temp_sd = sd(temp, na.rm = T), 
            temp_max = max(temp, na.rm = T)) %>% 
  ungroup
sst_NOAA_max2

plot_1 <- ggplot() +
  geom_bar(data = sst_NOAA_max1, aes(x = year, y = temp_mean, fill = site), 
           stat = "identity", position = "dodge", width = 0.7) +
  
  geom_errorbar(data = sst_NOAA_max1, aes(x = year,
                                         ymin = temp_mean-temp_sd, ymax = temp_mean+temp_sd, 
                                         colour = site), 
                position = "dodge") +
  geom_line(data = sst_NOAA_max1, aes(x = year, y = temp_max, colour = site), linewidth = 1)
plot_1


plot_2 <- ggplot() +
  geom_bar(data = sst_NOAA_max2, aes(x = site, y = temp_mean), 
           stat = "identity", position = "dodge", width = 0.7) +
  
  geom_errorbar(data = sst_NOAA_max2, aes(x = site,
                                         ymin = temp_mean-temp_sd, ymax = temp_mean+temp_sd, 
                                         colour = site), 
                position = "dodge") +
  geom_point(data = sst_NOAA_max2, aes(x = site, y = temp_max, colour = site), size = 3)
plot_2


# Mapping
map_data_world <- map_data("world")

map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  # Why +2000? => put all the values above 180 into a separate group
  # we can always include this into the mapping, as it won't hurt
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))
View(map_global_fix)

map_Med <- map_global_fix %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_quickmap(xlim = c(-10, 40), ylim = c(25, 50)) 
map_Med

map_NW_Atl <- map_global_fix %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_quickmap(xlim = c(-100, -50), ylim = c(25, 50)) 
map_NW_Atl

map_WA <- map_global_fix %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_quickmap(xlim = c(100, 130), ylim = c(-36, -10)) 
map_WA

fig <- ggdraw() +
  draw_plot(plot_2, height = 0.7) +
  draw_plot(map_Med, x = -0.1, y = 0.7, width = 0.5, height = 0.3) +
  draw_plot(map_NW_Atl, x = 0.18, y = 0.7, width = 0.5, height = 0.3) +
  draw_plot(map_WA, x = 0.4, y = 0.7, width = 0.5, height = 0.3) 
fig




# Exercise 3 --------------------------------------------------------------

# From scratch, re-write the full analysis for exercise 1 'The new age'
# Inset maps for each at the end of each line on the Y axis





# BONUS -------------------------------------------------------------------

# Create a faceted heatmap showing the monthly climatologies per site

sst_NOAA_month_1 <- sst_NOAA %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(site, month) %>% 
  mutate(anom = temp - mean(temp, na.rm = T)) %>% 
  ungroup()
sst_NOAA_month_1

sst_NOAA_month_2 <- sst_NOAA %>% 
  filter(site %in% selected_sites) %>%
  group_by(site) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE)) %>% 
  ungroup()



# heatmap, x=month, y=site, z=climatology
plot_4 <- ggplot() +
  geom_raster(data = sst_NOAA_month_1, aes(x = month, y = site, fill = anom)) +
  scale_fill_distiller(palette = "Spectral")
plot_4

plot_5 <- ggplot() +
  geom_raster(data = sst_NOAA_month_2, aes(x = month, y = site, fill = anom)) +
  scale_fill_distiller(palette = "Spectral")
plot_5


