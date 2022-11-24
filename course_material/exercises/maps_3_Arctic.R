# Arctic maps
# Sarina
# 22.11.2022


# Libraries ---------------------------------------------------------------

if(!require(ggspatial)){install.packages("ggspatial")}
library(ggspatial)
if(!require(ggOceanMaps)){install.packages("ggOceanMaps")}
library(ggOceanMaps)
if(!require(remotes)){install.packages("remotes")}
library(remotes)

remotes::install_github("MikkoVihtakari/ggOceanMapsData")
if(!require(ggOceanMapsData)){install.packages("ggOceanMapsData")}
library(ggOceanMapsData)


# Data --------------------------------------------------------------------

map_data_world <- map_data("world")
load("course_material/data/bathy_WA_df.RData")

# Dataset of fixed data (no 200 lon)
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) 

# in this we can search for Svalbard and use it, FILTER function
map_global_fix_svalbard <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>% 
  filter(region == "Norway" & subregion == "Svalbard")


# Example -----------------------------------------------------------------

# An Arctic plot
basemap(limits = c(-160, -80, 60, 85), rotate = TRUE)


# Exercise 1 --------------------------------------------------------------

# Directly access the shape of a region near a pole
# Plot with coord_map(projection = "ortho")

# general plot of the Arctic
ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0))

# search for Svlbard
ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0), 
            xlim = c(10, 34), ylim = c(74, 81))
  
# search for region in dataset (it would be the same, if I used map_global_fix_Svalbard)
plot_Spitsbergen <- map_data_world %>% 
  filter(region == "Norway" & subregion == "Svalbard") %>% 
  # plot only starts here
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))+
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0))
plot_Spitsbergen


# Exercise 2 --------------------------------------------------------------

# Add bathymetry to this plot
ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Look up the help file for moer info
  coord_map(projection = "ortho", orientation = c(90, 0, 0), 
            xlim = c(10, 34), ylim = c(74, 81)) +
geom_contour(data = bathy_WA_df, 
             aes(x = x, y = y, z = z, colour = after_stat(level)),
             linewidth = c(0.3))


# Exercise 3 --------------------------------------------------------------

# Use ggoceanmaps to create a similar plot

basemap(limits = c(10, 34, 74, 81), bathymetry = TRUE) # xlim (lon) and then ylim (l)

# BONUS -------------------------------------------------------------------

# Create a workflow for creating a polar plot for any region
# Add a red bounding box to highlight a region
# And different coloured points to show study sites


dt <- data.frame(lon = c(10, 10, 34, 34), lat = c(74, 81, 81, 74))
st.SQ <- data.frame(lon = c(15.883067, -19, -21), 
                    lat = 77.013917, 75, 70)

plot_Arctic <- basemap(limits = c(10, 34, 74, 81)) +
  geom_spatial_point(data = dt_SQ, aes(x = lon, y = lat), size = 2, 
                     shape = 21, fill = "red")
  geom_spatial_polygon(data = dt, aes(x = lon, y = lat), fill = NA, color = "red")
plot_Arctic







