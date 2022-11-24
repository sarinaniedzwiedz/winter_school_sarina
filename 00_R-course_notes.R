# ************************************************************
# ************************************************************
#
# R script - R course
# 19.11-28.11.2022
# link to slides: https://face-it-project.github.io/R_workshop/
#
# ************************************************************
# ************************************************************
# 

# DAY 1 -------------------------------------------------------------------

## Packages ---------------------------------------------------------------

  if(!require(tidyverse)){install.packages("tidyverse")}
  library(tidyverse)
 
  if(!require(lubridate)){install.packages("lubridate")}
  library(lubridate)

  if(!require(ggpubr)){install.packages("ggpubr")}
  library(ggpubr)

  if(!require(ggsn)){install.packages("ggsn")}
  library(ggsn)

  if(!require(marmap)){install.packages("marmap")}
  library(marmap)

  if(!require(palmerpenguins)){install.packages("palmerpenguins")}
  library(palmerpenguins)

  if(!require(ggspatial)){install.packages("ggspatial")}
  library(ggspatial)
  if(!require(ggOceanMaps)){install.packages("ggOceanMaps")}
  library(ggOceanMaps)
  if(!require(remotes)){install.packages("remotes")}
  library(remotes)

 # remotes::install_github("MikkoVihtakari/ggOceanMapsData")
  if(!require(ggOceanMapsData)){install.packages("ggOceanMapsData")}
  library(ggOceanMapsData)

  if(!require(cowplot)){install.packages("cowplot")}
  library(cowplot)

  # Change system language of time / date english, important for figures
  # switching back to german, type in german
  Sys.getenv()
  Sys.setlocale(category = "LC_TIME", locale = "en")
  
  # look at all datasets available in R 
  # data(package = .packages((all.available = TRUE)))


## Calculator ####
# R was designed as a statistics software
  5+3
  
# VARIABLES
# We can assign variables
# Assignment operator hit: ALT - 
# help function: put cursor on function and press F1
# Comment script out: ctrl+shift+c
# %>% is done by ctr+shift+m
# assign operators in a way that you never have to write 3-4 characters, before the right variable shows up
# if you go into the console you can press the up arrow and see what run before; goes infinitely

  a <- 2
  b <- -9
  a+b
  d <- 4
  rm(d) # remove the variable d; maybe I should add this in my r-scripts when I assign the operators (e.g. M)
  
  apples <- c(1.2, 4.2, 7.5) # c is a function; point for decimal places; comma to separate two pieces of information
  mean(apples)  # mean of apples
  sd(apples) # standard deviation of apples
  round(sd(apples), 2) # rounds the standard deviation to two decimal places
  # brackets hug the function inside; if you want to round the sd to two decimal places, you put it in the round() brackets
  
  
  # AVERAGE CLIMATOLOGY
  
  # load data: 
  sst_NOAA <- read_csv("../data/sst_NOAA.csv")
  # create average
  sst_monthly <- sst_NOAA %>% 
    mutate(month = month(t, label = T)) %>% 
    group_by(site, month) %>% 
    summarise(temp = round(mean(temp, na.rm = T), 3))
  # Visualise 
  ggplot(data = sst_monthly, aes(x = month, y = temp)) +
    geom_point(aes(colour = site)) +
    geom_line(aes(colour = site, group = site)) +
    labs(x = NULL, y = "Temperature (°C)")
  
  ggplot(data = sst_monthly, aes(x = month, y = temp)) +
    geom_point(aes(colour = site)) +
    geom_line(aes(colour = site, group = site)) +
    labs(x = "", y = "Temperature (°C)") +
    facet_wrap(~site, ncol = 1) # Create panels
  
 

  
  
  
  
  
  
# DAY 2 -------------------------------------------------------------------

## GitHub ####
  
  # GitHub connect to other projects
  # Create a project in R
  # go into project in the gitHub Website
  # Settings (tab on top) > Collaborators (on left side)
  # Green button: saying add people; enter gitHub name
  # Email, other person has to verify
  
  # If I am added to a project of someone else: I have to be added to project
  # Verification Email: accept: you are directly in project of other person
  # green button on top right corner: <> code
  # go into RStudio; create new project: File > New Project > Version Control > Git and paste the link in ‘Repository URL’. 
  # Before you click ’ Create Project’ note where RStudio intends to save the files and change this if desired. 
  
  # If you select project; choose the file in the R documentation, change it, it is saved into the R environment
  # if you want to upload it: choose Git Tab on top of the environment, Commit -> enter describtion of what you have done -> push
  
  # Choose other project; select file in the R documentation
  # choose GitHub Tab
  # Pull
  
  
  
  
  
  
  

# DAY 3 -------------------------------------------------------------------
  
  ## Plotting in R ####
  
  # tidyverse have very good teaching material!! 
  # https://r-graph-gallery.com/
  
  if(!require(palmerpenguins)){install.packages("palmerpenguins")}
  library(palmerpenguins)
  View(penguins)
  
  ggplot(data = penguins, 
                  aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(color = species))

  # two ways to organise spreadsheets: you would rather separate words by "_" 
  # "." might mean different things
  # do not ever start things with numbers
  # might make sense to not capatalise the start of the columns easier to write
  
  # columns from data go into aes(), properties go outside of 
  ggplot(data = penguins, 
                  aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(color = island))
  
  ggplot(data = penguins, 
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(), color = "red")
  
  ggplot(data = penguins, 
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(size = flipper_length_mm, shape = island))
  
  ggplot(data = penguins, 
         aes(x = body_mass_g, y = bill_length_mm, color = species)) +
    geom_point() +
    geom_smooth(method = "lm") +
    # change labels
     labs(x = "Body mass (g)", y = "Bill length (mm)", color = "Species") +
    # change legend position
    theme(legend.position = "bottom")
  
  
  ## Facets ####
  
  # ggpubr helps us to combine many different plots
  if(!require(ggpubr)){install.packages("ggpubr")}
  library(ggpubr)
  
  ggplot(data = penguins, 
         aes(x = body_mass_g, y = bill_length_mm, color = species)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(species~island)
  
  # assigning the ggplot code to a linear model
  lm_1 <-   ggplot(data = penguins, 
                    aes(x = body_mass_g, y = bill_length_mm, color = species)) +
    geom_point() +
    geom_smooth(method = "lm") 
  lm_1

  nlm_1 <- ggplot(data = penguins, 
                aes(x = body_mass_g, y = bill_length_mm, color = species)) +
    geom_point() +
    geom_smooth() 
  nlm_1  
  
  histogram_1 <- ggplot(data = penguins, 
                        # NB: There is no y-axis value for histograms
                        aes(x = body_mass_g)) + 
    geom_histogram(aes(fill = species), position = "stack", binwidth = 250) +
    # NB: We use 'fill' here rather than 'colour'
    labs(x = "Body mass (g)", fill = "Species")
  histogram_1
  
  box_1 <- ggplot(data = penguins, 
                 # Why 'as.factor()'?
                 aes(x = as.factor(year),
                     y = body_mass_g)) + 
    geom_boxplot(aes(fill = species)) +
    labs(x = "Year", y = "Body mass (g)", fill = "Species") 
  box_1
  
  #list the names of the plot objects to combine
  grid_1 <- ggarrange(lm_1, nlm_1, histogram_1, box_1,
                      # Set number of rows and columns
                      ncol = 2, nrow = 2,
                      # Label each figure
                      labels = c("a)", "b)", "c)", "d)"),
                      # Create common legend
                      common.legend = TRUE,
                      # Set legend position
                      legend = "bottom")
  grid_1
  
  ggsave(plot = grid_1, filename = "figures/grid_1.pdf")
  ggsave(plot = grid_1, filename = "figures/grid_1.png")
  ggsave(plot = grid_1, filename = "figures/grid_1.eps")
  # Change dimensions
  ggsave(plot = grid_1, filename = "figures/grid_1.png", 
         width = 10, height = 7.5)
  # Change DPI
  ggsave(plot = grid_1, filename = "figures/grid_1.png", dpi = 600)
  
  
  ## Colours ####
  # continuous colour scales 
  # continous value: size, temperature, concentration => gradient
  # scale_color_viridis_c
  # ...viridis is very good to visualise
  ggplot(data = penguins,
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(colour = bill_depth_mm)) +
    #scale_colour_distiller(palette = "Spectral") +
    scale_color_viridis_c(option = "A")
  
  # discreet value: species, sex => categories
  # scale_color_viridis_d
  # scale_colour_brewer
  ggplot(data = penguins,
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(colour = as.factor(year))) +
    # The discrete colour palette function
    scale_colour_brewer(palette = "Set1")
  
  
  # make your own color palette
  # http://medialab.github.io/iwanthue/
  # http://tristen.ca/hcl-picker/#/hlc/6/1.05/1F313F/D5DD5C
  ggplot(data = penguins,
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(colour = bill_depth_mm)) +
    scale_colour_gradientn(colours = c("#1F313F", "#19545D", "#19796E",
                                       "#449E6F", "#85C066", "#D5DC5C"))
  # #1F313F,#19545D,#19796E,#449E6F,#85C066,#D5DC5C
  
  # manually change the label text; be carefull with this, changes the sense of the plot
  ggplot(data = penguins,
         aes(x = body_mass_g, y = bill_length_mm)) +
    geom_point(aes(colour = as.factor(sex))) +
    # How to use custom palette
    scale_colour_manual(values = c("#A5A94D", "#9699C4"),
                        # How to change the legend text
                        labels = c("female", "male", "other")) + 
    # How to change the legend title
    labs(colour = "Sex") 
  
  
  ## Plot stats ####
  # t-test
  compare_means(bill_length_mm~sex, data = penguins, method = "t.test")
  # ANOVA
  compare_means(bill_length_mm~species, data = penguins, method = "anova")
  
  ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
    geom_boxplot(aes(fill = species), show.legend = F) +
    stat_compare_means(method = "anova")
  
  
  ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
    geom_boxplot(aes(fill = species), show.legend = F) +
    stat_compare_means(method = "anova", 
                       aes(label = paste0("p ", ..p.format..)), 
                       label.x = 2) +
    theme_bw()
  
  
  
  penguins_levels <- levels(penguins$species)
  my_comparisons <- list(c(penguins_levels[1], penguins_levels[2]), 
                         c(penguins_levels[2], penguins_levels[3]),
                         c(penguins_levels[1], penguins_levels[3]))
  # Then we stack it all together
  ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
    geom_boxplot(aes(fill  = species), colour = "grey40", show.legend = F) +
    stat_compare_means(method = "anova", colour = "grey50",
                       label.x = 1.8, label.y = 32) +
    # Add pairwise comparisons p-value
    stat_compare_means(comparisons = my_comparisons,
                       label.y = c(62, 64, 66)) +
    # Perform t-tests between each group and the overall mean
    stat_compare_means(label = "p.signif", 
                       method = "t.test",
                       ref.group = ".all.") + 
    # Add horizontal line at base mean
    geom_hline(yintercept = mean(penguins$bill_length_mm, na.rm = T), 
               linetype = 2) + 
    labs(y = "Bill length (mm)", x = NULL) +
    theme_bw()
  
  
  penguins_levels <- levels(penguins$species)
  my_comparisons <- list(c(penguins_levels[1], penguins_levels[2]), 
                         c(penguins_levels[2], penguins_levels[3]),
                         c(penguins_levels[1], penguins_levels[3]))
  # Then we stack it all together
  ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
    geom_boxplot(aes(fill  = species), colour = "grey40", show.legend = F) +
    stat_compare_means(method = "anova", colour = "grey50",
                       label.x = 1.8, label.y = 32) +
    # Add pairwise comparisons p-value
    stat_compare_means(comparisons = my_comparisons,
                       # hight on which the p values of t-test are
                       label.y = c(62, 64, 66)) +
    # Perform t-tests between each group and the overall mean
    stat_compare_means(label = "p.signif", 
                       method = "t.test",
                       ref.group = ".all.") + 
    # Add horizontal line at base mean of all groups
    geom_hline(yintercept = mean(penguins$bill_length_mm, na.rm = T), 
               linetype = 2) + 
    labs(y = "Bill length (mm)", x = NULL) +
    theme_bw()
  
  
  # help function: put cursor on function and press F1
  # bad practice to load file directly from excel; excel files might not work properly
  # always save the excel file as .csv before 
  
  

  
  
# DAY 4 -------------------------------------------------------------------

## Mapping  ---------------------------------------------------------------

  # PLot of the earth
  earth_1 <- ggplot() +
    # The global shape file
    borders(fill = "grey70", colour = "black") +
    # Equal sizing for lon/lat 
    coord_equal()
    
    # only Greenland, set limits for the long and lat
    #coord_equal(xlim = c(-75, -10), ylim = c(58, 85))
  earth_1
  
  
  # Extract a region, becuase Greenland is a bit messed up
  map_data('world') %>% 
    select(region) %>% 
    distinct() %>% # only give each region once
    arrange(region) # arange them alphabetically
  
  # 
  map_data_green <- map_data('world') %>% 
    filter(region == "Greenland") # Why '==' and not '='? '==' logical argument; the same as, equivalent to
  View(map_data_green)
  
  # maps are scatter plots
  ggplot(data = map_data_green, 
         aes(x = long, y = lat)) +
    geom_point()
  
  # however, we do not want dots in our maps, so we use POLYGONS
  # they can be a bit tricky
  green_2 <- ggplot(data = map_data_green, aes(x = long, y = lat)) +
    # What is this doing? we need to tell R, what is the group, the data is in group, see mapdata_green
    geom_polygon(aes(group = group), 
                 # Note these are outside of aes() 
                 fill = "chartreuse4", colour = "black")
  green_2
  
  # group is a column in the extracted spread sheet
  # see map_data_green
  # different groups define different parts of the maps
  
  
  # Specific labels
  green_3 <- green_2 +
    # Add Greenland text; x and y say where the middle of the label is
    annotate("text", label = "Greenland", 
             x = -40, y = 75.0, size = 7.0, fontface = "bold.italic") +
    # Add North Atlantic Ocean text
    annotate("text", label = "North\nAtlantic\nOcean", 
             x = -20, y = 64.0, size = 5.0,  angle = 330, colour = "navy") +
    # Add Baffin Bay label
    annotate("label", label = "Baffin\nBay", 
             x = -62, y = 70, size = 5.0, fill = "springgreen") +
    # Add black line under Greenland text
    annotate("segment", 
             x = -50, xend = -30, y = 73, yend = 73)
  green_3

  # North Atlantic label: \n makes a line break 
  
  

  # Scale Bars
  green_4 <- green_3 +
    # Set location of bar,
    scalebar(data = map_data_green, location = "bottomright", 
             # Size of scale bar
             dist = 500, dist_unit = "km", transform = TRUE,
             # Set particulars
             st.size = 4, height = 0.03, st.dist = 0.04) 
  green_4

  
  # Insetting a second map
  # you can make a spreadsheet with defining the different values of the 
  earth_2 <- earth_1 + 
    geom_rect(aes(xmin = -75, xmax = -10, ymin = 58, ymax = 85),
              fill = NA, colour = "red") +
    # What does this do?
    theme_void()
  earth_2

  green_5 <- green_4 +
    # Convert the earth plot to a grob... is necessary to inset it. 
    annotation_custom(grob = ggplotGrob(earth_2), 
                      xmin = -30, xmax = -10,
                      ymin = 76, ymax = 84)
  green_5  

  
  green_final <- green_5 +
    scale_x_continuous(breaks = seq(-60, -20, 20),
                       labels = c("60°W", "40°W", "20°W"),
                       position = "bottom") +
    scale_y_continuous(breaks = seq(60, 80, 10),
                       labels = c("60°N", "70°N", "80°N"),
                       position = "left") +
    labs(x = NULL, y = "") +
    theme_bw()
  green_final  
  
  # labs: on both x and y labels yu cannot see a label, 
  # however on y = "" there is still a label, just invisible
  # y = NULL gives you more space to plot
  
  

## Maps with style ---------------------------------------------------------

  ggplot() +
    borders(fill = "grey70", colour = "black") +
    coord_equal()
  # Problem: the longitude is bigger than 200... should only be to 180
  # This happens primarily because the Kamchatka peninsula goes over the date line
  
  map_global_fix <- map_data('world') %>% 
    rename(lon = long) %>% 
    # Why +2000? => put all the values above 180 into a separate group
    # we can always include this into the mapping, as it won't hurt
    mutate(group = ifelse(lon > 180, group+2000, group),
           lon = ifelse(lon > 180, lon-360, lon))
  
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # The default coordinate system, with specific limits
    coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)
  
  

### Bathymetry data ---------------------------------------------------------

  # This does not work... some bigger overall general not-fixable problem
  # It does work to download the data from the project, see line494
  # Download bathymetry data
  bathy_WA <-  getNOAA.bathy(lon1 = 111, lon2 = 117, 
                             # NB: smaller value first, i.e. more negative
                             lat1 = -36, lat2 = -19, 
                             # In degree minutes goes from 1-4
                             resolution = 4)
  
  # Comment script out: ctrl+shift+c
  # Convert to data.frame for use with ggplot2
  bathy_WA_df <- fortify.bathy(bathy_WA) %>% 
    # Remove altimetry data, filters everything above the surface out of the dataset
    filter(z <= 0) 
  
  # Save
  save(bathy_WA_df, file = "course_material/data/bathy_WA_df.RData")
  
  # load the data
  load("course_material/data/bathy_WA_df.RData")
  
  # plot the bathymetry
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Add 200 m contour
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z),
                 breaks = c(-200), 
                 linewidth = c(0.3), colour = "grey") +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  
  
  
  
  # add another depth line
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Add 200 and 2000 m contours
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z),
                 breaks = c(-200, -2000), 
                 linewidth = c(0.3), colour = "grey") +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  
  
  # Coloured lines
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Assign colour per depth
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z),
                 breaks = c(-200), linewidth = c(0.3), colour = "black") +
    # Assign colour per depth
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z),
                 breaks = c(-2000), 
                 linewidth = c(0.3), colour = "blue") +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  
  
  # assign the level to the colour
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Rather use `aes()`
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z, colour = after_stat(level)),
                 linewidth = c(0.3)) +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  # after_stat(level) super complicated, but magical, adds all the bathymetry lines, default colour label
  
  
  # Discrete colour paletts for contours
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Combine `aes()` and `breaks = c()` for more control
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z, colour = after_stat(level)),
                 breaks = c(-50, -200, -1000, -2000), 
                 linewidth = c(0.3)) +
    # Also change colour palette
    scale_colour_distiller(palette = "BuPu") + 
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  
  
  # Tidy up the appearance
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # create discrete factors
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))),
                 breaks = c(-50, -200, -1000, -2000), 
                 linewidth = c(0.3)) +
    # Use discrete palette
    scale_colour_brewer("Depth [m]", palette = "Set1", direction = -1) +  
    # Reverse legend order and make symbols thicker
    guides(color = guide_legend(reverse = TRUE, 
                                override.aes = list(linewidth = 5))) +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)
  
  
  
  
### Ocean temperature -------------------------------------------------------


  load("course_material/data/OISST_2000.RData")

  # plot western Australia
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), 
                 colour = "black", fill = "grey60") +
    geom_raster(data = OISST_2000, aes(fill = temp)) +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)

  # problem: water temperature would be on top of the land, so switch the layers
  
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    # First layer
    geom_raster(data = OISST_2000, aes(fill = temp)) + 
    # Second layer
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    # Third layer
    geom_contour(data = bathy_WA_df, 
                 aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
                 breaks = c(-50, -200, -1000, -2000), 
                 linewidth = c(0.3)) +
    guides(color = guide_legend(reverse = TRUE, 
                                override.aes = list(linewidth = 5))) + 
    scale_fill_viridis_c("Temperature [°C]") +
    scale_colour_brewer("Depth [m]", palette = "BuPu") +
    coord_cartesian(xlim = c(111, 117), 
                    ylim = c(-36, -19), expand = FALSE)

  
  # beautiful plots
  final_map <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_raster(data = OISST_2000, aes(fill = temp)) +
    geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
    geom_contour(data = bathy_WA_df,
                 aes(x = x, y = y, z = z, 
                     colour = as.factor(after_stat(level))), 
                 breaks = c(-50, -200, -1000, -2000), 
                 linewidth = c(0.3)) +
    guides(color = guide_legend(reverse = TRUE, 
                                override.aes = list(linewidth = 5))) + 
    scale_fill_viridis_c("Temperature [°C]") +
    scale_colour_brewer("Depth [m]", palette = "BuPu") +
    coord_cartesian(xlim = c(111, 117), ylim = c(-36, -19), expand = FALSE) +
    # Put x axis labels on top of figure and assign °E
    scale_x_continuous(position = "top", 
                       breaks = c(112, 114, 116), 
                       labels = c("112°E", "114°E", "116°E")) + 
    # Put y axis labels on right of figure and assign °S
    scale_y_continuous(position = "right",
                       breaks = c(-34, -28, -22), 
                       labels = c("34°S", "28°C", "22°C")) +
    # Remove the axis label text
    theme(axis.title = element_blank(),
          # Add black border
          panel.border = element_rect(fill = NA, colour = "black"), 
          # Change text size in legend
          legend.text = element_text(size = 7), 
          # Change legend title text size
          legend.title = element_text(size = 7), 
          # Change size of legend
          legend.key.height = unit(0.5, "cm"),
          # Add legend background
          legend.background = element_rect(fill = "white", colour = "black"),
          # Change position of legend
          legend.position = c(0.9, 0.5)
    )
  ggsave(plot = final_map, "figures/map_complete.pdf", height = 6, width = 9)
  
  
  

## Mapping Arctic ---------------------------------------------------

  # Fixed base map
  map_global_fix <- map_data('world') %>% 
    rename(lon = long) %>% 
    mutate(group = ifelse(lon > 180, group+2000, group),
           lon = ifelse(lon > 180, lon-360, lon))
  
  # Load sea surface temperatures for 2000-01-01
  load("course_material/data/OISST_2022.RData")
  
  
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Numeric sizing for lon/lat 
    coord_cartesian()

  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Equal sizing for lon/lat 
    coord_equal()  

  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Ratio (Y divided by X) sizing for lon/lat 
    coord_fixed(ratio = 2)  

  # This is a very good default
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Behind the scenes this adapts the "mercator" projection
    coord_quickmap()  

  # 
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    coord_sf() # sf = simple feature
  
  
  # Polar projections
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    scale_y_reverse() +
    # A very different projection
    coord_polar()
  # ...weeeeell. Not really useful
  
  # That looks better
  ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Look up the help file for moer info
    coord_map(projection = "ortho", orientation = c(90, 0, 0))
  # orthographic map
  
  
  map_global_fix %>% 
    filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group)) +
    # Filtering the OISST_2022 data directly in geom_tile()
    geom_tile(data = filter(OISST_2022,
                            lon > 9, lon < 28, lat > 76, lat < 81), 
              aes(fill = temp)) +
    coord_map(projection = "ortho", orientation = c(90, 0, 0))

  
  # or better still, use ggOcenMaps
  # tiny little lines of codes for pretty good maps
  # good intro: (https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html)
  basemap(limits = 60)
  # Works everywhere
  basemap(limits = c(100, 160, -20, 30), bathymetry = TRUE)
  # you can add glaciers
  basemap(limits = 60, glaciers = TRUE, bathymetry = TRUE)
  basemap(limits = c(-160, -80, 60, 85), rotate = TRUE)
  basemap(limits = c(-160, -80, 60, 85), rotate = TRUE)
  
  
  # Citing packages
  citation("ggOceanMaps")
  
  

  
  
# DAY 5 ---------------------------------------------------------------


## Tidy data ---------------------------------------------------------------
  
  
  # 1. each variable must have its own column
  # 2. each observation must have its own row (one penguin in one observation)
  # 3. each value must have its own cell
  # so generally tidy data are considered long rather than wide... 
  # => spreadsheet without gaps (or NA)
  # using R to tidy data makes it very easy to be able to see what we did in the data; unlike to excel
  # the column on the left should be the biggest variable, e.g. only three sites, and 10 years
  
  # load (untidy) data
  load("course_material/data/OISST_mangled.RData")
  
  # OISST 1 is correct
  head (OISST1)
  # that we can use to plot 
  
  # OISST 2 too wide, each site is a own column, but then you cannot use the variable "site"
  head (OISST2)
  # if something is too long, you use: pivot_longer; 
  OISST2_tidy <- OISST2 %>%
    pivot_longer(cols = c(Med, NW_Atl, WA), 
                 names_to = "site", values_to = "temp")
  head(OISST2_tidy)

  # OISST 3 too many variables; site and date in one column
  head(OISST3)
  # 
  OISST3_tidy <- OISST3 %>% 
    pivot_wider(id_cols = c(idx, temp), names_from = type, values_from = name)
  head(OISST3_tidy)  
  # thereby temperature is lost
  
  # Separating and uniting
  # sometimes comes from cruise data, one ID that connects to another metadata table
  # OISST 4a, site and date are in one column, so we separate by space
  head(OISST4a)
  OISST4a_tidy <- OISST4a %>% 
    separate(col = index, into = c("site", "t"), sep = " ")
  head(OISST4a_tidy)
  
  # year, month and day are in different columns; date itself is a variable
  head(OISST4b)
  OISST4b_tidy <- OISST4b %>% 
    unite(year, month, day, col = "t", sep = "-")
  head(OISST4b_tidy)

  
  # Joining between 4a and 4b
  OISST4_tidy <- right_join(OISST4b_tidy, OISST4a_tidy, by = c("site", "t"))
  head(OISST4_tidy)
  
  
  
  

## Taming Data -------------------------------------------------------------

  # once we have actual tidy data, how are we going to manipulating them
  # basically there are five functions we need
  # -   Arrange observations (rows) with `arrange()`  
  # -   Select variables (columns) with `select()`  
  # -   Filter observations (rows) with `filter()`  
  # -   Create new variables (columns) with `mutate()`  
  # -   Summarise data (rows+columns) with `summarise()`  
  
  # lubridate helps us with date values
  
  # load (untidy) data
  load("course_material/data/OISST_mangled.RData")
  
  # comparison operators:
  # -   Greater than: `>`  
  # -   Greater than or equal to: `>=`  
  # -   Less than: `<`  
  # -   Less than or equal to: `<=`  
  # -   Equal to: `==`  
  # -   Not equal to: `!=`  
  
  # Logical operator: 
  # -   and: `&`  
  # -   or: `|`  
  # -   not: `!` 
  
  OISST1_sub <- OISST1 %>% 
    filter(site == "Med", month(t) == 12 | month(t) == 1)
  OISST1_sub  

  # arrange(), if we want to order things
  # NAs will go to the bottom!!
  OISST1 %>% 
    arrange(site, temp) %>% 
    head()
  # first we arranged by site (alphabetically), then by temperature (small to big)

  OISST1 %>% 
    arrange(site, desc(temp)) %>%
    head()
  # highest temperature on top
  # if you desc(site), it goes from Z to A
  
  
  # select()
  # Select columns individually by name
  OISST1 %>% 
    select(site, t, temp)
  
  # Select all columns between site and temp like a sequence
  OISST1 %>% 
    select(site:t)
  
  # Select all columns except those stated individually
  OISST1 %>% 
    select(-t, -temp)
  
  # Select all columns except those within a given sequence
  # Note that the '-' goes outside of a new set of brackets
  # that are wrapped around the sequence of columns to remove
  OISST1 %>% 
    select(-(site:temp))
  
  # Change up order by specifying individual columns
  OISST1 %>% 
    select(temp, t, site)
  
  # Use the everything function to grab all columns 
  # not already specified
  OISST1 %>% 
    select(t, everything())
  
  # Or go bananas and use all of the rules at once
  # Remember, when dealing with tidy data, everything may be interchanged
  OISST1 %>% 
    select(temp:t, everything(), -site)

  
  # filter()
  # it automatically removes NAs
  OISST1 %>% 
    filter(site == "Med", 
           year(t) == 2008) %>% 
    head()
  
  # use the %n% comparison operator to make multiple logic statements
  OISST1 %>% 
    filter(site %in% c("Med", "WA"), 
           year(t) == 2009) %>% 
    tail()
  
  
  # mutate()
  OISST1 %>% mutate(kelvin = temp + 273.15) %>% head()
  
  
  # summarise()
  # removes missing values
  OISST1 %>% 
    mutate(mean_temp = mean(temp, na.rm = TRUE)) %>% 
    head()
  
  # overall mean of all temperatures
  OISST1 %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
    head()
  
  # combine more functions
  OISST1 %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE),
              sd_temp = sd(temp, na.rm = TRUE),
              min_temp = min(temp, na.rm = TRUE),
              max_temp = max(temp, na.rm = TRUE))
  
  

## Data Domesticating ------------------------------------------------------

  sst_NOAA <- read_csv("course_material/data/sst_NOAA1.csv")
  
  sst_NOAA_site <- sst_NOAA %>% group_by(site)
  # be careful that you don't forget, that you grouped it; gives wrong results in some analyses
  # so it can be useful to ungroup
  sst_NOAA_ungroup <- sst_NOAA_site %>% ungroup()
  # that is the same as sst_NOAA
  
  # Multiple groups
  # Create groupings based on temperatures
  sst_NOAA_temp_group <- sst_NOAA %>% 
    group_by(round(temp))
  sst_NOAA_temp_group
  
  # Create groupings based on site and month
  # it is very important to think about the order in which the groups are given
  sst_NOAA_temp_month_group <- sst_NOAA %>% 
    mutate(month = month(t)) %>% 
    group_by(site, month)
  sst_NOAA_temp_month_group

  # -   Generally we do not group objects separately
  # -   Grouping is performed within code chunks
  # -   `summarise()` has an ungrouping argument
  
  sst_NOAA_site_mean <- sst_NOAA %>% 
    # Group by the site column
    group_by(site) %>% 
    # Calculate means
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              # Count observations; makes sense to have for reporting
              count = n(),
              # Ungroup results, to be on the save side for further analyses; very useful after summarise
              .groups = "drop") # Or instead of this line: %>% 
  # ungroup()
  sst_NOAA_site_mean
  
  # you don't want to change the dataset, but only want to know at which sites the temperatures go above 20°C
  sst_NOAA_20 <- sst_NOAA %>%
    group_by(site) %>%
    filter(max(temp) > 20) %>% 
    ungroup()
  unique(sst_NOAA_20$site)
  
  # now we are looking at the difference of the temperature to the mean temperature by site
  # this works as we are working in layers; when the mean is calculated, the dataset is still grouped
  sst_NOAA_anom <- sst_NOAA %>%
    group_by(site) %>% 
    mutate(anom = temp - mean(temp, na.rm = T)) %>%
    ungroup()
  head(sst_NOAA_anom)
  
  # calculate the mean and standard deviation for two sites
  sst_NOAA %>% 
    filter(site == "Med" | site == "WA") %>%
    group_by(site) %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              sd_temp = sd(temp, na.rm = TRUE)) %>% 
    ungroup()

  # that yould be the better option than the bit above
  # First create a character vector containing the desired sites
  selected_sites <- c("Med", "WA")
  
  # Then calculate the statistics
  sst_NOAA %>% 
    filter(site %in% selected_sites) %>%
    group_by(site) %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              sd_temp = sd(temp, na.rm = TRUE)) %>% 
    ungroup()

  # only days with temperatures above 10 and below 15 °C and give the number of rows
  sst_NOAA %>% 
    filter(site == "Med", 
           temp > 10, temp < 15) %>% 
    nrow()
  
  #...alternative: but maybe not
  st_NOAA %>% 
    filter(site == "Med", 
           !(temp <= 10 | temp  >= 15)) %>% 
    nrow()
  
  
  # all put together: 
  # Load the SACTN Day 1 data
  read_csv("course_material/data/sst_NOAA1.csv") %>%
    # Then create a month abbreviation column
    mutate(month = month(t, label = TRUE)) %>% 
    # Then group by sites and months
    group_by(site, month) %>% 
    # Lastly calculate the mean
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              # and the SD
              sd_temp = sd(temp, na.rm = TRUE)) %>% 
    # ungroup() %>% 
    # Begin ggplot
    ggplot(aes(x = month, y = mean_temp, group = site)) + 
    # Create a ribbon with +- standard deviation
    geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
                fill = "black", alpha = 0.4) + 
    # Create dots
    geom_point(aes(colour = site)) + 
    # Create lines, you have to tell R, which points belong together
    geom_line(aes(colour = site, group = site)) + 
    # scale_x_date(labels = "%B") +
    # Change labels
    labs(x = "Month", y = "Temperature (°C)", colour = "Site") 
  
  
  # temperature above 15 °C per year and site
  sst_NOAA %>%  
    group_by(site, year(t)) %>%
    summarise(count = n(), 
              count_20 = sum(temp > 20)) %>% 
    mutate(prop_20 = count_20/count) %>% 
    arrange(prop_20) %>% 
    ungroup()
  
    