# Data Set Notes ---------------------------------------------------------------

#Coordinates shifted from plots in paper to protect privacy of trial

#trial_design: trial design
  #- tgti: target rate (focus)
  #- geometry: plot that rate will be located in 
  #- type: don't care about showing headland, just trial
#ry_plot_data: raw yield data
  #- yield_vol: crop yield at point
  #- X,Y: specific point location of yield measurement
  #- geometry: yield point transformed into a polygon
#data_sf  - processed yield
  #- yield: processed yield
  #- X,Y: specific point location of yield measurement
  #- x: polygon of processed yield measurement


#Necessary Packages ------------------------------------------------------------
library(sf)
library(tmap)
library(ggcorrplot)
library(tidyverse)
library(data.table)


## Grouping --------------------------------------------------------------------
# Current yield polygons too fine -> combine into larger groups (not a necessary step)

ry_plot_filter <- filter(ry_plot_data, flag_bad == 0) #remove observations flagged as bad
ry_plot_filter$obs_id <- 1:nrow(ry_plot_filter) #create an observation id

#Group yield polygons into groups of 10 (don't need as fine of detail) 
ry_plot_filter$group2 <- rep(1:2035, each = 10, length.out = 20351)
ry_plot_filter$group2[20351] <- 2035 # last is group of 11
ry_plot_avg <- ry_plot_filter %>% group_by(group2) %>% summarize(avg_yield = mean(yield_vol)) 


## Figure 3 Superimposed Map (bad colors) --------------------------------------

tm_shape(trial_design)+ tm_borders() + 
  tm_compass(type = "4star", size = 1.4, position = c("left", "bottom"),  cardinal.directions = c("N", "E", "S", "W")) + 
  tm_scale_bar() + 
  tm_fill(
    col = "tgti", 
    palette = "Greens",
    title = "Targeted\nNitrogen Rate\n(lbs)",
    style = "cat"
  ) + 
  tm_layout_to_add + 
  tm_layout(
    legend.position = c(0.25, 0.10),
    attr.position = c(0.25,0.0)
  ) + 
  tm_shape(ry_plot_avg) +
  tm_fill(
    col = "avg_yield", 
    palette = "YlOrRd", #-RdBu
    title = "Yield\n(bu/acre)",
    style = "cont",
    alpha = 0.4
  ) 

## Figure 4 Superimposed Map (better colors) --------------------------------------

tm_shape(trial_design) + tm_borders(alpha = 0) + 
  tm_fill(
    col = "tgti", 
    palette = "Blues",
    title = "Targeted\nNitrogen Rate\n(lbs)",
    alpha = 0.8,
    style = "cat",
  ) + 
  tm_shape(ry_plot_avg) + tm_polygons(col = "avg_yield", palette = "Reds",
                                      title = "Yield (bu/ac)",
                                      border.col = NA,
                                      border.alpha = 0.1,
                                      alpha = 0.3,
                                      style= "cont"
  ) +
  tm_layout_to_add + 
  tm_layout(
    legend.position = c(0.15, 0.15)
  ) 

## Figure 5 (Bivariate Color Map) ----------------------------------------------
library(cowplot) #for color legend
library(reshape2)

#Step 1: Finding yield measurements wihtin trial plots and combining information

trial_design$trial_id <- 1:nrow(trial_design) #Create id for trial plots (make them identifiable)
trial_design_trial_only <- filter(trial_design, type == "trial") #remove headland observation

points <-st_as_sf(ry_plot_filter , coords = c("X", "Y")) #convert coordinates to sf object
polyg <-trial_design_trial_only$geometry # list of intersecting polygoons
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers2 <- enframe(FrameData) %>% unnest(value) #gives list of index and intersection group
colnames(identifiers2) <- c("trial_id", "obs_id")


#Step 2: Merge information for trial design and raw yield data into one data frame
identify_trial2 <- data.frame(merge(trial_design_trial_only, identifiers2, by = "trial_id")) 
combine_identity_info2 <- inner_join(identify_trial2, ry_plot_filter, by = c("obs_id"))

#If using larger groups of yield polygons
comb2 <- inner_join(ry_plot_avg, combine_identity_info2, by = "group2") # get group of 10 information
comb3 <- comb2 %>% group_by(group2) %>% select(group2, geometry, tgti, avg_yield)  %>% summarise(avg_tgti = mean(tgti)) # average group of 10 information


#Step 3: Create 9 classes (3x3)

#Beginning and end flips at some point - why have twice
bivariate_color_scale <- tibble(
  "3 - 3" = "#1B1B1B", 
  "2 - 3" = "#5698B9",
  "1 - 3" = "#5AC8C8", 
  "3 - 2" = "#8C62AA",
  "2 - 2" = "#9B9B9B", 
  "1 - 2" = "#ACE4E4",
  "3 - 1" = "#BE64AC", 
  "2 - 1" = "#DFB0D6",
  "1 - 1" = "#E5E5E5" 
) %>% gather("group", "fill")

bivariate_color_scale2 <- tibble(
  "3 - 3" = "#E5E5E5", 
  "2 - 3" = "#5698B9",
  "1 - 3" = "#5AC8C8", 
  "3 - 2" = "#8C62AA",
  "2 - 2" = "#9B9B9B", 
  "1 - 2" = "#ACE4E4",
  "3 - 1" = "#BE64AC", 
  "2 - 1" = "#DFB0D6",
  "1 - 1" = "#1B1B1B" 
) %>% gather("group", "fill")


# create 3 buckets for Yield
quantiles_yield <-  ry_plot_avg %>%
  pull(avg_yield) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for Target
quantiles_tgti<- comb3 %>%
  pull(avg_tgti) %>%
  quantile(probs = seq(0, 1, length.out = 4))


determine_quantiles <- ry_plot_avg %>% 
  mutate(
    yield_quantiles = cut(
      avg_yield,
      breaks = quantiles_yield,
      include.lowest = TRUE
    ), 
    target_quantiles = cut(
      comb3$avg_tgti,
      breaks = quantiles_tgti,
      include.lowest = TRUE
    ),
    group = paste(
      as.numeric(target_quantiles), "-",
      as.numeric(yield_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

quant_colr<- determine_quantiles[!duplicated(determine_quantiles$group),4:7] 

# Step 4: Create plot
library(ggspatial) # for north arrow
#Create color map -> fill each group of 10 yield polygons with the proper quantile group fill with trial design plots put on top for reference
map <- determine_quantiles %>%
  ggplot() +
  geom_sf(data = determine_quantiles$geometry, aes(fill = determine_quantiles$fill), color = NA) + 
  scale_fill_identity() +
  geom_sf(data = trial_design$geometry, color = "black", aes(fill = NA)) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),  height = unit(1, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_orienteering) +
  annotation_scale(pad_x = unit(1, "in"), pad_y = unit(0, "in"), line_width = 0.5, height = unit(0.25, "cm"))

#Step 5: Create color legend
legend <- ggplot() +
  geom_tile(
    data = quant_colr,
    mapping = aes(
      x = target_quantiles,
      y = yield_quantiles,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher Application Rate ->",
       y = "Higher Yield ->") +
  theme(
    axis.title = element_text(size = 6),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) 
  ) +
  coord_fixed()

# Step 6: Use cowplot to put together, with legend on left side of map - zoom out to get pictures to not overlap
ggdraw() +
  draw_plot(map, 0.05, 0.05, 1, 1) +
  draw_plot(legend, 0, 0, 0.5, 0.3)


#### Grouping Trial Plots ------------------------------------------------------
### Combine trial plots into larger groups
#Extract coordinates
bbox_list <- lapply(st_geometry(trial_design_trial_only$geometry), st_bbox)
#To df
maxmin <- as.data.frame(t(matrix(unlist(bbox_list),ncol=nrow(trial_design_trial_only))))
colnames(maxmin) <- c("xmin", "ymin", "xmax", "ymax")

#Try clustering (similar coordinates)
km.out <- kmeans(scale(maxmin[,c(1,3)]), centers = 4, nstart = 20) #4 vertical columns
trial_design_trial_only$groupvert <- km.out$cluster 

km.out2 <- kmeans(scale(maxmin[,c(2,4)]), centers = 5, nstart = 20) #5 horizantal rows
trial_design_trial_only$grouphor <- km.out2$cluster

#combine rows and columns to get groups
trial_design_trial_only$group <- paste0(as.character(trial_design_trial_only$grouphor), sep ="-", as.character(trial_design_trial_only$groupvert))


#### Figure 6 ------------------------------------------------------------------
#Using processed data

#Step 1: Find which trial plot processed yield point is located in.
points <-st_as_sf(data_sf, coords = c("X", "Y"))
polyg <-trial_design_trial_only$geometry # LIST OF INTERSECTION POLYGONS
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers <- enframe(FrameData) %>% unnest(value) #gives list of index and intersection group
colnames(identifiers) <- c("trial_id", "obs_id")

# Step 2: Combine information into 1 data frame
identify_trial <- data.frame(merge(trial_design_trial_only, identifiers, by = "trial_id")) 
combine_identity_info <- inner_join(identify_trial, data_sf, by = c("obs_id"))


# Step 3: calculate correlation within each group
calc_corr <- combine_identity_info %>%
  group_by(group) %>% 
  summarize(correlation = cor(yield, input_rate))

td_new <- trial_design_trial_only %>% #add buffer to boundaries to they will touch
  group_by(group) %>%
  summarise(geometry = sf::st_union(st_buffer(geometry, dist = 0.1)))%>%
  ungroup()

correlation_trial <- inner_join(td_new, calc_corr, by = "group")

# Step 4: Create correlation plot
library(scales)
correlation_trial %>% 
  ggplot() +
  geom_sf( color = "black", aes(fill = correlation)) +  
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), high = muted("red"), limits = c(-1,1)) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + labs(fill='Correlation') +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0, "in"),  height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering) +
  annotation_scale(pad_x = unit(1, "in"), pad_y = unit(0, "in"))

## Figure 7 --------------------------------------------------------------------

# Interactive Graphics

library(ggiraph)

## Figure 7b

#Step 1: Correlation Plot with Correlation and R2 output

corr_data <- correlation_trial %>% mutate(
  tooltip_text = (paste0("R =", round(correlation,3), ",",
                         " R^2 = ", round((correlation^2), 3)))
) 

g <- corr_data %>% # create an interactive version of the correlation plot in figure 6
  ggplot() +
  geom_sf_interactive(color = "black", aes(fill = correlation, tooltip = tooltip_text, data_id = group)) +  
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), high = muted("red"), limits = c(-1,1)) +
  theme(axis.text.x = element_blank(), #remove coordinate labels and background color
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + labs(fill='Correlation') + 
  annotation_scale(pad_x = unit(1, "in"), pad_y = unit(0, "in"), line_width = 0.5, height = unit(0.25, "cm"))

#Step 2: Scatterplot

my_colors <- RColorBrewer::brewer.pal(5, "Blues")[1:5]

scat_data <- combine_identity_info %>%
  mutate(
    tooltip_text = paste0("Yield =", round(yield,3), " bu/ac, ", 
                          "Target Rate = ", round(tgti,3), " lbs, ",
                          "As Applied Rate = ", round(input_rate,3), " lbs")
  ) 

latest_target <- ggplot(scat_data, aes(x =  input_rate, y = yield, color = as.factor(tgti))) + geom_point_interactive(aes(tooltip = tooltip_text, data_id = group)) +
  scale_fill_manual(values= my_colors,
                    name = "Target Nitrogen (lbs)",
                    aesthetics = c("colour", "fill"))  + theme_bw() +
  xlab("Applied Input Rate (lbs)") + ylab("Yield (bu/ac)") #scatterplot of as-applied data


#Step 3: Plots are connected by trial_id

library(patchwork)
#Links Corrleation Plot with ScatterPlot
girafe(code = print(g + latest_target), 
       width_svg = 8, height_svg = 4, options = list(
         opts_hover_inv(css = "opacity:0.2;"), # Inverse Hovering - add opacity to points in scatterplot not selected
         opts_hover(girafe_css(
           css = "stroke:gray;",
           line = "fill:none",
           area = "stroke-width:2px;stroke:black", #coloring for trial polygon
           point = "fill:orange;stroke-width:1px;stroke:orange", #coloring for scatterplot
         )), 
         opts_tooltip(opacity = 0.5) #Highlight connected data
       ))

### -- 
