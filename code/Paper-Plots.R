## Necessary Packages-----------------------------------------------------------

library(sf)
library(ggplot2)
library(tmap)
library(ggcorrplot)
library(patchwork)
library(flextable)
library(officer)
library(parallel)
library(tidyverse)
library(corrplot)
library(data.table)


## Data frames that would be provided ------------------------------------------

# trial_design (6 columns)
  # - Columns of Interest
  #   1. tgti (in lbs)
  #   2. type: headland or trial
  #   3. geometry: polygon coordinates of trial plot
  #   6. tgt_gal: target rate in gallons 
  # - One row for each trial plot
# ry_plot_data: Raw yield (19 columns)
  # - Columns of Interest
  #   2. yield_vol
  #   9. yield_id
  #   10/11. X/Y (coordinate)
  #   17. geometry (yield polygons)
  # - One row for each yield measurement
# data_sf: processed data frame with yield and input rate together


## Figure 3  -------------------------------------------------------------------

#tmap_mode("view")#Interactive tmap - leaflet (ability to zoom) - also can remove one of the shown maps (for example, can just use trial design or yield)

#Resource: https://r-tmap.github.io/tmap-book/layout.html

tm_shape(trial_design) + tm_borders() + #first trial design (bottom layer)
  tm_fill(
    col = "tgti", 
    palette = "Greens",
    title = "Targeted\nNitrogen Rate\n(lbs)",
    style = "cat"
  ) + 
  tm_layout_to_add + 
  tm_layout( #legend position
    legend.position = c(0.25, 0.10),
    attr.position = c(0.25,0.0)
  ) +
  tm_shape(filter(ry_plot_data, flag_bad == 0)) + #second yield information (top layer)
  tm_fill(
    col = "yield_vol", 
    palette = "YlOrRd", 
    title = "Yield\n(bu/acre)",
    style = "cont",
    alpha = 0.4 #transparency to top layer
  ) 


#tmap_mode("plot") #Turn Interactive Map Back to static


## Figure 4 --------------------------------------------------------------------

trial_design_copy <- trial_design
trial_design_copy$tgt_gal <- as.factor(trial_design_copy$tgt_gal)

tm_shape(trial_design) + tm_borders() + 
  tm_fill(
    col = "tgti", 
    palette = "Blues",
    title = "Targeted\nNitrogen Rate\n(lbs)",
    #alpha = 0.9,
    # legend.is.portrait = FALSE,
    style = "cat",
  ) + 
  tm_shape(filter(ry_plot_data, flag_bad == 0)) + tm_polygons(col = "yield_vol", 
                                                              palette = "Reds",
                                                              title = "Yield (bu/ac)",
                                                              #style = "jenks",
                                                              #legend.hist = TRUE,
                                                              border.col = NA,
                                                              border.alpha = 0.1,
                                                              alpha = 0.4,
                                                              style= "cont"
  ) +
  tm_layout_to_add + 
  tm_layout(
    legend.position = c(0.15, 0.15)
  ) 

## Figure 5 (Bivariate Color Map) ----------------------------------------------


trial_design$trial_id <- 1:nrow(trial_design) #Create id for trial plots (make them identifiable)
trial_design_trial_only <- filter(trial_design, type == "trial") #only want trial, not headland

library(cowplot) #for color lgend

ry_plot_filter <- filter(ry_plot_data, flag_bad == 0) #only want data not flagged for issues
ry_plot_filter$obs_id <- 1:nrow(ry_plot_filter) #give each yield point an id

#Step 1: Find yield points within each trial design plot. Connect this information into one data frame
library(reshape2)
points <-st_as_sf(ry_plot_filter, coords = c("X", "Y"))
polyg <-trial_design_trial_only$geometry # LIST OF INTERSECTION POLYGONS
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers2 <- melt(FrameData)[,-1] #gives list of index and intersection group
colnames(identifiers2) <- c("obs_id", "trial_id")

identify_trial2 <- data.frame(merge(trial_design_trial_only, identifiers2, by = "trial_id")) #Data frame of all of the points in what trial plot
identify_yield2 <- data.frame(merge(ry_plot_filter, identifiers2, by = "obs_id")) #raw yield in what trial plot
combine_identity_info2 <-  inner_join(identify_trial2, identify_yield2, by = c("obs_id", "trial_id")) #final combined data frame

#Step 2: Create Quantiles and Assign Colors

#9 classes
#Resource: https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/

#Beginning and end flips at some point - attempt to stop this (find better way later)
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
quantiles_yield <-  combine_identity_info2 %>%
  pull(yield_vol) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for Target
quantiles_tgti<- combine_identity_info2 %>%
  pull(tgti) %>%
  quantile(probs = seq(0, 1, length.out = 4))

#Assigns each yield and target rate into a group/color
determine_quantiles <- combine_identity_info2 %>%
  mutate(
    yield_quantiles = cut(
      yield_vol,
      breaks = quantiles_yield,
      include.lowest = TRUE
    ),
    target_quantiles = cut(
      tgti,
      breaks = quantiles_tgti,
      include.lowest = TRUE
    ),
    group = paste(
      as.numeric(target_quantiles), "-",
      as.numeric(yield_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

determine_quantiles_cut <- determine_quantiles[,c(1,2,10,17,22,27,30)] #cut out unnecessary columns
colnames(determine_quantiles_cut)[6] <- "geometry" #rename yield polygon column


quant_colr <- determine_quantiles[,28:30] #obtain just yield quantile, target quantile, and color columns
quant_colr2<- quant_colr[!duplicated(quant_colr),] #unique colors with quantiles

determine_quantiles_rem_dup <- determine_quantiles_cut[!duplicated(determine_quantiles_cut$yield_id, fromLast=T), ]

map <- determine_quantiles_rem_dup %>% #color each yield polygon based on quantiles
  ggplot() +
  geom_sf(data = determine_quantiles_rem_dup$geometry, aes(fill = determine_quantiles_rem_dup$fill), color = NA) + 
  scale_fill_identity() +
  geom_sf(data = trial_design$geometry, color = "black", aes(fill = NA)) + #add boundaries of trial plots
  theme(axis.text.x = element_blank(), #remove coordinate labels and background color
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

legend <- ggplot() + #creates legend of the colors for combination of quantiles
  geom_tile(
    data = quant_colr2,
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
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) #Rotate labels to make them easier to read
  ) +
  coord_fixed()


ggdraw() + #put map and legend next to each other 
  draw_plot(map, 0.05, 0.05, 1, 1) +
  draw_plot(legend, 0, 0, 0.5, 0.3)

#---------------------

## ScatterPlot of as-appplied vs yield

# create 3 buckets for yield associated with input application
quantiles_yield <- data_sf %>%
  pull(yield) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for as-applied application
quantiles_tgti<- data_sf %>%
  pull(input_rate) %>%
  quantile(probs = seq(0, 1, length.out = 4))

#Assign each applied/yield combo to a quantile
determine_quantiles2 <- data_sf %>%
  mutate(
    gini_quantiles = cut(
      yield,
      breaks = quantiles_yield,
      include.lowest = TRUE
    ),
    target_quantiles = cut(
      input_rate,
      breaks = quantiles_tgti,
      include.lowest = TRUE
    ),
    group = paste(
      as.numeric(target_quantiles), "-",
      as.numeric(gini_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

#Plot scatterplot with background colors matching the legend above

determine_quantiles2 %>% ggplot(aes(x = input_rate, y = yield)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=quantiles_tgti[[2]],ymin=-Inf,ymax=quantiles_yield[[2]]),
            fill="#E5E5E5") +
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[2]],xmax=quantiles_tgti[[3]],ymin=-Inf,ymax=quantiles_yield[[2]]),
            fill="#DFB0D6") + 
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[3]],xmax=Inf,ymin=-Inf,ymax=quantiles_yield[[2]]),
            fill="#BE64AC") + 
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=quantiles_tgti[[2]], ymin = quantiles_yield[[2]], ymax=quantiles_yield[[3]]),
            fill="#ACE4E4") + 
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[2]],xmax=quantiles_tgti[[3]],ymin = quantiles_yield[[2]], ymax=quantiles_yield[[3]]),
            fill="#9B9B9B") + 
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[3]],xmax=Inf,ymin = quantiles_yield[[2]], ymax=quantiles_yield[[3]]),
            fill="#8C62AA") +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=quantiles_tgti[[2]],ymin=quantiles_yield[[3]], ymax=Inf),
            fill="#5AC8C8") +
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[2]],xmax=quantiles_tgti[[3]],ymin=quantiles_yield[[3]],ymax=Inf),
            fill="#5698B9") + 
  geom_rect(data=NULL,aes(xmin=quantiles_tgti[[3]],xmax=Inf,ymin=quantiles_yield[[3]],ymax=Inf),
            fill="#1B1B1B") +
  geom_point(color = "white", fill = "black", shape = 21) + 
  xlab("Applied Input Rate (lbs)") + ylab("Yield (bu/ac)")




#### Figure 6 ------------------------------------------------------------------

#The Code lines here are 1) finding where the yield/as-applied points are in the trial_design plot

#Step 1: Identify which trial plot each as-applied measurement is found in 
#library(reshape2)
points <-st_as_sf(data_sf, coords = c("X", "Y"))
polyg <-trial_design_trial_only$geometry # LIST OF INTERSECTION POLYGONS
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers <- melt(FrameData)[,-1] #gives list of index and intersection group
colnames(identifiers) <- c("obs_id", "trial_id")

identify_trial <- data.frame(merge(trial_design_trial_only, identifiers, by = "trial_id")) #Data frame of all of the points in what trial plot
identify_yield <- data.frame(merge(data_sf, identifiers, by = "obs_id")) #processed yield in what trial plot
combine_identity_info <-  inner_join(identify_trial, identify_yield, by = c("obs_id", "trial_id"))

#Step 2: Calculate corr(as-applied, yield) within each trial plot
calc_corr <- combine_identity_info %>% group_by(trial_id) %>% summarize(correlation = cor(yield, input_rate)) 
correlation_trial <- inner_join(trial_design, calc_corr, by = "trial_id") #combine with trial design information 


library(scales)
#correlation between as-applied and yield within proper trial plot
correlation_trial %>%
  ggplot() +
  geom_sf(color = "black", aes(fill = correlation)) +  
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), high = muted("red"), limits = c(-1,1)) +
  theme(axis.text.x = element_blank(), #remove coordinate labels and background color
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + labs(fill='Correlation')


## Figure 7 --------------------------------------------------------------------

# Interactive Graphics

library(ggiraph)

#Step 1: Correlation Plot with Correlation and R2 output

corr_data <- correlation_trial %>% mutate(
  tooltip_text = (paste0("R =", round(correlation,3), ",",
                         " R^2 = ", round((correlation^2), 3)))
) #Information want to display when hover over a trial plot in correlation plot

g <- corr_data %>% #same as Figure 6, but with tooltip and id for interactivity
  ggplot() +
  geom_sf_interactive(color = "black", aes(fill = correlation, tooltip = tooltip_text, data_id = trial_id)) +  
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), high = muted("red"), limits = c(-1,1)) +
  theme(axis.text.x = element_blank(), #remove coordinate labels and background color
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + labs(fill='Correlation') #same correlation plot as Figure 6


scat_data <- combine_identity_info %>% 
  mutate(
    tooltip_text = paste0("Yield =", round(yield,3), " bu/ac, ", 
                          "Target Rate = ", round(tgti,3), " lbs, ",
                          "As Applied Rate = ", round(input_rate,3), " lbs")
  ) #Information want to display when hover over a point in scatterplot

my_colors <- RColorBrewer::brewer.pal(5, "Blues")[1:5] #get desired blues for scatterplot color scheme

latest_target <- ggplot(scat_data, aes(x =  input_rate, y = yield, color = as.factor(tgti))) + geom_point_interactive(aes(tooltip = tooltip_text, data_id = trial_id)) +
  geom_smooth(se = FALSE, color = "black") + 
  #facet_wrap(vars(tgti)) +
  scale_fill_manual(values= my_colors,
                    name = "Target Nitrogen (lbs)",
                    aesthetics = c("colour", "fill"))  + 
  xlab("Applied Input Rate (lbs)") + ylab("Yield (bu/ac)") #scatterplot of as-applied data vs yield

#Plots are connected by data_id = trial_id

#Links Corrleation Plot with ScatterPlot
girafe(code = print(g + latest_target), 
       width_svg = 8, height_svg = 4, options = list(
         opts_hover_inv(css = "opacity:0.05;"), # Inverse Hovering - add opacity to points in scatterplot not selected
         opts_hover(css = "stroke-width:3;"), 
         opts_hover(css = "fill:yellow;stroke:yellow;"), #Highlight connected data
         opts_tooltip(opacity = 0.5)#opacity of tooltip box
       ))


