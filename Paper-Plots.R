## Necessary Packages-----------------------------------------------------------

library(here)
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


#Field Using as Example
ffy <- "Wendte_LaueLib80_2021"

#Source Files 
source(("Code/prepare.R"))
source(("Code/unpack_field_parameters.R")) #Extracts the necessary information

dictionary <- fromJSON(
  file.path(
    "data",
    "variable_name_dictionary.json"
  ),
  flatten = TRUE
) %>% 
  data.table()


input_type <- "N"

########################
## From Original Report
########################


## Read in datasets for visualization ------------------------------------------ 

analysis_data <- "data/Analysis/analysis_data.rds" %>% 
  readRDS() %>% 
  setnames(
    paste0(tolower(input_type), "_rate"),
    "input_rate"
  )

if (list.files("data/Trial_Design",pattern = ".shp") %>% length() > 1) {
  trial_design <- here("data", paste0("Trial_Design/trial-design-", tolower(input_type), ".shp")) %>% 
    st_read() %>% 
    setnames(names(.), tolower(names(.))) %>% 
    st_transform_utm()
} else{
  trial_design <- here("data","Trial_Design/trial-design-n.shp") %>% 
    st_read() %>% 
    setnames(names(.), tolower(names(.))) %>% 
    st_transform_utm()
}

if("type" %in% names(trial_design) == TRUE){
  trial_design <- trial_design %>%
    mutate(type = tolower(type))
}else{
  trial_design <- trial_design %>%
    mutate(area = st_area(.)) %>%
    mutate(type = case_when(area >= median(area)*2 ~ "Headland",
                            area < median(area)*2 ~ "Trial"))
}


yield_polygons <- readRDS(here("data", "Intermediate/yield_polygons.rds"))

input_data <- readRDS(
  "data/as_applied_n.rds")


w_trial_info <- filter(
  trial_info,
  input_type == "N"
)


get_input <- function(opt_gc_data, c_type, w_zone){
  opt_gc_data[
    type == c_type & zone_txt == paste0("Zone ", w_zone), 
    input_rate
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
}

get_pi <- function(opt_gc_data, c_type, w_zone){
  opt_gc_data[
    type == c_type & zone_txt == paste0("Zone ", w_zone),
    profit_hat
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
}

get_pi_dif <- function(pi_dif_test_zone, w_zone){
  pi_dif_test_zone[
    zone_txt == paste0("Zone ", w_zone), 
    point_est_dif
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
}



get_pi_ci_whole <- function(whole_profits_test, type_sh){
  
  pi_up <-whole_profits_test[
    type_short==paste0(type_sh), 
    point_est_dif + 1.96 * point_est_dif_se
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
  
  pi_low <- whole_profits_test[
    type_short==paste0(type_sh), 
    point_est_dif - 1.96 * point_est_dif_se
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
  
  return(paste0("$", pi_low, " - $", pi_up))
  
}

get_pi_ci_zone <- function(pi_dif_test_zone, w_zone){
  
  pi_up <- pi_dif_test_zone[
    zone_txt == paste0("Zone ", w_zone), 
    point_est_dif + 1.96 * point_est_dif_se
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
  
  pi_low <- pi_dif_test_zone[
    zone_txt == paste0("Zone ", w_zone), 
    point_est_dif - 1.96 * point_est_dif_se
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
  
  return(paste0("$", pi_low, " and $", pi_up))
  
}

get_t_value <- function(pi_dif_test_zone, w_zone){
  pi_dif_test_zone[
    zone_txt == paste0("Zone ", w_zone), 
    t
  ] %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
} 


## Read in all the analysis results -------------------------------------------- 

results <- readRDS("data/Analysis/analysis_results.rds") %>% 
  filter(input_type == "N")

data_sf <- results$data[[1]]
field_vars <- results$field_vars[[1]]
field_plots <- results$field_plots[[1]]
eval_data <- results$eval_data[[1]]
opt_gc_data <- results$opt_gc_data[[1]]
whole_profits_test <- results$whole_profits_test[[1]]
pi_dif_test_zone <- results$pi_dif_test_zone[[1]]

num_zones <- data_sf$zone_txt %>% 
  unique() %>% 
  length()


## Get the aspect ratio of the field -------------------------------------------

field_bbox <- st_bbox(analysis_data)

sn_length <- field_bbox["ymax"] - field_bbox["ymin"]
ew_length <- field_bbox["xmax"] - field_bbox["xmin"]

sn_ew_ratio <- sn_length / ew_length

## Trial design information ----------------------------------------------------

dict_td <- dictionary[type == "trial_design", ]
col_list <- dict_td[, column]

trial_design <- make_var_name_consistent(
  trial_design, 
  dict_td 
) %>% 
  setnames(
    paste0("tgt_", tolower(input_type)), 
    "tgti"
  )

field_data2 <- filter(field_data, field_year == ffy)

if (input_type == "N") {
  
  trial_design <- trial_design %>%
    mutate(tgt_gal = tgti)
  
  trial_design <- mutate(trial_design, 
                         tgti = convert_N_unit(
                           input_data_n$form, 
                           input_data_n$unit, 
                           tgti, 
                           field_data2$reporting_unit
                         ) + n_base_rate
  )
} else if (input_type == "S") {
  #--- seed rate conversion ---#
  if (any(trial_design$tgti > 10000)){
    #--- convert to K ---#
    trial_design <- mutate(trial_design, tgti = tgti / 1000)
  }
}

if (input_type == "N") {
  tgt_gal_ls <- unique(trial_design$tgt_gal) 
  tgt_gal_ls <- tgt_gal_ls[order(tgt_gal_ls)]
} else if (input_type == "N") {
  tgt_gal_ls <- NA
}

tgti_ls <- unique(trial_design$tgti) 
tgti_ls <- tgti_ls[order(tgti_ls)]

## Trial-design ----------------------------------------------------------------

if (sn_ew_ratio > 1.1) {
  
  tm_tgti <- tm_shape(trial_design) +
    tm_fill(
      col = "tgti", 
      palette = "Greens",
      title = "Targeted\nNitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cat"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
  
} else {
  
  tm_tgti <- tm_shape(trial_design) +
    tm_fill(
      col = "tgti", 
      palette = "Greens",
      title = "Targeted Nitrogen Rate (lbs)",
      legend.is.portrait = FALSE,
      style = "cat"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
  
}

## As-planted Seed Rate --------------------------------------------------------


input_plot_data <- trial_design %>% 
  filter(type != "headland") %>% 
  input_data[., ]

if (sn_ew_ratio > 1.1) {
  tm_input_aa <- tm_shape(input_plot_data) +
    tm_fill(
      col = "input_rate", 
      palette = "Greens",
      title = "As-applied\nNitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
  
} else {
  tm_input_aa <- tm_shape(input_plot_data) +
    tm_fill(
      col = "input_rate", 
      palette = "Greens",
      title = "As-applied Nitrogen Rate (lbs)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
  
}

## Raw yield -------------------------------------------------------------------

ry_plot_data <- trial_design %>% 
  filter(type != "Headland") %>% 
  yield_polygons[., ]

if (sn_ew_ratio > 1.1) {
  tm_ry <- tm_shape(filter(ry_plot_data, flag_bad == 0)) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_ry <- tm_shape(filter(ry_plot_data, flag_bad == 0)) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield \n (bu/acre)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}

## Yield (processed) -----------------------------------------------------------


analysis_data_td <- trial_design %>% 
  filter(type != "Headland") %>% 
  analysis_data[., ]

if (sn_ew_ratio > 1.1) {
  tm_p_yield <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_p_yield <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield (bu/acre)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}


## Seed rate (processed) -------------------------------------------------------

if (sn_ew_ratio > 1.1) {
  tm_p_input <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "input_rate", 
      palette = "YlGn",
      title = "Nitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_p_input <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "input_rate", 
      palette = "YlGn",
      title = "Nitrogen Rate (lbs)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}


## Figure 3 --------------------------------------------------------------------


tmap_mode("view")#Interactive tmap - leaflet (ability to zoom) - also can remove one of the shown maps (for example, can just use trial design or yield)

#Resource: https://r-tmap.github.io/tmap-book/layout.html

tm_shape(trial_design) + tm_scale_bar() + tm_borders() + tm_compass(size = 1, position = c("left", "top")) +
  tm_fill(
    col = "tgti", 
    palette = "Greens",
    title = "Targeted\nNitrogen Rate\n(lbs)",
    # legend.is.portrait = FALSE,
    #style = "cont",
    style = "cat"
    #alpha = 0.5 # add transparency to hopefully see both
  ) + 
  tm_layout_to_add + 
  tm_layout(
    # legend.outside.position = "bottom",
    legend.position = c(0.25, 0.10),
    attr.position = c(0.25,0.0)
  ) + #tm_shape(trial_design) + tm_borders() + 
  tm_shape(filter(ry_plot_data, flag_bad == 0)) +
  tm_fill(
    col = "yield_vol", 
    palette = "YlOrRd", #-RdBu
    title = "Yield\n(bu/acre)",
    # legend.is.portrait = FALSE,
    style = "cont",
    alpha = 0.4
  ) 


tmap_mode("plot") #Turn Interactive Map Back to static


## Figure 4 --------------------------------------------------------------------

trial_design_copy <- trial_design
trial_design_copy$tgt_gal <- as.factor(trial_design_copy$tgt_gal)

tmap_mode("view")

tm_shape(trial_design_copy) + tm_borders() + 
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
    # legend.outside.position = "bottom",
    legend.position = c(0.15, 0.15)
  ) 

tmap_mode("plot")




#### Figure 6 ------------------------------------------------------------------

trial_design$trial_id <- 1:nrow(trial_design) #Create id for trial plots (make them identifiable)
ry_plot_data_copy <- ry_plot_data

#Using processed data
#The Code lines here are 1) finding where the yield/as-applied points are in the trial_design plot

trial_design_trial_only <- filter(trial_design, type == "trial")
library(reshape2)
points <-st_as_sf(data_sf, coords = c("X", "Y"))
polyg <-trial_design_trial_only$geometry # LIST OF INTERSECTION POLYGONS
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers <- melt(FrameData)[,-1] #gives list of index and intersection group
colnames(identifiers) <- c("obs_id", "trial_id")

identify_trial <- data.frame(merge(trial_design_trial_only, identifiers, by = "trial_id")) #Data frame of all of the points in what trial plot
identify_yield <- data.frame(merge(data_sf, identifiers, by = "obs_id")) #processed yield in what trial plot
combine_identity_info <-  inner_join(identify_trial, identify_yield, by = c("obs_id", "trial_id"))

calc_corr <- combine_identity_info %>% group_by(trial_id) %>% summarize(correlation = cor(yield, input_rate))
correlation_trial <- inner_join(trial_design, calc_corr, by = "trial_id")


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

## Attempt 5 (Scatterplots of data)

#Example of what we want to show in linked plots - Scatterplot of actual data (colored by trial design)
my_colors <- RColorBrewer::brewer.pal(5, "Blues")[1:5]

## Figure 7 --------------------------------------------------------------------

# Interactive Graphics

library(ggiraph)

#Step 1: Correlation Plot with Correlation and R2 output

corr_data <- correlation_trial %>% mutate(
  tooltip_text = (paste0("R =", round(correlation,3), ",",
                         " R^2 = ", round((correlation^2), 3)))
) #Information want to display when hover

g <- corr_data %>%
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
  ) #Information want to display when hover

latest_target <- ggplot(scat_data, aes(x =  input_rate, y = yield, color = as.factor(tgti))) + geom_point_interactive(aes(tooltip = tooltip_text, data_id = trial_id)) +
  geom_smooth(se = FALSE, color = "black") + 
  #facet_wrap(vars(tgti)) +
  scale_fill_manual(values= my_colors,
                    name = "Target Nitrogen (lbs)",
                    aesthetics = c("colour", "fill"))  + 
  xlab("Applied Input Rate (lbs)") + ylab("Yield (bu/ac)")#scatterplot of as-applied data

#Plots are connected by trial_id

#Links Corrleation Plot with ScatterPlot
girafe(code = print(g + latest_target), 
       width_svg = 8, height_svg = 4, options = list(
         opts_hover_inv(css = "opacity:0.05;"), # Inverse Hovering - add opacity to points in scatterplot not selected
         opts_hover(css = "stroke-width:3;"), 
         opts_hover(css = "fill:yellow;stroke:yellow;"),
         opts_tooltip(opacity = 0.5)#Highlight connected data
       ))




## Figure 5 (Bivariate Color Map) ----------------------------------------------

library(cowplot) #for color lgend

ry_plot_filter <- filter(ry_plot_data, flag_bad == 0)
ry_plot_filter$obs_id <- 1:nrow(ry_plot_filter)

#library(reshape2)
points <-st_as_sf(ry_plot_filter, coords = c("X", "Y"))
polyg <-trial_design_trial_only$geometry # LIST OF INTERSECTION POLYGONS
z <- st_intersects(polyg,points) #Return index of points within each intersection
FrameData <- lapply(z, function(x) as.data.frame(x))
identifiers2 <- melt(FrameData)[,-1] #gives list of index and intersection group
colnames(identifiers2) <- c("obs_id", "trial_id")

identify_trial2 <- data.frame(merge(trial_design_trial_only, identifiers2, by = "trial_id")) #Data frame of all of the points in what trial plot
identify_yield2 <- data.frame(merge(ry_plot_filter, identifiers2, by = "obs_id")) #raw yield in what trial plot
combine_identity_info2 <-  inner_join(identify_trial2, identify_yield2, by = c("obs_id", "trial_id"))

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

determine_quantiles_cut <- determine_quantiles[,c(1,2,10,17,22,27,30)]
colnames(determine_quantiles_cut)[6] <- "geometry"


quant_colr <- determine_quantiles[,28:30]
quant_colr2<- quant_colr[!duplicated(quant_colr),] #unique colors with quantiles

determine_quantiles_rem_dup <- determine_quantiles_cut[!duplicated(determine_quantiles_cut$yield_id, fromLast=T), ]

map <- determine_quantiles_rem_dup %>%
  ggplot() +
  geom_sf(data = determine_quantiles_rem_dup$geometry, aes(fill = determine_quantiles_rem_dup$fill), color = NA) + 
  scale_fill_identity() +
  geom_sf(data = trial_design$geometry, color = "black", aes(fill = NA)) +
  theme(axis.text.x = element_blank(), #remove coordinate labels and background color
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

legend <- ggplot() +
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


ggdraw() +
  draw_plot(map, 0.05, 0.05, 1, 1) +
  draw_plot(legend, 0, 0, 0.5, 0.3)

#---------------------

## ScatterPlot

# create 3 buckets for yield associated with input application
quantiles_yield <- data_sf %>%
  pull(yield) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for input application
quantiles_tgti<- data_sf %>%
  pull(input_rate) %>%
  quantile(probs = seq(0, 1, length.out = 4))

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

#Squares

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
#geom_point(color = "black", fill = "white", shape = 21)



