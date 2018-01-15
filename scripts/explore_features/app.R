#
# This script creates a Shiny app to visualize and explore various features,
# with ability to aggregate by time and geography
#


# Set up ------------------------------------------------------------------

library(tidyverse)
library(rlang)
#library(data.table) # makes it easier to access columns by quoted names
library(shiny)
library(leaflet)

path_app <- "/Users/kathy/Documents/_projects/the-rat-hack/scripts/explore_features/" 

# load data
load(paste0(path_app, "data/app_data.RData"))


# UI ----------------------------------------------------------------------

# use some example options for now
agg_geo_opt <- "census_tract_2010"

agg_time_opt <- "year"
agg_time_val <- 2017

agg_feature_opt <- "feature_id"
agg_feature_val <- "restaurant_inspections_frequency"

# Server ------------------------------------------------------------------

#features_dt <- data.table(features)

# filter dataset based on time period and feature selection
features_filtered <- features %>% 
  filter(UQ(sym(agg_time_opt)) == agg_time_val & UQ(sym(agg_feature_opt)) == agg_feature_val) 

# aggregate by geography
features_agg <- features_filtered %>% 
  group_by(UQ(sym(agg_geo_opt))) %>% 
  summarise(value_mean = mean(value),
            value_sum = sum(value),
            value_sd = sd(value))

# join aggregated features to shapefiles
if(agg_geo_opt == "census_tract_2010") {
  features_census_merged <- merge(census_tract, 
                                 features_agg,
                                 by.x = "TRACT", 
                                 by.y = "census_tract_2010")
  
} else if(agg_geo_opt == "census_block_2010") {
  features_census_merged <- merge(census_block, 
                                 features_agg,
                                 by.x = "GEOID10", 
                                 by.y = "census_block_2010")
}

# a quick palette
pal <- colorNumeric(
  palette = "Blues",
  domain = features_census_merged$value_mean)

# map
m <- leaflet(data = features_census_merged) %>% 
  addTiles() %>% 
  addPolygons(
    stroke = TRUE,
    color = "black",
    weight = 1,
    fillColor = ~pal(value_mean),
    fillOpacity = 0.7,
    smoothFactor = 0.5
  )
m