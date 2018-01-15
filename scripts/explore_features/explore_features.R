library(leaflet)
library(tidyverse)
library(rgdal)
library(tictoc)
library(data.table) # for faster aggregation
library(lubridate) # must be loaded after data.table
library(ISOweek) # get month from year and week

setwd("/Users/kathy/Documents/_projects/the-rat-hack/scripts/explore_features")


# Load data ---------------------------------------------------------------

# feature tables
restaurant_inspections_overdue <- read_csv(
  "data/feature_tables/restaurant_inspections_overdue.csv",
  col_types = cols(
    feature_id = col_character(),
    feature_type = col_character(),
    feature_subtype = col_integer(),
    year = col_integer(),
    week = col_integer(),
    census_block_2010 = col_character(),
    value = col_integer()
  )
)

restaurant_inspections_frequency <- read_csv(
  "data/feature_tables/restaurant_inspections_frequency.csv",
  col_types = cols(
    feature_id = col_character(),
    feature_type = col_character(),
    feature_subtype = col_integer(),
    year = col_integer(),
    week = col_integer(),
    census_block_2010 = col_character(),
    value = col_double()
  )
)

# shapefiles
census_tract <- readOGR("data/census_shapefiles/census_tract_2010", "gz_2010_11_140_00_500k")
census_block <- readOGR("data/census_shapefiles/census_block_2010", "tl_2016_11_tabblock10")

#head(census_block@data)
#head(census_tract@data)


# Process data ------------------------------------------------------------

# combine feature tables
features <- bind_rows(restaurant_inspections_frequency,
                      restaurant_inspections_overdue)

#head(features)

# add tract id and month
# tic("add month, multi-step")
# features <- features %>% 
#   mutate(census_tract_2010 = str_sub(census_block_2010, 6, 11),
#          week_pad = str_pad(week, width = 2, side = "left", pad = "0"),
#          isoweek_str = paste0(year, "-W", week_pad, "-", 4),
#          month = month(ISOweek2date(isoweek_str)))
# toc()

# tic("add month, single step") # bit faster but not too much
# features <- features %>% 
#   mutate(census_tract_2010 = str_sub(census_block_2010, 6, 11),
#          month = month(ISOweek2date(paste0(year, "-W", str_pad(week, 2, "left", "0"), "-", 4))))
# toc()

# tic("add month, base function") # not faster
# features$census_tract_2010 <- str_sub(features$census_block_2010, 6, 11)
# features$month <- month(ISOweek2date(paste0(features$year, "-W", str_pad(features$week, 2, "left", "0"), "-", 4)))
# toc()

# tic("add month, ungrouped") # not a problem with grouping!
# features <- features %>% 
#   ungroup() %>% 
#   mutate(census_tract_2010 = str_sub(census_block_2010, 6, 11),
#          week_pad = str_pad(week, width = 2, side = "left", pad = "0"),
#          isoweek_str = paste0(year, "-W", week_pad, "-", 4),
#          month = month(ISOweek2date(isoweek_str)))
# toc()

# add census tract ID
features <- features %>%
  mutate(census_tract_2010 = str_sub(census_block_2010, 6, 11))

# get month value using crosswalk - this is much faster than calculating 
# directly from the entire dataset!
month_cw <- features %>% 
  select(year, week) %>% 
  distinct() %>% 
  mutate(week_str = str_pad(week, width = 2, side = "left", pad = "0"),
         isoweek_str = paste0(year, "-W", week_str, "-", 4), # using Thursday ensures that we get Jan for the first week, rather than Dec of previous year
         month = month(ISOweek2date(isoweek_str))) %>% 
  select(year, week, month)

# join back to main dataset
features <- features %>% 
  left_join(month_cw, by = c("year", "week"))

# convert to data.table for faster aggregation
features_dt <- data.table(features)


# Aggregate based on input ------------------------------------------------

agg_geo <- "census_tract_2010"

agg_time <- "year"
agg_time_value <- 2008

agg_feature <- c("feature_id", "feature_type", "feature_subtype")
agg_feature_id <- "restaurant_inspections_frequency"

agg_by <- c(agg_geo, agg_time)

# allow users to select geography, time period, maybe also feature granularity 
# (type/subtype)?

## filter dataset first before doing anything else
# tic("filter using data.table") # slightly faster
# features_dt_filtered <- features_dt[year == 2017 & feature_id == "restaurant_inspections_frequency",]
# toc()

tic("filter using dplyr") # using this because neater
features_dt_filtered <- features_dt %>% filter(year == 2017, feature_id == "restaurant_inspections_frequency")
toc()

# if(all(agg_by %in% c("week", "census_block_2010"))) {
#   # skip aggregation if by week and block
#   features_agg <- features_dt
#   
# } else {
#   
#   agg_by <- c("feature_id", "feature_type", "feature_subtype", agg_by)
#   
#   # add time period if aggregating by month
#   #
#   
#   #agg_by <- paste(agg_by, collapse = ", ")
#   
#   # tic("using dplyr")
#   # features_agg <- features %>% 
#   #   group_by_at(vars(one_of(agg_by))) %>% 
#   #   summarise(value_mean = mean(value),
#   #             value_sum = sum(value),
#   #             value_sd = sd(value))
#   # toc()
#   
#   # features_dt <- data.table(features)
#   
#   # tic("using data.table, sorted")
#   # setkeyv(features_dt, agg_by)
#   # 
#   # features_agg <- features_dt[, list(
#   #   value_mean = mean(value),
#   #   value_sum = sum(value),
#   #   value_sd = sd(value)),
#   #   by = agg_by]
#   # toc()
#   
#   #tic("using data.table, not sorted") # not setting key is actually the fastest!
# 
#   features_agg <- features_dt[, list(
#     value_mean = mean(value),
#     value_sum = sum(value),
#     value_sd = sd(value)),
#     by = agg_by]
#   #toc()
#   
#   
# }

# merge data to shapefiles
if(agg_geo == "census_tract_2010") {
  # nest by geography
  # features_agg_nested <- features_agg %>% 
  #   nest(-census_tract_2010)
  
  tic("using dplyr")
  features_agg <- features_dt_filtered %>% 
    group_by(census_tract_2010) %>% 
    summarise(value_mean = mean(value),
              value_sum = sum(value),
              value_sd = sd(value))
  toc()
  
  # tic("using data.table") # hmm, having trouble with by = argument.. let's table this
  # features_dt_filtered <- features_dt_filtered %>% ungroup()
  # features_agg <- features_dt_filtered[,list(value_mean = mean(value),
  #     value_sum = sum(value),
  #     value_sd = sd(value)),
  #   by = list(census_tract_2010)]
  # toc()
           
  # join aggregated features to shapefiles
  census_tract_features <- merge(census_tract, 
                                 #features_agg_nested, 
                                 features_agg,
                                 by.x = "TRACT", 
                                 by.y = "census_tract_2010")
  
} else if(agg_geo == "census_block_2010") {
  # nest by geography
  # features_agg_nested <- features_agg %>% 
  #   nest(-census_block_2010)  
  
  features_agg <- features_dt_filtered %>% 
    group_by(census_block_2010) %>% 
    summarise(value_mean = mean(value),
              value_sum = sum(value),
              value_sd = sd(value))
  
  # join aggregated features to shapefiles
  census_block_features <- merge(census_block, 
                                 #features_agg_nested, 
                                 features_agg,
                                 by.x = "GEOID10", 
                                 by.y = "census_block_2010")
}

# map geography

# a quick palette
pal <- colorNumeric(
  palette = "Blues",
  domain = census_tract_features$value_mean)

m <- leaflet(data = census_tract_features) %>% 
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






