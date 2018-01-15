#
# This script combines feature tables and loads census shapefiles into R
#

# Set up ------------------------------------------------------------------

library(tidyverse)
library(rgdal)
library(lubridate)
library(ISOweek)

path_data_in <- "/Users/kathy/Documents/_projects/the-rat-hack/data/"
path_data_out <- "/Users/kathy/Documents/_projects/the-rat-hack/scripts/explore_features/data/"


# Read in data ------------------------------------------------------------

# feature tables
restaurant_inspections_overdue <- read_csv(
  paste0(path_data_in, "feature_tables/restaurant_inspections_overdue.csv"),
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
  paste0(path_data_in, "feature_tables/restaurant_inspections_frequency.csv"),
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
census_tract <- readOGR(paste0(path_data_in, "dc_census_tract_shapefiles/census_2010"), "gz_2010_11_140_00_500k")
census_block <- readOGR(paste0(path_data_in, "dc_census_block_shapefiles/census_2010"), "tl_2016_11_tabblock10")


# Process data ------------------------------------------------------------

# combine feature tables
features <- bind_rows(restaurant_inspections_frequency,
                      restaurant_inspections_overdue)

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


# Save data ---------------------------------------------------------------

save(features,
     census_tract,
     census_block,
     file = paste0(path_data_out, "app_data.RData"))

