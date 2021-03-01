# Prep temperature data

# Author: Name
# Version: 2021-02-22

# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(raster)
library(fixest)
library(sf)

# Parameters
temperature_filepath <- "~/GitHub/precipitation-fragility/data/air.mon.anom.nc"
countries_filepath <- "~/GitHub/precipitation-fragility/data/ne_50m_admin_0_countries_lakes/"

countries <-
  read_sf(countries_filepath) %>%
  st_as_sf() %>%
  filter(SOVEREIGNT == ADMIN)

#===============================================================================

# Read in the raster file as a RasterBrick
temp_raster <- raster::brick(temperature_filepath)

# KEY: adjusting the extent of the precipitation dataset to match countries shapefile dataset extent
temp_raster <- rotate(temp_raster)

# Find names for entries we want to subset
view(names(temp_raster))

# Create a RasterStack object of raster layers with our variable from 2006-2020
temp_1990_2020 <- temp_raster %>% subset(1681:2052)

# Extract mm/day country-wide averages for every month, averaged across all the cells in each country
temp_by_month <- 
  raster::extract(temp_1990_2020, countries, small = T, df = TRUE) %>%
  na.omit()
temp_by_month <- summarise(group_by(temp_by_month, ID), across(everything(), list(mean)))

# Assign indices to countries in our set
country_index <- countries$SOVEREIGNT %>% as_tibble() %>% mutate(ID = row_number())

temp_by_month <- temp_by_month %>% left_join(country_index, by = "ID")
temp_by_month <- temp_by_month %>% pivot_longer(cols = starts_with("X"), names_to = "month", values_to = "temp")

# Multiply mm/day averages for each month by # days in that month to get annual cumulative rainfall
annual <- temp_by_month %>% 
  dplyr::select(-ID) %>%
  mutate(month=str_remove(month, "X"), month=str_remove(month, "_1"), month=str_replace_all(month, "\\.", "-")) %>% 
  mutate(month2=ymd(month)) %>%
  mutate(precip=case_when(
    month(month2) %in% c(1, 3, 5, 7, 8, 10, 12) ~ precip * 31,
    month(month2) %in% c(4, 6, 9, 11) ~ precip * 30,
    month(month2) %in% c(2) ~ precip * 28.25
  )) %>%
  mutate(year=year(month)) %>%
  dplyr::select(-month) %>%
  group_by(value, year) %>%
  summarize(yearly_prec = sum(precip, na.rm = TRUE))

# Join annual cumulative rainfall with countries shapefile
precip_final <- countries %>% left_join(annual, by=c("SOVEREIGNT"="value"))

# Write RDS
precip_final %>% write_rds("~/GitHub/precipitation-fragility/data/precip_1990_2020.rds")

