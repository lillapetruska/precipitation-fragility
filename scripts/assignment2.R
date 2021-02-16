# Project Assignment 2

# Author: Lilla
# Version: 2021-02-15

# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(raster)
library(fixest)
library(sf)
# Parameters
fsi <- read_rds(here::here("data/fsi.rds"))
precip_filepath <- "~/Github/precipitation-fragility/data-raw/precip_mon_mean.nc"
countries_filepath <- "~/GitHub/precipitation-fragility/data/ne_50m_admin_0_countries_lakes/"

country_names_recode <-
  c(
    "Congo Democratic Republic" = "Democratic Republic of the Congo",
    "Cote d'Ivoire" = "Ivory Coast",
    "Kyrgyz Republic" = "Kyrgyzstan",
    "Guinea Bissau" = "Guinea-Bissau",
    "Serbia" = "Republic of Serbia",
    "Israel and West Bank" = "Israel",
    "Tanzania" = "United Republic of Tanzania",
    "Slovak Republic" = "Slovakia",
    "Czech Republic" = "Czechia",
    "United States" = "United States of America",
    "Timor-Leste" = "East Timor",
    "Congo Republic" = "Republic of the Congo",
    "Swaziland" = "eSwatini",
    "Cape Verde" = "Cabo Verde",
    "Sao Tome and Principe" = "São Tomé and Principe",
    "Micronesia" = "Federated States of Micronesia",
    "Brunei Darussalam" = "Brunei",
    "Bahamas" = "The Bahamas",
    "Eswatini" = "eSwatini"
  )
#===============================================================================

fsi <-
  fsi %>%
  rename_all(~ str_remove(., ":")) %>%
  rename_all(~ str_to_lower(str_replace_all(., "!!|\\s+", "_"))) %>%
  mutate(
    year = year(year),
    rank = str_extract(rank, "\\d*"),
    country = recode(country, !!! country_names_recode)
  )

model <- feols(total ~ 0 | country + year, data = fsi)

summary(model)
# R^2 is 0.978897, which is very high. Means that 0.021103 of variation could
# be explained by something else like precipitation

v <-
  fsi %>%
  group_by(country) %>%
  mutate(mean_total = mean(total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(demeaned_total = total - mean_total) %>%
  group_by(country) %>%
  mutate(sd_mean_total = sd(demeaned_total)) %>%
  ungroup() %>%
  mutate(size_IV = sd_mean_total / mean_total) %>%
  ggplot(aes(size_IV)) +
  geom_histogram() +
  labs(x = "standard deviation of demeaned outcome / mean outcome")

#-------------------------------------------------------------------------------
precip_raster <- raster::brick(precip_filepath)

precip_raster %>% filter(str_detect(names, "2020")) # subset based on names

precip_2006_2020 <- precip_raster %>% subset(325:504)

# Read in country boundaries shapefile
countries <- read_sf(countries_filepath) %>% st_as_sf()

precip_by_country <- raster::extract(precip_2006_2020, countries, small = T)


view(precip_by_country)

view(precip_by_country[[1]])

