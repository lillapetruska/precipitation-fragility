# Project Assignment 2

# Author: Lilla
# Version: 2021-02-15

# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(raster)
library(fixest)
library(exactextractr)
library(sf)
# Parameters
fsi <- read_rds(here::here("data/fsi.rds"))
precip_filepath <- "~/Github/precipitation-fragility/data-raw/precip_mon_mean.nc"
chirps_filepath <- "~/GitHub/precipitation-fragility/data/chirps-v2.0.annual.nc"
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

fsi_countries <- unique(fsi$country)

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
#chirps_raster <- raster::brick(chirps_filepath)

#names(chirps_raster)
 # subset based on names
precip_2006_2020 <- precip_raster %>% subset(325:504)

#chirps_raster <- subset(chirps_raster, c("X2006.01.01", "X2007.01.01", "X2008.01.01",
                                       #  "X2009.01.01", "X2010.01.01", "X2011.01.01", "X2012.01.01", "X2013.01.01", "X2014.01.01",
                                        # "X2015.01.01", "X2016.01.01", "X2017.01.01", "X2018.01.01", "X2019.01.01", "X2020.01.01"))

#prec <- raster::calc(chirps_raster, fun = mean, na.rm=T)

# Read in country boundaries shapefile
countries <-
  read_sf(countries_filepath) %>%
  st_as_sf() %>%
  filter(SOVEREIGNT == ADMIN) %>%
  filter(SOVEREIGNT %in% fsi_countries)

country_index <- countries$SOVEREIGNT %>% as_tibble() %>% mutate(ID = row_number())

#precip_by_country <- raster::extract(precip_2006_2020, countries, small = T, df = TRUE)

#annual_precip <-
  # precip_by_country %>%
  # pivot_longer(cols = starts_with("X"), names_to = "month", values_to = "precip") %>%
  # group_by(ID, month) %>%
  # summarize(avg_monthly_prec = mean(precip, na.rm = TRUE)) %>%
  # ungroup() %>%
  # mutate(month = str_extract(month, "\\d{4}")) %>%
  # group_by(ID, month) %>%
  # summarize(annual_precip = sum(avg_monthly_prec, na.rm = TRUE)) %>%
  # ungroup() %>%
  # left_join(country_index, by = "ID")

#Test1

precip_by_country <- exact_extract(precip_2006_2020, countries)

#lapply(precip_by_country[[1]], sum) %>% unlist()

#precip_by_country %>% map(~ .[[.]] %>% sum(.) %>% unlist(.))

y <- data.frame()
for (i in 1:178) {
  v <- lapply(precip_by_country[[i]], mean) %>% unlist()
  y <- bind_rows(y, v)
}

y <- y %>% mutate(ID=row_number()) %>% left_join(country_index, by = "ID")

y %>% pivot_longer(cols = starts_with("X"), names_to = "month", values_to = "precip")

annual <- y %>% 
  dplyr::select(-coverage_fraction, -ID) %>%
  pivot_longer(cols = starts_with("X"), names_to = "month", values_to = "precip") %>%
  mutate(month=str_remove(month, "X"), month=str_replace_all(month, "\\.", "-")) %>%
  mutate(month=ymd(month)) %>%
  mutate(precip=case_when(
    month(month) %in% c(1, 3, 5, 7, 8, 10, 12) ~ precip * 31,
    month(month) %in% c(4, 6, 9, 11) ~ precip * 30,
    month(month) %in% c(2) ~ precip * 28.25
  )) %>%
  mutate(year=year(month)) %>% 
  dplyr::select(-month) %>% 
  group_by(value, year) %>% 
  summarize(yearly_prec = sum(precip, na.rm = TRUE))

countries %>% left_join(annual, by=c("SOVEREIGNT"="value")) %>% filter(year==2020) %>% 
write_rds("~/")

plot(precip_2006_2020$X2007.09.01)
