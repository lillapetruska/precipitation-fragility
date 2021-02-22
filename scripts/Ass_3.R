library(tidyverse)
library(fixest)
library(here)
library(sf)
library(lubridate)
  
  
`%notin%` <- negate(`%in%`)
#Set filepaths
fsi <- read_rds("~/GitHub/precipitation-fragility/data/fsi.rds")
precip_filepath <- "~/Github/precipitation-fragility/data/precip_cleaned.rds"
precip_final <- read_rds(precip_filepath)

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

# Cleaning FSI data 
fsi <- 
  fsi %>%
  rename_all(~ str_remove(., ":")) %>%
  rename_all(~ str_to_lower(str_replace_all(., "!!|\\s+", "_"))) %>%
  mutate(
    year = year(year),
    rank = str_extract(rank, "\\d*"),
    country = recode(country, !!! country_names_recode)
  )

fsi_countries <- unique(fsi$country)

precip_final <- precip_final %>% st_drop_geometry()
precip_final <- precip_final %>% filter(SOVEREIGNT %in% fsi_countries)

a <- fsi %>% unite("ID", country, year) %>% select(ID)
b <- precip_final %>% unite("ID", SOVEREIGNT, year) %>% select(ID) %>% unique() %>% pull(ID)
missing_values <- setdiff(b, a$ID)

precip_final <- 
  precip_final %>% 
  unite("ID", SOVEREIGNT, year, remove = FALSE) %>% 
  filter(ID %notin% missing_values) %>% 
  select(-ID)

precip <- 
  precip_final %>%
  dplyr::select(SOVEREIGNT, year, yearly_prec) %>% 
  rename("country" = SOVEREIGNT)

combo <- 
  precip %>% 
  left_join(fsi, by = c("country", "year")) %>% 
  dplyr::select(country, year, yearly_prec, total) %>% 
  mutate(
    yearly_prec_sqrd = yearly_prec^2,
    yearly_prec_lag1 = lag(yearly_prec),
    yearly_prec_lag1_sqrd = yearly_prec_lag1,
    yearly_prec_lag2 = lag(yearly_prec_lag1),
    yearly_prec_lag2_sqrd = yearly_prec_lag2
  ) %>% 
  drop_na()


reg <- fixest::feols(total ~ yearly_prec | country + year, data = combo)
summary(reg)

reg_2 <- fixest::feols(total ~ yearly_prec + yearly_prec_sqrd | country + year, data = combo)
summary(reg_2)

reg_3 <- fixest::feols(total ~ yearly_prec_lag1 | country + year, data = combo)
summary(reg_3)

reg_4 <- fixest::feols(total ~ yearly_prec + yearly_prec_lag1 + yearly_prec_lag2 | country + year, data = combo)
summary(reg_4)

reg_5 <- fixest::feols(total ~ yearly_prec + yearly_prec_sqrd + yearly_prec_lag1 + yearly_prec_lag1_sqrd + yearly_prec_lag2 + yearly_prec_lag2_sqrd | country + year, data = combo)
summary(reg_5)

reg_6 <- fixest::feols(total ~ poly(yearly_prec, 2) + poly(yearly_prec_lag1, 2) + poly(yearly_prec_lag2, 2) | country + year, data = combo)
summary(reg_6)

reg_6 <- fixest::feols(log(total) ~ poly(yearly_prec, 2) + poly(yearly_prec_lag1, 2) + poly(yearly_prec_lag2, 2) | country + year, data = combo)
summary(reg_6)
