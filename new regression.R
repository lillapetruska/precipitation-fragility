# Mass Mobilization Data

# Author: Name
# Version: 2021-02-22

# Libraries
library(tidyverse)
library(sf)
library(fixest)

precip_filepath <- "~/Github/precipitation-fragility/data/precip_1990_2020.rds"
precipitation <- read_rds(precip_filepath)

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
    "Eswatini" = "eSwatini",
    "United Arab Emirate" = "United Arab Emirates",
    "Bosnia" = "Bosnia and Herzegovina",
    "Serbia and Montenegro" = "Republic of Serbia",
    "Timor Leste" = "East Timor"
  )

#===============================================================================

mob <- read_csv("~/Downloads/dataverse_files/mmALL_073120_csv.csv")

mob <- 
  mob %>% 
  group_by(country, year) %>% 
  summarize(num_conflict = n()) %>% 
  mutate(country = recode(country, !!! country_names_recode))

precipitation <- precipitation %>% st_drop_geometry()

precipitation <- 
  precipitation %>%
  dplyr::select(SOVEREIGNT, year, yearly_prec) %>% 
  rename("country" = SOVEREIGNT)

setdiff(mob$country, precipitation$country)

precip_mob <- 
  precipitation %>% 
  left_join(mob, by = c("country", "year")) %>% 
  mutate(
    yearly_prec2 = yearly_prec^2,
    yearly_prec_lag1 = lag(yearly_prec),
    yearly_prec_lag2 = lag(yearly_prec, n = 2)
  ) %>% 
  drop_na()

fixest::feols(num_conflict ~ 0 | country + year, data = precip_mob)

x <- 0:4300

model <- fixest::feols(num_conflict ~ poly(yearly_prec_lag2, 2) | country + year, data = precip_mob)

yy = x*model$coefficients[1] + x^2*model$coefficients[2]  

plot(x, yy, type = "l", las = 1, ylab = "number of conflicts", xlab = "yearly precipitation two years ago")

precip_mob$pred <- predict(model, newdata = precip_mob)
 
precip_mob %>% 
  #filter(country == "Zimbabwe") %>% 
  ggplot(aes(x = yearly_prec_lag2)) + 
  #geom_line(aes(y = pred)) + 
  geom_smooth(aes(y = num_conflict))

precip_mob %>% 
  ggplot(aes(x = year, y = num_conflict, group = country)) + 
  geom_line()

precip_mob %>% 
  group_by(country) %>% 
  mutate(average_precipitation = mean(yearly_prec)) %>% 
  ungroup() %>% 
  mutate(precip_dif = yearly_prec_lag2 - average_precipitation) %>% 
  filter(country == "United Kingdom") %>% 
  ggplot(aes(x = precip_dif, y = pred)) + 
  geom_point()



