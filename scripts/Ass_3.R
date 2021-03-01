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
  group_by(country) %>% 
  mutate(
    yearly_prec_sqrd = yearly_prec^2,
    yearly_prec_lag1 = lag(yearly_prec),
    yearly_prec_lag2 = lag(yearly_prec_lag1)
  ) %>% 
  drop_na()


reg <- fixest::feols(total ~ yearly_prec | country + year, data = combo)
summary(reg)

reg_2 <- fixest::feols(total ~ yearly_prec + yearly_prec_sqrd | country + year, data = combo)
summary(reg_2)

# reg_4 <- fixest::feols(total ~ yearly_prec + yearly_prec_lag1 + yearly_prec_lag2 | country + year, data = combo)
# summary(reg_4)

reg_6 <- fixest::feols(total ~ poly(yearly_prec, 2, raw = TRUE) +  poly(yearly_prec_lag1, 2, raw = TRUE) + poly(yearly_prec_lag2, 2, raw = TRUE) | country + year, data = combo)
summary(reg_6)

coef_matrix <- matrix(nrow = 100, ncol = 6)  
num_observations <- dim(combo)[1]  
for (i in 1:100)  {
  samp <- sample(1:num_observations, size = num_observations, replace = T)  
  newdata = combo[samp,]
  mod <- fixest::feols(total ~ poly(yearly_prec, 2) + poly(yearly_prec_lag1, 2) + poly(yearly_prec_lag2, 2) | country + year, data = newdata) 
  coef_matrix[i,] <- coef(mod) 
  print(i)  
}


# x <- 9:4303
# plot(1, xlim = c(0, 4500), ylim = c(10, 120), las = 1, xlab = "yearly_precipitation", ylab = "fsi")  #i'm starting by initialing an empty plotting window.  then i'm looping over my bootstrap estimates, making our predicted y's for each bootstrap, and plotting the line.
# for (i in 1:100) {
#   yy <- x * coef_matrix[i,1] + x^2 * coef_matrix[i,2]  + x * coef_matrix[i,3] + x^2 * coef_matrix[i,4] + x * coef_matrix[i,5] + x^2 * coef_matrix[i,6]
#   yy <- yy - yy[x = 2000]
#   lines(x, yy, lwd = 0.5)
# }

names <- c("B1", "B2", "B3", "B4", "B5", "B6")

CIs <- 
  confint(reg_6) %>% 
  bind_cols(reg_6$coefficients) %>% 
  bind_cols(names) %>% 
  rename("estimate" = ...3, "beta" = ...4)

CIs %>% 
  ggplot(aes(x = beta)) +
  geom_point(aes(y = estimate)) +
  geom_point(aes(y = `2.5 %`), shape = 95, size = 5) + 
  geom_point(aes(y = `97.5 %`), shape = 95, size = 5) +
  geom_hline(yintercept = 0) + 
  theme_minimal() +
  labs(
    y = " ",
    x = " "
  )

combo %>% 
  ggplot(aes(x = yearly_prec)) +
  geom_histogram(binwidth = 70)

combo$pred <- predict(reg_6, newdata = combo)

combo %>% 
  filter(country == "Zimbabwe") %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = total), color = "blue") + 
  geom_line(aes(y = yearly_prec))



