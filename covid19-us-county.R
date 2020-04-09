library(tidyverse)
library(tidycensus)

v18 <- load_variables(2018, "acs5", cache = TRUE)
data(fips_codes)

county_covid19 = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

county_pop <- get_decennial(geography = "county", variables = "P001001") %>% select(-NAME,-variable) %>% 
  rename(fips = GEOID, pop = value)

county_gini <- get_acs(geography = "county", variables = "B19013_001") %>% select(-NAME,-variable,-moe) %>% 
  rename(fips = GEOID, gini = estimate)

county_income <- get_acs(geography = "county", variables = "B06011_001") %>% select(-NAME,-variable,-moe) %>% 
  rename(fips = GEOID, income = estimate)

county_data = merge(x = filter(county_covid19, date == "2020-04-07"), y = county_gini, by = "fips", all.x = TRUE) %>% select(-date,-county,-state)
county_data = merge(x = county_data, y = county_pop, by = "fips", all.x = TRUE)
county_data = merge(x = county_data, y = county_income, by = "fips", all.x = TRUE)

reg = lm(formula = log(cases) ~ log(income), data = county_data)
summary(reg)

reg = lm(formula = log(cases) ~ log(income) + log(pop), data = county_data)
summary(reg)

reg = lm(formula = log(cases) ~ log(income) + log(gini) + log(pop), data = county_data)
summary(reg)

