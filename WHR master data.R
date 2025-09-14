## creates and sets for annual update the master longitudinal data file
## includes WHR dta plus missing GINI data

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning

# some custom functions
source("~/Data/r/basic functions.R")


# steps
# 1 load current year file "Data for Table 2.1. should have all prior year data
# 2 pull out only latest year - CURRENT = 2023
# 3 load existing whr all rds file
# 4 add new year to existing years
# watch for country name changes from prior years

whrall_1 <- readxl::read_excel("data/DataForTable2.1_2024.xls") %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(country_name = ifelse(
    country_name == "State of Palestine", "Palestinian Territories", country_name)) %>%
  rename(ladder_score = life_ladder) %>%
  group_by(country_name) %>%
  fill(log_gdp_per_capita:negative_affect , .direction = "downup") %>%
  ungroup()

glimpse(whrall_1)

whrall_1 %>%
  count(year)

# load kaggle 2021 set to get region names
ctreg <- readr::read_csv("data/world-happiness-report-2021.csv") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(country_name = ifelse(
    country_name == "Czech Republic", "Czechia", country_name)) %>%
  mutate(country_name = ifelse(
    country_name == "Turkey", "Türkiye", country_name)) %>%
  mutate(country_name = ifelse(
    country_name == "Swaziland", "Eswatini", country_name)) %>%
  select (country_name, region_whr = regional_indicator)

glimpse(ctreg)

## country & region only

# join to whr23 on country
whrall_2 <- whrall_1 %>%
  merge(ctreg, all = TRUE) %>%
  as_tibble() %>%
  select(country_name, region_whr, year, everything()) %>%
  mutate(region_whr = ifelse(country_name == "Congo (Kinshasa)", "Sub-Saharan Africa", region_whr)) %>%
  mutate(region_whr = ifelse(country_name == "Angola", "Sub-Saharan Africa", region_whr)) %>%
  arrange(country_name, year) %>%
  filter(!is.na(ladder_score)) %>%
  mutate(region_whr = case_when(
    country_name %in% c("Belize", "Cuba", "Guyana", "Suriname", "Trinidad and Tobago") ~ "Latin America and Caribbean",
    country_name == "Bhutan" ~ "South Asia",
    country_name %in% c("Central African Republic", "Djibouti", "Eswatini", "Somalia", 
      "Somaliland region", "South Sudan", "Sudan", "Swaziland") ~ "Sub-Saharan Africa",
    country_name %in% c("Oman", "Qatar", "Syria" ) ~ "Middle East and North Africa",
    TRUE ~ region_whr))

glimpse(whrall_2)

whrall_2 %>%
  filter(is.na(region_whr)) %>%
  count(country_name)

whrall_2 %>%
  count(region_whr)

# read in GINI data for all years -------------------------------------------------------------------

## using WDI package
# WB category definitions
# https://datahelpdesk.worldbank.org/knowledgebase/articles/378834-how-does-the-world-bank-classify-countries
ginisa = WDI::WDI(indicator='SI.POV.GINI', start=2000, end=2023, extra = TRUE)

glimpse(ginisa)

ginis <- ginisa %>%
  as_tibble() %>%
  select(-status, -lastupdated) %>%
  ## fix regions
  mutate(region =
           case_when(country == "Czechia" ~ "Europe & Central Asia",
                     country == "Viet Nam" ~ "East Asia & Pacific",
                     TRUE ~ region)) %>%
  filter(region != "Aggregates") %>%
  arrange(country, year) %>%
  rename(gini = SI.POV.GINI) %>%
  mutate(ginifill = gini) %>%
  group_by(country) %>%
  mutate(gini_avg = mean(gini, na.rm = TRUE)) %>%
  fill(ginifill, .direction = "downup") %>%
  ungroup() %>%
  mutate(gini_latest = ifelse(is.na(gini), ginifill, gini)) %>%
  filter(year >= 2005) %>%
  select(country:gini, gini_latest, ginifill, gini_avg, everything()) %>%
  rename(country_name = country) %>%
  mutate(country_name =
           case_when(
             #country_name == "Czechia" ~ "Czech Republic",
                     country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                     country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                     country_name == "Cote d'Ivoire" ~ "Ivory Coast",
                     country_name == "Egypt, Arab Rep." ~ "Egypt",
#                     country_name == "Eswatini" ~ "Swaziland",
                     country_name == "Gambia, The" ~ "Gambia",
                     country_name == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
                     country_name == "Iran, Islamic Rep." ~ "Iran",
                     country_name == "Korea, Rep." ~ "South Korea",
                     country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                     country_name == "Lao PDR" ~ "Laos",
                     country_name == "Russian Federation" ~ "Russia",
                     country_name == "Slovak Republic" ~ "Slovakia",
                     country_name == "Syrian Arab Republic" ~ "Syria",
 #                    country_name == "Turkiye" ~ "Turkey",
                     country_name == "Venezuela, RB" ~ "Venezuela",
                     country_name == "Viet Nam" ~ "Vietnam",
                     country_name == "West Bank and Gaza" ~ "Palestinian Territories",
                     country_name == "Yemen, Rep." ~ "Yemen",
                     TRUE ~ country_name)) 

glimpse(ginis)

# merge gini to whr
whr_all <- whrall_2 %>%
  merge(ginis, by = c("country_name", "year"), all = TRUE) %>%
  as_tibble() %>%
  select(country_name, year, iso3c, region, region_whr, region_gini = region,
         ladder_score:negative_affect, gini:gini_avg, capital:lending) %>%
  arrange(country_name, year) %>%
  filter(!is.na(ladder_score)) %>%
  mutate(region_gini = case_when(
    is.na(region_gini) & region_whr == "East Asia" ~ "East Asia & Pacific",
    is.na(region_gini) & region_whr == "Middle East and North Africa" 
    ~ "Middle East & North Africa",
    is.na(region_gini) & region_whr == "Sub-Saharan Africa" 
    ~ "Middle East & North Africa",
    TRUE ~ region_gini))

glimpse(whr_all)

whr_all %>%
  count(region_gini, region_whr)

saveRDS(whr_all, file = "data/whr_all.rds")
