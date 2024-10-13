## analysis on the WHR data using DataExplorer, explorer, correlationfunnel, collapse explorer? gtextras?

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning

# some custom functions
source("~/Data/r/basic functions.R")

# sets theme as default for all plots
theme_set(theme_light)

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text


### load data created in WHP23 EDA.R
whr23_fig2_1a <- readRDS(file = "~/Data/r/World Happiness Report/data/whr23_fig2_1.rds") %>%
	filter(!is.na(ladder_score)) %>%
	rename(region_whr = region)
glimpse(whr23_fig2_1a)


### add GINI indices from world bank - what are most common latest years for each country? Use most recent 5 years for each country?

## using WDI package
  # WB category definitions
  # https://datahelpdesk.worldbank.org/knowledgebase/articles/378834-how-does-the-world-bank-classify-countries
ginisa = WDI::WDI(indicator='SI.POV.GINI', start=2000, end=2023, extra = TRUE)

ginisa %>%
	count(country, region) %>%
	filter(is.na(region)) %>%
	view()

glimpse(ginisa)

ginis <- ginisa %>%
	as_tibble() %>%
	select(-status, -lastupdated) %>%
	## fix regions
	mutate(region =
				 	case_when(country == "Czechia" ~ "Europe & Central Asia",
				 						country == "Viet Nam" ~ "East Asia & Pacific",
				 						TRUE ~ region)) %>%
	filter(region != "Aggregates")%>%
	arrange(country, year) %>%
	rename(gini = SI.POV.GINI) %>%
	mutate(ginifill = gini) %>%
	group_by(country) %>%
	mutate(gini_avg = mean(gini, na.rm = TRUE)) %>%
	fill(ginifill, .direction = "downup") %>%
	ungroup() %>%
	filter(year == 2022) %>%
	mutate(gini_latest = ifelse(is.na(gini), ginifill, gini)) %>%
	select(country:gini, gini_latest, ginifill, gini_avg, everything()) %>%
	rename(country_name = country) %>%
	mutate(country_name =
				 	case_when(country_name == "Czechia" ~ "Czech Republic",
				 						country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
				 						country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
				 						country_name == "Cote d'Ivoire" ~ "Ivory Coast",
				 						country_name == "Egypt, Arab Rep." ~ "Egypt",
				 						country_name == "Eswatini" ~ "Swaziland",
				 						country_name == "Gambia, The" ~ "Gambia",
				 						country_name == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
				 						country_name == "Iran, Islamic Rep." ~ "Iran",
				 						country_name == "Korea, Rep." ~ "South Korea",
				 						country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
				 						country_name == "Lao PDR" ~ "Laos",
				 						country_name == "Russian Federation" ~ "Russia",
				 						country_name == "Slovak Republic" ~ "Slovakia",
				 						country_name == "Turkiye" ~ "Turkey",
				 						country_name == "Venezuela, RB" ~ "Venezuela",
				 						country_name == "Viet Nam" ~ "Vietnam",
				 						country_name == "West Bank and Gaza" ~ "Palestinian Territories",
				 						country_name == "Yemen, Rep." ~ "Yemen",
				 						TRUE ~ country_name))

ginis %>%
	select(gini_latest, gini_avg) %>%
	skimr::skim()

whr23_fig2_1 <- whr23_fig2_1a %>%
	merge(ginis, all = TRUE) %>%
	as_tibble() %>%
	select(country_name, iso3c, region, region_whr, whr_year:lowerwhisker, logged_gdp_per_capita,
				 gini_avg, gini_latest, everything()) %>%
	## fill in Taiwan, no longer in this set but still available at https://pip.worldbank.org/country-profiles/TWN
	mutate(gini_avg = ifelse(country_name == "Taiwan Province of China", 32.09833333, gini_avg)) %>%
	mutate(gini_latest = ifelse(country_name == "Taiwan Province of China", 31.48, gini_latest)) %>%
	filter(!is.na(ladder_score))
glimpse(whr23_fig2_1)

saveRDS(whr23_fig2_1, file = "~/Data/r/World Happiness Report/data/whr23_fig2_1.rds")


whr23_fig2_1 %>%
	count(region, region_whr) %>%
	view()

whr23_fig2_1 %>%
	filter(region == "Middle East & North Africa" & region_whr == "Western Europe") %>%
	view()



## EDA with gini

# difference between average and latest
whr23_fig2_1 %>%
	mutate(gini_diff = gini_latest - gini_avg) %>%
	skimr::skim(gini_diff)

whr23_fig2_1 %>%
	filter(!is.na(gini_avg)) %>%
	mutate(gini_diff = gini_latest - gini_avg) %>%
	select(country_name, gini_latest, gini_avg, gini_diff, region_whr) %>%
	arrange(gini_diff) %>%
	ggplot(aes(gini_diff)) +
				 	geom_density(fill = "blue") +
	xlim(-9, 6)

whr23_fig2_1 %>%
	filter(!is.na(gini_avg)) %>%
	mutate(gini_diff = gini_latest - gini_avg) %>%
	select(country_name, gini_latest, gini_avg, gini_diff, region_whr) %>%
	arrange(gini_diff) %>%
	ggplot() +
	geom_segment( aes(x=reorder(country_name, -gini_diff), xend=country_name, y=gini_latest, yend=gini_avg), color="grey") +
	geom_point( aes(x=country_name, y=gini_latest), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
	geom_point( aes(x=country_name, y=gini_avg), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
	coord_flip()+
#	theme_ipsum() +
	theme(
		legend.position = "none",
	) +
	xlab("") +
	ylab("Value of Y") +
	facet_wrap(~ region_whr, scales = "free_y")

library(DataExplorer)

plot_missing(whr23_fig2_1)

whr23_fig2_1 %>%
	filter(is.na(gini_avg)) %>%
	count(country_name)

whr23_fig2_1 %>%
	select(gini_avg, gini_latest) %>%
	skimr::skim()

whr23_fig2_1 %>%
	select(ladder_score, standard_error_of_ladder_score, gini_avg, logged_gdp_per_capita,
				 social_support:perceptions_of_corruption,
				 explained_by_log_gdp_per_capita:residual) %>%
	filter(!is.na(residual)) %>%
	filter(!is.na(gini_avg)) %>%
	DataExplorer::plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 3))

plot_scatterplot(
	whr23_fig2_1 %>% select(gini_avg, ladder_score, logged_gdp_per_capita,
													social_support:perceptions_of_corruption),
	by = "gini_avg", nrow = 3L)

## negative relationship, expected since higher GINI means more inequality

library(explore)

# basic table characteristics
whr23_fig2_1 %>%
	describe_tbl()

whr23_fig2_1 %>%
	# select(ladder_score, social_support:perceptions_of_corruption,
	# 			 explained_by_log_gdp_per_capita:residual) %>%
	describe_all() %>%
	view()


### merge gini to WHR old way with excel sheet
# https://data.worldbank.org/indicator/SI.POV.GINI
ginis2 <- readxl::read_excel("~/Data/r/World Happiness Report/data/API_SI.POV.GINI_DS2_en_excel_v2_5994963.xlsx",
														sheet = "Data") %>%
	rename(country_name = `Country Name`, country_code = `Country Code`) %>%
	select(country_name, country_code, `2000`:`2022`) %>%
	filter(!grepl('dividend', country_name)) %>%
	filter(!grepl('income', country_name)) %>%
	# work rowwise to get avg and latest from 2000 to 2022
	rowwise() %>%
	mutate(gini_avg = mean(c_across(c(-country_name:-country_code)), na.rm = TRUE)) %>%
	# move average to front to get latest
	select(country_name, country_code, gini_avg, everything()) %>%
	## does a coalesce to get last non-NA but reverse for all cols but name and code
	mutate(gini_latest = do.call(coalesce, rev(across(-country_name:-gini_avg)))) %>%
	ungroup() %>%
	mutate(country_name =
				 	case_when(country_name == "Czechia" ~ "Czech Republic",
				 						country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
				 						country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
				 						country_name == "Cote d'Ivoire" ~ "Ivory Coast",
				 						country_name == "Egypt, Arab Rep." ~ "Egypt",
				 						country_name == "Eswatini" ~ "Swaziland",
				 						country_name == "Gambia, The" ~ "Gambia",
				 						country_name == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
				 						country_name == "Iran, Islamic Rep." ~ "Iran",
				 						country_name == "Korea, Rep." ~ "South Korea",
				 						country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
				 						country_name == "Lao PDR" ~ "Laos",
				 						country_name == "Russian Federation" ~ "Russia",
				 						country_name == "Slovak Republic" ~ "Slovakia",
				 						country_name == "Turkiye" ~ "Turkey",
				 						country_name == "Venezuela, RB" ~ "Venezuela",
				 						country_name == "Viet Nam" ~ "Vietnam",
				 						country_name == "West Bank and Gaza" ~ "Palestinian Territories",
				 						country_name == "Yemen, Rep." ~ "Yemen",
				 						TRUE ~ country_name)) %>%
	select(country_name, country_code, gini_avg, gini_latest)
glimpse(ginis)

