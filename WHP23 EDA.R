## doing EDA on the WHR data using DataExplorer, explorer, correlationfunnel, collapse explorer? gtextras?

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(skimr) # EDA tools

## load data for figure 2.1, horizontal bar of ladder score and factors

whr23_fig2_1a <- readxl::read_excel("data/DataForFigure2.1WHR2023.xls") %>%
	clean_names() %>%
	as_tibble() %>%
	mutate(residual = dystopia_residual - ladder_score_in_dystopia) %>%
	select(-residual_is_dystopia_minus_dystopia_plus_residual) %>%
	mutate(whr_year = 2023) %>%
	mutate(country_name = case_when(country_name == "Czechia" ~ "Czech Republic",
																	country_name == "State of Palestine" ~ "Palestinian Territories",
																	country_name ==  "Turkiye" ~ "Turkey",
																	TRUE ~ country_name))

glimpse(whr23_fig2_1a)

## or straight from web
library(readxl)
url1 <- "https://happiness-report.s3.amazonaws.com/2023/DataForFigure2.1WHR2023.xls"
destfile1 <- "DataForFigure2_1WHR2023.xls"
curl::curl_download(url1, destfile1)
whr23_fig2_1a <- readxl::read_excel(destfile1) %>%
	clean_names() %>%
	as_tibble() %>%
	mutate(residual = dystopia_residual - ladder_score_in_dystopia) %>%
	select(-residual_is_dystopia_minus_dystopia_plus_residual) %>%
	mutate(whr_year = 2023) %>%
	mutate(country_name = case_when(country_name == "Czechia" ~ "Czech Republic",
																	country_name == "State of Palestine" ~ "Palestinian Territories",
																	country_name ==  "Turkiye" ~ "Turkey",
																	TRUE ~ country_name))

# load kaggle 2021 set to get region names
ctreg <- readr::read_csv("data/world-happiness-report-2021.csv") %>%
	as_tibble() %>%
	clean_names() %>%
	select (country_name, region = regional_indicator)

glimpse(ctreg)


## country & region only

# join to whr23 on country
whr23_fig2_1 <- whr23_fig2_1a %>%
	merge(ctreg, all = TRUE) %>%
	as_tibble() %>%
	select(country_name, region, whr_year, everything()) %>%
	mutate(region = ifelse(country_name == "Congo (Kinshasa)", "Sub-Saharan Africa", region))

glimpse(whr23_fig2_1)
head(whr23_fig2_1)

saveRDS(whr23_fig2_1, file = "data/whr23_fig2_1.rds")


ginis <- readxl::read_excel("data/API_SI.POV.GINI_DS2_en_excel_v2_5994963.xlsx",
														sheet = "Data") %>%
	rename(country_name = `Country Name`, country_code = `Country Code`) %>%
	select(country_name, country_code, `2015`:`2022`) %>%
	filter(!grepl('dividend', country_name)) %>%
	filter(!grepl('income', country_name)) %>%
	## does a coalesce to get last non-NA but reverse for all cols but name and code
	mutate(gini_latest = do.call(coalesce, rev(across(-country_name:-country_code)))) %>%
	mutate(country_name =
				 	case_when(country_name == "Czechia" ~ "Czech Republic",
				 						country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
				 						country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
				 						country_name == "Cote d'Ivoire" ~ "Ivory Coast",
				 						country_name == "Eswatini" ~ "Swaziland",
				 						country_name == "Gambia, The" ~ "Gambia",
				 						country_name == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
				 						country_name == "Iran, Islamic Rep." ~ "Iran",
				 						country_name == "Korea, Rep." ~ "South Korea",
				 						country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
				 						country_name == "Lao PDR" ~ "Laos",
				 						country_name == "Russian Federation" ~ "Russia",
				 						country_name == "Turkiye" ~ "Turkey",
				 						country_name == "Venezuela, RB" ~ "Venezuela",
				 						country_name == "Viet Nam" ~ "Vietnam",
				 						country_name == "West Bank and Gaza" ~ "Palestinian Territories",
				 						country_name == "Yemen, Rep." ~ "Yemen",
				 						TRUE ~ country_name)) %>%
	select(country_name, country_code, gini_latest)

whr23_fig2_1b <- whr23_fig2_1 %>%
	merge(ginis, all = TRUE) %>%
	as_tibble() %>%
	select(country_name, country_code, region, whr_year:lowerwhisker, logged_gdp_per_capita,
				 gini_latest, everything()) %>%
	mutate(region = ifelse(country_name == "Congo (Kinshasa)", "Sub-Saharan Africa", region)) %>%
	filter(!is.na(ladder_score))
glimpse(whr23_fig2_1b)


## DataExplorer for EDA
library(DataExplorer) # EDA tools

# summary of completes, missings
de1 <- introduce(whr23_fig2_1b)
view(de1)
plot_intro(whr23_fig2_1b)
plot_missing(whr23_fig2_1b)

## frequency distributions for discrete vars...only one if region...not needed for this set.
## but I cleaned before I ran this...run this before cleaning to find issues quickly
plot_bar(whr23_fig2_1b)

## histograms of continuous vars...need to set # of rows higher than 4 depending on # of vars. or cut into 2 sets
plot_histogram(whr23_fig2_1b, nrow = 5L)

## correlation...but remove columns and clean NA in pipe
whr23_fig2_1 %>%
	select(-whr_year, -ladder_score_in_dystopia, -upperwhisker, -lowerwhisker) %>%
	filter(!is.na(residual)) %>%
	plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 4))

plot_correlation()

## principal component
whr23_pca <- whr23_fig2_1 %>%
	filter(!is.na(ladder_score)) %>%
 	select(ladder_score, logged_gdp_per_capita:perceptions_of_corruption)

plot_prcomp(na.omit(whr23_pca))

## quantil-quantile to visualize deviation from probability dist
## not really helpful?
whr23_qq <- whr23_fig2_1 %>% select(region, ladder_score)

plot_qq(whr23_qq)
plot_qq(whr23_qq, by = "region")

## scatterplots!
whr23_scatter <- whr23_fig2_1 %>%
	select(ladder_score, social_support:perceptions_of_corruption,
				 explained_by_log_gdp_per_capita:residual)

plot_scatterplot(whr23_scatter, by = "ladder_score", nrow = 6L,
								 geom_point_args = list("fill" = ))

plot_scatterplot(
	whr23_fig2_1 %>% select(ladder_score, social_support:perceptions_of_corruption,
													dystopia_residual, residual), by = "ladder_score", nrow = 3L)

whr23_fig2_1 %>%
	ggplot(aes(x = perceptions_of_corruption, y = ladder_score)) +
	geom_point(aes(color = region)) +
	theme(legend.position = "none")

 ## explore geom_point_args...how to pass fill for dots

### summary for DataExplorer = lots of helpful features to quickly scan data. might be nice to have facet features,
## and better explain a use-case for coloring points in scatter


##### explore
### works within tidyverse pipes, so a bit more easy that data explorer
library(explore)

whr23_fig2_1 %>%
	count(region)

 # basic table characteristics
whr23_fig2_1 %>%
	describe_tbl()

 ## na, unique, mean, min, max
whr23_fig2_1 %>%
	select(ladder_score, social_support:perceptions_of_corruption,
				 explained_by_log_gdp_per_capita:residual) %>%
	describe_all()


# describes var with measures of variance for continuous, or counts of discrete
whr23_fig2_1 %>%
	describe(ladder_score)

whr23_fig2_1 %>%
	describe(region)

# launches shiny app to see individual variables, or use a target.
# categoricals plotted with bar and counts, continuous with area plots
explore(whr23_fig2_1 %>% select(-country_name, -whr_year, -region))

whr23_fig2_1 %>%
	select(-whr_year, -ladder_score_in_dystopia, -upperwhisker, -lowerwhisker) %>%
	explore()

## in a pipe, explore specific vars. does area plots for continuous, bar plots for discrete
whr23_fig2_1 %>%
	explore(ladder_score)

whr23_fig2_1 %>%
	explore(region)

## creates html report of all individual reports
whr23_fig2_1 %>%
	report(output_dir = "~/data/r/World-Happiness-Report")

### skimr
whr23_fig2_1 %>%
	select(ladder_score, social_support:perceptions_of_corruption, residual) %>%
	skim()

