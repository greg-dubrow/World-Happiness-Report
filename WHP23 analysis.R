## analysis on the WHR data using DataExplorer, explorer, correlationfunnel, collapse explorer? gtextras?
## hierarchical clustering and dendogram? https://uc-r.github.io/hc_clustering


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


View(DataExplorer::plot_scatterplot)

# load data created in WHP23 EDA.R -------------------------------------------------------------------

### load data created in WHP23 EDA.R
whr23_fig2_1 <- readRDS(file = "~/Data/r/World Happiness Report/data/whr23_fig2_1.rds") %>%
	mutate(income = ifelse(is.na(income), "Not classified", income)) %>%
	mutate(lending = ifelse(is.na(lending), "Not classified", lending)) %>%
	mutate(region = ifelse(is.na(region), "East Asia & Pacific", region))
glimpse(whr23_fig2_1)

whr23_fig2_1 %>%
	count(region, region_whr)

whr23_fig2_1 %>%
	count(country_name)

whr23_fig2_1 %>%
	filter(region_whr %in% c("Central and Eastern Europe", "Western Europe")) %>%
	ggplot(aes(x = reorder(country_name, +perceptions_of_corruption), y = perceptions_of_corruption,
				 fill = region_whr)) +
	geom_col() 	+
	geom_text(aes(label= round(perceptions_of_corruption, 2)),
						color = "black", size = 5, vjust = 1.5) +
	coord_flip() +
	theme_minimal() +
	theme(legend.position = "none")

whr23_fig2_1 %>%
	group_by(region_whr) %>%
	summarise(mean_corr = mean(perceptions_of_corruption))


# Region EDA -------------------------------------------------------------------
## region EDA
# means, med, min max

whr23_fig2_1 %>%
	group_by(region) %>%
	summarise(mean_ladder = mean(ladder_score))


# Scatterplots:
# 	Each variable against ladder score. Color code by region. Figure out way to do interactive with rollover for country name?
# 	Are the most generous countries the happiest?
# 	Relationship between GDP & happiness

whr23_fig2_1 %>%
	ggplot(aes(x = ladder_score, y = gini_avg)) +
	geom_point() +
	geom_smooth() +
	facet_wrap( ~ region)


# Regression to either replicate report or do one predicting a single year? Or year by year to see which
# gganimate go year by year on scatterplots of variables against happiness score...or do interactive, select variable, country/region


### regression
glimpse(whr23_fig2_1)
model1a <- whr23_fig2_1 %>%
	select(country_name, ladder_score, gini_avg, logged_gdp_per_capita, social_support, healthy_life_expectancy,
				 	freedom_to_make_life_choices, generosity, perceptions_of_corruption) %>%
	filter(!is.na(gini_avg)) %>%
	filter(!is.na(healthy_life_expectancy))

skimr::skim(model1a)

model1 <- lm(ladder_score ~ gini_avg+logged_gdp_per_capita+social_support+healthy_life_expectancy+
						 	freedom_to_make_life_choices+generosity+perceptions_of_corruption, data = model1a)
summary(model1)
names(model1)
model1$coefficients
model1$fitted.values

model1a$ladder_pred <- model1$fitted.values

## using this example, plot actual/predicted on vertical lines with fitted abline
set.seed(123)
iris2 <- iris[sample(1:nrow(iris), 30),]
model <- lm(Petal.Length ~ Sepal.Length, data=iris2)
iris2$fitted <- predict(model)

model$coefficients[1]

ggplot(iris2, aes(x = Sepal.Length, y = Petal.Length)) +
	geom_linerange(aes(ymin = fitted, ymax = Petal.Length),
								 colour = "purple") +
	geom_abline(intercept = model$coefficients[1],
							slope = model$coefficients[2]) +
	geom_emoji(aes(image = ifelse(abs(Petal.Length-fitted) > 0.5, '1f622', '1f600')))


ggplot(model1, aes(x=predict(model1), y= model1a$ladder_score)) +
	geom_point() +
	geom_abline(intercept=0, slope=1) +
	labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

## for just europe, add educational attainment or literacy to model



## PCA
whr23_pca1 <- as.data.frame(whr23_fig2_1 %>%
	select(country_name, ladder_score, social_support:perceptions_of_corruption, gini_latest)) %>%
	column_to_rownames(var = "country_name") %>%
	filter(!is.na(gini_latest)) %>%
	filter(!is.na(healthy_life_expectancy))

glimpse(whr23_pca1)
skimr::skim(whr23_pca1)

whr23_pca <- prcomp(whr23_pca1, scale = TRUE)
whr23_pca$sdev

whr23_pca$rotation
whr23_pca$rotation <- -whr23_pca$rotation

whr23_pca$center
whr23_pca$scale
whr23_pca$x
whr23_pca$x <- -whr23_pca$x

biplot(p,
			 lab = NULL,
			 legendPosition = 'right',
			 colby = 'genotype',
			 colLegendTitle = 'Genotype',
			 # ellipse config
			 ellipse = TRUE,
			 ellipseLevel = 0.95,
			 ellipseFill = TRUE,
			 ellipseAlpha = 1/4,
			 ellipseLineSize = 0)

biplot(whr23_pca, scale = 0, choices = 1:2,
			 ellipse = TRUE, ellipseLevel = 0.95,
			 ellipseFill = TRUE,)
biplot(whr23_pca, scale = 0, choices = 3:4)

whr23_pca_states1 <- as_tibble(whr23_pca$x)
whr23_pca_states <- data.frame(country = row.names(whr23_pca_states), whr23_pca_states1)

ggplot(whr23_pca_states, aes(PC1, PC2)) +
	modelr::geom_ref_line(h = 0) +
	modelr::geom_ref_line(v = 0) +
	geom_text(aes(label = country), size = 3) +
	xlab("First Principal Component") +
	ylab("Second Principal Component") +
	ggtitle("First Two Principal Components of WHR Data")

