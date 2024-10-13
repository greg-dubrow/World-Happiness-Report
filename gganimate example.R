library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gganimate)

# some custom functions
source("~/Data/r/basic functions.R")

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

# read in whr dataset crated in data/corruption and happiness.R
whr_all <- readRDS("data/whr_all.rds")

glimpse(whr_all)

whr_all %>%
ggplot(aes(year, ladder_score, color = country_name) +
	geom_point() +
	#here comes the gganimate code
@	labs(title = 'Days: {frame_time}', x = 'Chick weight (grams)', y = 'Chick number') +
	transition_time(Time) +
	ease_aes('linear')
