## looks at relatonship between corruption and happiness and other facets of the happiness score

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gregeRs) # some custom functions


# source("~/Data/r/basic functions.R")

# sets theme as default for all plots
theme_set(theme_light)

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

## load all year longitudinal set
 ## need to ungroup it, or go back and redo file and ungroup before saving
whr_all <- readRDS("data/whr_all.rds")

glimpse(whr_all)

whr_all %>%
  ungroup() %>%
  count(year)

glimpse(whr23_fig2_1)

## principal component?
library(gridExtra)

