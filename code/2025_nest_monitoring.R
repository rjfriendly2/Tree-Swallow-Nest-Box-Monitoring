### Tree Swallow Nest Box Monitoring
### Date: 5/28/2025
### By: Randall J. Friendly
###
### This will be a working r script to estimate hatch dates for banding, visuals, and summary statistics

### Activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)

# Read in data
visit_data25 <- read_excel("data/2025_BET_nest_record.xlsx")

# view the data
head(visit_data25)

# make variables factor variables
visit_data25$species <- as.factor(visit_data25$species)
visit_data25$observer <- as.factor(visit_data25$observer)
visit_data25$year <- as.factor(visit_data25$year)
visit_data25$site <- as.factor(visit_data25$site)
visit_data25$box_number <- as.factor(visit_data25$box_number)
head(visit_data25)

# view data
summary(visit_data25)

