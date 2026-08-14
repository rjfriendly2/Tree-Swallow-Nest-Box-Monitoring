### Tree Swallow Nest Box Monitoring
### Date: 12/4/2025
### By: Randall J. Friendly
###
### This is the post season analysis

### Activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(writexl)

# Read in data
visit_data26 <- read_excel("data/2026_BET_nest_record.xlsx")
visit_sum_data26 <- read_excel("data/2026_BET_nest_record_summary.xlsx")

# make variables factor variables
# 2026 BET nest record
visit_data26$species <- as.factor(visit_data26$species)
visit_data26$observer <- as.factor(visit_data26$observer)
visit_data26$year <- as.factor(visit_data26$year)
visit_data26$site <- as.factor(visit_data26$site)
visit_data26$box_number <- as.factor(visit_data26$box_number)
visit_data26$date <- as.Date(visit_data26$date)
head(visit_data26)

# 2026 BET nest record summary
head(visit_sum_data26)
visit_sum_data26$box_number <- as.factor(visit_sum_data26$box_number)
visit_sum_data26$species <- as.factor(visit_sum_data26$species)
visit_sum_data26$site <- as.factor(visit_sum_data26$site)
visit_sum_data26$year <- as.factor(visit_sum_data26$year)
visit_sum_data26$clutch_initiation <- as.Date(visit_sum_data26$clutch_initiation)
visit_sum_data26$clutch_completion <- as.Date(visit_sum_data26$clutch_completion)
visit_sum_data26$predicted_hatch <- as.Date(visit_sum_data26$predicted_hatch)
visit_sum_data26$incubation_start <- as.Date(visit_sum_data26$incubation_start)
visit_sum_data26$actual_hatch <- as.Date(visit_sum_data26$actual_hatch)
visit_sum_data26$predicted_fledge <- as.Date(visit_sum_data26$predicted_fledge)
visit_sum_data26$actual_fledge <- as.Date(visit_sum_data26$actual_fledge)
visit_sum_data26$successful <- as.factor(visit_sum_data26$successful)
head(visit_sum_data26)

### Get the earliest clutch intitiation
summary(visit_sum_data26) # look at min and max
summary(visit_data26)

### Get incubation length statistics
visit_sum_data26$incub_length <- as.numeric(visit_sum_data26$actual_hatch - visit_sum_data26$incubation_start)
summary(visit_sum_data26) # look at mean min and max
se_incubation <- sd(visit_sum_data26$incub_length, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data26$incub_length)))
# Incubation mean 13.5 +- 0.26, range (10 - 16)

### Get Success rate
visit_sum_data26 %>%
  summarise(
    num_success = sum(successful == "Y", na.rm = TRUE),
    success_rate = mean(successful == "Y", na.rm = TRUE)
  )


### Get Nest Box Occupancy rate
visit_sum_data26 %>%
  summarise(
    total_boxes = n_distinct(box_number),
    occupied_boxes = sum(!is.na(clutch_initiation)),
    occupancy_rate = occupied_boxes / total_boxes
  )

### Get the nestling age for each nest on fledge date
visit_sum_data26$nestling_age <- as.numeric(visit_sum_data26$actual_fledge - visit_sum_data26$actual_hatch)
summary(visit_sum_data26)
se_nestling_age <- sd(visit_sum_data26$nestling_age, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data26$nestling_age)))

### Get the total of eggs laid accross all nests
sum(visit_sum_data26$number_eggs)
sum(visit_sum_data26$number_hatched)

### Get standard error
se_clutch_size <- sd(visit_sum_data26$number_eggs, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data26$number_eggs)))
se_hatch <- sd(visit_sum_data26$number_hatched, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data26$number_hatched)))
se_fledged <- sd(visit_sum_data26$number_fledged, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data26$number_fledged)))

### Get the total number of fledglings.
sum(visit_sum_data26$number_fledged, na.rm = TRUE)

### Get proportion of successful fledged nest boxes
visit_sum_data26 %>%
  summarise(
    successful_boxes = sum(successful == "Y", na.rm = TRUE),
    used_boxes = sum(!is.na(successful)),
    success_rate = successful_boxes / used_boxes
  )

### Get proportion of successfull fledged nests from ACTIVE boxes
visit_sum_data26 %>%
  summarise(
    active_boxes = sum(!is.na(clutch_initiation)),
    successful_active_boxes = sum(!is.na(clutch_initiation) & successful == "Y", na.rm = TRUE),
    success_rate = successful_active_boxes / active_boxes
  )

### Get a proportioon of hatched eggs from ACTIVE boxes
visit_sum_data26 %>%
  summarise(
    active_nests = sum(!is.na(clutch_initiation)),
    hatched_nests = sum(!is.na(clutch_initiation) & !is.na(actual_hatch)),
    hatch_rate = hatched_nests / active_nests
  )

### Get a proportion of fledged birds from hatched nests
visit_sum_data26 %>%
  summarise(
    hatched_nests = sum(!is.na(actual_hatch)),
    fledged_nests = sum(!is.na(actual_hatch) & !is.na(actual_fledge)),
    fledge_rate = fledged_nests / hatched_nests
  )


### create a summary table for tree swallow nest boxes
#subset to new dataframe
summary_2026 <- visit_sum_data26 %>%
  select(box_number,
         clutch_initiation,
         actual_hatch,
         actual_fledge,
         number_eggs,
         number_hatched,
         number_fledged,
         successful)

summary_2026_text <- visit_sum_data26 %>%
  select(box_number,
         clutch_initiation,
         actual_hatch,
         actual_fledge,
         number_eggs,
         number_hatched,
         number_fledged,
         successful)

#convert dates to abbreviated month and day
summary_2026_text$clutch_initiation <- format(summary_2026_text$clutch_initiation, "%b %d")
summary_2026_text$actual_hatch <- format(summary_2026_text$actual_hatch, "%b %d")
summary_2026_text$actual_fledge <- format(summary_2026_text$actual_fledge, "%b %d")
summary_2026_text


# create a row for numeric totals
egg_totals <- visit_sum_data26 %>%
  summarise(box_number = "Total",
            clutch_initiation = as.Date(NA),
            actual_hatch = as.Date(NA),
            actual_fledge = as.Date(NA),
            number_eggs = sum(number_eggs, na.rm = TRUE),
            number_hatched = sum(number_hatched, na.rm = TRUE),
            number_fledged = sum(number_fledged, na.rm = TRUE),
            successful = NA)


# create a row for averages
mean_row <- visit_sum_data26 %>%
  summarise(
    box_number = "Mean",
    clutch_initiation = as.Date(
      mean(as.numeric(clutch_initiation), na.rm = TRUE),
      origin = "1970-01-01"
    ),
    actual_hatch = as.Date(
      mean(as.numeric(actual_hatch), na.rm = TRUE),
      origin = "1970-01-01"
    ),
    actual_fledge = as.Date(
      mean(as.numeric(actual_fledge), na.rm = TRUE),
      origin = "1970-01-01"
    ),
    number_eggs = mean(number_eggs, na.rm = TRUE),
    number_hatched = mean(number_hatched, na.rm = TRUE),
    number_fledged = mean(number_fledged, na.rm = TRUE),
    successful = NA
  )

# calculate SEs
date_se <- visit_sum_data26 %>%
  summarise(
    clutch_initiation_se =
      sd(as.numeric(clutch_initiation), na.rm = TRUE) /
      sqrt(sum(!is.na(clutch_initiation))),
    
    actual_hatch_se =
      sd(as.numeric(actual_hatch), na.rm = TRUE) /
      sqrt(sum(!is.na(actual_hatch))),
    
    actual_fledge_se =
      sd(as.numeric(actual_fledge), na.rm = TRUE) /
      sqrt(sum(!is.na(actual_fledge)))
  )

# calculate Ses for egg, hatch, fledge counts
count_se <- visit_sum_data26 %>%
  summarise(
    number_eggs_se =
      sd(number_eggs, na.rm = TRUE) /
      sqrt(sum(!is.na(number_eggs))),
    
    number_hatched_se =
      sd(number_hatched, na.rm = TRUE) /
      sqrt(sum(!is.na(number_hatched))),
    
    number_fledged_se =
      sd(number_fledged, na.rm = TRUE) /
      sqrt(sum(!is.na(number_fledged)))
  )


mean_row_text <- summary_2026 %>%
  summarise(
    box_number = "Mean ± SE",
    
    clutch_initiation = paste0(
      as.Date(mean(as.numeric(clutch_initiation), na.rm = TRUE),
              origin = "1970-01-01"),
      " ± ",
      round(sd(as.numeric(clutch_initiation), na.rm = TRUE) /
              sqrt(sum(!is.na(clutch_initiation))), 1),
      " d"
    ),
    
    actual_hatch = paste0(
      as.Date(mean(as.numeric(actual_hatch), na.rm = TRUE),
              origin = "1970-01-01"),
      " ± ",
      round(sd(as.numeric(actual_hatch), na.rm = TRUE) /
              sqrt(sum(!is.na(actual_hatch))), 1),
      " d"
    ),
    
    actual_fledge = paste0(
      as.Date(mean(as.numeric(actual_fledge), na.rm = TRUE),
              origin = "1970-01-01"),
      " ± ",
      round(sd(as.numeric(actual_fledge), na.rm = TRUE) /
              sqrt(sum(!is.na(actual_fledge))), 1),
      " d"
    ),
    
    number_eggs = paste0(
      round(mean(number_eggs, na.rm = TRUE), 2), " ± ",
      round(sd(number_eggs, na.rm = TRUE) /
              sqrt(sum(!is.na(number_eggs))), 2)
    ),
    
    number_hatched = paste0(
      round(mean(number_hatched, na.rm = TRUE), 2), " ± ",
      round(sd(number_hatched, na.rm = TRUE) /
              sqrt(sum(!is.na(number_hatched))), 2)
    ),
    
    number_fledged = paste0(
      round(mean(number_fledged, na.rm = TRUE), 2), " ± ",
      round(sd(number_fledged, na.rm = TRUE) /
              sqrt(sum(!is.na(number_fledged))), 2)
    ),
    
    successful = NA
  )

#create a dataframe to bind together
df_text <- summary_2026_text
df_text$clutch_initiation <- as.character(df_text$clutch_initiation)
df_text$actual_hatch <- as.character(df_text$actual_hatch)
df_text$actual_fledge <- as.character(df_text$actual_fledge)
df_text$actual_hatch <- as.character(df_text$actual_hatch)
df_text$number_eggs <- as.character(df_text$number_eggs)
df_text$number_hatched <- as.character(df_text$number_hatched)
df_text$number_fledged <- as.character(df_text$number_fledged)
head(df_text)

egg_totals_text <- egg_totals
egg_totals_text$clutch_initiation <- as.character(egg_totals_text$clutch_initiation)
egg_totals_text$actual_hatch <- as.character(egg_totals_text$actual_hatch)
egg_totals_text$actual_fledge <- as.character(egg_totals_text$actual_fledge)
egg_totals_text$actual_hatch <- as.character(egg_totals_text$actual_hatch)
egg_totals_text$number_eggs <- as.character(egg_totals_text$number_eggs)
egg_totals_text$number_hatched <- as.character(egg_totals_text$number_hatched)
egg_totals_text$number_fledged <- as.character(egg_totals_text$number_fledged)
head(egg_totals_text)

df_final <- bind_rows(
  df_text,
  egg_totals_text,
  mean_row_text
)

##### export dataframe
write_xlsx(df_final, "output/TreeSwallow_breeding_summary.xlsx")






############ Summmarize Banding Data #############

# Read in data
adult_band26 <- read_excel("data/2026_BET_adult_banding_data.xlsx")
chick_band26 <- read_excel("data/2026_BET_chick_banding_data.xlsx")

# prepare adult banding dataframe
adult_band26$box_number <- as.factor(adult_band26$box_number)
adult_band26$species <- as.factor(adult_band26$species)
adult_band26$site <- as.factor(adult_band26$site)
adult_band26$year <- as.factor(adult_band26$year)
adult_band26$bander <- as.factor(adult_band26$bander)
adult_band26$date <- as.Date(adult_band26$date)
adult_band26$disposition <- as.factor(adult_band26$disposition)
adult_band26$age <- as.factor(adult_band26$age)
adult_band26$how_aged <- as.factor(adult_band26$how_aged)
adult_band26$sex <- as.factor(adult_band26$sex)
adult_band26$fat <- as.factor(adult_band26$fat)
adult_band26$cp <- as.factor(adult_band26$cp)
adult_band26$bp <- as.factor(adult_band26$bp)
adult_band26$pp_molt <- as.factor(adult_band26$pp_molt)
adult_band26$pp_wear <- as.factor(adult_band26$pp_wear)

# prepare chick banding dataframe
chick_band26$box_number <- as.factor(chick_band26$box_number)
chick_band26$species <- as.factor(chick_band26$species)
chick_band26$site <- as.factor(chick_band26$site)
chick_band26$year <- as.factor(chick_band26$year)
chick_band26$bander <- as.factor(chick_band26$bander)
chick_band26$date <- as.Date(chick_band26$date)
chick_band26$disposition <- as.factor(chick_band26$disposition)
chick_band26$age <- as.factor(chick_band26$age)
chick_band26$sex <- as.factor(chick_band26$sex)
chick_band26$fat <- as.factor(chick_band26$fat)
chick_band26$condition <- as.factor(chick_band26$condition)

# check your data
summary(adult_band26)
summary(chick_band26)

### Remove any rows that have no values (N/A) in 'disposition' column
chick_band26 <- chick_band26[!is.na(chick_band26$disposition), ]

### Subset newly bands and recaptured adults
adult_recaps26 <- adult_band26[adult_band26$disposition == "R", ]
adult_new26 <- adult_band26[adult_band26$disposition == "1", ]
summary(adult_new26)
summary(adult_recaps26)


### Get summary table of the adult morphological measurements.
morph_summary <- adult_band26 %>%
  group_by(sex) %>%
  summarise(
    across(
      c(culmen, wing_chord, flat_wing, mass),
      ~paste0(
        round(mean(.x, na.rm = TRUE), 2),
        " ± ",
        round(sd(.x, na.rm = TRUE)/ sqrt(sum(!is.na(.x))), 2)
      )
    )
  )

### export morph summary
write_xlsx(morph_summary, "output/adult_morphometrics.xlsx")


### Get summary table of the chick morphological measurements.
morph_summary_chicks <- chick_band26 %>%
  summarise(
    across(
      c(culmen, flat_wing, longest_broken_primary, mass),
      ~paste0(
        round(mean(.x, na.rm = TRUE), 2),
        " ± ",
        round(sd(.x, na.rm = TRUE)/ sqrt(sum(!is.na(.x))), 2)
      )
    )
  )

### export morph summary
write_xlsx(morph_summary_chicks, "output/chick_morphometrics.xlsx")



### How many nest boxes did we band from?
length(unique(chick_band26$box_number))
unique(chick_band26$box_number)


##### Read in previous years data to compare results
# NOTE: the lack of data entry limits what you can compare













