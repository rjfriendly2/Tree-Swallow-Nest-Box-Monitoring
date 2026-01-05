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
visit_data25 <- read_excel("data/2025_BET_nest_record.xlsx")
visit_sum_data25 <- read_excel("data/2025_BET_nest_record_summary.xlsx")

# make variables factor variables
# 2025 BET nest record
visit_data25$species <- as.factor(visit_data25$species)
visit_data25$observer <- as.factor(visit_data25$observer)
visit_data25$year <- as.factor(visit_data25$year)
visit_data25$site <- as.factor(visit_data25$site)
visit_data25$box_number <- as.factor(visit_data25$box_number)
visit_data25$date <- as.Date(visit_data25$date)
head(visit_data25)

# 2025 BET nest record summary
head(visit_sum_data25)
visit_sum_data25$box_number <- as.factor(visit_sum_data25$box_number)
visit_sum_data25$species <- as.factor(visit_sum_data25$species)
visit_sum_data25$site <- as.factor(visit_sum_data25$site)
visit_sum_data25$year <- as.factor(visit_sum_data25$year)
visit_sum_data25$clutch_initiation <- as.Date(visit_sum_data25$clutch_initiation)
visit_sum_data25$clutch_completion <- as.Date(visit_sum_data25$clutch_completion)
visit_sum_data25$predicted_hatch <- as.Date(visit_sum_data25$predicted_hatch)
visit_sum_data25$incubation_start <- as.Date(visit_sum_data25$incubation_start)
visit_sum_data25$actual_hatch <- as.Date(visit_sum_data25$actual_hatch)
visit_sum_data25$predicted_fledge <- as.Date(visit_sum_data25$predicted_fledge)
visit_sum_data25$actual_fledge <- as.Date(visit_sum_data25$actual_fledge)
visit_sum_data25$successful <- as.factor(visit_sum_data25$successful)
head(visit_sum_data25)

### Get the earliest clutch intitiation
summary(visit_sum_data25) # look at min and max

### Get incubation length statistics
visit_sum_data25$incub_length <- as.numeric(visit_sum_data25$actual_hatch - visit_sum_data25$incubation_start)
summary(visit_sum_data25) # look at mean min and max
se_incubation <- sd(visit_sum_data25$incub_length, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data25$incub_length)))
# Incubation mean 17.12 +- 0.3, range (15 - 20)

### Get Success rate
visit_sum_data25 %>%
  summarise(
    num_success = sum(successful == "Y"),
    success_rate = mean(successful == "Y")
  )

### Get the nestling age for each nest on fledge date
visit_sum_data25$nestling_age <- as.numeric(visit_sum_data25$actual_fledge - visit_sum_data25$actual_hatch)
summary(visit_sum_data25)
se_nestling_age <- sd(visit_sum_data25$nestling_age, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data25$nestling_age)))

### Get the total of eggs laid accross all nests
sum(visit_sum_data25$number_eggs)
sum(visit_sum_data25$number_hatched)

### Get standard error
se_clutch_size <- sd(visit_sum_data25$number_eggs, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data25$number_eggs)))
se_hatch <- sd(visit_sum_data25$number_hatched, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data25$number_hatched)))
se_fledged <- sd(visit_sum_data25$number_fledged, na.rm = TRUE) / sqrt(sum(!is.na(visit_sum_data25$number_fledged)))

### Get the total number of fledglings.
sum(visit_sum_data25$number_fledged)

### create a summary table for tree swallow nest boxes
#subset to new dataframe
summary_2025 <- visit_sum_data25 %>%
  select(box_number,
         clutch_initiation,
         actual_hatch,
         actual_fledge,
         number_eggs,
         number_hatched,
         number_fledged,
         successful)

summary_2025_text <- visit_sum_data25 %>%
  select(box_number,
         clutch_initiation,
         actual_hatch,
         actual_fledge,
         number_eggs,
         number_hatched,
         number_fledged,
         successful)

#convert dates to abbreviated month and day
summary_2025_text$clutch_initiation <- format(summary_2025_text$clutch_initiation, "%b %d")
summary_2025_text$actual_hatch <- format(summary_2025_text$actual_hatch, "%b %d")
summary_2025_text$actual_fledge <- format(summary_2025_text$actual_fledge, "%b %d")
summary_2025_text


# create a row for numeric totals
egg_totals <- visit_sum_data25 %>%
  summarise(box_number = "Total",
            clutch_initiation = as.Date(NA),
            actual_hatch = as.Date(NA),
            actual_fledge = as.Date(NA),
            number_eggs = sum(number_eggs, na.rm = TRUE),
            number_hatched = sum(number_hatched, na.rm = TRUE),
            number_fledged = sum(number_fledged, na.rm = TRUE),
            successful = NA)


# create a row for averages
mean_row <- visit_sum_data25 %>%
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
date_se <- visit_sum_data25 %>%
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
count_se <- visit_sum_data25 %>%
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


mean_row_text <- summary_2025 %>%
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
df_text <- summary_2025_text
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
adult_band25 <- read_excel("data/2025_BET_adult_banding_data.xlsx")
chick_band25 <- read_excel("data/2025_BET_chick_banding_data.xlsx")

# prepare adult banding dataframe
adult_band25$box_number <- as.factor(adult_band25$box_number)
adult_band25$species <- as.factor(adult_band25$species)
adult_band25$site <- as.factor(adult_band25$site)
adult_band25$year <- as.factor(adult_band25$year)
adult_band25$bander <- as.factor(adult_band25$bander)
adult_band25$date <- as.Date(adult_band25$date)
adult_band25$disposition <- as.factor(adult_band25$disposition)
adult_band25$age <- as.factor(adult_band25$age)
adult_band25$how_aged <- as.factor(adult_band25$how_aged)
adult_band25$sex <- as.factor(adult_band25$sex)
adult_band25$fat <- as.factor(adult_band25$fat)
adult_band25$cp <- as.factor(adult_band25$cp)
adult_band25$bp <- as.factor(adult_band25$bp)
adult_band25$pp_molt <- as.factor(adult_band25$pp_molt)
adult_band25$pp_wear <- as.factor(adult_band25$pp_wear)

# prepare chick banding dataframe
chick_band25$box_number <- as.factor(chick_band25$box_number)
chick_band25$species <- as.factor(chick_band25$species)
chick_band25$site <- as.factor(chick_band25$site)
chick_band25$year <- as.factor(chick_band25$year)
chick_band25$bander <- as.factor(chick_band25$bander)
chick_band25$date <- as.Date(chick_band25$date)
chick_band25$disposition <- as.factor(chick_band25$disposition)
chick_band25$age <- as.factor(chick_band25$age)
chick_band25$sex <- as.factor(chick_band25$sex)
chick_band25$fat <- as.factor(chick_band25$fat)
chick_band25$condition <- as.factor(chick_band25$condition)

# check your data
summary(adult_band25)
summary(chick_band25)

### Remove any rows that have no values (N/A) in 'disposition' column
chick_band25 <- chick_band25[!is.na(chick_band25$disposition), ]

### Subset newly bands and recaptured adults
adult_recaps25 <- adult_band25[adult_band25$disposition == "R", ]
adult_new25 <- adult_band25[adult_band25$disposition == "1", ]
summary(adult_new25)
summary(adult_recaps25)


### Get summary table of the adult morphological measurements.
morph_summary <- adult_band25 %>%
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
morph_summary_chicks <- chick_band25 %>%
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
length(unique(chick_band25$box_number))
unique(chick_band25$box_number)


##### Read in previous years data to compare results
# NOTE: the lack of data entry limits what you can compare













