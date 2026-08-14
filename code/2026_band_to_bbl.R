### Tree Swallow Nest Box Monitoring
### Date: 07/21/2026
### By: Randall J. Friendly
###
### This is to take the banding data and reformat it to submit it to BBL

## Activate packages. I Basically took packages used for data wrangling and brought them here.
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(writexl)

#read banding data
adult_band_2026 <- read_excel("data/2026_BET_adult_banding_data.xlsx")
chick_band_2026 <- read_excel("data/2026_BET_chick_banding_data.xlsx")

#work with adult banding data first

#separate newly banded birds from recaps
# using dplyr
adult_new_bands <- adult_band_2026 %>%
  filter(disposition == "1")

tres_recaps <- adult_band_2026 %>%
  filter(disposition == "R")

#create new dataframe to match columns for BBL and work with
bbl_bands <- adult_new_bands

#see datafram
head(bbl_bands)
summary(bbl_bands)

#Get the three date columns from the "date" column
bbl_bands <- bbl_bands %>%
  mutate(
    year = year(date),
    month = sprintf("%02d", month(date)),
    day = sprintf("%02d", day(date))
  )

#create a new column that will have "Box" in front of the box_number
bbl_bands <- bbl_bands %>%
  mutate(box_id = paste("Box", box_number))

#create a new column that will have "Flat Wing:" in front of the flat_wing
bbl_bands <- bbl_bands %>%
  mutate(flat_wing_label = paste("Flat Wing:", flat_wing))

#create a new column that will have "PP Wear:" in front of the pp_wear
bbl_bands <- bbl_bands %>%
  mutate(pp_wear_label = paste("PP Wear:", pp_wear))

#concatenate the three new columns into one
bbl_bands <- bbl_bands %>%
  mutate(
    remarks = paste(box_id, flat_wing_label, pp_wear_label, sep = ", ")
  )

###chagne how_aged enteries to PL from P
bbl_bands <- bbl_bands %>%
  mutate(
    how_aged = ifelse(toupper(how_aged) == "P", "PL", how_aged)
  )

# create a how-sexed column
bbl_bands <- bbl_bands %>%
  mutate(
    how_sexed = case_when(
      sex == "M" ~ "CL",
      sex == "F" ~ "BP",
      TRUE ~ NA_character_
    )
  )



#reorder columns to match bbl excel template
bbl_bands <- bbl_bands %>%
  select(
    band_number,
    species,
    disposition,
    year,
    month,
    day,
    age,
    sex,
    how_sexed,
    site,
    remarks,
    bander,
    culmen,
    mass,
    fat,
    bp,
    cp,
    mite_holes
  )

###export this
write_xlsx(bbl_bands, "BBL/bbl_bands.xlsx")


###KEEP IN MIND, THE SECOND YEAR BIRDS WILL HAVE A UNIQUE ENTRY UNLIKE THE AHYs


#now work with chick banding data

#create new dataframe to match columns for BBL and work with
bbl_chick_bands <- chick_band_2026

#Get the three date columns from the "date" column
bbl_chick_bands <- bbl_chick_bands %>%
  mutate(
    year = year(date),
    month = sprintf("%02d", month(date)),
    day = sprintf("%02d", day(date))
  )

#create a new column that will have "Box" in front of the box_number
bbl_chick_bands <- bbl_chick_bands %>%
  mutate(box_id = paste("Box", box_number))

#create a new column that will have "Flat Wing:" in front of the flat_wing
bbl_chick_bands <- bbl_chick_bands %>%
  mutate(flat_wing_label = paste("Flat Wing:", flat_wing))

#concatenate the two new columns into one
bbl_chick_bands <- bbl_chick_bands %>%
  mutate(
    remarks = paste(box_id, flat_wing_label, sep = ", ")
  )

#reorder columns to match bbl excel template
bbl_chick_bands <- bbl_chick_bands %>%
  select(
    band_number,
    species,
    disposition,
    year,
    month,
    day,
    age,
    sex,
    site,
    remarks,
    bander,
    culmen,
    mass,
    fat
  )


###export this
write_xlsx(bbl_chick_bands, "BBL/bbl_chick_bands.xlsx")


###work with recaptured birds
#create new dataframe to match columns for BBL and work with
bbl_recaps_bands <- tres_recaps

#Get the three date columns from the "date" column
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(
    year = year(date),
    month = sprintf("%02d", month(date)),
    day = sprintf("%02d", day(date))
  )

#create a new column that will have "Box" in front of the box_number
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(box_id = paste("Box", box_number))

#create a new column that will have "Flat Wing:" in front of the flat_wing
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(flat_wing_label = paste("Flat Wing:", flat_wing))

#create a new column that will have "PP Wear:" in front of the pp_wear
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(pp_wear_label = paste("PP Wear:", pp_wear))

#concatenate the three new columns into one
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(
    remarks = paste(box_id, flat_wing_label, pp_wear_label, sep = ", ")
  )

###chagne how_aged enteries to PL from P
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(
    how_aged = ifelse(toupper(how_aged) == "P", "PL", how_aged)
  )


# create a how-sexed column
bbl_recaps_bands <- bbl_recaps_bands %>%
  mutate(
    how_sexed = case_when(
      sex == "M" ~ "CL",
      sex == "F" ~ "BP",
      TRUE ~ NA_character_
    )
  )

#reorder columns to match bbl excel template
bbl_recaps_bands <- bbl_recaps_bands %>%
  select(
    band_number,
    species,
    disposition,
    year,
    month,
    day,
    age,
    sex,
    how_sexed,
    site,
    remarks,
    bander,
    culmen,
    mass,
    fat,
    bp,
    cp,
    mite_holes
  )

###export this
write_xlsx(bbl_recaps_bands, "BBL/bbl_recaps_bands.xlsx")






