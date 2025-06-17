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
library(writexl)

# Read in data
visit_data25 <- read_excel("data/2025_BET_nest_record.xlsx")
visit_sum_data25 <- read_excel("data/2025_BET_nest_record_summary.xlsx")

# view the data
head(visit_data25)

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
head(visit_sum_data25)


# view data
summary(visit_data25)

# create a histogram that shows a visual on nest initiation dates
ggplot(visit_sum_data25, aes(x = clutch_initiation)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "1 day",          # Tick every day; change to "2 days", "1 week", etc.
    date_labels = "%b %d") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/phenology_updates/clutch_init.jpg",width = 5, height = 5, dpi = 600)

# create historgram that shows visual of number of eggs laid
# subset to get rows with max egg number per unique nest box
vd_egg <- visit_data25 %>%
  group_by(box_number) %>%
  slice_max(order_by = eggs_number, with_ties = FALSE) %>%
  ungroup()

ggplot(vd_egg, aes(x = eggs_number)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, max(vd_egg$eggs_number), by = 1)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("output/phenology_updates/number_eggs.jpg",width = 5, height = 5, dpi = 600)

# create a historgram that shows the projected hatch dates
ggplot(visit_sum_data25, aes(x = predicted_hatch)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "1 day",          # Tick every day; change to "2 days", "1 week", etc.
    date_labels = "%b %d") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("output/phenology_updates/predicted_hatch.jpg",width = 5, height = 5, dpi = 600)

summary(visit_sum_data25)


# create a new data frame that shows the next nest checks for completed nests
completed_nests <- visit_sum_data25 %>%
  select(box_number, clutch_initiation, clutch_completion, predicted_hatch, actual_hatch) %>%
  mutate(next_check_7days = clutch_completion + 7) %>%
  mutate(nestling_age_11 = predicted_hatch + 11) %>%
  mutate(real_nestling_age_11 = actual_hatch + 11)

# create a histogram that shows the dates of where the nestlings should be 11 days old
ggplot(completed_nests, aes(x = nestling_age_11)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "1 day",          # Tick every day; change to "2 days", "1 week", etc.
    date_labels = "%b %d") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("output/phenology_updates/nestling_age_11.jpg",width = 5, height = 5, dpi = 600)

# Get the averages of 5 variable dates of interest
#visualize how r reads the data
head(completed_nests) # they are as <date> 
date_columns <- completed_nests[, c("clutch_initiation", "clutch_completion", "predicted_hatch", "nestling_age_11")]
date_columns_num <- as.data.frame(lapply(date_columns, as.numeric))
ave_date_columns_num <- colMeans(date_columns_num, na.rm = TRUE)
average_dates <- as.Date(ave_date_columns_num, origin = "1970-01-01")
sum_table <- data.frame(column = names(average_dates), average_date = average_dates)
print(sum_table)
write_xlsx(sum_table, "output/phenology_updates/summary_mean_table.xlsx")

# create a histogram that shoes the actual dates of hatch.
ggplot(completed_nests, aes(x = actual_hatch)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "1 day",          # Tick every day; change to "2 days", "1 week", etc.
    date_labels = "%b %d") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("output/phenology_updates/actual_hatch.jpg",width = 5, height = 5, dpi = 600)

# create a histogram that shows the dates of where the nestlings WILL ACTUALLY be 11 days old
ggplot(completed_nests, aes(x = real_nestling_age_11)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "1 day",          # Tick every day; change to "2 days", "1 week", etc.
    date_labels = "%b %d") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("output/phenology_updates/real_nestling_age_11.jpg",width = 5, height = 5, dpi = 600)






