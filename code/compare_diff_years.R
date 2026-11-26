### Tree Swallow Nest Box Monitoring
### Date: 12/17/2025
### By: Randall J. Friendly
###
### Compare different years of nest box monitoring

## Activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
#library(openxlsx)
library(ggplot2)
library(tidyr)
library(writexl)
library(patchwork)

## Read in data
visit_sum_data17 <- read_excel("data/2017_BET_nest_record_summary.xlsx")
visit_sum_data18 <- read_excel("data/2018_BET_nest_record_summary.xlsx")
visit_sum_data23 <- read_excel("data/2023_BET_nest_record_summary.xlsx")
visit_sum_data25 <- read_excel("data/2025_BET_nest_record_summary.xlsx")
visit_sum_data26 <- read_excel("data/2026_BET_nest_record_summary.xlsx")

## Fix the dataframes so that R knows what kind of variable its working with
# 2017
visit_sum_data17$box_number <- as.factor(visit_sum_data17$box_number)
visit_sum_data17$species <- as.factor(visit_sum_data17$species)
visit_sum_data17$site <- as.factor(visit_sum_data17$site)
visit_sum_data17$year <- as.factor(visit_sum_data17$year)
visit_sum_data17$clutch_initiation <- as.Date(visit_sum_data17$clutch_initiation)
visit_sum_data17$clutch_completion <- as.Date(visit_sum_data17$clutch_completion)
visit_sum_data17$predicted_hatch <- as.Date(visit_sum_data17$predicted_hatch)
visit_sum_data17$incubation_start <- as.Date(visit_sum_data17$incubation_start)
visit_sum_data17$actual_hatch <- as.Date(visit_sum_data17$actual_hatch)
visit_sum_data17$predicted_fledge <- as.Date(visit_sum_data17$predicted_fledge)
visit_sum_data17$actual_fledge <- as.Date(visit_sum_data17$actual_fledge)
visit_sum_data17$successful <- as.factor(visit_sum_data17$successful)

# 2018
visit_sum_data18$box_number <- as.factor(visit_sum_data18$box_number)
visit_sum_data18$species <- as.factor(visit_sum_data18$species)
visit_sum_data18$site <- as.factor(visit_sum_data18$site)
visit_sum_data18$year <- as.factor(visit_sum_data18$year)
visit_sum_data18$clutch_initiation <- as.Date(visit_sum_data18$clutch_initiation)
visit_sum_data18$clutch_completion <- as.Date(visit_sum_data18$clutch_completion)
visit_sum_data18$predicted_hatch <- as.Date(visit_sum_data18$predicted_hatch)
visit_sum_data18$incubation_start <- as.Date(visit_sum_data18$incubation_start)
visit_sum_data18$actual_hatch <- as.Date(visit_sum_data18$actual_hatch)
visit_sum_data18$predicted_fledge <- as.Date(visit_sum_data18$predicted_fledge)
visit_sum_data18$actual_fledge <- as.Date(visit_sum_data18$actual_fledge)
visit_sum_data18$successful <- as.factor(visit_sum_data18$successful)

# 2023
visit_sum_data23$box_number <- as.factor(visit_sum_data23$box_number)
visit_sum_data23$species <- as.factor(visit_sum_data23$species)
visit_sum_data23$site <- as.factor(visit_sum_data23$site)
visit_sum_data23$year <- as.factor(visit_sum_data23$year)
visit_sum_data23$clutch_initiation <- as.Date(visit_sum_data23$clutch_initiation)
visit_sum_data23$clutch_completion <- as.Date(visit_sum_data23$clutch_completion)
visit_sum_data23$predicted_hatch <- as.Date(visit_sum_data23$predicted_hatch)
visit_sum_data23$incubation_start <- as.Date(visit_sum_data23$incubation_start)
visit_sum_data23$actual_hatch <- as.Date(visit_sum_data23$actual_hatch)
visit_sum_data23$predicted_fledge <- as.Date(visit_sum_data23$predicted_fledge)
visit_sum_data23$actual_fledge <- as.Date(visit_sum_data23$actual_fledge)
visit_sum_data23$successful <- as.factor(visit_sum_data23$successful)

# 2025
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

# 2026
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

summary(visit_sum_data17)
summary(visit_sum_data18)
summary(visit_sum_data23)
summary(visit_sum_data25)
summary(visit_sum_data26)


# Stack these dataframes into one
sum_data_all <- bind_rows(visit_sum_data17, visit_sum_data18, visit_sum_data23, visit_sum_data25, visit_sum_data26)


# Get the mean clutch intiation date by year and include range and SE.
mean_dates_range_se <- sum_data_all %>%
  group_by(year) %>%
  summarise(
    mean_date = as.Date(mean(clutch_initiation, na.rm = TRUE), origin = "1970-01-01"),
    earliest = min(clutch_initiation, na.rm = TRUE),
    latest = max(clutch_initiation, na.rm = TRUE),
    se_days = sd(as.numeric(clutch_initiation), na.rm = TRUE) / sqrt(n())
  )

# create month-day date with constant year, to make creating a figure easier
mean_dates_range_se <- mean_dates_range_se %>%
  mutate(
    md_date = as.Date(
      paste0("2000-", format(mean_date, "%m-%d"))
    )
  )



## Get a visualizations of the clutch initiation time
library(ggplot2)

init_plot <- ggplot(mean_dates_range_se, aes(x = year, y = md_date)) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = md_date - se_days,
    ymax = md_date + se_days
  ),
  width = 0.2
  ) +
  scale_y_date(
    date_breaks = "2 days",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Year",
    y = "Mean Clutch Initiation Date"
  ) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  
# create new data frame just to get cumulative mean init date
mean_init_date_all <- sum_data_all
# add day of year column
mean_init_date_all$clutch_doy <- as.POSIXlt(mean_init_date_all$clutch_initiation)$yday + 1
mean_doy <- mean(mean_init_date_all$clutch_doy, na.rm = TRUE)
mean_date <- as.Date(mean_doy - 1, origin = "2000-01-01")
format(mean_date, "%b %d")


###### DO the same for hatch dates

# Get the mean hatch date by year and include range and SE.
mean_hatch_range_se <- sum_data_all %>%
  group_by(year) %>%
  summarise(
    mean_hdate = as.Date(mean(actual_hatch, na.rm = TRUE), origin = "1970-01-01"),
    earliest = min(actual_hatch, na.rm = TRUE),
    latest = max(actual_hatch, na.rm = TRUE),
    se_days = sd(as.numeric(actual_hatch), na.rm = TRUE) / sqrt(n())
  )

# create month-day date with constant year, to make creating a figure easier
mean_hatch_range_se <- mean_hatch_range_se %>%
  mutate(
    md_hdate = as.Date(
      paste0("2000-", format(mean_hdate, "%m-%d"))
    )
  )


### get a visial of hatch date timings
hatch_plot <- ggplot(mean_hatch_range_se, aes(x = year, y = md_hdate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = md_hdate - se_days,
    ymax = md_hdate + se_days
  ),
  width = 0.2
  ) +
  scale_y_date(
    date_breaks = "2 days",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Year",
    y = "Mean Hatch Date"
  ) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Get the mean fledge date by year and include range and SE.
mean_fledge_range_se <- sum_data_all %>%
  group_by(year) %>%
  summarise(
    mean_fdate = as.Date(mean(actual_fledge, na.rm = TRUE), origin = "1970-01-01"),
    earliest = min(actual_fledge, na.rm = TRUE),
    latest = max(actual_fledge, na.rm = TRUE),
    se_days = sd(as.numeric(actual_fledge), na.rm = TRUE) / sqrt(n())
  )

# create month-day date with constant year, to make creating a figure easier
mean_fledge_range_se <- mean_fledge_range_se %>%
  mutate(
    md_fdate = as.Date(
      if_else(
        is.na(mean_fdate),
        NA_character_,
      paste0("2000-", format(mean_fdate, "%m-%d"))
    )
  )
  )



## Get a visualizations of the fledge times
fledge_plot <- ggplot(mean_fledge_range_se, aes(x = year, y = md_fdate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = md_fdate - se_days,
    ymax = md_fdate + se_days
  ),
  width = 0.2
  ) +
  scale_y_date(
    date_breaks = "2 days",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Year",
    y = "Mean Fledge Date"
  ) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



### Lets stack these plots
combined_plot <- (init_plot / hatch_plot / fledge_plot) +
  plot_annotation(tag_levels = "a")
combined_plot


#or
combined_plot2 <- (init_plot + hatch_plot + fledge_plot) +
  plot_annotation(tag_levels = "a")
combined_plot2

ggsave("output/tree_swallow_phenology.jpg",
       plot = init_plot + hatch_plot + fledge_plot,
       width = 10,
       dpi = 300)






### create a frequency distribution figure for when swallows initiate and hatch and fledge date

# convert dates to day of year
visit_sum_data26$init_doy <- as.numeric(format(visit_sum_data26$clutch_initiation, "%j"))
visit_sum_data26$hatch_doy <- as.numeric(format(visit_sum_data26$actual_hatch, "%j"))
visit_sum_data26$fledge_doy <- as.numeric(format(visit_sum_data26$actual_fledge, "%j"))

init_hatch_phen <- visit_sum_data26 %>%
  select(init_doy, hatch_doy, fledge_doy) %>%
  pivot_longer(cols = everything(),
               names_to = "event",
               values_to = "doy") %>%
  mutate(event = recode(event,
                        init_doy = "Clutch Initiation",
                        hatch_doy = "Hatch",
                        fledge_doy = "Fledge"))

mean_lines <-init_hatch_phen %>%
  group_by(event) %>%
  summarize(mean_doy = mean(doy, na.rm = TRUE))

ggplot(init_hatch_phen, aes(x = doy, fill = event)) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "identity",
                 color = "black") +
  geom_vline(data = mean_lines,
             aes(xintercept = mean_doy, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(140, 210, by = 4),
    labels = function(x) format(as.Date(x - 1, origin = "2026-01-01"), "%b %d")
  ) + 
  scale_fill_manual(
    values = c("Clutch Initiation" = "#4E79A7",
               "Hatch" = "#F28E2B",
               "Fledge" = "#59A14F")
  ) +
  scale_color_manual(
    values = c("Clutch Initiation" = "#4E79A7",
               "Hatch" = "#F28E2B",
               "Fledge" = "#59A14F") 
    ) +
  labs(
    title = "2026 Tree Swallow Nest Monitoring",
    x = "Date",
    y = "Number of Nests",
  ) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

#save ggplot figure
ggsave("output/initiation_hatch_fledge_2026.jpg", width = 7, height = 4, units = "in", dpi = 300)

### See if the unoccupied nest boxes were occupied in previous years

#create an occupancy indicator for all years
sum_data_all <- sum_data_all %>%
  mutate(
    occupied = !is.na(clutch_initiation) &
      trimws(as.character(clutch_initiation)) != ""
  )

#identify empty nest boxes in 2026
empty_boxes_26 <- sum_data_all %>%
  filter(year == 2026, occupied == FALSE) %>%
  distinct(box_number)

#determine whether empty boxes were occupied in previous years
previous_occupancy <- sum_data_all %>%
  filter(year != 2026, occupied == TRUE) %>%
  group_by(box_number) %>%
  summarise(
    previously_occupied = TRUE,
    occupied_years = paste(sort(unique(year)), collapse = ", "),
    number_of_occupied_years = n_distinct(year),
    .groups = "drop"
  )

#combine results
empty_boxes_history <- empty_boxes_26 %>%
  left_join(previous_occupancy, by = "box_number") %>%
  mutate(
    previously_occupied = coalesce(previously_occupied, FALSE),
    occupied_years = coalesce(occupied_years, "None"),
    number_of_occupied_years = coalesce(number_of_occupied_years, 0L)
  )

#summarize
empty_boxes_history %>%
summarise(
  total_empty_boxes_2026 = n(),
  previously_occupied = sum(previously_occupied),
  never_occupied = sum(!previously_occupied),
  percent_previously_occupied =
    previously_occupied / total_empty_boxes_2026 * 100
)








