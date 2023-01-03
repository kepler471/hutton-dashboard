library(tidyverse)
library(shiny)
library(fs)
library(lubridate)

data_path <- "data/"
files <- dir(data_path)
site_files <- files[files != "Sites.csv"]

site_paths <- paste0(data_path, site_files)

site_metadata <- read_csv(paste0(data_path, "Sites.csv"))

site_data <- map_dfr(
  site_paths,
  ~
    read_csv(
      .x,
      col_types = "ciiiddddi",
      na = c("NA", NA))
)

site_data_fmt <-
  site_data %>%
  ## left_join(site_metadata, by = c("Site" = "Site_ID")) %>%
  mutate(ob_time = date(dmy_hm(ob_time))) %>%
  select(-c(day, month))

hutton <- function(.data) {
  site_data_fmt %>%
    group_by(Site, ob_time) %>%
    summarise(
      min_temp = min(air_temperature),
      humid_hours = sum(rltv_hum >= 90.0)
    ) %>%
    mutate(
        warm = pmin(lag(min_temp, n = 1), lag(min_temp, n = 2)) >= 10,
        humid = pmin(lag(humid_hours, n = 1), lag(humid_hours, n = 2)) >= 6
    ) %>%
    ungroup()
}
