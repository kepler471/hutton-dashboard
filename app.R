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
  left_join(site_metadata, by = c("Site" = "Site_ID")) %>%
  mutate(ob_time = dmy_hm(ob_time))

hutton <- function(.data) {
  .data %>%
    group_by(hour, month, day) %>%

}
