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
  mutate(ob_time = dmy_hm(ob_time)) %>%
  select(-c(hour, day, month))

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

#' Main plot of the dashboard
#'
#' Is able to produce a single summary statistic (eg. max, min, mean) for a
#'  given aggregate time unit.
#'
#' @note Maybe this function should return data. Have a separate plotter. This
#'  way, different aggregations could be combined, but only the plotter would
#'  have to be aware of that logic
#' @param obs Data frame of weather observations
#' @param sites List of sites to filter observations by
#' @param variable The observation type to display
#' @param t_rep The unit of time represented in the x axis
#' @param t_agg The unit of time for aggregation
#' @param .fn Function to compute summary statistic on `variable`
plot_primary <- function(obs, sites, variable, t_rep, t_agg, .fn) {
  units <- list("months" = 0, "days" = 1, "hours" = 2)

  ## Time unit of aggregation (agg) cannot be larger than the unit of time
  ## represented (t_rep) in the x-axis. If this situation is given, set agg to
  ## be equal to t_rep.
  if (units[[t_agg]] > units[[t_rep]]) {
    t_agg <- t_rep
  }

  obs %>%
    filter(Site %in% sites) %>%
    mutate(ob_time = floor_date(ob_time, t_agg)) %>%
    group_by(Site, ob_time) %>%
    summarise(
      max = max({{ variable }}, na.rm = TRUE),
      min = min({{ variable }}, na.rm = TRUE),

    )

}
