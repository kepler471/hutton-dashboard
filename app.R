library(tidyverse)
library(shiny)
library(fs)
library(lubridate)
library(leaflet)

data_path <- "data/"
files <- dir(data_path)
site_files <- files[files != "Sites.csv"]

site_paths <- paste0(data_path, site_files)

site_metadata <- read_csv(paste0(data_path, "Sites.csv"))

site_data <- map_dfr(
  site_paths,
  ~ read_csv(
    .x,
    col_types = "ciiiddddi",
    na = c("NA", NA)
  )
) %>%
  mutate(ob_time = dmy_hm(ob_time)) %>%
  select(-c(hour, day, month))


## TODO: Need to cleanup all the datetime data. Fix the excess observations,
##  non-existant dates ...

#' Calculate the Hutton Criteria
#'
#' @description TODO
#'
#' @param obs Data Frame of weather observations
hutton <- function(obs) {
  obs %>%
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

#' Aggregate and summarise weather observations
#'
#' Is able to produce a single summary statistic (eg. max, min, mean) for a
#'  given aggregate time unit.
#'
#' @note Maybe this function should return data. Have a separate plotter. This
#'  way, different aggregations could be combined, but only the plotter would
#'  have to be aware of that logic. Keeps this func simple and focussed on
#'  individual aggregations.
#'
#' @param obs Data frame of weather observations
#' @param sites List of sites to filter observations by
#' @param variable The measurement to display
#' @param t_rep The unit of time represented in the x axis
#' @param t_agg The unit of time for aggregation
#' @param .fn Function to compute summary statistic on `variable`. Must accept
#'  `na.rm` as a second arg.
aggregate.primary <- function(obs, variable, t_rep, t_agg, .fn) {
  ## If aggregation and representation are hourly, there is no calculation to
  ## make as this is already the structure of the data.
  if (t_rep == "hours" & t_agg == "hours") {
    return(obs)
  }

  units <- list("hours" = 0, "days" = 1, "months" = 2)

  ## Time unit of aggregation (agg) cannot be smaller than the unit of time
  ## represented (t_rep) in the x-axis. If this situation is given, set agg to
  ## be equal to t_rep.
  if (units[[t_agg]] < units[[t_rep]]) {
    t_agg <- t_rep
  }

  obs %>%
    mutate(ob_time = floor_date(ob_time, t_agg)) %>%
    group_by(Site, ob_time) %>%
    summarise(stat = .fn({{ variable }}, na.rm = TRUE)) %>%
    ungroup(ob_time)
}

#' Primary plot of weather observations
#'
#' @note TODO: Can library(units) be used here to deal with the time units?
#'  Alternatively, just make the `choice` variable and all the `t_*` variables
#'  numeric, and include a lookup.
#'
#' @param obs Aggregated data frame of weather observations
#' @param variable The measurement to display
#' @param t_rep The unit of time represented in the x axis
#' @param t_range The range of values that t_rep will be displayed within,
#'  e.g. days in the week (t_rep = "days", t_range = "weeks") -> [1:7]
#'  e.g. days in the month (t_rep = "days", t_range = "months") -> [1:31]
#' @param t_agg The unit of time for aggregation
plot.primary <- function(obs, variable, t_rep, t_agg, t_range = "years") {
  choice <- paste(t_rep, t_range)

  x_rep <- switch(
    choice,
    "days weeks" = function(x) wday(x, week_start = 1),
    "days months" = mday,
    "days years" = identity, # Calendar time
    "hours days" = hour,
    "hours weeks" = function(x) (wday(x, week_start = 1) - 1) * hour(x),
    "hours months" = function(x) (mday(x) - 1) * hour(x),
    "weeks years" = week,
    "months years" = month
  )

  p <- obs %>%
    add_metadata() %>%
    mutate(x = x_rep(ob_time)) %>%
    ggplot(aes(x = x, y = stat, color = Site_Name))

  ## if (choice == "hours days") {
  ##   return(p + geom_line())
  ## } else {
  ##   return(p + geom_point())
  ## }
  return(p + geom_point())
}

## TODO
aggregate.hutton <- function() {}
plot.hutton <- function() {}

## TODO
aggregate.map <- function() {}
plot.map <- function() {}

example_sites <- site_metadata %>% filter(Site_Name %in% c("Aldergrove", "Heathrow", "Leuchars", "Shawbury"))

add_metadata <- function(obs) {
  obs %>% left_join(site_metadata, by = c("Site" = "Site_ID"))
}

## Test aggregate.primary and plot.primary
if (FALSE) {
  site_data %>%
    semi_join(example_sites, by = c("Site" = "Site_ID")) %>%
    aggregate.primary(air_temperature, "days", "days", max) %>%
    plot.primary("", "days", "days", "months")
}

#### Server ####


ui <- fluidPage(
  fluidRow(
    leafletOutput("map")
  ),
  fluidRow(
    textOutput("clicked")
  ),
  fluidRow(
    tableOutput("metadata")
  )
)

server <- function(input, output, session) {
  ## TODO: Reactive to produce the metadata. This, or another reactive should
  ##  manage the selected weather stations.

  selected <- c("Heathrow", "Abbotsinch")

  metadata <- reactive({
    site_metadata
  })

  output$metadata <- renderTable(metadata())
  ## output$selected <- reactive({
  ##   metadata() %>% filter(selected)
  ## })

  output$map <- renderLeaflet({
    leaflet(
      data = metadata() %>%
        mutate(colour = if_else(Site_Name %in% selected, "green", "red"))
    ) %>%
      addTiles() %>%
      addScaleBar() %>%
      addMiniMap() %>%
      addCircleMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        label = ~ Site_Name,
        layerId = ~ Site_ID,
        color = ~ colour
      )
  })

  output$something <- renderPrint({reactiveValuesToList(input)})

  observe({
    click<-input$map_marker_click
    if(is.null(click)) {
      return()
    }
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    text2<-paste("You've selected point ", click$id, text)
    output$clicked<-renderText({
      text2
    })

    selected <<- c(selected, click$id)

    leafletProxy(
      "map",
      data = metadata() %>%
        mutate(colour = if_else(Site_Name %in% selected, "green", "red"))
    ) %>%
      clearMarkers() %>% # TODO: does this need to be optimised? maybe just remove the edited markers
      addCircleMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        label = ~ Site_Name,
        layerId = ~ Site_ID,
        color = ~ colour
      )
  })
}

shinyApp(ui, server)
