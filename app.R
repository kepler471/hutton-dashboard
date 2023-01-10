library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(leaflet)

## TODO: include potato icons?
## TODO: try https://rstudio.github.io/gridlayout/

data_path <- "data/"
files <- dir(data_path)
site_files <- files[files != "Sites.csv"]
site_paths <- paste0(data_path, site_files)
site_metadata <- read_csv(paste0(data_path, "Sites.csv"))
site_names <- setNames(site_metadata$Site_Name, site_metadata$Site_ID)
site_data <- map_dfr(
  site_paths,
  ~ read_csv(
    .x,
    col_types = "ciiiddddi",
    na = c("NA", NA)
  )
) %>%
  ## Remove duplicate records where at least one is NA
  arrange(Site, ob_time, rltv_hum) %>%
  fill(rltv_hum, .direction = "down") %>%
  ## Remove all duplicates
  distinct() %>%
  mutate(ob_time = dmy_hm(ob_time, quiet = TRUE)) %>%
  drop_na(ob_time) %>%
  select(-c(hour, day, month))

#' Reference of the ordering of the time units
#'
#' Returns the numerical value of the time units, or with `as.names = TRUE` will
#' return the formatted name of the unit.
time_aggregation <- function(as.names = FALSE, formatter = stringr::str_to_title) {
  labels <- c("hours", "days", "weeks", "months", "years")
  values <- c(0, 1, 2, 3, 4)
  if (as.names) return(setNames(labels, formatter(labels)))

  setNames(values, labels)
}

add_metadata <- function(obs) {
  obs %>% left_join(site_metadata, by = c("Site" = "Site_ID"))
}

#' Calculate the Hutton criteria
#'
#' TODO: remove t_agg here, definition is in days
#' For a given unit of time `t_agg`, finds the minimum temperature, and the
#' number of hours above the humidity threshold, for the previous two units of
#' time. The Hutton criteria are reached when both the previous two days have a
#' minimum temperature of at leat 10C, and at least 6 hours of relative humidity
#' >= 90%.
#'
#' @param obs Data Frame of weather observations
#'
#' @return Data Frame with Hutton criteria calculations for each time unit
#' \describe{
#'   \item{Site}{unique site id}
#'   \item{ob_time}{time of measurement}
#'   \item{min_temp}{minimum temperature at ob_time}
#'   \item{humid_hours}{number of hours above humidity threshold at ob_time}
#'   \item{warm}{were ob_time - 1 and ob_time_2 "warm"?}
#'   \item{warm.n}{average min_temp of ob_time - 1 and ob_time - 2}
#'   \item{humid}{were ob_time - 1 and ob_time_2 "humid"?}
#'   \item{humid.n}{average humid_hours of ob_time - 1 and ob_time - 2}
#' }
hutton <- function(obs, t_agg = "days") {
  obs %>%
    mutate(ob_time = floor_date(ob_time, t_agg)) %>%
    group_by(Site, ob_time) %>%
    summarise(
      min_temp = min(air_temperature, na.rm = TRUE),
      humid_hours = sum(rltv_hum >= 90.0)
    ) %>%
    mutate(
      warm = pmin(lag(min_temp, n = 1), lag(min_temp, n = 2)) >= 10,
      warm.n = (lag(min_temp, n = 1) + lag(min_temp, n = 2)) / 2,
      humid = pmin(lag(humid_hours, n = 1), lag(humid_hours, n = 2)) >= 6,
      humid.n = (lag(humid_hours, n = 1) + lag(humid_hours, n = 2)) / 2
    ) %>%
    ungroup()
}

#' Aggregate and summarise weather observations
#'
#' Is able to produce a single summary statistic (eg. max, min, mean) for a
#'  given aggregate time unit.
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
  if (t_rep == "hours" & t_agg == "hours") return(obs)

  units <- time_aggregation()

  ## Time unit of aggregation (agg) cannot be smaller than the unit of time
  ## represented (t_rep) in the x-axis. If this situation is given, set agg to
  ## be equal to t_rep.
  ## NOTE: This should/will be handled by the server
  if (units[t_agg] < units[t_rep]) t_agg <- t_rep

  obs %>%
    mutate(ob_time = floor_date(ob_time, t_agg)) %>%
    group_by(Site, ob_time) %>%
    summarise(stat = .fn(.data[[variable]], na.rm = TRUE)) %>%
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

  x_rep <- switch(choice,
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
    ggplot(aes(x = x, y = stat, color = Site_Name)) # TODO: dynamic naming of stat

  ## TODO: handle choice of line/points
  return(p + geom_point())
}

#' Plot the Hutton criteria for a selected weather station
plot.hutton <- function(obs) {
  ## TODO: Could make the colour schemes the same
  temp_lim <- 30
  hum_lim <- 75

  legend_icons <- c(
    "Low hum/low temp" = "circle filled",
    ## "Low hum/mid temp" = "circle filled",
    "Low hum/critical temp" = "circle filled",
    ## "Mid hum/low temp" = "circle filled",
    ## "Mid hum/mid temp" = "circle filled",
    ## "Mid hum/critical temp" = "circle filled",
    "Critical hum/low temp" = "square filled",
    ## "Critical hum/mid temp" = "square filled",
    "Critical hum/critical temp" = "square filled",
    "Missing data" = "cross"
  )

  obs %>%
    mutate(
      T = case_when(warm ~ 30, !warm ~ min_temp, is.na(warm) ~ 0),
      H = case_when(humid ~ 75L, !humid ~ humid_hours, is.na(humid) ~ 35L) / 75,
      H_shape = factor(if_else(is.na(humid), "Missing data", if_else(humid, "Critical hum/low temp", "Low hum/low temp")))
      ## H_shape = addNA(if_else(humid, 1, 0), ifany = TRUE)
    ) %>%
    add_metadata() %>%
    ggplot() +
    geom_point(
      aes(
        x = day(ob_time),
        y = Site_Name,
        size = H,
        shape = H_shape,
        fill = T
      )
    ) +
    scale_shape_manual(
      values = legend_icons,
      drop = FALSE
    ) +
    scico::scale_fill_scico(palette = "bilbao", limits = c(-6, 30)) +
    xlab(NULL) + ylab(NULL) +
    guides(
      fill = guide_legend(
        title = "Low to critical temperature",
        title.position = "top",
        direction = "horizontal",
        label = FALSE,
        override.aes = list(
          size = rep(4, 4),
          shape = rep("circle filled", 4),
          fill = scico(6, palette = "bilbao")[c(2:4, 6)]
        )
      ),
      size = guide_legend(
        title = "Low to critical humidity",
        title.position = "top",
        direction = "horizontal",
        label = FALSE,
        override.aes = list(
          size = c(2:6),
          shape = c(rep("circle filled", 4), "square filled")
        )
      ),
      shape = guide_legend(
        override.aes = list(
          values = legend_icons,
          size = c(2, 2, 4, 4, 3),
          fill = c(rep(scico(5, palette = "bilbao")[c(2,5)], 2), 1)
        ),
        title = waiver()
      )
    ) +
    theme(panel.grid = element_blank())
}

site_data %>% hutton() %>% filter(month(ob_time) == 9) %>% plot.hutton()

legend_icons <- c(
  "Safe humidity" = "circle filled",
  "Safe temperature"
)

ui <- fluidPage(
  fluidRow(
    column(
      8,
      fluidRow(
        plotOutput("primary"),
        ## TODO: Add selector for summary function
        selectInput(
          "variable",
          "Select Weather Variable",
          choices = c("Wind speed (knots)" = "wind_speed",
                      "Air temperature (C)" = "air_temperature",
                      "Relative humidity (%)" = "rltv_hum",
                      "Visibility (m)" = "visibility"),
          selected = "air_temperature"
        ),
        selectInput(
          "range",
          "Date Range",
          choices = time_aggregation(as.names = TRUE),
          selected = "years"
        ),
        selectInput(
          "t_agg",
          "Time Aggregation",
          choices = time_aggregation(as.names = TRUE),
          selected = "days"
        ),
        selectInput(
          "t_rep",
          "Time Representation",
          choices = time_aggregation(as.names = TRUE),
          selected = "days"
        )
      ),
      fluidRow(
        plotOutput("hutton"),
        sliderInput(
          "month_selector",
          "Choose month",
          min = date("2022-01-01"),
          max = date("2022-11-01"), # TODO: make 12?
          value = date("2022-11-01"),
          ## step = "months",
          timeFormat = "%b"
        )
      )
    ),
    column(
      4,
      leafletOutput("map", height = 1000)
    )
  )
)

server <- function(input, output, session) {
  ## TODO: Create the UI inputs as reactives with renderUI. Use this to limit
  ##  the possible combinations of time units and ranges

  selected <- reactiveVal(c("Heathrow", "Abbotsinch"))
  metadata <- reactive(site_metadata)
  filtered <- reactive(metadata() %>% filter(Site_Name %in% selected()))
  ## TODO: reactive to generate filtered site_data
  output$primary <- renderPlot({
    ## TODO: Decide plot dimensions and scale at here
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      aggregate.primary(input$variable, input$t_rep, input$t_agg, max) %>%
      plot.primary("", input$t_rep, input$t_agg, input$range)
  })

  ## TODO: add a toggle to show/hide wamr & humid variables
  output$hutton <- renderPlot({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      hutton() %>%
      filter(month(ob_time) == month(input$month_selector)) %>%
      plot.hutton()
  })

  output$map <- renderLeaflet({
    ## TODO: use Stadia.AlidadeSmooth tileset
    leaflet(
      data = metadata() %>%
        mutate(colour = if_else(Site_Name %in% selected(), "green", "red")),
      options = leafletOptions(
        zoomControl = FALSE,
        maxZoom = 10,
        minZoom = 4)
    ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~Site_Name,
        layerId = ~Site_ID,
        color = ~colour
      )
  })

  ## TODO: Try freezeReactiveValue for better performance/no flickering
  observeEvent(
    input$map_marker_click,
    {
      click <- input$map_marker_click

      prev_selection <- selected()
      new_selection <- site_names[as.character(click$id)]

      if (new_selection %in% prev_selection) {
        if (length(prev_selection > 1)) {
          ## Remove from selection
          selected(prev_selection[prev_selection != new_selection])
        }
      } else if (length(prev_selection) < 5) {
        ## Add to selection
        selected(c(prev_selection, new_selection))
      } else {
        ## Show error thax max limit is reached
        shinyalert(
          text = paste(
            "There is a maximum of", 5, "weather stations",
            "Please deselect a station to make room for the one you want"
          ),
          size = "xs",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = FALSE
        )
      }

      leafletProxy(
        "map",
        data = metadata() %>%
          ## TODO: Make colour of marker the same as the plot colour
          mutate(colour = if_else(Site_Name %in% selected(), "green", "red"))
      ) %>%
        clearMarkers() %>% # TODO: does this need to be optimised? maybe just remove the edited markers
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          label = ~Site_Name,
          layerId = ~Site_ID,
          color = ~colour
        ) %>%
        setView(
          lng = input$map_center$lng,
          lat = input$map_center$lat,
          zoom = input$map_zoom,
          options = list(animate = FALSE)
        )
    }
  )
}

shinyApp(ui, server)

#' (WIP): Map marker click handler
#'
#' @note Might be more trouble than worth to use this, rather than just leaving
#'  the code in the server function.
on_click <- function(new_selection, prev_selection, max.length = 5) {
  if (new_selection %in% prev_selection) {
    if (length(prev_selection > 1)) {
      ## Remove from selection
      return(prev_selection[prev_selection != new_selection])
    }
  } else if (length(prev_selection) < max.length) {
    ## Add to selection
    return(c(prev_selection, new_selection))
  }
    ## Show error thax max limit is reached
  return(
    shinyalert(
      text = paste(
        "There is a maximum of", 5, "weather stations",
        "Please deselect a station to make room for the one you want"
      ),
      size = "xs",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )
  )
}
