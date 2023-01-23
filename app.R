library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(leaflet)
library(scico)
library(glue)
library(shinyWidgets)
library(DT)

## TODO: include potato icons?

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
#'
#' For a given day, finds the minimum temperature, and the number of hours above
#' the humidity threshold, for the previous two days The Hutton criteria are
#' reached when both the previous two days have a minimum temperature of at leat
#' 10C, and at least 6 hours of relative humidity >= 90%.
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
hutton <- function(obs) {
  obs %>%
    mutate(ob_time = floor_date(ob_time, "days")) %>%
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

#' Primary plot of weather observations
#'
#' @param obs Data Frame of weather observations
#' @param variable The measurement to display
#' @param .fn Summary function to apply to `variable`
#' @param t_agg The unit of time for aggregation
#' @param t_rep The unit of time represented in the x axis
#'  e.g. days in the week (`t_agg` = "days", `t_rep` = "weeks") -> [1:7]
#'  e.g. days in the month (`t_agg` = "days", `t_rep` = "months") -> [1:31]
#'  e.g. calendar date (`t_agg` = "days" *OR* "hours", `t_rep` = "years")
plot.primary <- function(obs, variable, .fn, t_agg, t_rep) {
  variable <- unname(variable)
  t_agg <- unname(t_agg)
  t_rep <- unname(t_rep)
  choice <- paste(t_agg, t_rep)

  x_rep <- switch(choice,
    "hours days" = hour,
    "hours weeks" = function(x) (24 * (wday(x, week_start = 1) - 1)) + hour(x),
    "days weeks" = function(x) wday(x, week_start = 1),
    "hours months" = function(x) (24 * (mday(x) - 1)) + hour(x),
    "days months" = mday,
    "hours years" = identity,
    "days years" = as_date,
    "weeks years" = week,
    "months years" = month
  )

  p <- obs %>%
    mutate(ob_time = floor_date(ob_time, t_agg)) %>%
    group_by(Site, ob_time) %>%
    summarise({{ variable }} := .fn(.data[[variable]], na.rm = TRUE)) %>%
    ungroup(ob_time) %>%
    add_metadata() %>%
    mutate(x = x_rep(ob_time)) %>%
    ggplot(aes(x = x, y = .data[[variable]], color = Site_Name)) # TODO: dynamic naming of stat

  if (choice %in% c("hours years", "days years")) {
    return(p + geom_line())
  }
  return(p + geom_point())
}

plot.hutton <- function(obs, only_hutton = FALSE) {

#' Plot the Hutton criteria for a selected weather station
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

  ## TODO: add a toggle to show only points with full Hutton criteria
  if (only_hutton) {
    obs <- filter(obs, warm & humid)
  }

  ## TODO: document the magic numbers here
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "variable",
        "Select Weather Variable",
        choices = c("Wind speed (knots)" = "wind_speed",
                    "Air temperature (C)" = "air_temperature",
                    "Relative humidity (%)" = "rltv_hum",
                    "Visibility (m)" = "visibility"),
        selected = "air_temperature"
      ),
      ## selectInput(
      ##   "statistic",
      ##   "Choose summary statistic",
      ##   choices = c("Minimum" = min,
      ##               "Maximum" = max)
      ## ),
      selectInput(
        "t_agg",
        "Time Aggregation",
        choices = time_aggregation(as.names = TRUE),
        selected = "hours"
      ),
      selectInput(
        "t_rep",
        "Time Representation",
        choices = time_aggregation(as.names = TRUE),
        selected = "years"
      ),
      selectInput(
        "month_selector",
        "Choose month to check for Hutton Criteria",
        choices = setNames(
          seq(date("2022-01-01"), date("2022-11-01"), by = "months"),
          format(seq(date("2022-01-01"), date("2022-11-01"), by = "months"), "%B %Y")
        ),
        selected = date("2022-11-01")
      ),
      h3("Download"),
      ## TODO: centre these buttons, and provide whitespace below
      fluidRow(
        downloadButton("download_csv", "Past Week Summary Table [csv]"),
        downloadButton("download_Rmd", "All Outputs [docx]")
      ),
      leafletOutput("map", height = "600px")
    ),
    mainPanel(
      plotOutput("primary"),
      plotOutput("hutton"),
      ## TODO: format figures to lower s.f. in table output
      ## TODO: remove search from table
      dataTableOutput("last_week"),
      ),
    position = "right"
  )
)

#' Server
#'
#' @param input
#' @param output
#' @param session
#' @return
#' @author Stelios Georgiou
server <- function(input, output, session) {
  ## TODO: Use freeze to remove the flickering
  ## TODO: Create the UI inputs as reactives with renderUI. Use this to limit
  ##  the possible combinations of time units and ranges

  max_stations <- 5

  selected <- reactiveVal(c("Heathrow", "Abbotsinch"))
  filtered <- reactive(site_metadata %>% filter(Site_Name %in% selected()))

  ## TODO: Decide plot dimensions and scale at here
  primary_plot <- reactive({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      plot.primary(input$variable, min, input$t_agg, input$t_rep)
  })

  ## TODO: add a toggle to show/hide wamr & humid variables
  hutton_plot <- reactive({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      hutton() %>%
      filter(month(ob_time) == month(input$month_selector)) %>%
      plot.hutton()
  })

  leaflet_map <- reactive({
    ## TODO: use Stadia.AlidadeSmooth tileset
    leaflet(
      data = site_metadata %>%
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

  last_week <- reactive({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      add_metadata() %>%
      filter(ob_time >= date("2022-11-24")) %>% # TODO: calculate "latest_date" then go 6 days back
      mutate(ob_time = as_date(ob_time)) %>%
      rename(date = ob_time) %>%
      group_by(Site_Name, date) %>%
      summarise(
        across(
          c(wind_speed, air_temperature, rltv_hum, visibility),
          ~ mean(., na.rm = TRUE)
        )
      )
  })

  output$primary <- renderPlot(primary_plot())

  output$hutton <- renderPlot(hutton_plot())

  output$map <- renderLeaflet(leaflet_map())

  output$last_week <- DT::renderDataTable(last_week())

  output$download_csv <- downloadHandler(
    filename = function() paste0("last_week", ".csv"),
    content = function(file) write_csv(last_week(), file)
  )

  output$download_Rmd <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        primary = primary_plot(),
        hutton = hutton_plot(),
        last_week = last_week(),
        map = leaflet_map()
      )
      rmarkdown::render(
                   tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv())
                 )
    }
  )

  ## TODO: Try freezeReactiveValue for better performance/no flickering
  observeEvent(
    input$map_marker_click,
    {
      click <- input$map_marker_click

      prev_selection <- selected()
      new_selection <- site_names[as.character(click$id)]

      ## TODO: move logic out of server
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
          text = glue(
            "You may select a maximum of {max_stations} weather stations. ",
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
        data = site_metadata %>%
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
