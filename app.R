library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(leaflet)
library(scico)
library(glue)
library(shinyWidgets)
library(DT)
library(ggthemes)

## TODO: include potato icons?
## TODO: Global App Options list?
## e.g.
## list(
##   max_stations = 5,
##   table_decimal_points = 2
## )


load("sites.RData")
site_names <- setNames(site_metadata$Site_Name, site_metadata$Site_ID)

#' Reference for the ordering of the time units
#'
#' Returns the numerical value of the time units, or with `as.names = TRUE` will
#' return the formatted name of the unit. Useful for limiting the valid time
#' aggregations in UI selections.
#' @param as.names Default = FALSE, to show numerical values. TRUE to show named
#'   labels.
#' @param formatter Function to format named labels
time_units <- function(as.names = FALSE, formatter = stringr::str_to_title) {
  labels <- c("hours", "days", "weeks", "months", "years")
  values <- c(0, 1, 2, 3, 4)
  if (as.names) return(setNames(labels, formatter(labels)))

  setNames(values, labels)
}

#' Add metadata to site data
add_metadata <- function(obs) {
  obs %>% left_join(site_metadata, by = c("Site" = "Site_ID"))
}

#' Primary plot of weather observations
#'
#' @description TODO
#' @details TODO
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
  choice <- paste(t_agg, t_rep)

  ## define a function to change how the data is represented in time
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
    ## NOTE: na.rm = TRUE with min/max leads to some Inf values. How to deal with these?
    summarise({{ variable }} := .fn(.data[[variable]], na.rm = TRUE)) %>%
    ungroup(ob_time) %>%
    add_metadata() %>%
    mutate(x = x_rep(ob_time)) %>%
    ggplot(aes(x = x, y = .data[[variable]], color = Site_Name))

  if (choice %in% c("hours years", "days years")) {
    p <- p + geom_line(size = 1, alpha = 0.75)
  } else {
    p <- p + geom_point(size = 3, alpha = 0.50)
  }

  p +
    scale_colour_brewer(palette = "Set2") +
    theme_minimal() +
    labs(x = NULL) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
}

#' Calculate the Hutton criteria
#'
#' For a given day, finds the minimum temperature, and the number of hours above
#' the humidity threshold, for the previous two days The Hutton criteria are
#' reached when both the previous two days have a minimum temperature of at leat
#' 10C, and at least 6 hours of relative humidity >= 90%.
#'
#' @details Additional metrics `warm.n` and humid.n are included. These take an
#'   average of the previous two days recordings for `min_temp` and
#'   `humid_hours`, which can be used as a rough calculation for how warm or
#'   humid it was.
#'
#' @param obs Data Frame of weather observations
#'
#' @return Data Frame with Hutton criteria calculations for each time unit
#' \describe{
#'   \item{`Site`}{unique site id}
#'   \item{`ob_time`}{time of measurement}
#'   \item{`min_temp`}{minimum temperature at ob_time}
#'   \item{`humid_hours`}{number of hours above humidity threshold at ob_time}
#'   \item{`warm`}{were ob_time - 1 and ob_time_2 "warm"?}
#'   \item{`warm.n`}{average min_temp of ob_time - 1 and ob_time - 2}
#'   \item{`humid`}{were ob_time - 1 and ob_time_2 "humid"?}
#'   \item{`humid.n`}{average humid_hours of ob_time - 1 and ob_time - 2}
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

#' Plot the Hutton criteria for a selected weather station
#'
#' @details A *dark* *square* denotes days where both Hutton Criteria have been
#'   met.
#'
#' @note The following was intended functionality, but produced an error, so
#'   just using the `min_temp` and `humid_hours` values for each day:
#'   For when the criteria are not met, `warm.n` and `humid.n` are used as a
#'   rough guess for how close the measurements are to the criteria. This
#'   function uses some internal figures to set the size and colour scales of
#'   the output. The colour scale is set to vary in darkness based on the
#'   temperature (`warm.n` when temperature criteria `warm` is not met). The
#'   maximum value of a non-critical temperature is made different (light)
#'   enough to easily distinguish it from a data point with critical
#'   temperature. The humidity dictates the shape, with a square to clearly
#'   display critical humidity (when `humid` is TRUE), and circles to show
#'   non-critical. The size of these circles increase with `humid.n`
plot.hutton <- function(obs, only_hutton = FALSE) {
  temp_lim <- 30
  hum_lim <- 75

  ## define the possible icon shapes.
  ## Names are legend names
  ## Values are ggplot2 geom_point type
  legend_icons <- c(
    "Low hum/low temp" = "circle filled",
    "Low hum/critical temp" = "circle filled",
    "Critical hum/low temp" = "square filled",
    "Hutton Criteria met" = "square filled",
    "Missing data" = "cross"
  )

  ## TODO: add a UI toggle to show only points with full Hutton criteria (WIP)
  if (only_hutton) {
    obs <- filter(obs, warm & humid)
  }

  ## NOTE: numbers in here are set to make the size and colour scales easily
  ## distinguishable from critical humidity and temperatures
  obs %>%
    mutate(
      ## T = case_when(warm ~ 30, !warm ~ warm.n, is.na(warm) ~ 0),
      T = case_when(warm ~ 30, !warm ~ min_temp, is.na(warm) ~ 0),
      ## H = case_when(humid ~ 75L, !humid ~ as.integer(humid.n), is.na(humid) ~ 35L) / 75, # TODO: fix error here
      H = case_when(humid ~ 75L, !humid ~ humid_hours, is.na(humid) ~ 35L) / 75,
      H_shape = factor(if_else(is.na(humid), "Missing data", if_else(humid, "Critical hum/low temp", "Low hum/low temp")))
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
    ## Set colour range
    scico::scale_fill_scico(palette = "bilbao", limits = c(-6, 30)) +
    xlab(NULL) + ylab(NULL) +
    ## Manual Legends to describe plot components
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
        title = ""
      )
    ) +
    theme_minimal() +
    scale_y_discrete(position = "right") +
    scale_x_continuous(limits = c(1, 31), breaks = seq(1, 31, by = 1)) +
    theme(
      title = element_text(hjust = 0.5, size = 16),
      panel.grid = element_blank(),
      legend.position = "left",
      axis.text = element_text(size = 14),
      axis.text.y.right = element_text(angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 12)
    )
}

ui <- fluidPage(
  titlePanel(
    h1("Monitoring blight in Blighty", align = "center"),
    windowTitle = "Hutton Dash"
  ),
  hr(),
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
      selectInput(
        "statistic",
        "Choose summary statistic",
        choices = c("Mean" = "mean",
                    "Maximum" = "max",
                    "Minimum" = "min"),
        selected = "mean"
      ),
      selectInput(
        "t_agg",
        "Time Aggregation",
        choices = time_units(as.names = TRUE)[time_units() < time_units()["years"]],
        selected = "days"
      ),
      selectInput(
        "t_rep",
        "Time Representation",
        choices = time_units(as.names = TRUE)[time_units() > time_units()["hours"]],
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
      hr(),
      leafletOutput("map", height = "600px"),
      hr(),
      h4("Download"),
      ## TODO: centre these buttons, and provide whitespace below
      fluidRow(
        downloadButton("download_csv", "Past Week Summary Table [csv]"),
        downloadButton("download_Rmd", "All Outputs [docx]")
      )
    ),
    mainPanel(
      plotOutput("primary"),
      hr(),
      plotOutput("hutton"),
      hr(),
      dataTableOutput("last_week")
    ),
    position = "right"
  )
)

#' App server
#'
#' @details The main objects in the application are the primary plot, the Hutton
#'   plot, the data table, the map, and the inputs. The main outputs are bound
#'   to reactives, rather than being defined within `render*` functions, so that
#'   they can be accessed by the document renderer, and also so that they can be
#'   bound with `bindCache` to changes to their inputs. This caching is used for
#'   the plots and the map, where time can be saved if the user requests a
#'   graphic that has already been displayed.
#' @param input
#' @param output
#' @param session
#' @return
server <- function(input, output, session) {
  max_stations <- 5

  ## Store of the selected markers on the map
  selected <- reactiveVal(c("Heathrow", "Abbotsinch"))

  filtered <- reactive(site_metadata %>% filter(Site_Name %in% selected()))

  ## Wrapper for `identity` to allow it to work with the `na.rm` argument that
  ## is used by summery stats functions
  no_aggregation <- function(x, na.rm) identity(x)

  primary_plot <- reactive({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      plot.primary(input$variable, get(input$statistic), input$t_agg, input$t_rep)
  }) %>%
    bindCache(input$variable, input$statistic, input$t_agg, input$t_rep, filtered())

  hutton_plot <- reactive({
    site_data %>%
      semi_join(filtered(), by = c("Site" = "Site_ID")) %>%
      hutton() %>%
      filter(month(ob_time) == month(input$month_selector)) %>%
      plot.hutton() +
      ggtitle(
        paste(
          "Monitoring warmth and humidity with the Hutton Criteria for",
          as.character(format(as.Date(input$month_selector), "%B"))
        )
      )
  }) %>%
    bindCache(input$month_selector, filtered())

  leaflet_map <- reactive({
    ## TODO: use Stadia.AlidadeSmooth tileset
    leaflet(
      data = site_metadata %>%
        mutate(colour = if_else(Site_Name %in% selected(), "green", "grey")),
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
  }) %>%
    bindCache(selected())

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


  output$primary <- renderPlot(primary_plot()) %>% bindCache(primary_plot())

  output$hutton <- renderPlot(hutton_plot()) %>% bindCache(hutton_plot())

  output$map <- renderLeaflet(leaflet_map()) %>% bindCache(leaflet_map())

  output$last_week <- DT::renderDataTable({
    datatable(
      last_week(),
      options = list(
        dom = 'tp',
        pageLength = 7
      )
    ) %>%
      DT::formatRound(
        columns = c("wind_speed", "air_temperature", "rltv_hum", "visibility"),
        digits = 2
      )
  })

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
        last_week = last_week()
      )
      rmarkdown::render(
                   tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv())
                 )
    }
  )

  ## Limit choice of aggregation function based on Time Aggregation
  observeEvent(
    input$t_agg,
    {
      ## If t_agg = "hours" is selected, don't offer any summary stat functions,
      ## (no aggregation)
      if (input$t_agg == "hours") {
        updateSelectInput(
          inputId = "statistic",
          choices = c("Raw data (no aggregation)" = "no_aggregation")
        )
      } else {
        updateSelectInput(
          inputId = "statistic",
          choices = c("Mean" = "mean",
                      "Maximum" = "max",
                      "Minimum" = "min")
        )
      }
    }
  )

  ## Limit choice of Time Aggregation based on Time Representation
  observeEvent(
    input$t_rep,
    {
      ## TODO: This can cause some momentary (visible) changing of plots, as shiny
      ## might be making reactive changes one by one. Is there a way to have changes
      ## take place instantly? Maybe forcing the t_agg var to something rather than
      ## letting it reset might help?
      valid_aggs <- time_units(TRUE)[time_units() < time_units()[input$t_rep]]

      ## The "week" of the month can be ambiguous - don't allow this situation
      if (input$t_rep == "months") {
        valid_aggs <- valid_aggs[valid_aggs != "weeks"]
      }

      updateSelectInput(
        inputId = "t_agg",
        choices = valid_aggs
      )
    }
  )

  ## Handle map marker clicks
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

      ## Update map markers
      leafletProxy(
        "map",
        data = site_metadata %>%
          ## TODO: Make colour of marker the same as the plot colour. This
          ## would make it easy to match the plot information to location on map
          mutate(colour = if_else(Site_Name %in% selected(), "green", "gray"))
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
