library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(ggiraph)
library(ROracle)
library(readxl)
library(DT)
library(data.table)
library(mgcv)
library(shinyalert)
library(lubridate)
library(ggExtra)

#' busy_indicator
#'
#' A busy indicator
#'
#' @param text The text to show
#' @param img An anitmated gif
#' @param wait The amount of time to wait before showing the busy indicator. The
#'   default is 1000 which is 1 second.
#'
#' @export
busy_indicator <- function(
  text = "Calculation in progress..",
  img = "../busy_indicator/ajaxloaderq.gif",
  wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "../busy_indicator/busy_indicator.css")
    ))
    , div(class = "shinysky-busy-indicator", p(text), img(src = img))
    , tags$script(sprintf(
      "	setInterval(function() {
  		 	 if ($('html').hasClass('shiny-busy')) {
  		    setTimeout(function() {
  		      if ($('html').hasClass('shiny-busy')) {
  		        $('div.shinysky-busy-indicator').show()
  		      }
  		    }, %d)
  		  } else {
  		    $('div.shinysky-busy-indicator').hide()
  		  }
  		},100)
  		", wait)
    )
  )
}

# Js feature that resets the app, needs to be global
js_reset_code <- "shinyjs.reset = function() {history.go(0)}"

# Writing a custom plotting function that will work for every variable selected---
# It will be a ggplot object that we then convert into a girafe object
# girafe is a package that allows interactivity.
plotting_function <- function(input_variable) {
  p1_ggplot <- ggplot(df_qry,
                      aes(DATECT, y = df_qry[, input_variable])) +
    geom_point_interactive(aes(data_id = checked, tooltip = checked,
                               colour = df_qry[, input_variable]), size = 3) +
    #geom_line(aes(y = df_qry$pred), colour = "red") +
    xlab("Date") +
    ylab(paste("Your variable:", input_variable)) +
    ggtitle(paste(input_variable, "time series")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())
  p1_girafe <- girafe(code = print(p1_ggplot),
                      width_svg = 6, height_svg = 5)
  p1_girafe <- girafe_options(p1_girafe, opts_selection(
    type = "multiple",
    css = "fill:#FF3333;stroke:black;"),
    opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
}

# Custom plotting function to construct a heatmap calendar of when data has been/has not been checked
plot_heatmap_calendar <- function(input_variable, df_qry) {
  # Transforming query dataframe with lubridate to fit the format needed for a heatmap calendar
  date_coverage_df <- df_qry %>%
    mutate(year = year(DATECT),
           day_of_the_week = lubridate::wday(DATECT, label = TRUE, week_start = 1),
           month = lubridate::month(DATECT, label = TRUE, abbr = FALSE),
           week = isoweek(DATECT),
           day = day(DATECT)) %>%
    select(year, month, day, week,
           day_of_the_week)
  date_coverage_df$week <- as.double(date_coverage_df$week)
  date_coverage_df <- dplyr::mutate(date_coverage_df,
                                    week = case_when(month == "December" & week == 1 ~ 53,
                                                     month == "January" & week %in% 52:53 ~ 0,
                                                     TRUE ~ week))
  # Assign the selected variable to the table ()
  date_coverage_df$variable <- input_variable

  # Hard coded result, will need to be changed
  date_coverage_df$has_been_checked <- FALSE
  date_coverage_df$has_been_checked[date_coverage_df$month == "February"] <- TRUE
  ####

  heatmap_plot <- ggplot(date_coverage_df,
                         aes(day_of_the_week, -week,
                             fill = has_been_checked)) +
    geom_tile(color = "white", size = 0.1) +
    facet_wrap(year~month, nrow = 4, ncol = 3, scales = "free") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 6)) +
    removeGrid()
  heatmap_plot
}
