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

#' busyIndicator
#' 
#' A busy indicator
#' 
#' @param text The text to show
#' @param img An anitmated gif
#' @param wait The amount of time to wait before showing the busy indicator. The
#'   default is 1000 which is 1 second.
#'   
#' @export
busyIndicator <- function(text = "Calculation in progress..",img = "../busyIndicator/ajaxloaderq.gif", wait=1000) {
	tagList(
  		singleton(tags$head(
    		tags$link(rel="stylesheet", type="text/css",href="../busyIndicator/busyIndicator.css")
  			))
  		,div(class="shinysky-busy-indicator",p(text),img(src=img))
  		,tags$script(sprintf(
  		"	setInterval(function(){
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
  		",wait)
  			)
	)	
}

# Js feature that resets the app, needs to be global
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page


# Writing a custom plotting function that will work for every variable selected---
# It will be a ggplot object that we then convert into a girafe object
# girafe is a package that allows interactivity.
plotting_function <- function(input_variable){
  p1_ggplot <- ggplot(df_qry, aes(DATECT, y = df_qry[, input_variable])) +
    geom_point_interactive(aes(data_id = checked, tooltip = checked,
                               colour = df_qry[, input_variable]), size = 3) +
    #geom_line(aes(y = df_qry$pred), colour = "red") + 
    xlab("Date") +ylab(paste("Your variable:", input_variable)) +
    ggtitle(paste(input_variable, "time series")) +
    theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
  p1_girafe <- girafe(code = print(p1_ggplot), width_svg = 6, height_svg = 5)
  p1_girafe <- girafe_options(p1_girafe, opts_selection(
    type = "multiple", css = "fill:#FF3333;stroke:black;"),
    opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
  p1_girafe
}