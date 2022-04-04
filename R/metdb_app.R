library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ROracle)
library(mgcv)
library(DT)
library(data.table)
library(shinyalert)
library(lubridate)
library(ggExtra)

metdbApp <- function(...) {
  
  # Define UI for the app
  ui <- dashboardPage(skin = "green",
                      dashboardHeader(title = "Met Data Validation"),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Dashboard", tabName = "dashboard"),
                          menuItem("Information", tabName = "information")
                        )
                      ),
                      dashboardBody(
                        useShinyjs(),
                        tabItems(
                          tabItem(tabName = "dashboard",
                                  fluidRow(
                                    box(title = "Database Controls",
                                        status = "success", solidHeader = TRUE,
                                        helpText("Select your required processing start and end times below."),
                                        column(width = 6,
                                               uiOutput("start_date")),
                                        column(width = 3,
                                               numericInput("shour",
                                                            value = 00, label = "Hour",
                                                            min = 0, max = 23, step = 1)),
                                        column(width = 3,
                                               numericInput("smin",
                                                            value = 00, label = "Minute",
                                                            min = 0, max = 59, step = 1)),
                                        column(width = 6,
                                               uiOutput("end_date")),
                                        column(width = 3,
                                               numericInput("ehour",
                                                            value = 00, label = "Hour",
                                                            min = 0, max = 23, step = 1)),
                                        column(width = 3,
                                               numericInput("emin",
                                                            value = 00, label = "Minute",
                                                            min = 0, max = 59, step = 1)),
                                        sliderInput("intslider",
                                                    label = "Smoothness (number of knots in cr spline):",
                                                    min = 1, max = 32, value = 10, step = 1),
                                        uiOutput("select_variables"),
                                        actionButton("retrieve_data", "Retrieve from database")
                                    ),
                                    box(title = "Calendar heatmap plot",
                                        status = "success", solidHeader = TRUE,
                                        plotOutput("heatmap_plot")
                                    ),
                                  ),
                                  hidden(
                                    fluidRow(id = "extracted_data",
                                             box(title = "Plotted Extracted Data",
                                                 status = "success", solidHeader = TRUE,
                                                 uiOutput("mytabs"),
                                                 shinyjs::disabled(actionButton("reset",
                                                                                label = "Restart app")),
                                                 shinyjs::disabled(actionButton("delete",
                                                                                label = "Delete selection")),
                                                 shinyjs::disabled(actionButton("nochange",
                                                                                label = "Finished checking variable for date range.")),
                                             ),
                                             box(title = "Data Change Log",
                                                 status = "success", solidHeader = TRUE,
                                                 dataTableOutput("summarytable"),
                                                 shinyjs::disabled(actionButton("submitchanges", "Submit changes"))
                                             )
                                    )
                                  ),
                          ),
                          tabItem(tabName = "information",
                                  h2("Information placeholder"),
                                  p("This app provides an interface to the field sites database and allows a user to plot data, remove dubious data and fill gaps with predictions."))
                        )
                      )
  )
  
  server <- function(input, output, session) {
    # Setting up the server----
    Sys.setenv(TZ = "GMT")
    Sys.setenv(ORA_SDTZ = "GMT")
    
    # Reading in the data flags ----
    data_flags$code <- as.character(data_flags$code)
    
    # Making database connection----
    drv <- dbDriver("Oracle")
    con <- dbConnect(drv,
                     dbname = Sys.getenv("DBNAME"),
                     username = Sys.getenv("DBUID"),
                     password = Sys.getenv("DBPWD")
    )
    table_name <- "MET_30MIN"
    db_names <<- dbListFields(con, table_name)
    db_names_for_box <- db_names[!db_names %in%
                                   c("DATECT", "TIMESTAMP", "datect_num", "checked", "pred")]
    
    # Format the dates for R----
    df_proc <- data.frame(
      start_date = "1995/01/01 00:00",
      end_date = "2019/12/31 00:00",
      ghgName = "co2"
    )
    df_proc$start_date <- as.POSIXct(
      df_proc$start_date, format = "%Y/%m/%d %H:%M", tz = "UTC")
    df_proc$end_date <- as.POSIXct(
      df_proc$end_date, format = "%Y/%m/%d %H:%M", tz = "UTC")
    
    # Create a reactive element with the earliest start date
    first_start_date <- reactive({
      min(as.Date(df_proc$start_date))
    })
    
    # Create a reactive element with the latest end date
    last_end_date <- reactive({
      max(as.Date(df_proc$end_date))
    })
    
    # Create a date input for the user to select start date
    output$start_date <- renderUI({
      dateInput("sdate",
                value = as.Date(strptime("01/07/2017", "%d/%m/%Y"), tz = "UTC"),
                min = first_start_date(),
                max = last_end_date(), label = "Start date"
      )
    })
    
    # Create a date input for the user to select end date
    output$end_date <- renderUI({
      dateInput("edate",
                value = as.Date(strptime("01/08/2017", "%d/%m/%Y"), tz = "UTC"),
                min = first_start_date(), max = last_end_date(), label = "End date"
      )
    })
    
    # Create a dataframe with all the information about the start and end time,
    # and cr spline (do we need this?)----
    job_df <- eventReactive(input$retrieve_data, {
      start_date <- paste(sprintf("%02d", day(input$sdate)), "/",
                          sprintf("%02d", month(input$sdate)), "/", year(input$sdate), " ",
                          sprintf("%02d", input$shour), ":", sprintf("%02d", input$smin), sep = "")
      start_date <- as.POSIXct(strptime(start_date, "%d/%m/%Y %H:%M"), tz = "UTC")
      end_date <- paste(sprintf("%02d", day(input$edate)), "/",
                        sprintf("%02d", month(input$edate)), "/", year(input$edate), " ",
                        sprintf("%02d", input$ehour), ":",
                        sprintf("%02d", input$emin), sep = "")
      end_date <- as.POSIXct(strptime(end_date, "%d/%m/%Y %H:%M"), tz = "UTC")
      n_times <- input$intslider
      # create a sequence of timestamps
      datect <- seq(start_date, end_date,
                    length = n_times
      )
      
      data.frame(
        datech = format(datect, "%Y/%m/%d %H:%M"),
        n_times = n_times,
        datect = datect
      )
    })
    
    # Rendering the box that we need to allow the user to select
    # all of the variables they wish to check----
    output$select_variables <- renderUI({
      checkboxGroupButtons("variable_check",
                           label = h5("Variables to be checked"),
                           choices = as.list(db_names_for_box), selected = NULL
      )
    })
    
    # Creating empty dataframes----
    change_summary <- data.frame()
    
    # Data retrieval functionality-----
    observeEvent(input$retrieve_data, {
      # enabling previously disabled buttons
      shinyjs::show("extracted_data")
      enable("reset")
      enable("delete")
      enable("nochange")
      
      # make a query for every variable that has been checked by the user.
      if (!is.null(input$variable_check)) {
        qry_variables <- paste(input$variable_check, collapse = ", ")
        qry <- paste0(
          "SELECT DATECT, TIMESTAMP, ", qry_variables, " FROM ", table_name,
          " WHERE DATECT > TO_DATE('", job_df()$datech[1],
          "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
          job_df()$datech[6], "', 'yyyy/mm/dd hh24:mi')"
        )
        df_qry <<- dbGetQuery(con, qry)
        df_qry$checked <<- as.factor(rownames(df_qry))
        df_qry$datect_num <<- as.numeric(df_qry$DATECT)
        
        # Add a tab to the plotting panel for each variable that has been selected by the user.
        # TO DO: I need to add a condition here that stops the user from adding duplicate variables at a later stage
        output$mytabs = renderUI({
          number_of_tabs = length(input$variable_check)
          my_tabs = lapply(paste(input$variable_check), function(i) {
            tabPanel(i,
                     value = i,
                     girafeOutput(paste0(i, "_interactive_plot")),
            ) 
          })
          do.call(tabsetPanel, c(my_tabs, id = "plotTabs"))
        })
        
        observe(
          lapply(paste(input$variable_check), function(i) {
            output[[paste0(i, "_interactive_plot")]] <-
              renderggiraph(plotting_function(i))
          })
        )
        
        # Give a warning if there are no variables selected.
      } else if (is.null(input$variable_check)) {
        shinyjs::alert("Please select one or more variables before extracting data from the database.")
      }
      
      # Creating a calendar heatmap plot that will be plotted depending on the tab selected in plotTabs
      heatmap_plot_selected <- reactive({
        req(input$plotTabs)
        plot_heatmap_calendar(input$plotTabs, df_qry)
      })
      
      output$heatmap_plot <- renderPlot(heatmap_plot_selected())
    })
    
    # Creating reactive variables-----
    selected_state <- reactive({
      input[[paste0(input$plotTabs, "_interactive_plot_selected")]]
    })
    
    # Delete button functionality----
    observeEvent(input$delete, {
      if (is.null(selected_state())) {
        shinyjs::alert("Please select a point to delete.")
      } else {
        shinyjs::enable("submitchanges")
        
        # Extract all the reasons for deletion for the data points
        delete_reasons <- data_flags %>%
          filter(cat == "initial_flag")
        # Turn it into a list for selection
        delete_reasons <- delete_reasons$information
        
        # Extract gapfilling methods
        gapfill_options <- data_flags %>%
          filter(cat != "initial_flag")
        gapfill_options <- gapfill_options$information
        
        # Gapfilling - NOTE, does not yet change depending on method selection
        y <- df_qry[, input$plotTabs]
        m <- gam(y ~ s(datect_num, bs = "cr", k = input$intslider),
                 data = df_qry, na.action = na.exclude)
        df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
        
        # Pop up modal that will ask the user why a point is being deleted.
        showModal(modalDialog(
          h2("Please supply additional information:"),
          h4("What is the reason for deleting the selected point(s)?"),
          selectInput("var_reason", label = h5("Reason for point removal."),
                      choices = delete_reasons),
          h4("What is the gapfilling method you would like to use?"),
          selectInput("select_gapfill", label = h5("Gap-Filling Method"),
                      choices = gapfill_options),
          sliderInput("intslider",
                      label = "Smoothness (number of knots in cr spline):",
                      min = 1, max = 32, value = 10, step = 1),
          easyClose = TRUE,
          footer = tagList(
            actionButton("var_reason1", "Confirm deletion reason and gapfill method.")
          )
        ))
      }
    })
    
    # Confirmation message for deleting a point, and all under-the-hood changes start here
    observeEvent(input$var_reason1, {
      removeModal()
      sendSweetAlert(
        session,
        title = "Success!",
        type = "success",
        btn_labels = "Ok",
        closeOnClickOutside = TRUE,
        width = NULL
      )
      df_list <- list()
      for (i in selected_state()) {
        # Here I am creating a df to keep track of the changes made to the data.
        changed_df <- data.frame()
        changed_df <- colnames(c(
          "variable", "checked", "gapfill_code",
          "gapfill_initials", "gapfill_info", "old_value", "flag_info",
          "flag_code", "flag_initials", "new_value", "user"
        ))
        changed_df$variable <- input$plotTabs
        changed_df$point_id <- i
        changed_df$old_value <- df_qry[df_qry$checked %in% i, input$plotTabs]
        changed_df$flag_info <- input$var_reason
        add_code <- data_flags %>%
          dplyr::filter(information == input$var_reason)
        changed_df$flag_code <- add_code$code
        changed_df$flag_initials <- add_code$initials
        
        # Gapfill method information to be added
        add_fill <- data_flags %>%
          dplyr::filter(information == add_code$information)
        changed_df$gapfill_info <- input$select_gapfill
        changed_df$gapfill_code <- add_fill$code
        changed_df$gapfill_initials <- add_fill$initials
        
        changed_df$user <- Sys.info()["user"]
        # Change the old value to the new value depending on the predicted value
        df_qry[df_qry$checked %in% i, input$plotTabs] <<- NA
        df_qry[is.na(df_qry[, input$plotTabs]), input$plotTabs] <<-
          df_qry$pred[is.na(df_qry[, input$plotTabs])]
        # Now adding the new value to the changed df, using the exact same command as above for the old value
        changed_df$new_value <- df_qry[df_qry$checked %in% i, input$plotTabs]
        changed_df <- bind_rows(changed_df) # %>% as.data.frame()
        df_list[[i]] <- changed_df
      }
      changed_df <- bind_rows(df_list)
      
      # If statement that creates the dataframe in memory if it does not already exist
      if (length(change_summary) == 0) {
        change_summary <<- changed_df
      } else {
        # or appends the dataframe if it does already exist
        change_summary <<- rbind(change_summary, changed_df)
      }
      
      change_summary <<- change_summary[c(
        "variable", "point_id", "old_value", "new_value", "flag_code",
        "flag_initials", "flag_info",
        "gapfill_code", "gapfill_initials", "gapfill_info",
        "user"
      )]
      
      # Re-plotting plot after deletion is confirmed to illustrate changes
      shinyjs::show("plotted_data")
      enable("reset")
      enable("delete")
      enable("nochange")
      
      # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
      plot_selected <- reactive({
        req(input$plotTabs)
        plotting_function(input$plotTabs)
      })
      # Re-render
      output$interactive_plot <- renderggiraph(plot_selected())
      
      # Here I am making a table that shows the changes that have been made
      display_table <<- as.data.table(change_summary)
      
      output$summarytable <- renderDataTable({
        datatable(display_table,
                  # Not that datatable is originally written in javascript
                  # Hence why there are some unusually formatted options here
                  # Like class = 'compact' and a 'text-align' = 'center'
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 25, 50)),
                  rownames = FALSE, class = "compact"
        ) %>%
          formatRound(columns = c(3:4), digits = 2) %>%
          formatStyle(columns = c(1:11), "text-align" = "center")
      })
    })
    
    # Reset button functionality----
    observeEvent(input$reset, {
      showModal(modalDialog(
        title = "Are you sure you want to restart the app? All progress will be lost",
        footer = tagList(
          actionButton("confirm_reset", "I want to restart the app."),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
      
      observeEvent(input$confirm_reset, {
        session$reload()    
      })
    })
    
    # Finished checking, close tab functionality----
    observeEvent(input$nochange, {
      removeTab("plotTabs", input$plotTabs)
      # Insert validation flag for date range here
    })
    
    # Writing tables to a database----
    observeEvent(input$submitchanges, {
      if (dbExistsTable(con, "MET_30MIN_VALIDATED_TEST")) {
        dbWriteTable(con,
                     "MET_30MIN_VALIDATED_TEST",
                     df_qry,
                     append = TRUE
        )
      } else {
        dbWriteTable(con,
                     "MET_30MIN_VALIDATED_TEST",
                     df_qry
        )
      }
      
      if (dbExistsTable(con, "MET_30MIN_VALIDATION_LOG_TEST")) {
        dbWriteTable(con,
                     "MET_30MIN_VALIDATION_LOG_TEST",
                     change_summary,
                     append = TRUE
        )
      } else {
        dbWriteTable(con,
                     "MET_30MIN_VALIDATION_LOG_TEST",
                     change_summary
        )
      }
    })
  }
  
  shinyApp(ui, server, ...)
  
}