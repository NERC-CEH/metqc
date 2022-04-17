here::i_am("R/metdb_app.R")
library(here)
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
library(openair)
library(powerjoin)
source(here("R", "imputation.R"))
source(here("R", "plotting.R"))
#source(here("R", "metdb_app.R"))

# # to run
# devtools::load_all()
# Sys.setenv(DBNAME = "budbase.nerc-bush.ac.uk/BUA")
# Sys.setenv(DBUID  = "BU_FIELD_SITES")
# Sys.setenv(DBPWD  = "0ig2mtYUL9")
# Sys.getenv("DBUID")
# metdbApp()
#rm(list = c("metdbApp"))
#rm(list=ls(all=TRUE))

metdbApp <- function(...) {
  # Reading in the gap-filling methods and codes----
  df_method <- readRDS(file = here("data", "df_method.rds"))
  v_names <- readRDS(file = here("data", "v_mainmet_name.rds"))
  
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
                                    box(title = "Data Selection",
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
                                                 selectInput("select_gapfill", label = h5("Gap-Filling Method"),
                                                   choices = list(gf_methods = df_method$method)),
                                                 selectInput("select_covariate", label = h5("Covariate"),
                                                   choices = list("TA",
                                                                  "TS",
                                                                  "SW_IN", 
                                                                  "PPFD_IN", 
                                                                  "WTD", 
                                                                  "SWC")),
                                                 shinyjs::disabled(actionButton("reset",
                                                                                label = "Restart app")),
                                                 shinyjs::disabled(actionButton("impute",
                                                                                label = "Impute selection")),
                                                 shinyjs::disabled(actionButton("replot",
                                                                                label = "Replot graph")),
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
    this_year <- as.POSIXlt(Sys.Date())$year + 1900
  
    # # Making database connection----
    # drv <- dbDriver("Oracle")
    # con <- dbConnect(drv,
                     # dbname = Sys.getenv("DBNAME"),
                     # username = Sys.getenv("DBUID"),
                     # password = Sys.getenv("DBPWD")
    # )
    # table_name <- "MAINMET_RAW"
    # v_names <- dbListFields(con, table_name)
 
    # Read in ERA5 data----
    fname <- here("data", "df_era5.rds")
    df_era5 <- readRDS(fname)    
    
    # Read in the previously validated data----
    fname <- here("data", "UK-AMo_mainmet_val.rds")
    l_val <- readRDS(fname)
    # dim(l_val$df); dim(l_val$df_qc)
    # names(l_val$df); names(l_val$df_qc)
    # str(l_val$df); str(l_val$df_qc)
    # summary(l_val$df)
    # summary(l_val$df_qc)

    # Reading in the logger data----
    # url_mainmet <- paste0("https://gws-access.jasmin.ac.uk/public/dare_uk/plevy/UK-AMo/UK-AMo_mainmet_", this_year, "_agf.rds")
    # l_gf <- readRDS(url(url_mainmet, "rb"))
    ##* WIP temporarily read from local file; the url above should work but not tested thoroughly
    fname_mainmet <- here("data", "UK-AMo_mainmet_2022_agf.rds")
    l_gf <- readRDS(fname_mainmet)

    # add in some variable not yet recorded in ICOS files; this is a temporary fix
    l_gf$df$RN  <- NA; l_gf$df_qc$RN  <- NA
    l_gf$df$PWS <- NA; l_gf$df_qc$PWS <- NA
    l_gf$df$VIS <- NA; l_gf$df_qc$VIS <- NA  

    l_gf$df    <- l_gf$df   [, v_names]
    l_gf$df_qc <- l_gf$df_qc[, v_names]

    l_val$df    <- power_full_join(l_val$df,    l_gf$df,    by = "DATECT", conflict = coalesce_xy)
    l_val$df_qc <- power_full_join(l_val$df_qc, l_gf$df_qc, by = "DATECT", conflict = coalesce_xy)
# dim(l_val$df)
# dim(l_gf$df)
# str(dft)
    
    date_of_first_new_record <- min(l_gf$df$DATECT, na.rm = TRUE)
    date_of_last_new_record  <- max(l_gf$df$DATECT, na.rm = TRUE)

    #v_names <<- dbListFields(con, table_name)
    v_names_for_box <- v_names[!v_names %in%
                                   c("DATECT", "TIMESTAMP", "datect_num", "checked", "pred")]
    
  
    # Format the dates for R----
    df_proc <- data.frame(
      start_date = "1995/01/01 00:00",
      end_date = "2022/12/31 00:00"
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
                value = as.Date(date_of_first_new_record, tz = "UTC"),
                #value = as.Date(strptime("01/01/2022", "%d/%m/%Y"), tz = "UTC"),
                min = first_start_date(),
                max = last_end_date(), label = "Start date"
      )
    })
    
    # Create a date input for the user to select end date
    output$end_date <- renderUI({
      dateInput("edate",
                value = as.Date(date_of_last_new_record, tz = "UTC"),
                #value = as.Date(strptime("01/03/2022", "%d/%m/%Y"), tz = "UTC"),
                min = first_start_date(), max = last_end_date(), label = "End date"
      )
    })
    
    # Create a dataframe with all the information about the start and end time,
    # and cr spline (do we need this?)----
    job_df <- eventReactive(input$retrieve_data, {
      start_date_ch <- paste(sprintf("%02d", day(input$sdate)), "/",
                          sprintf("%02d", month(input$sdate)), "/", year(input$sdate), " ",
                          sprintf("%02d", input$shour), ":", sprintf("%02d", input$smin), sep = "")
      start_date <- as.POSIXct(strptime(start_date_ch, "%d/%m/%Y %H:%M"), tz = "UTC")
      end_date_ch <- paste(sprintf("%02d", day(input$edate)), "/",
                        sprintf("%02d", month(input$edate)), "/", year(input$edate), " ",
                        sprintf("%02d", input$ehour), ":",
                        sprintf("%02d", input$emin), sep = "")
      end_date <- as.POSIXct(strptime(end_date_ch, "%d/%m/%Y %H:%M"), tz = "UTC")
      list(start_date = start_date,
             end_date = end_date,
             start_date_ch = start_date_ch,
             end_date_ch = end_date_ch)
    })
    
    # Rendering the box that we need to allow the user to select
    # all of the variables they wish to check----
    output$select_variables <- renderUI({
      checkboxGroupButtons("variable_check",
                           label = h5("Variables to be checked"),
                           choices = as.list(v_names_for_box), selected = v_names_for_box
      )
    })
    
    # Creating empty dataframes----
    change_summary <- data.frame()
    
    # Data retrieval functionality-----
    observeEvent(input$retrieve_data, {
      # enabling previously disabled buttons
      shinyjs::show("extracted_data")
      enable("reset")
      enable("impute")
      enable("replot")
      enable("nochange")
      
      # make a query for every variable that has been checked by the user.
      if (!is.null(input$variable_check)) {
        #qry_variables <- paste(input$variable_check, collapse = ", ")
        qry_variables <- paste(v_names, collapse = ", ")
        table_name <- "MAINMET_RAW"
        qry <- paste0(
          "SELECT DATECT, ", qry_variables, " FROM ", table_name,
          " WHERE DATECT > TO_DATE('", job_df()$start_date_ch,
          "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
          job_df()$end_date_ch, "', 'yyyy/mm/dd hh24:mi')"
        )
        # qry <- paste0(
          # "SELECT DATECT, ", qry_variables, " FROM ", table_name,
          # " WHERE DATECT > TO_DATE('", format(date_of_first_new_record - 11*86400, "%Y/%m/%d %H:%M"),
          # "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
          # format(date_of_last_new_record, "%Y/%m/%d %H:%M"), "', 'yyyy/mm/dd hh24:mi')"
        # )

        #df_qry <<- dbGetQuery(con, qry)
        df_qry <<- subset(l_val$df,   DATECT >= job_df()$start_date &
                                      DATECT <= job_df()$end_date)
        ## should rename df_qc_qry for clarity?
        df_qc <<- subset(l_val$df_qc, DATECT >= job_df()$start_date &
                                      DATECT <= job_df()$end_date)
        # make a corresponding subset of the ERA5 data
        df_era5_qry <<- subset(df_era5, DATECT >= job_df()$start_date &
                                        DATECT <= job_df()$end_date)
        # browser()
        df_qry$checked <<- as.factor(rownames(df_qry))   ## ?!df_qry$
        df_qry$datect_num <<- as.numeric(df_qry$DATECT)  ## !df_qry$
        #v_hour <<- as.POSIXlt(df_qry$DATECT)$hour
        
        table_name <- "MAINMET_RAW_QC"
        qry <- paste0(
          "SELECT DATECT, ", qry_variables, " FROM ", table_name,
          " WHERE DATECT > TO_DATE('", job_df()$start_date_ch,
          "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
          job_df()$end_date_ch, "', 'yyyy/mm/dd hh24:mi')"
        )
        # df_qc <<- dbGetQuery(con, qry)

      # dim(df_qry)
      # dim(df_qc)
      # summary(df_qry)
      # summary(df_qc)

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
    
    # Impute button functionality----
    observeEvent(input$impute, {
      if (is.null(selected_state())) {
        shinyjs::alert("Please select a point to impute.")
      } else {
        shinyjs::enable("submitchanges")
        
        # # Extract all the reasons for imputation of the data points
        # impute_reasons <- data_flags %>%
          # filter(cat == "initial_flag")
        # # Turn it into a list for selection
        # impute_reasons <- impute_reasons$information
        
        # # Extract gapfilling methods
        # gapfill_options <- data_flags %>%
          # filter(cat != "initial_flag")
        # gapfill_options <- gapfill_options$information
        
        # Gapfilling - NOTE, does not yet change depending on method selection
        #df_qc[df_qry$checked %in% selected_state(), input$plotTabs] <<- 1 # not needed?
        
        l_gf <- list(df = df_qry, df_qc = df_qc)

        l_gf <- impute(y = input$plotTabs, l_gf, method = input$select_gapfill, 
          x = input$select_covariate, df_era5 = df_era5_qry, k = input$intslider,
          selection = df_qry$checked %in% selected_state())
        
        df_qry <<- l_gf$df
        df_qc  <<- l_gf$df_qc
        
        # Re-plotting plot after imputation is confirmed to illustrate changes
        shinyjs::show("plotted_data")
        enable("reset")
        enable("impute")
        enable("nochange")
        
        # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
        plot_selected <- reactive({
          req(input$plotTabs)
          plotting_function(input$plotTabs)
        })
        # Re-render
        output$interactive_plot <- renderggiraph(plot_selected())
      }
    })

    # Replot button functionality----
    ##* WIP this does not work; graphs should update after imputation, but do not - PL 11/04/2022
    observeEvent(input$replot, {
      # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
      plot_selected <- reactive({
        req(input$plotTabs)
        plotting_function(input$plotTabs)
      })
      # Re-render
      output$interactive_plot <- renderggiraph(plot_selected())
    })
  
    # Confirmation message for deleting a point, and all under-the-hood changes start here
    ##* WIP this button and actions can be removed - QC df is enough, and reasons not needed - PL 11/04/2022
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
        # Here I am creating a dataframe to keep track of the changes made to the data.
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
      
      # Re-plotting plot after imputation is confirmed to illustrate changes
      shinyjs::show("plotted_data")
      enable("reset")
      enable("impute")
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
      shinyjs::enable("submitchanges")
      # Insert validation flag for date range here
    })
    
    # # Writing tables to a database----
    ##* WIP - temporarily removed - writes to file instead. Requires SQL UPDATE code here
    # observeEvent(input$submitchanges, {
      # if (dbExistsTable(con, "MAINMET")) {
        # dbWriteTable(con,
                     # "MAINMET",
                     # df_qry,
                     # append = TRUE
        # )
      # } else {
        # dbWriteTable(con,
                     # "MAINMET",
                     # df_qry
        # )
      # }
      
      # if (dbExistsTable(con, "MAINMET_QC")) {
        # dbWriteTable(con,
                     # "MAINMET_QC",
                     # change_summary,
                     # append = TRUE
        # )
      # } else {
        # dbWriteTable(con,
                     # "MAINMET_QC",
                     # change_summary
        # )
      # }
      # dbCommit(con) ##* WIP is this the correct place for this?
    # })    

    # Writing validated data to file----
    observeEvent(input$submitchanges, {
      df_qc$validator <- as.character(Sys.info()["user"])
      # create a backup copy without the changes
      fname <- here("data", "UK-AMo_mainmet_val_backup.rds")
      saveRDS(l_val, file = fname)

      # overwrite existing data with changes in query
      l_val$df    <- power_full_join(l_val$df,    df_qry, by = "DATECT", conflict = coalesce_yx)
      l_val$df_qc <- power_full_join(l_val$df_qc, df_qc,  by = "DATECT", conflict = coalesce_yx)
      fname <- here("data", "UK-AMo_mainmet_val.rds")
      saveRDS(l_val, file = fname)      
    })
  }
  
  shinyApp(ui, server, ...)
  
}
