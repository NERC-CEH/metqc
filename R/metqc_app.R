# here::i_am("R/metqc_app.R")
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
library(lubridate)
library(ggExtra)
library(openair)
library(powerjoin)
library(pins)
library(glue)
library(shinycssloaders)
library(shinyalert)
# source(here("R", "imputation.R"))
# source(here("R", "plotting.R"))
# source(here("R", "metqc_app.R"))

# # to run
# rm(list=ls(all=TRUE))
# devtools::load_all()
# metqcApp()
Sys.setenv(DBNAME = "budbase.nerc-bush.ac.uk/BUA")
Sys.setenv(DBUID = "BU_FIELD_SITES")
Sys.setenv(DBPWD = "0ig2mtYUL9")
Sys.getenv("DBUID")

metqcApp <- function(...) {
  # Reading in the gap-filling methods and codes----
  v_names <- readRDS(file = here("data", "v_mainmet_name.rds"))

  gf_choices <- setNames(df_method$method, df_method$method_longname)
  
  # Define UI for the app
  ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Met Data Validation",
                    tags$li(class = "dropdown", actionLink("change_user", textOutput('user_name_text'), style="font-weight: bold;color:white;"))),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Information", tabName = "information")
      )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "dashboard",
          fluidRow(
            box(
              title = "Data Selection",
              status = "success", solidHeader = TRUE,
              helpText("Select your required processing start and end times below."),
              column(
                width = 6,
                uiOutput("start_date")
              ),
              column(
                width = 3,
                numericInput("shour",
                  value = 00, label = "Hour",
                  min = 0, max = 23, step = 1
                )
              ),
              column(
                width = 3,
                numericInput("smin",
                  value = 00, label = "Minute",
                  min = 0, max = 59, step = 1
                )
              ),
              column(
                width = 6,
                uiOutput("end_date"),
                tags$style(HTML(".datepicker {z-index:99999 !important;}"))
              ),
              column(
                width = 3,
                numericInput("ehour",
                  value = 00, label = "Hour",
                  min = 0, max = 23, step = 1
                )
              ),
              column(
                width = 3,
                numericInput("emin",
                  value = 00, label = "Minute",
                  min = 0, max = 59, step = 1
                )
              ),
              # uiOutput("select_variables"),
              actionButton("retrieve_data", "Retrieve from database")
            ),
            box(
              title = "Validation Calendar",
              status = "success", solidHeader = TRUE,
              plotOutput("heatmap_plot")
            ),
          ),
          hidden(
            fluidRow(
              id = "extracted_data",
              box(
                title = "Extracted Data",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                shinycssloaders::withSpinner(uiOutput("mytabs")), 
                selectInput("select_imputation",
                  label = h5("Gap-Filling Method"),
                  choices = gf_choices
                ),
                actionButton("impute",
                  label = "Impute selection"
                ),
                actionButton("finished_check",
                  label = "Finished checking variable for date range."
                ),
                checkboxGroupInput("qc_tokeep", "Do not alter data estimated by",
                  choiceNames = df_method$method_longname,
                  choiceValues = df_method$qc),
                uiOutput("impute_extra_info"),
                actionButton("reset",
                  label = "Restart app"
                ),
                actionButton("submitchanges", "Submit changes to file"),
                actionButton("submitchanges_cloud", "Submit changes to cloud")
              ),
            )
          ),
        ),
        tabItem(
          tabName = "information",
          h2("Information placeholder"),
          p("This app provides an interface to the field sites database and allows a user to plot data, remove dubious data and fill gaps with predictions.")
        )
      )
    )
  )

  server <- function(input, output, session) {
    
    # list of possible users - hard-coded for now  
    v_usernames <- c("leav", "dunhar", "karung", "plevy", "matj", "MauGre", "mcoy", "neimul",
      "sarle", "wilfinc")
    
    # a modal dialog where the user can enter their user name.
    username_modal <- modalDialog(
      title = "Enter user name",
      selectInput('input_username','Select from:', v_usernames),
      easyClose = F,
      footer = tagList(
        actionButton("ok", "OK")
      )
    )

    # Show the model on start up ...
    showModal(username_modal)
    
    # allow user to trigger modal with button press
    observeEvent(input$change_user, {
      showModal(username_modal)
    })

    observeEvent(input$ok, {
      removeModal()
      # save the username
      username <<- input$input_username
      output$user_name_text <- renderText({paste0('Current user:  ', input$input_username)})
    })

    # Read in ERA5 data----
    # read from JASMIN
    # url_era5 <- paste0("https://gws-access.jasmin.ac.uk/public/dare_uk/plevy/UK-AMo/df_era5.rds")
    # df_era5 <- readRDS(url(url_era5, "rb"))
    # OR read from local file
    # fname <- here("data", "df_era5.rds")
    # df_era5 <- readRDS(fname)
    # OR read from pin on Connect server
    board <- board_rsconnect()
    df_era5 <- pin_read(board, "plevy/era5_data")
    names(df_era5); dim(df_era5)

    # Reading in this year's Level 1 data----
    # read from JASMIN
    # this_year <- as.POSIXlt(Sys.Date())$year + 1900
    # url_mainmet <- paste0("https://gws-access.jasmin.ac.uk/public/dare_uk/plevy/UK-AMo/UK-AMo_mainmet_", this_year, "_agf.rds")
    #l_lev1 <- readRDS(url(url_mainmet, "rb"))
    # OR read from local file
    # fname_mainmet <- here("data", "UK-AMo_mainmet_2022_agf.rds")
    # l_lev1 <- readRDS(fname_mainmet)
    # OR read from pin on Connect server
    l_lev1 <- pin_read(board, "plevy/level1_data")

    # Read in the previously validated data----
    # read from local file
    # fname <- here("data", "UK-AMo_mainmet_val.rds")
    # l_lev2 <- readRDS(fname)
    # OR read from pin on Connect server
    l_lev2 <- pin_read(board, "plevy/level2_data")

    # Here we join the existing Level 2 data with new Level 1 data
    # Where records already exist in the Level 2 data, these are preserved
    # and only new Level 1 data is added to the resulting data frame.
    l_lev2$df <- power_full_join(l_lev2$df, l_lev1$df,
      by = "DATECT", conflict = coalesce_xy
    )
    l_lev2$df_qc <- power_full_join(l_lev2$df_qc, l_lev1$df_qc,
      by = "DATECT", conflict = coalesce_xy
    )

    date_of_first_new_record <- min(l_lev1$df$DATECT, na.rm = TRUE)
    date_of_last_new_record  <- max(l_lev1$df$DATECT, na.rm = TRUE)

    # v_names <<- dbListFields(con, table_name)
    v_names_for_box <- v_names[!v_names %in%
      c("DATECT", "TIMESTAMP", "datect_num", "checked", "pred")]
    
    v_names_checklist <- reactiveValues()

    # Format the dates for R----
    df_proc <- data.frame(
      start_date = "1995/01/01 00:00",
      end_date = "2022/12/31 00:00"
    )
    df_proc$start_date <- as.POSIXct(
      df_proc$start_date,
      format = "%Y/%m/%d %H:%M", tz = "UTC"
    )
    df_proc$end_date <- as.POSIXct(
      df_proc$end_date,
      format = "%Y/%m/%d %H:%M", tz = "UTC"
    )

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
        # value = as.Date(strptime("01/01/2022", "%d/%m/%Y"), tz = "UTC"),
        min = first_start_date(),
        max = last_end_date(),
        label = "Start date"
      )
    })

    # Create a date input for the user to select end date
    output$end_date <- renderUI({
      dateInput("edate",
        value = as.Date(date_of_last_new_record, tz = "UTC"),
        # value = as.Date(strptime("01/03/2022", "%d/%m/%Y"), tz = "UTC"),
        min = first_start_date(),
        max = last_end_date(),
        label = "End date"
      )
      })

    # Create a dataframe with the start and end dates,
    df_daterange <- eventReactive(input$retrieve_data, {
      start_date_ch <- paste(sprintf("%02d", day(input$sdate)), "/",
        sprintf("%02d", month(input$sdate)),
        "/", year(input$sdate),
        " ",
        sprintf("%02d", input$shour),
        ":",
        sprintf("%02d", input$smin),
        sep = ""
      )
      start_date <- as.POSIXct(
        strptime(start_date_ch, "%d/%m/%Y %H:%M"),
        tz = "UTC"
      )
      end_date_ch <- paste(sprintf("%02d", day(input$edate)), "/",
        sprintf("%02d", month(input$edate)),
        "/", year(input$edate), " ",
        sprintf("%02d", input$ehour), ":",
        sprintf("%02d", input$emin),
        sep = ""
      )
      end_date <- as.POSIXct(
        strptime(end_date_ch, "%d/%m/%Y %H:%M"),
        tz = "UTC"
      )
      list(
        start_date = start_date,
        end_date = end_date,
        start_date_ch = start_date_ch,
        end_date_ch = end_date_ch
      )
    })

    # Rendering the box that we need to allow the user to select
    # all of the variables they wish to check----
    # output$select_variables <- renderUI({
      # checkboxGroupButtons("variable_check",
        # label = h5("Variables to be checked"),
        # choices = as.list(v_names_for_box), selected = v_names_for_box
      # )
    # })

    # The optional rendering of UI elements depending on which
    # imputation method has been selected
    output$impute_extra_info <- renderUI({
      req(input$select_imputation)
      if (input$select_imputation == "time") {
        sliderInput("intslider",
          label = "Smoothness (number of knots in cr spline):",
          min = 1, max = 50, value = 10, step = 1
        )
      } else if (input$select_imputation == "regn") {
        selectInput("select_covariate",
          label = h5("Covariate"),
          choices = v_names_for_box
        )
      }
    })

    # Data retrieval functionality-----
    observeEvent(input$retrieve_data, {

      for(i in 1:length(v_names)){
        v_names_checklist[[v_names[i]]] <- FALSE
      }
      
      # enabling previously disabled buttons
      shinyjs::show("extracted_data")
      
      l_qry <<- list()
      
      # # database version
      # # make a query for date range specified by the user.
      # qry_variables <- paste(v_names_for_box, collapse = ", ")
      # table_name <- "MAINMET_RAW"
      # l_qry$df <<- paste0(
        # "SELECT DATECT, ", qry_variables, " FROM ", table_name,
        # " WHERE DATECT > TO_DATE('", df_daterange()$start_date_ch,
        # "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
        # df_daterange()$end_date_ch, "', 'yyyy/mm/dd hh24:mi')"
      # )
      # table_name <- "MAINMET_RAW_QC"
      # l_qry$df_qc <<- paste0(
        # "SELECT DATECT, ", qry_variables, " FROM ", table_name,
        # " WHERE DATECT > TO_DATE('", df_daterange()$start_date_ch,
        # "', 'yyyy/mm/dd hh24:mi') AND DATECT < TO_DATE('",
        # df_daterange()$end_date_ch, "', 'yyyy/mm/dd hh24:mi')"
      # )

      # file / dataframe version      
      l_qry$df <<- subset(l_lev2$df, DATECT >= df_daterange()$start_date &
        DATECT <= df_daterange()$end_date)
      l_qry$df_qc <<- subset(l_lev2$df_qc, DATECT >= df_daterange()$start_date &
        DATECT <= df_daterange()$end_date)
      # make a corresponding subset of the ERA5 data
      df_era5_qry <<- subset(df_era5, DATECT >= df_daterange()$start_date &
        DATECT <= df_daterange()$end_date)

      l_qry$df$checked <<- as.factor(rownames(l_qry$df))
      l_qry$df$datect_num <<- as.numeric(l_qry$df$DATECT)

      # Add a tab to the plotting panel for each variable that has been selected by the user.
      output$mytabs <- renderUI({
        my_tabs <- lapply(paste(v_names_for_box), function(i) {
          tabPanel(i,
            value = i,
            tags$style(HTML(paste0('.tabbable > .nav > li > a[data-value=', i, '] {border: transparent;background-color:', ifelse(v_names_checklist[[i]] == TRUE, '#bcbcbc', 'transparent'), ';}'))),
            girafeOutput(paste0(i, "_interactive_plot")),
          )
        })
        do.call(tabsetPanel, c(my_tabs, id = "plotTabs"))
      })
      
      observe(
        lapply(paste(v_names_for_box), function(i) {
          output[[paste0(i, "_interactive_plot")]] <-
            renderggiraph(plotting_function(i))
        })
      )

      # Creating a calendar heatmap plot that will be plotted depending on the tab selected in plotTabs
      heatmap_plot_selected <- reactive({
        req(input$plotTabs)
        plot_heatmap_calendar(l_qry$df_qc)
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

        # # Extract all the reasons for imputation of the data points
        # impute_reasons <- data_flags %>%
        # filter(cat == "initial_flag")
        # # Turn it into a list for selection
        # impute_reasons <- impute_reasons$information

        # # Extract gapfilling methods
        # gapfill_options <- data_flags %>%
        # filter(cat != "initial_flag")
        # gapfill_options <- gapfill_options$information

        #l_imp <- l_qry

        l_qry <<- impute(
          y = input$plotTabs,
          l_met = l_qry,
          method = input$select_imputation,
          qc_tokeep = as.numeric(input$qc_tokeep),
          x = input$select_covariate,
          df_era5 = df_era5_qry,
          k = input$intslider,
          plot_graph = FALSE,
          selection = l_qry$df$checked %in% selected_state()
        )

        #l_qry <<- l_imp

        # Re-plotting plot after imputation is confirmed to illustrate changes
        shinyjs::show("plotted_data")
        enable("reset")
        enable("impute")
        enable("finished_check")

        # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
        plot_selected <- reactive({
          req(input$plotTabs)
          plotting_function(input$plotTabs)
        })
        # Re-render
        output[[paste0(input$plotTabs, "_interactive_plot")]] <- renderggiraph(plot_selected())
      }
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
    observeEvent(input$finished_check, {
      #removeTab("plotTabs", input$plotTabs)

      # Insert validation flag for date range here
      v_names_checklist[[input$plotTabs]] <- TRUE

      # # Check if all values are true, only then enable the submit button
      # if (all(v_names_checklist$finished_checking) == TRUE) {
        # shinyjs::enable("submitchanges")
      # }
    })

    # # Writing tables to a database----
    ##* WIP - temporarily removed - writes to file instead. Requires SQL UPDATE code here
    # observeEvent(input$submitchanges, {
    # if (dbExistsTable(con, "MAINMET")) {
    # dbWriteTable(con,
    # "MAINMET",
    # l_qry$df,
    # append = TRUE
    # )
    # } else {
    # dbWriteTable(con,
    # "MAINMET",
    # l_qry$df
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
      
      # disable button and show working
      runjs('document.getElementById("submitchanges").textContent="Submitting changes...";')
      shinyjs::disable("submitchanges")
      
      #l_qry$df_qc$validator <- as.character(Sys.info()["user"])
      l_qry$df_qc$validator <- username
      # create a backup copy without the changes
      fname <- here("data", "UK-AMo_mainmet_val_backup.rds")
      saveRDS(l_lev2, file = fname)

      # overwrite existing data with changes in query
      l_lev2$df    <<- power_full_join(l_lev2$df,    l_qry$df, by = "DATECT", conflict = coalesce_yx)
      l_lev2$df_qc <<- power_full_join(l_lev2$df_qc, l_qry$df_qc,  by = "DATECT", conflict = coalesce_yx)
      # save to local file
      fname <- here("data", "UK-AMo_mainmet_val.rds")
      saveRDS(l_lev2, file = fname)
      
      runjs('document.getElementById("submitchanges").textContent="Submit changes to file";')
      shinyjs::enable("submitchanges")
    })
    
    # Writing validated data to pin----
    observeEvent(input$submitchanges_cloud, {
    
      # Update button text
      runjs('document.getElementById("submitchanges_cloud").textContent="Submitting changes...";')

      # disable button while working
      shinyjs::disable("submitchanges_cloud")

      # write to pin on Connect server
      pin_write(board,
        l_lev2,
        name = "level2_data", type = "rds")
        
      time_diff <- difftime(as.POSIXct(Sys.time()), as.POSIXct(pins::pin_meta(board, 'plevy/level2_data')$created), units = 'mins')

      if(time_diff < 2){
          shinyalert(
          title = "Data successfully saved to cloud",
          #text = "This is a modal",
          size = "m", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 10000,
          imageUrl = "",
          animation = TRUE
        )

        } else{
          shinyalert(
            title = "Error saving data",
            text = "Data took over 2 minuets to write. Data may not have saved correctly to the cloud.",
            size = "m", 
            closeOnEsc = FALSE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "error",
            showConfirmButton = FALSE,
            showCancelButton = TRUE,
            cancelButtonText = "Cancel",
            timer = 10000,
            imageUrl = "",
            animation = TRUE
          )

        }
      
      # remove button activation and reactivate button
      runjs('document.getElementById("submitchanges_cloud").textContent="Submit";')
      shinyjs::enable("submitchanges_cloud")
      
      })
    
    }

  shinyApp(ui, server, ...)
}
