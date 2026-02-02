library(here)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
# library(ROracle)
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
library(stringr)
library(forcats)
library(shinyvalidate)
library(markdown)
library(sodium)
# source(here("R", "imputation.R"))
# source(here("R", "plotting.R"))
# source(here("R", "metqc_app.R"))

# load user base with login
user_base <- readRDS("./ignore/user_base.rds")
###############################

# # to run
# rm(list=ls(all=TRUE))
# devtools::load_all()
# metqcApp()
Sys.setenv(DBNAME = "budbase.nerc-bush.ac.uk/BUA")
Sys.setenv(DBUID = "BU_FIELD_SITES")
Sys.setenv(DBPWD = "0ig2mtYUL9")
Sys.getenv("DBUID")
source(here::here("R", "mod_upload.R"))
source(here::here("R", "mod_colmap.R"))
source(here::here("R", "mod_campbell_logger_import.R"))
dict_list <- readRDS(here("data", "col_vocabulary.rds"))

metqcApp <- function(...) {
  # Reading in the gap-filling methods and codes----
  v_names <- readRDS(file = here("data", "v_mainmet_name.rds"))

  #gf_choices <- setNames(df_method$method, df_method$method_longname)

  # Define UI for the app
  ui <- dashboardPage(
    skin = "green",
    # ---------------------- HEADER ----------------------
    dashboardHeader(
      title = "Met Data Validation",
      tags$li(
        class = "dropdown",
        actionLink(
          "change_user",
          textOutput('user_name_text'),
          style = "font-weight: bold;color:white;"
        )
      )
    ),
    # ---------------------- SIDEBAR ----------------------
    dashboardSidebar(
      sidebarMenu(
        id = 'tabs',
        menuItem("Dashboard", tabName = "dashboard", icon = icon('database')),
        menuItem("Flags", tabName = "flags", icon = icon('flag')),
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Download", tabName = "download", icon = icon('download')),
        menuItem(
          "Information",
          tabName = "information",
          icon = icon('info'),
          menuSubItem('Gap-fill methods', tabName = 'gapfill_guide'),
          menuSubItem('App guide', tabName = 'app_guide'),
          menuSubItem('Data process guide', tabName = 'data_guide')
        )
      )
    ),
    # ---------------------- BODY ----------------------
    dashboardBody(
      useShinyjs(),
      # 1️⃣ Sodium-based authentication UI
      uiOutput("auth_ui"),
      # 2️⃣ Main content appears *only after full authentication*
      conditionalPanel(
        condition = "output.fully_authenticated == true",
        tabItems(
          # ----------- DASHBOARD TAB -----------
          tabItem(
            tabName = "dashboard",
            fluidRow(
              box(
                title = "Data Selection",
                status = "success",
                solidHeader = TRUE,
                helpText(
                  "Select your required processing start and end times below."
                ),

                column(width = 6, uiOutput("start_date")),
                column(
                  width = 3,
                  numericInput(
                    "shour", value = 0, label = "Hour (24 hour)", min = 0, max = 23, step = 1
                  )
                ),
                column(
                  width = 3,
                  numericInput(
                    "smin", value = 0, label = "Minute", min = 0, max = 59, step = 1
                  )
                ),
                
                column(
                  width = 6,
                  uiOutput("end_date"),
                  tags$style(HTML(".datepicker {z-index:99999 !important;}"))
                ),
                column(
                  width = 3,
                  numericInput(
                    "ehour", value = 0, label = "Hour  (24 hour)", min = 0, max = 23, step = 1
                  )
                ),
                column(
                  width = 3,
                  numericInput(
                    "emin", value = 0, label = "Minute", min = 0, max = 59, step = 1
                  )
                ),
                
                actionButton("retrieve_data", "Retrieve from database"),
                actionButton("compare_vars", "Compare variables")
              ),
              
              hidden(
                div(
                  id = "validation_calendar_outer",
                  box(
                    id = 'validation_calendar',
                    title = "Validation Calendar",
                    status = "success",
                    solidHeader = TRUE,
                    shinycssloaders::withSpinner(plotOutput("heatmap_plot"))
                  )
                )
              )
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
                  actionButton("impute", label = "Impute selection"),
                  actionButton("finished_check", label = "Finished checking variable for date range."),
                  uiOutput("impute_extra_info"),
                  actionButton("reset", label = "Restart app"),
                  actionButton("submitchanges_cloud", label = "Submit changes")
                )
              )
            )
          ),
          
          # ----------- FLAGS TAB -----------
          tabItem(
            tabName = "flags",
            fluidRow(
              box(
                id = 'flag_details_box',
                title = 'Add data flag',
                status = "success",
                solidHeader = TRUE,
                helpText("Select the start and end times for the data flag."),
                width = 6,
                
                fluidRow(
                  column(6, uiOutput('flag_date_start_input')),
                  column(6, uiOutput('flag_date_end_input'))
                ),
                
                fluidRow(
                  column(
                    6,
                    selectInput(
                      'flag_all_day',
                      label = 'Flag the entire day?',
                      choices = c('Yes', 'No'),
                      selected = 'Yes'
                    )
                  )
                ),
                
                fluidRow(
                  conditionalPanel(
                    "input.flag_all_day == 'No'",
                    column(
                      6,
                      numericInput(
                        "flag_start_hour", value = 0, label = "Start hour (24 hour)",
                        min = 0, max = 23, step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "flag_end_hour", value = 0, label = "End hour (24 hour)",
                        min = 0, max = 23, step = 1
                      )
                    )
                  )
                ),
                
                fluidRow(
                  column(
                    6,
                    selectInput('flag_var', 'Variable', choices = v_names[!v_names %in% "DATECT"])
                  ),
                  column(
                    6,
                    textInput('flag_comm', 'Comment', placeholder = 'Why is this data being flagged?')
                  )
                ),
                column(6, actionButton("add_flag", "Add Flag"))
              )
            ),
            shinycssloaders::withSpinner(
              DT::dataTableOutput('flag_table', width = '50%')
            ),
            actionButton('save_flags_btn', 'Save'),
            actionButton('reset_flags_btn', 'Reset')
          ),
          
          # ----------- UPLOAD TAB -----------
          tabItem(
            tabName = "upload",
            fluidPage(
              mod_upload_ui("upload_module"),
              mod_colmap_ui("colmap_module")
            )
          ),
          # ----------- DOWNLOAD TAB -----------
          tabItem(
            tabName = 'download',
            fluidRow(
              box(
                id = 'download_box',
                title = 'Data download',
                status = "success",
                solidHeader = TRUE,
                selectInput(
                  'download_file',
                  'Data to download:',
                  choices = c(
                    'Level 1' = 'lev1',
                    'Level 2' = 'lev2',
                    'CEDA' = 'ceda',
                    'Data flags' = 'flags'
                  )
                ),
                downloadButton('download_data', label = 'Download')
              )
            )
          ),
          # ----------- INFORMATION TABS -----------
          tabItem(tabName = "information"),
          tabItem(
            tabName = "gapfill_guide",
            includeMarkdown("./vignettes/gap_fill_methods.md")
          ),
          tabItem(
            tabName = "app_guide",
            includeMarkdown("./vignettes/app_user_guide.md")
          ),
          tabItem(
            tabName = "data_guide",
            includeHTML('./vignettes/metdb_shiny_version.html')
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    
    # ---- Load your upload + mapping modules (unchanged) ----
    uploaded <- mod_upload_server("upload_module")
    mapped_data <- mod_colmap_server("colmap_module", uploaded, dict_list)
    
    observeEvent(mapped_data(), {
      showNotification("✅ Mapping confirmed! Ready to process data.")
    })
    
    ##########################
    # shinyvalidate statements
    ##########################
    iv <- InputValidator$new()
    iv$add_rule("sdate", sv_required())
    iv$add_rule("edate", sv_required())
    iv$enable()
    
    
    # ---- Authentication states ----
    credentials <- reactiveValues(
      login_ok = FALSE,           # sodium-based login
      username_selected = FALSE,  # secondary username selection
      user = NULL,
      selected_username = NULL
    )
    
    
    # ---- Sodium login UI ----
    output$auth_ui <- renderUI({
      if (!credentials$login_ok) {
        tagList(
          h3("🔐 Please log in"),
          textInput("login_user", "Username"),
          passwordInput("login_pass", "Password"),
          actionButton("login_btn", "Login")
        )
      } else {
        NULL
      }
    })
    
    
    # ---- Sodium-based login check ----
    observeEvent(input$login_btn, {
      # Make sure both fields are filled
      req(input$login_user, input$login_pass)
      
      # Look up the user
      user_row <- user_base[user_base$user == input$login_user, ]
      
      # Check that the username exists
      if (nrow(user_row) == 0) {
        showNotification("❌ Invalid username or password", type = "error")
        return()
      }
      
      # Extract the stored hash (raw vector) from the list column
      stored_hash <- user_row$password_hash[[1]]
      
      # Verify password
      if (sodium::password_verify(stored_hash, input$login_pass)) {
        credentials$login_ok <- TRUE
        credentials$user <- input$login_user
        showNotification(paste("✅ Login successful for", input$login_user))
        
        # Show your existing username modal after successful login
        showModal(username_modal)
      } else {
        showNotification("❌ Invalid username or password", type = "error")
      }
    })
    
    
    # ---- Secondary username modal (your existing code) ----
    v_usernames <- c("boduf", "wgt", "jarcgtas")
    
    username_modal <- modalDialog(
      title = "Enter user name",
      selectInput('input_username', 'Select from:', v_usernames),
      easyClose = FALSE,
      footer = tagList(actionButton("ok", "OK"))
    )
    
    
    # Allow user to change username (after sodium login)
    observeEvent(input$change_user, {
      req(credentials$login_ok)
      showModal(username_modal)
    })
    
    
    # Handle username confirmation
    observeEvent(input$ok, {
      req(input$input_username)
      credentials$username_selected <- TRUE
      credentials$selected_username <- input$input_username
      removeModal()
      showNotification(paste("✅ Active username:", input$input_username))
    })
    
    
    # ---- Control full access ----
    output$fully_authenticated <- reactive({
      credentials$login_ok && credentials$username_selected
    })
    outputOptions(output, "fully_authenticated", suspendWhenHidden = FALSE)
    
    
    # ---- Header display ----
    output$user_name_text <- renderText({
      if (credentials$login_ok && credentials$username_selected) {
        paste(credentials$user, "(", credentials$selected_username, ")")
      } else {
        ""
      }
    })

    ###
    ##Observe event for shinyvalidate dates
    ##
    observeEvent(input$retrieve_data, label = "validator for dates", {
      if (!iv$is_valid()) {
        showModal(modalDialog("Please fill in both dates.", easyClose = TRUE))
        return()
      }
      
      if (input$edate < input$sdate) {
        showModal(modalDialog(
          title = "Invalid Dates",
          "⚠️ End date must not be earlier than start date.",
          easyClose = TRUE
        ))
      }
    })
    
    # output$date_warning <- renderText({
    #   browser()
    #   req(input$start_date, input$end_date, input$retrieve_data)
    #   if (input$end_date < input$start_date) {
    #     "Warning: End date must not be earlier than start date."
    #   } else {
    #     "" # No warning
    #   }
    # })
    
    observeEvent(input$ok, {
      removeModal()
      # save the username
      username <<- input$input_username
      output$user_name_text <- renderText({
        paste0('Current user:  ', input$input_username)
      })
    })
    
    disable('compare_vars')
    
    board <- board_connect()
    
    # Read in data flags
    flagged_data <- pin_read(board, "wilfinc/flagged_data")
    df_flagged_data <- reactiveVal(flagged_data)
    
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
    
    # Read in the previously validated data from pin on Connect server
    l_lev2 <- pin_read(board, "plevy/level2_data")
    
    # Here we join the existing Level 2 data with new Level 1 data.
    # Where records already exist in the Level 2 data, these are preserved
    # and only new Level 1 data is added to the resulting data frame.
    
    l_lev2$dt <- power_full_join(
      l_lev2$dt,
      l_lev1$dt,
      by = "DATECT",
      conflict = coalesce_xy
    )
    l_lev2$dt_qc <- power_full_join(
      l_lev2$dt_qc,
      l_lev1$dt_qc,
      by = "DATECT",
      conflict = coalesce_xy
    )
    l_lev2$dt_era5 <- power_full_join(
      l_lev2$dt_era5,
      l_lev1$dt_era5,
      by = "DATECT",
      conflict = coalesce_xy
    )
    
    date_of_first_new_record <- as.POSIXct(Sys.Date() - 225, tz = "UTC")
    date_of_last_new_record <- max(l_lev1$dt$DATECT, na.rm = TRUE)
    
    # v_names <<- dbListFields(con, table_name)
    v_names_for_box <- v_names[
      !v_names %in%
        c("DATECT", "TIMESTAMP", "datect_num", "checked", "pred")
    ]
    
    v_names_checklist <- reactiveValues()
    
    # Format the dates for R----
    df_proc <- data.frame(
      start_date = "1995/01/01 00:00",
      end_date = "2026/12/31 00:00"
    )
    df_proc$start_date <- as.POSIXct(
      df_proc$start_date,
      format = "%Y/%m/%d %H:%M",
      tz = "UTC"
    )
    df_proc$end_date <- as.POSIXct(Sys.Date() - 2, tz = "UTC")
    #   df_proc$end_date,
    #   format = "%Y/%m/%d %H:%M", tz = "UTC"
    # )
    
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
      dateInput(
        "sdate",
        value = as.Date(date_of_first_new_record, tz = "UTC"),
        #value = as.Date(date_of_first_new_record, tz = "UTC"),
        min = first_start_date(),
        max = last_end_date(),
        label = "Start date"
      )
    })
    
    # Create a date input for the user to select end date
    output$end_date <- renderUI({
      dateInput(
        "edate",
        value = as.Date(date_of_last_new_record, tz = "UTC"),
        # value = as.Date(strptime("01/03/2022", "%d/%m/%Y"), tz = "UTC"),
        min = first_start_date(),
        max = last_end_date(),
        label = "End date"
      )
    })
    
    # output$flag_date_range_input <- renderUI({
    #   dateRangeInput("flag_date_range",
    #             start = as.Date(date_of_last_new_record, tz = "UTC"),
    #             end = as.Date(date_of_last_new_record, tz = "UTC"),
    #             min = first_start_date(),
    #             max = date_of_last_new_record,
    #             label = "Date range"
    #   )
    # })
    
    output$flag_date_start_input <- renderUI({
      dateInput(
        "flag_date_start",
        value = as.Date(date_of_last_new_record, tz = "UTC"),
        min = first_start_date(),
        max = date_of_last_new_record,
        label = "Start date",
        weekstart = 1
      )
    })
    
    output$flag_date_end_input <- renderUI({
      dateInput(
        "flag_date_end",
        value = as.Date(date_of_last_new_record, tz = "UTC"),
        min = first_start_date(),
        max = date_of_last_new_record,
        label = "End date",
        weekstart = 1
      )
    })
    
    # Create a dataframe with the start and end dates,
    df_daterange <- eventReactive(input$retrieve_data, {
      start_date_ch <- paste(
        sprintf("%02d", day(input$sdate)),
        "/",
        sprintf("%02d", month(input$sdate)),
        "/",
        year(input$sdate),
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
      end_date_ch <- paste(
        sprintf("%02d", day(input$edate)),
        "/",
        sprintf("%02d", month(input$edate)),
        "/",
        year(input$edate),
        " ",
        sprintf("%02d", input$ehour),
        ":",
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
    
    # The optional rendering of UI elements depending on which
    # imputation method has been selected
    output$impute_extra_info <- renderUI({
      req(input$select_imputation)
      if (input$select_imputation == "time") {
        sliderInput(
          "intslider",
          label = "Smoothness (number of knots in cr spline):",
          min = 1,
          max = 50,
          value = 10,
          step = 1
        )
      } else if (input$select_imputation == "regn") {
        selectInput(
          "select_covariate",
          label = h5("Covariate"),
          choices = v_names_for_box
        )
      }
    })
    
    # Data retrieval functionality-----
    observeEvent(input$retrieve_data, {
      for (i in 1:length(v_names)) {
        v_names_checklist[[v_names[i]]] <- FALSE
      }
      
      # enabling previously disabled buttons
      shinyjs::show("extracted_data")
      shinyjs::show("validation_calendar_outer")
      
      l_qry <<- list()
      
      # file / dataframe version
      l_qry$dt <<- subset(
        l_lev2$dt,
        DATECT >= df_daterange()$start_date &
          DATECT <= df_daterange()$end_date
      )
      l_qry$dt_qc <<- subset(
        l_lev2$dt_qc,
        DATECT >= df_daterange()$start_date &
          DATECT <= df_daterange()$end_date
      )
      # make a corresponding subset of the ERA5 data
      l_qry$dt_era5 <<- subset(
        l_lev2$dt_era5,
        DATECT >= df_daterange()$start_date &
          DATECT <= df_daterange()$end_date
      )
      
      l_qry$dt$checked <<- as.factor(rownames(l_qry$dt))
      l_qry$dt$datect_num <<- as.numeric(l_qry$dt$DATECT)
      
      # Add a tab to the plotting panel for each variable that has been selected by the user.
      output$mytabs <- renderUI({
        my_tabs <- lapply(paste(v_names_for_box), function(i) {
          tabPanel(
            i,
            value = i,
            tags$style(HTML(paste0(
              '.tabbable > .nav > li > a[data-value=',
              i,
              '] {border: transparent;background-color:',
              ifelse(v_names_checklist[[i]] == TRUE, '#bcbcbc', 'transparent'),
              ';}'
            ))),
            girafeOutput(paste0(i, "_interactive_plot")),
          )
        })
        do.call(tabsetPanel, c(my_tabs, id = "plotTabs"))
      })
      
      observe(
        lapply(paste(v_names_for_box), function(i) {
          output[[paste0(i, "_interactive_plot")]] <-
            renderGirafe(plotting_function(i))
        })
      )
      
      # Creating a calendar heatmap plot that will be plotted depending on the tab selected in plotTabs
      heatmap_plot_selected <- reactive({
        req(input$plotTabs)
        plot_heatmap_calendar(l_qry$dt_qc)
      })
      
      output$heatmap_plot <- renderPlot(heatmap_plot_selected())
      
      enable('compare_vars')
    })
    
    output$flag_table <- DT::renderDataTable({
      DT::datatable(
        df_flagged_data(),
        colnames = c(
          'Date',
          'Time (UTC)',
          'Timestamp (UTC)',
          'Variable',
          'QC',
          'Comment',
          'Validator',
          'Date flagged'
        ),
        #colnames = c('date' = 'Date', 'time' = 'Time (UTC)', 'timestamp' = 'Timestamp (UTC)', 'variable' =  'Variable', 'qc' = 'QC', 'comment' = 'Comment', 'validator' = 'Validator', 'date_flagged' = 'Date flagged'),
        selection = 'none',
        rownames = FALSE,
        options = list(
          dom = 'rtilp',
          scrollX = TRUE,
          pageLength = 25,
          info = FALSE,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c('10', '25', '50', '100', 'All')
          )
        )
      ) %>%
        formatDate(3, method = 'toLocaleString')
    })
    
    # add flag
    observeEvent(input$add_flag, {
      v_flag_date_seq <- NULL
      
      if (input$flag_all_day == 'Yes') {
        v_flag_date_seq <- with_tz(
          seq(
            ymd_hms(paste0(input$flag_date_start, ' ', '00:00:00'), tz = 'GB'),
            ymd_hms(
              paste0(input$flag_date_end + 1, ' ', '00:00:00'),
              tz = 'GB'
            ),
            by = 'hour'
          ),
          tzone = 'UTC'
        )
      }
      
      if (input$flag_all_day == 'No') {
        v_flag_date_seq <- with_tz(
          seq(
            ymd_hms(
              paste0(
                input$flag_date_start,
                ' ',
                str_pad(input$flag_start_hour, 2, 'left', '0'),
                ':00:00'
              ),
              tz = 'GB'
            ),
            ymd_hms(
              paste0(
                input$flag_date_end,
                ' ',
                str_pad(input$flag_end_hour, 2, 'left', '0'),
                ':00:00'
              ),
              tz = 'GB'
            ),
            by = 'hour'
          ),
          tzone = 'UTC'
        )
      }
      
      new_df <- data.table(
        date = as.Date(v_flag_date_seq),
        time = format(v_flag_date_seq, "%H:%M:%S"),
        timestamp = v_flag_date_seq,
        variable = input$flag_var,
        qc = 8,
        comment = input$flag_comm,
        validator = username,
        date_flagged = Sys.Date()
      )
      
      updated_flagged_data <- rbind(new_df, df_flagged_data())
      df_flagged_data(updated_flagged_data)
    })
    
    # save flags
    observeEvent(input$save_flags_btn, {
      # do a check to see if the data has changed? could be time intensive if data gets big? Is it worth it?
      # if(identical(df_flagged_data, flagged_data))
      
      # Update button text
      runjs(
        'document.getElementById("save_flags_btn").textContent="Saving changes...";'
      )
      # disable button while working
      shinyjs::disable("save_flags_btn")
      
      #cat(which(df_flagged_data()$date_flagged == Sys.Date()))
      
      # change the qc codes in dt_qc
      for (i in which(df_flagged_data()$date_flagged == Sys.Date())) {
        l_lev2$dt_qc[
          trunc(l_lev2$dt_qc$DATECT, 'hour') == df_flagged_data()$timestamp[i],
          which(colnames(l_lev2$dt_qc) == df_flagged_data()$variable[i])
        ] <- 8
        l_lev2$dt_qc$validator[
          trunc(l_lev2$dt_qc$DATECT, 'hour') == df_flagged_data()$timestamp[i]
        ] <- 'data flagged'
      }
      
      #write to pin on Connect server
      pin_write(board, df_flagged_data(), name = "flagged_data", type = "rds")
      
      time_diff_flags <- difftime(
        as.POSIXct(Sys.time()),
        as.POSIXct(pins::pin_meta(board, 'wilfinc/flagged_data')$created),
        units = 'mins'
      )
      
      # write lev_2 to pin
      pin_write(board, l_lev2, name = "plevy/level2_data", type = "rds")
      
      # write CEDA formatted data to pin
      df_ceda <- format_for_ceda(l_lev2)
      pin_write(board, df_ceda, name = "ceda_data", type = "rds")
      
      time_diff_lev2 <- difftime(
        as.POSIXct(Sys.time()),
        as.POSIXct(pins::pin_meta(board, 'plevy/level2_data')$created),
        units = 'mins'
      )
      
      if (time_diff_flags < 2 & time_diff_lev2 < 2) {
        shinyalert(
          title = "Data successfully saved to cloud",
          size = "m",
          closeOnEsc = FALSE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          imageUrl = "",
          animation = TRUE,
          callbackR = function(value) {
            shinyalert(
              title = 'Would you like to apply the new data flags now? This will reset the app.',
              size = "m",
              closeOnEsc = FALSE,
              closeOnClickOutside = FALSE,
              html = FALSE,
              type = "info",
              showConfirmButton = TRUE,
              showCancelButton = TRUE,
              confirmButtonText = "Yes",
              cancelButtonText = "No",
              confirmButtonCol = "#329664",
              imageUrl = "",
              animation = TRUE,
              callbackR = function(value) {
                if (value == TRUE) {
                  # re-load the data from the server
                  
                  l_lev2 <<- pin_read(board, "plevy/level2_data")
                  
                  l_lev2$dt <<- power_full_join(
                    l_lev2$dt,
                    l_lev1$dt,
                    by = "DATECT",
                    conflict = coalesce_xy
                  )
                  l_lev2$dt_qc <<- power_full_join(
                    l_lev2$dt_qc,
                    l_lev1$dt_qc,
                    by = "DATECT",
                    conflict = coalesce_xy
                  )
                  l_lev2$dt_era5 <<- power_full_join(
                    l_lev2$dt_era5,
                    l_lev1$dt_era5,
                    by = "DATECT",
                    conflict = coalesce_xy
                  )
                  session$reload()
                }
              }
            )
          }
        )
      } else {
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
          #timer = 10000,
          imageUrl = "",
          animation = TRUE
        )
      }
      
      # reload and update the df_flags file
      flagged_data <<- pin_read(board, "wilfinc/flagged_data")
      df_flagged_data(flagged_data)
      
      # remove button activation and reactivate button
      runjs('document.getElementById("save_flags_btn").textContent="Save";')
      shinyjs::enable("save_flags_btn")
    })
    
    # reset flag table
    observeEvent(input$reset_flags_btn, {
      showModal(modalDialog(
        title = "Are you sure you want to reset the data flags? All unsaved flags will be lost",
        footer = tagList(
          actionButton("confirm_flag_reset", "Reset flags"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
      
      observeEvent(input$confirm_flag_reset, {
        df_flagged_data(flagged_data)
        shinyjs::reset("flag_details_box")
        removeModal()
      })
    })
    
    # compare variables modal
    observeEvent(input$compare_vars, {
      plot_data <- reactive({
        data.frame(x = l_qry$dt[, input$x_var], y = l_qry$dt[, input$y_var])
      })
      
      output$compare_vars_plot <- renderPlot({
        ggplot(data = plot_data(), aes(x = x, y = y)) +
          geom_point() +
          labs(x = input$x_var, y = input$y_var) +
          theme_bw()
      })
      
      showModal(
        modalDialog(
          fluidPage(
            fluidRow(
              column(
                6,
                selectInput('x_var', 'X variable:', choices = v_names_for_box)
              ),
              column(
                6,
                selectInput(
                  'y_var',
                  'Y variable:',
                  choices = v_names_for_box,
                  selected = v_names_for_box[2]
                )
              )
            ),
            fluidRow(
              shinycssloaders::withSpinner(plotOutput("compare_vars_plot"))
            )
          ),
          footer = modalButton("Close"),
          easyClose = FALSE,
          size = "l"
        )
      )
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
        l_qry <<- impute(
          y = input$plotTabs,
          l_met = l_qry,
          method = input$select_imputation,
          qc_tokeep = as.numeric(input$qc_tokeep),
          x = input$select_covariate,
          k = input$intslider,
          plot_graph = FALSE,
          selection = l_qry$dt$checked %in% selected_state()
        )
        
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
        output[[paste0(
          input$plotTabs,
          "_interactive_plot"
        )]] <- renderGirafe(plot_selected())
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
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        if (input$download_file == 'lev1') {
          paste("level_1-", Sys.Date(), ".zip", sep = "")
        } else if (input$download_file == 'lev2') {
          paste("level_2-", Sys.Date(), ".zip", sep = "")
        } else if (input$download_file == 'ceda') {
          paste("ceda-", Sys.Date(), ".zip", sep = "")
        } else if (input$download_file == 'flags') {
          paste("flagged_data-", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        if (input$download_file == 'lev1') {
          runjs(
            'document.getElementById("download_data").textContent="Preparing download...";'
          )
          shinyjs::disable("download_data")
          tmpdir <- tempdir()
          setwd(tempdir())
          fs <- c('level_1-data.csv', 'level_1-qc.csv')
          data.table::fwrite(l_lev1$dt, 'level_1-data.csv')
          data.table::fwrite(l_lev1$dt_qc, 'level_1-qc.csv')
          zip(zipfile = file, files = fs)
          runjs(
            'document.getElementById("download_data").textContent="Download";'
          )
          shinyjs::enable("download_data")
        } else if (input$download_file == 'lev2') {
          runjs(
            'document.getElementById("download_data").textContent="Preparing download...";'
          )
          shinyjs::disable("download_data")
          tmpdir <- tempdir()
          setwd(tempdir())
          fs <- c('level_2-data.csv', 'level_2-qc.csv')
          data.table::fwrite(l_lev2$dt, 'level_2-data.csv')
          data.table::fwrite(l_lev2$dt_qc, 'level_2-qc.csv')
          zip(zipfile = file, files = fs)
          runjs(
            'document.getElementById("download_data").textContent="Download";'
          )
          shinyjs::enable("download_data")
        } else if (input$download_file == 'ceda') {
          runjs(
            'document.getElementById("download_data").textContent="Preparing download...";'
          )
          shinyjs::disable("download_data")
          tmpdir <- tempdir()
          setwd(tempdir())
          fs <- c('ceda-data.csv')
          df_ceda <- format_for_ceda(l_lev2)
          data.table::fwrite(df_ceda, 'ceda-data.csv')
          zip(zipfile = file, files = fs)
          runjs(
            'document.getElementById("download_data").textContent="Download";'
          )
          shinyjs::enable("download_data")
        } else if (input$download_file == 'flags') {
          runjs(
            'document.getElementById("download_data").textContent="Preparing download...";'
          )
          shinyjs::disable("download_data")
          data.table::fwrite(flagged_data, file)
          runjs(
            'document.getElementById("download_data").textContent="Download";'
          )
          shinyjs::enable("download_data")
        }
      }
    )
    
    # Writing validated data to pin---- From main Dashboard
    observeEvent(input$submitchanges_cloud, {
      # Update button text
      runjs(
        'document.getElementById("submitchanges_cloud").textContent="Submitting changes...";'
      )
      
      # disable button while working
      shinyjs::disable("submitchanges_cloud")
      shinyjs::disable("edit_table_cols")
      
      # update lev2 with l_qry
      l_qry$dt_qc$validator <- username
      
      # overwrite existing data with changes in query
      l_lev2$dt <<- power_full_join(
        l_lev2$dt,
        l_qry$dt,
        by = "DATECT",
        conflict = coalesce_yx
      )
      l_lev2$dt_qc <<- power_full_join(
        l_lev2$dt_qc,
        l_qry$dt_qc,
        by = "DATECT",
        conflict = coalesce_yx
      )
      l_lev2$dt_era5 <<- power_full_join(
        l_lev2$dt_era5,
        l_qry$dt_era5,
        by = "DATECT",
        conflict = coalesce_yx
      )
      
      # write to pin on Connect server
      pin_write(board, l_lev2, name = "plevy/level2_data", type = "rds")
      
      # write CEDA formatted data to pin
      df_ceda <- format_for_ceda(l_lev2)
      pin_write(board, df_ceda, name = "ceda_data", type = "rds")
      
      time_diff <- difftime(
        as.POSIXct(Sys.time()),
        as.POSIXct(pins::pin_meta(board, 'plevy/level2_data')$created),
        units = 'mins'
      )
      
      if (time_diff < 2) {
        shinyalert(
          title = "Data successfully saved to cloud",
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
      } else {
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
      runjs(
        'document.getElementById("submitchanges_cloud").textContent="Submit";'
      )
      shinyjs::enable("submitchanges_cloud")
    })
  }
  
  shinyApp(ui, server, ...)
}