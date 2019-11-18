# initial problems either caused by 
# naming output plot object 'timeplot' instead of 'plot'
# or having 'Plot time Series' button to initally make graph
# guess former
#install.packages(c("readxl", "ggiraph"))
#Load required packages
library(shiny)
library(lubridate)
library(plyr)
library(ggplot2)
#library(openair)
library(ROracle)
library(readxl)
library(ggiraph)
library(mgcv)
"%!in%" <- Negate("%in%")

# Set timezone to GMT to stop R/Oracle changing dates based on daylight saving time
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

# Set database connection
dbuid <- "BU_FIELD_SITES" 
dbpwd <- "0ig2mtYUL9" 
drv <- dbDriver("Oracle")
con <- dbConnect(drv, dbname = "budbase.nerc-bush.ac.uk/BUA",
                      username = dbuid,
                      password = dbpwd)
# needs to be uppercase for queries to work                      
table_name <- "MET_30MIN"                      
dbNames <- dbListFields(con, table_name)
                      
df_proc <- data.frame(
  startDate = "1995/01/01 00:00",
  endDate   = "2019/12/31 00:00",
  ghgName = "co2"
)

#Format the dates for R
df_proc$startDate <- as.POSIXct(df_proc$startDate, format = "%Y/%m/%d %H:%M", tz = "UTC")
df_proc$endDate   <- as.POSIXct(df_proc$endDate, format = "%Y/%m/%d %H:%M", tz = "UTC")

<!--- { ui -->

# Define UI for the app
ui <- shinyUI(navbarPage("Met Data Validation",
                 tabPanel("Validate Data",
                            mainPanel(
                              fluidRow(
                                 helpText("This app provides an interface to the field sites database and allows a user to plot data, remove dubious data and fill gaps with predictions.")
                                ),
                              fluidRow(
                                column(width = 4,
                                       uiOutput("var_filter")),
                                column(width = 4,
                                       uiOutput("var_filter_col")),
                                column(width = 4,
                                       uiOutput("landuse_filter")),
                                uiOutput("date_info")
                              ),
                              fluidRow(
                                column(width = 6,
                                       h3("Select Start Data and Time")),
                                column(width = 6,
                                       h3("Select End Data and Time"))
                              ),
                              fluidRow(
                                column(width = 2,
                                       uiOutput("start_date")),
                                column(width = 2,
                                       numericInput("shour", value = 00, label = "Hour", min = 0, max = 24, step = 1)),
                                column(width = 2,
                                       numericInput("smin", value = 00, label = "Minute", min = 0, max = 60, step = 1)),

                                column(width = 2,
                                       uiOutput("end_date")),
                                column(width = 2,
                                       numericInput("ehour", value = 00, label = "Hour", min = 0, max = 24, step = 1)),
                                column(width = 2,
                                       numericInput("emin", value = 00, label = "Minute", min = 0, max = 60, step = 1))
                            ),
                            fluidRow(
                              sliderInput("intslider", label = "Smoothness (number of knots in cr spline):", min = 1, max = 32, value = 10, step = 1)
                            ),
                            fluidRow(
                              actionButton("seejobsummary", "Confirm Run Settings")
                            ),
                            fluidRow(
                              tableOutput("job_table")
                              #DT::dataTableOutput("job_table")
                              #helpText("Show something here with synopsis of choices for processing, e.g. a table of the options.")
                            ),
                            fluidRow(
                              actionButton("retrieve_data", "Retrieve from database"),
                              actionButton("write_data", "Write to database")
                            ),
                            uiOutput("submit_info"),
                            fluidRow(
                              uiOutput("sector_filter"),
                              actionButton("plottime", label = "Plot Time Series"),
                              actionButton("reset", label = "Reset selection"),
                              actionButton("delete", label = "Delete selection"),
                              actionButton("replot", label = "Replot graph"),
                                 h4("Selected states"),
                                 tableOutput("datatab")
                            ),
                            girafeOutput("plot")
                          )
                 ),
                 tabPanel("Information",
                          p("CBED is a model which provides Concentratio-Based Estimates of Deposition for a number of pollutants (Smith 2000, 2001). Dry deposition (i.e. var flux), wet deposition (in precipitation), and aerosol particle deposition are modelled explicitly, to six different land-use types."),
                          uiOutput("git_link")
                 )

    )
)
<!--- } -->

# Define server logic required for the app
server <- shinyServer(function(input, output, session) {

  selected_state <- reactive({
    input$plot_selected
  })

  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })

  observeEvent(input$delete, {
    #df_qry[df_qry$checked %in% selected_state(), input$select_var] <<- NA
    df_qry[df_qry$checked %in% selected_state(), input$select_var] <<- NA
    df_qry[is.na(df_qry[, input$select_var]), input$select_var] <<- df_qry$pred[is.na(df_qry[, input$select_var])]
    #df_qry$TS[df_qry$checked %in% selected_state()] <<- NA
  })

  output$job_table <- renderTable({
    out <- df_qry[df_qry$checked %in% selected_state(), 1:3]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })

  #Create select input UI element with var options
  output$var_filter <- renderUI({
    selectInput("select_var", label = h3("Select Variable"), 
    choices = as.list(dbNames) )
  })

  #Create select input UI element with var options
  output$var_filter_col <- renderUI({
    selectInput("select_col", label = h3("Select Variable for Colour Scale"), 
    choices = as.list(dbNames) )
  })

  #Create select input UI element with land-use options
  output$landuse_filter <- renderUI({
    selectInput("select_landuse", label = h3("Select Gap-Filling Method"), choices = as.list(c("gridavg", "arable", "forest", "grass", "moor", "urban")) )
  })
    
  #Create a sentence of metadata about the site, station and var selection made.
  output$date_info <- renderUI ({
    helpText(paste("You have selected var ", as.character(input$select_var), ". Select your required processing start and end times below."))
  })
  
  #Create a reactive element with the earliest start date
  first_start_date <- reactive({
    min(as.Date(df_proc$startDate))
  })
  
  #Create a reactive element with the latest end date
  last_end_date <- reactive({
    max(as.Date(df_proc$endDate))
  })
  
  #Create a date input for the user to select start date
  output$start_date <- renderUI ({
    dateInput("sdate", 
      value = as.Date(strptime("01/07/2017", "%d/%m/%Y"), tz = "UTC"), 
      min = first_start_date(), 
      max = last_end_date(), label = "Date")
  })
  
  #Create a date input for the user to select end date
  output$end_date <- renderUI ({
    dateInput("edate", value = as.Date(strptime("01/08/2017", "%d/%m/%Y"), tz = "UTC"), 
      min = first_start_date(), max = last_end_date(), label = "Date")
  })
  
  #Create a dataframe with all the information about the job
  # the dataframe is only created (or recreated) when the View Job Summary button is pressed
  job_df <- eventReactive(input$seejobsummary, {
    startDate <- paste(sprintf("%02d", day(input$sdate)), "/", sprintf("%02d", month(input$sdate)), "/", year(input$sdate), " ", sprintf("%02d", input$shour), ":", sprintf("%02d", input$smin), sep = "")    
    startDate <- as.POSIXct(strptime(startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
    endDate   <- paste(sprintf("%02d", day(input$edate)), "/", sprintf("%02d", month(input$edate)), "/", year(input$edate), " ", sprintf("%02d", input$ehour), ":", sprintf("%02d", input$emin), sep = "")
    endDate   <- as.POSIXct(strptime(endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
    nTimes <- input$intslider
    # create a sequence of timestamps
    datect <- seq(startDate, endDate, length = nTimes)

    data.frame(datech = format(datect, "%Y/%m/%d %H:%M"),
               varName = input$select_var, 
               landuse = input$select_landuse, 
               nTimes = nTimes,
               datect = datect)
  })
  
  #Render the job info dataframe as a table
  output$job_table <- renderTable({
    job_df()
  })
  
  # Run CBED dry dep
  observeEvent(input$retrieve_data, {
    # make an SQL query to select all fields between start and end dates
    qry <- paste0("SELECT * FROM ", table_name, 
                 " WHERE DATECT > TO_DATE('", job_df()$datech[1], "', 'yyyy/mm/dd hh24:mi') 
                     AND DATECT < TO_DATE('", job_df()$datech[6], "', 'yyyy/mm/dd hh24:mi')")             
    df_qry <<- dbGetQuery(con, qry)
    df_qry$checked <<- as.factor(rownames(df_qry))
    df_qry$DATECT_NUM <<- as.numeric(df_qry$DATECT)
    y <- df_qry[, input$select_var]
    m <- gam(y ~ s(DATECT_NUM, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
    df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
    
    ggp <<- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) +     
              geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) +
              geom_line(aes(y = df_qry$pred)) +
              #ylim(0, NA) +
              xlab("Date")      + ylab("Your variable")      + ggtitle("Time series of your variable")
    
    #Render the job info dataframe as a table
      output$job_table <- renderTable({
        as.data.frame(as.matrix(summary(df_qry[, input$select_var])))
      })
  })
  
  # Run CBED wet dep
  observeEvent(input$write_data, {
    # declare brick for deposition rasters for multiple times
    b_F <<- brick(r, values = FALSE, nl = input$intslider)
    #b_Fwet <- brick(r, values = FALSE, nl = nTimes)

    for (itime in 1:input$intslider){
      r_F <<- getWetDep(pollutant = input$select_col, datect = job_df()$datect[itime], 
                   landuse = input$select_landuse, mm = TRUE)
      #r_Fwet <- getWetDep(pollutant = pollutant, datect = datect, landuse = landuse, mm = TRUE)
      b_F[[itime]] <<- r_F
      #b_Fwet[[itime]] <- r_Fwet
    }                  
  })
  
  #Set the link to the JASMIN public group workspace where ouput will be provided
  pubgws_url <-a("CBED EIDC page", href = "https://catalogue.ceh.ac.uk/documents/bfa37333-b22e-41b8-829a-10c57d50e13f")
  
  #When the submit job action button is pressed display a message which states how long the job might take and information on where to access the results.
  observeEvent(input$retrieve_data, {
    output$submit_info <- renderUI ({
      helpText(paste("Your data has been extracted. Plot using the replot button below."))
    })
  })
  
  
  #When the submit job action button is pressed display a message which states how long the job might take and information on where to access the results.
  observeEvent(input$write_data, {
    output$submit_info <- renderUI ({
      helpText(paste("Your model run has completed. Plot and download the output via the options below."))
    })
  })
  
  #Set the link to the CBED page on EIDC
  eidc_url <- a("CBED EIDC Documentation", href = "https://catalogue.ceh.ac.uk/documents/bfa37333-b22e-41b8-829a-10c57d50e13f")
  
  #Create an output element for the url
  output$git_link <- renderUI({
    tagList("CBED pages on EIDC:", eidc_url)
  })
  
  
  # Plot results as maps
  observeEvent(input$plotmap, {
    output$mapPlot<-renderPlot(
      {
        names(b_F) <- job_df()$datect
        plot(b_F)
      })
    })
  
  observeEvent(input$replot, {
    y <- df_qry[, input$select_var]
    m <- gam(y ~ s(DATECT_NUM, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
    df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)

    ggp <- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) + 
    geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) + 
    geom_line(aes(y = df_qry$pred), colour = "red") + 
    #ylim(0, NA) + 
    xlab("Date")      + ylab("Your variable")      + ggtitle("Time series of your variable")
    output$plot <- renderggiraph({
      x <- girafe(code = print(ggp), width_svg = 6, height_svg = 5)
      x <- girafe_options(x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
      x
    })
  })

  # download netCDF files
  output$netcdf <- downloadHandler(
filename = function() {
    paste('rCBED-', Sys.Date(), '.nc', sep='')
  },
    content = function(file) {
      writeRaster(b_F, file)
    }
  )
})

# Run the application 
shinyApp(ui = ui, server = server)

