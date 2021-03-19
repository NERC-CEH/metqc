#MET_DB APP UI


#remotes::install_version("shiny", "1.3.2", upgrade=FALSE)


#install.packages(c("DBI"))
#install.packages(c("lubridate"))
#install.packages(c("ggplot2"))
#install.packages(c("readxl", "ggiraph"))
#install.packages(c("plyr", "dplyr", "data.table"))
#install.packages(c("lubridate", "shinyjs", "shinythemes", "shinyalert))
#install.packages(c("DT", "ROracle"))
#Load required packages
library(shiny)
library(shinyjs)
library(shinythemes)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ROracle) 
library(readxl)
library(DT)
library(data.table)
library(mgcv)
library(shinyalert)


"%!in%" <- Negate("%in%")
source('./busy-indicator.R')

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





# Define UI for the app
ui <- shinyUI(navbarPage("Met Data Validation", theme=shinytheme("united"), position = "fixed-top",
                         tabPanel("Validate Data",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Data Validation", align = "center"),
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                sidebarPanel(
                                                  #within the Validate data tab there is a mainPanel (this just means in the centre of the page).
                                                  # you can also have a leftPanel if  you want everything on the left hand side.
                                                  fluidRow(
                                                    #fluidRow is just ui generic text in this case.
                                                    #as the first row, it will be at the top of the main panel
                                                    helpText("This app provides an interface to the field sites database and allows a user to plot data, remove dubious data and fill gaps with predictions.")
                                                  ),
                                                  fluidRow(
                                                    helpText("Select your required processing start and end times below."),
                                                    column(width = 6,
                                                           uiOutput("start_date")),
                                                    column(width = 3,
                                                           numericInput("shour", value = 00, label = "Hour", min = 0, max = 23, step = 1)),
                                                    column(width = 3,
                                                           numericInput("smin", value = 00, label = "Minute", min = 0, max = 59, step = 1)),
                                                    column(width = 6,
                                                           uiOutput("end_date")),
                                                    column(width = 3,
                                                           numericInput("ehour", value = 00, label = "Hour", min = 0, max = 23, step = 1)),
                                                    column(width = 3,
                                                           numericInput("emin", value = 00, label = "Minute", min = 0, max = 59, step = 1))
                                                  ),
                                                  fluidRow(
                                                    #slider input is similar to numericInput
                                                    sliderInput("intslider", label = "Smoothness (number of knots in cr spline):", min = 1, max = 32, value = 10, step = 1)
                                                  ),
                                                  fluidRow(
                                                    #actionButton is what I referred to in the example with the submit button.
                                                    #the left hand side is your attached name to call in the server.
                                                    #the right hand side is the text to be displayed in the app.
                                                    actionButton("seejobsummary", "Confirm Run Settings"),
                                                    shinyjs::disabled(actionButton("retrieve_data", "Retrieve from database")),
                                                    actionButton("restart", "Start over")
                                                  )),
                                                hidden(mainPanel(
                                                  id = "showpanel",
                                                  fluidRow(
                                                    #h3("Output"),
                                                    uiOutput("submit_info"),
                                                    column(width = 4,
                                                           uiOutput("var_filter"),
                                                           uiOutput("var_filter_col"),
                                                           uiOutput("landuse_filter"),
                                                           uiOutput("var_info"),
                                                           shinyjs::disabled(actionButton("replot", label = "Replot graph"))),
                                                    #shinyjs::disabled(actionButton("plottime", label = "Plot Time Series")),
                                                    column(width = 8,
                                                           hidden(fluidRow(id = "plotted_data",
                                                                           h4("Plotted data"),
                                                                           shinyjs::disabled(actionButton("reset", label = "Reset selection")),
                                                                           shinyjs::disabled(actionButton("delete", label = "Delete selection")),
                                                                           shinyjs::disabled(actionButton("nochange", label = "No change needed")),
                                                                           shinyjs::disabled(actionButton("submitchanges", "Submit changes")),
                                                                           girafeOutput("plot")
                                                           ))))
                                                )),
                                                br(),
                                                hidden(fluidRow(id = "progress_row",
                                                                h2("Review progress", align = "center"),
                                                                column(width = 6,
                                                                       plotOutput("progressbar", width = "100%")),
                                                                column(width = 6,
                                                                       div(DT::dataTableOutput("summarytable"),style = "width:75%")),
                                                                column(width = 4,
                                                                       actionButton("write_data", "Write to database"))
                                                ))
                                  )),
                         #The other panel starts here, the second tab in the navbar page.
                         #note it is after a comma and after: 
                         #
                         #tabPanel(Validate Data",
                         #whatever was in the tabPanel
                         #),
                         #
                         #that is how you order the pages.
                         tabPanel("Information",
                                  p("CBED is a model which provides Concentratio-Based Estimates of Deposition for a number of pollutants (Smith 2000, 2001). Dry deposition (i.e. var flux), wet deposition (in precipitation), and aerosol particle deposition are modelled explicitly, to six different land-use types."),
                                  uiOutput("git_link")
                         ) # end of tabPanel
                         ,busyIndicator(text = "Please wait...", wait = 500)) #end of navbar page
) 


#MET_DB APP SERVER

server <- shinyServer(function(input, output, session) {
  
  #reading in the data flags ----
  data_flags <- read.csv("./data_flags.csv")
  data_flags$code <- as.character(data_flags$code)
  
  selected_state <- reactive({
    input$plot_selected
  })
  
  accumulated_df <- data.frame()
  reviewed_df <- data.frame()
  change_summary <- data.frame()
  
  var_choices <- reactive({
    var_choices <- dbNames
    variables_to_remove <- c("DATECT", "TIMESTAMP","checked","DATECT_NUM","pred")
    var_choices <- var_choices[!var_choices %in% variables_to_remove]
    var_choices <- var_choices[!var_choices %in% input$variable_check]
    
  })
  
  #here is the first observeEvent, which dictates what to do when the reset button is pressed
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })
  
  #the second observeEvent, which dictates what to do when the delete button is pressed
  observeEvent(input$delete, {
    if(is.null(selected_state())){
      shinyjs::alert("Please select a point to delete.")
    } else{
      shinyjs::enable("submitchanges")
      
      delete_reasons <- data_flags %>% 
        filter(cat == "initial_flag")
      delete_reasons <- delete_reasons$information
      
      showModal(modalDialog(
        h4("What is the reason for deleting the point?"),
        selectInput("var_reason", label = h5("Reason for point removal."), choices = delete_reasons),
        easyClose = TRUE,
        footer = tagList(
          actionButton("var_reason1", "Confirm deletion reason."),
        )))
      
      
      observeEvent(input$var_reason1,{
        if(length(input$var_reason)!= 0){
          shinyjs::alert("Reason for deletion confirmed. Please submit changes.")
          removeModal()
        }
        
        df_list <- list()
        for(i in selected_state()){
          #here I am creating a df to keep track of the changes made to the data.
          #set column names
          changed_df <- data.frame()
          changed_df <- colnames(c("variable","checked","gapfill_code","gapfill_initials","gapfill_info","old_value","flag_info","flag_code","flag_initials","new_value","user"))
          changed_df$variable <- input$select_var
          changed_df$point_id <- i
          changed_df$old_value <- df_qry[df_qry$checked %in% i, input$select_var]
          changed_df$gapfill_info <- input$select_landuse
          changed_df$flag_info <-  input$var_reason
          add_code <- data_flags %>% 
            filter(information == input$var_reason)
          changed_df$flag_code <- add_code$code
          changed_df$flag_initials <- add_code$initials
          add_fill <- data_flags %>% 
            filter(information == input$select_landuse)
          changed_df$gapfill_code <- add_fill$code
          changed_df$gapfill_initials <- add_fill$initials
          
          changed_df$user <- Sys.info()['user']
          #change the old value to the new value depending on the predicted value
          df_qry[df_qry$checked %in% i, input$select_var] <<- NA
          df_qry[is.na(df_qry[, input$select_var]), input$select_var] <<- df_qry$pred[is.na(df_qry[, input$select_var])]
          #now adding the new value to the changed df, using the exact same command as above for the old value
          changed_df$new_value <- df_qry[df_qry$checked %in% i, input$select_var]
          changed_df <- bind_rows(changed_df) #%>% as.data.frame()
          df_list[[i]] <- changed_df
        }
        #browser()
        
        changed_df <- bind_rows(df_list)
        #if statement that creates the dataframe in memory if it does not already exist
        if(length(change_summary)==0){
          change_summary <<- changed_df
        } else {
          #or appends the dataframe if it does already exist
          change_summary <<- rbind(change_summary, changed_df)
        }
        #change column order
        change_summary <<- change_summary[c("variable","point_id","old_value","new_value","flag_code","flag_initials","flag_info","gapfill_code","gapfill_initials","gapfill_info","user")]
        
        
        
        #Re-plotting plot after deletion is confirmed to illustrate changes
        shinyjs::show("plotted_data")
        enable("reset")
        enable("delete")
        enable("nochange")
        y <- df_qry[, input$select_var]
        m <- gam(y ~ s(DATECT_NUM, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
        df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
        
        
        ggp <- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) + 
          geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) + 
          geom_line(aes(y = df_qry$pred), colour = "red") + 
          #ylim(0, NA) + 
          xlab("Date") + ylab(paste("Your variable:", input$select_var)) + ggtitle(paste(input$select_var, "time series")) +
          theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
        output$plot <- renderggiraph({
          x <- girafe(code = print(ggp), width_svg = 6, height_svg = 5)
          x <- girafe_options(x, opts_selection(
            type = "multiple", css = "fill:#FF3333;stroke:black;"),
            opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
          x
          
        })
        
      })
      
      #here I am making a table that shows the changes that have been made
      output$summarytable <- renderDataTable({
        datatable(change_summary,
                  #not that datatable is originally written in javascript
                  #hence why there are some unusually formatted options here 
                  #like class = 'compact' and a 'text-align' = 'center'
                  options = list(pageLength = 5, lengthMenu = c(5,10,25,50)), rownames = FALSE,class = 'compact') %>%
          formatRound(columns = c(3:4), digits = 2)%>% 
          formatStyle(columns = c(1:11), 'text-align' = 'center')}
      )
      
      
      
    }
  })
  
  
  #Third observeEvent: no change button
  observeEvent(input$nochange, {
    showModal(modalDialog( 
      title = paste("Can you confirm", input$select_var, "has been checked and no changes are required?"),
      footer = tagList(actionButton("confirm", "Confirm and submit variable."), 
                       modalButton("Cancel")),
      easyClose = TRUE))
  })
  
  #Fourth observeEvent: confirm button in dialogue box
  observeEvent(input$confirm,{
    #When 'Confirm is clicked, an alert is shown:
    if(length(input$confirm)!= 0){
      shinyjs::alert("Variable checked. Please review below and write to the database.")
      removeModal()
    }
    
    #After closing alert message:
    disable("submitchanges")
    disable("nochange")
    disable("delete")
    
    #Variable is submitted (same action as submit changes button)
    shinyjs::show("progress_row")
    newvar <- input$select_var
    ## Adding the progress bar 
    output$progressbar <- renderPlot({
      #Extracting relevant variables names and making dataframe that has FALSE assigned to every other variable name
      reviewed_df <<- as.data.frame(var_choices())
      reviewed_df$reviewed <<- FALSE
      #Creating dataframe which  sets variable to TRUE is it is in input$select_var (variable checked and submitted)
      now_true <- reviewed_df %>%
        filter(var_choices() == newvar) 
      now_true$reviewed <- TRUE
      
      colnames(reviewed_df) <- c("var_choices", "reviewed")
      colnames(now_true) <- c("var_choices", "reviewed")
      
      if(length(accumulated_df) == 0){
        #if the dataframe exists already - replace the row in reviewed df and assign as accumulated_df
        reviewed_df$reviewed[which(reviewed_df$var_choices==now_true$var_choices)] <- now_true$reviewed
        accumulated_df <<- reviewed_df
      } else{
        #if it does exist already, add new variable to accumulated df 
        accumulated_df$reviewed[which(accumulated_df$var_choices==now_true$var_choices)] <- now_true$reviewed
        accumulated_df <<- accumulated_df
      }
      
      #some variables do not have to be included in the plot
      accumulated_df$reviewed <- factor(accumulated_df$reviewed, levels = c("TRUE","FALSE"))
      
      progress_plot <- ggplot(accumulated_df) +
        geom_tile(aes(x= var_choices,y= "",fill = reviewed))+
        geom_text(aes(x= var_choices,y= "",label = var_choices),
                  color = "white", size =3,position = position_stack(vjust = 1),angle = 90)+
        scale_y_discrete(""
                         # ,expand = c(0,2)
        )+
        scale_fill_manual(breaks = c("TRUE", "FALSE"),
                          values = c("#2F8C1F", "#EB1A1A"))+
        theme(
          panel.background = element_blank(),
          axis.ticks.y =  element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
        )
      progress_plot
    }
    ,width = "auto",height = 275
    )
    
    #Variable that has been submitted is no longer shown on the Variables dropdown 
    output$var_filter <- renderUI({
      #instead of just calling dbNames, I'm making a new object and removing the variables that don't need to be checked.
      #ie the timestamp, point id and predicted values.
      selectInput("select_var", label = h5("Variable"), 
                  choices = as.list(var_choices(), -c(input$select_var)))
    })
    output$var_filter_col <- renderUI({
      selectInput("select_col", label = h5("Variable for Colour Scale"), 
                  choices = as.list(var_choices(), -c(input$select_var)))
    })
    
    #Plot of variable that has been submitted disappears (plot space is empty), and user must click replot to check other variables
    shinyjs::show("plotted_data")
    enable("reset")
    enable("delete")
    enable("nochange")
    y <- df_qry[, input$select_var]
    m <- gam(y ~ s(DATECT_NUM, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
    df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
    
    
    ggp <- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) + 
      geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) + 
      geom_line(aes(y = df_qry$pred), colour = "red") + 
      #ylim(0, NA) + 
      xlab("Date") + ylab(paste("Your variable:", input$select_var)) + ggtitle(paste(input$select_var, "time series")) +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    output$plot <- renderggiraph({
      x <- girafe(code = print(ggp), width_svg = 6, height_svg = 5)
      x <- girafe_options(x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
      x
    })
  })
  
  
  
  
  
  
  observeEvent(input$seejobsummary, {
    shinyjs::enable("retrieve_data")
    shinyjs::disable("seejobsummary")
  })
  
  #the first output, rendering a table with the data, depending on the reactive value in selected_state.
  #it returns "out" which is a subset of all the data in df_qry that match selected_state
  #it is then saved as output$job_table
  #and called in the ui as tableOutput("job_table")
  output$job_table <- renderTable({
    out <- df_qry[df_qry$checked %in% selected_state(), 1:3]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  
  #Create select input UI element with var options
  output$var_filter <- renderUI({
    #instead of just calling dbNames, I'm making a new object and removing the variables that don't need to be checked.
    #ie the timestamp, point id and predicted values.
    selectInput("select_var", label = h5("Variable"), 
                choices = as.list(var_choices()))
  })
  
  #Create select input UI element with var options
  output$var_filter_col <- renderUI({
    selectInput("select_col", label = h5("Variable for Colour Scale"), 
                choices = as.list(var_choices()))
  })
  
  #Create select input UI element with land-use options
  gapfill_options <- data_flags %>% 
    filter(cat != "initial_flag")
  gapfill_options <- gapfill_options$information
  
  output$landuse_filter <- renderUI({
    selectInput("select_landuse", label = h5("Gap-Filling Method"), choices = gapfill_options)
  })
  
  #Create a sentence of metadata about the site, station and var selection made.
  output$var_info <- renderUI({
    helpText(paste0("You have selected variable ", as.character(input$select_var), ". Click Replot to start checking."))
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
              max = last_end_date(), label = "Start date")
  })
  
  #Create a date input for the user to select end date
  output$end_date <- renderUI ({
    dateInput("edate", value = as.Date(strptime("01/08/2017", "%d/%m/%Y"), tz = "UTC"), 
              min = first_start_date(), max = last_end_date(), label = "End date")
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
               #varName = input$select_var, 
               #landuse = input$select_landuse, 
               nTimes = nTimes,
               datect = datect)
  })
  
  # Run CBED dry dep
  observeEvent(input$retrieve_data, {
    #enabling previously disabled buttons 
    shinyjs::show("showpanel")
    enable("replot")
    enable("plottime")
    disable("retrieve_data")
    # make an SQL query to select all fields between start and end dates
    qry <- paste0("SELECT * FROM ", table_name, 
                  " WHERE DATECT > TO_DATE('", job_df()$datech[1], "', 'yyyy/mm/dd hh24:mi') 
                     AND DATECT < TO_DATE('", job_df()$datech[6], "', 'yyyy/mm/dd hh24:mi')")             
    df_qry <<- dbGetQuery(con, qry)
    df_qry$checked <<- as.factor(rownames(df_qry))
    df_qry$DATECT_NUM <<- as.numeric(df_qry$DATECT)
    
    #creating new dataframe with just relevant options, in order to be used in the selectInput() function in the modal.
    df_qry_choices <- df_qry %>%
      select(-DATECT,-TIMESTAMP,-checked,-DATECT_NUM) %>%
      select_if(function(x) any(!is.na(x))) #removing columns where all values are NA (for variables that have no valid data)
    
    
    showModal(modalDialog(
      h4("Are there are any variables that do not need checking?"),
      selectInput("variable_check", "Variables NOT to be checked",
                  choices = as.list(var_choices()), multiple = TRUE),
      footer = tagList(
        modalButton("All variables need checking."),
        actionButton("variables_not_included", "Confirm variables for exclusion.")
      ),
      h6("Note every variable will have to be checked before data can be written to the database.")
    ))
    
    
    observeEvent(input$variables_not_included,{
      if(length(input$variable_check)!=0){
        if(input$select_var == input$variable_check){
          shinyjs::alert("Variables selected for exclusion cannot match initial variable selected.")
        } else{
          shinyjs::alert("Variables selected for exclusion.")
          removeModal()
        }
      }else{
        shinyjs::alert("Please select variables for exclusion or click 'All variables need checking'.")
      }
      disable("retrieve_data")
    })
    
    
    #Render the job info dataframe as a table
    output$job_table <- renderTable({
      as.data.frame(as.matrix(summary(df_qry[, input$select_var])))
    })
  })
  
  # Run CBED wet dep
  observeEvent(input$write_data, {
    if(FALSE %in% accumulated_df$reviewed){
      false_list <- accumulated_df[FALSE %in% accumulated_df$reviewed,]
      false_list <- false_list%>%
        select(variable_names)
      false_list <- as.character(false_list$variable_names)
      print(paste0("Not all variables have been checked. Please check ",false_list,"."))
    } else{
      print("Ready to write to database.")
    }
    
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
  
  #submit change button - doesn't do anything just yet.
  observeEvent(input$submitchanges,{
    shinyjs::show("progress_row")
    #by assigning newvar here and not using input$select_var directly I am preventing the progress bar 
    #being automatically updated when a new variable is selected, but only once submitchanges has been clicked
    newvar <- input$select_var
    #also adding the progress bar once the replot button has been clicked
    output$progressbar <- renderPlot({
      #extracting the relevant variables names 
      #make a dataframe that has FALSE assigned to every variable name
      reviewed_df <<- as.data.frame(var_choices())
      reviewed_df$reviewed <<- FALSE
      #creating a true dataframe, makes a row that sets variable to TRUE is it is in input$select_var
      now_true <- reviewed_df %>%
        filter(var_choices() == newvar) 
      now_true$reviewed <- TRUE
      
      #just get the brackets out of the df column name to avoid non-function error.
      colnames(reviewed_df) <- c("var_choices", "reviewed")
      colnames(now_true) <- c("var_choices", "reviewed")
      
      if(length(accumulated_df) == 0){
        #if the dataframe exists already - replaced the row in reviewed df and assign as accumulated_df
        reviewed_df$reviewed[which(reviewed_df$var_choices==now_true$var_choices)] <- now_true$reviewed
        accumulated_df <<- reviewed_df
      } else{
        #if it does exist already, add new variable to accumulated df 
        accumulated_df$reviewed[which(accumulated_df$var_choices==now_true$var_choices)] <- now_true$reviewed
        accumulated_df <<- accumulated_df
      }
      
      #some variables do not have to be included in the plot
      #we'll make this variable by selection at some point
      accumulated_df$reviewed <- factor(accumulated_df$reviewed, levels = c("TRUE","FALSE"))
      
      progress_plot <- ggplot(accumulated_df) +
        geom_tile(aes(x= var_choices,y= "",fill = reviewed))+
        geom_text(aes(x= var_choices,y= "",label = var_choices),
                  color = "white", size =3,position = position_stack(vjust = 1),angle = 90)+
        scale_y_discrete(""
                         # ,expand = c(0,2)
        )+
        scale_fill_manual(breaks = c("TRUE", "FALSE"),
                          values = c("#2F8C1F", "#EB1A1A"))+
        theme(
          panel.background = element_blank(),
          axis.ticks.y =  element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
        )
      progress_plot
    }
    ,width = "auto",height = 275
    )
    disable("submitchanges")
    #browser()
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
    shinyjs::show("plotted_data")
    enable("reset")
    enable("delete")
    enable("nochange")
    y <- df_qry[, input$select_var]
    m <- gam(y ~ s(DATECT_NUM, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
    df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
    
    #If statement to trigger an alert when a variable doesn't have valid data 
    if(is.na(input$select_var)){
      shinyjs::alert(input$select_var, "has no valid data")
      
    }
    
    ggp <- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) + 
      geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) + 
      geom_line(aes(y = df_qry$pred), colour = "red") + 
      #ylim(0, NA) + 
      xlab("Date") + ylab(paste("Your variable:", input$select_var)) + ggtitle(paste(input$select_var, "time series")) +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    output$plot <- renderggiraph({
      x <- girafe(code = print(ggp), width_svg = 6, height_svg = 5)
      x <- girafe_options(x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
      x
    })
  })
  
  
  
  
  observeEvent(input$restart,{
    shinyjs::enable("seejobsummary")
    hideElement("showpanel")
    hideElement("plotted_data")
    hideElement("progress_row")
    updateNumericInput(session,"shour", value = 00, label = "Hour", min = 0, max = 23, step = 1)
    updateNumericInput(session,"smin", value = 00, label = "Minute", min = 0, max = 59, step = 1)
    updateNumericInput(session,"ehour", value = 00, label = "Hour", min = 0, max = 23, step = 1)
    updateNumericInput(session,"emin", value = 00, label = "Minute", min = 0, max = 59, step = 1)
    session$sendCustomMessage(type = 'plot_set', message = character(0))
    df_qry <<- data.frame()
    accumulated_df <<- data.frame()
    reviewed_df <<- data.frame()
    change_summary <<- data.frame()
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

#this bit of code just tells R to run the app, and specifies what to use as the ui and what to use as the server.
#straightforward in this script, but in some larger apps you might want to have the ui and server in seperate scripts
#it would work the exact same, but you'd have so source the scripts before running the app

#source("script_with_ui.R")
#source("script_with_server.R")
#and then you can run 

# Run the application 
shinyApp(ui = ui, server = server)
