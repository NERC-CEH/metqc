server <- shinyServer(function(input, output, session) {
  # Setting up the server----
  Sys.setenv(TZ = "GMT")
  Sys.setenv(ORA_SDTZ = "GMT")

  # Reading in the data flags ----
  data_flags <- read_csv("~/met_db/data/data_flags.csv")
  data_flags$code <- as.character(data_flags$code)

  # Making database connection----
  dbuid <- "BU_FIELD_SITES"
  dbpwd <- "0ig2mtYUL9"
  drv <- dbDriver("Oracle")
  con <- dbConnect(drv,
    dbname = "budbase.nerc-bush.ac.uk/BUA",
    username = dbuid,
    password = dbpwd
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
  accumulated_df <- data.frame()
  reviewed_df <- data.frame()
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
      for (i in input$variable_check) {
        appendTab("plotTabs", tabPanel(i), select = TRUE)
      }
      # Give a warning if there are no variables selected.
    } else if (is.null(input$variable_check)) {
      shinyjs::alert("Please select one or more variables before extracting data from the database.")
    }

    # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
    plot_selected <- reactive({
      req(input$plotTabs)
      plotting_function(input$plotTabs)
    })
    output$interactive_plot <- renderggiraph(plot_selected())

    # Creating a calendar heatmap plot that will be plotted depending on the tab selected in plotTabs
    heatmap_plot_selected <- reactive({
      req(input$plotTabs)
      plot_heatmap_calendar(input$plotTabs, df_qry)
    })

    output$heatmap_plot <- renderPlot(heatmap_plot_selected())
  })

  # Creating reactive variables-----
  selected_state <- reactive({
    input$interactive_plot_selected
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
      js$reset()
    })
  })

  # Finished checking, close tab functionality----
  observeEvent(input$nochange, {
    removeTab("plotTabs", input$plotTabs)
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
    
    
    browser()
    
  })





  # Older code starts below, keeping to reuse some elements.
  # No change button functionality----
  #
  #
  #   # Confirm button functionality----
  #   observeEvent(input$confirm, {
  #     #When 'Confirm is clicked, an alert is shown:
  #     if(length(input$confirm)!= 0) {
  #       shinyjs::alert("Variable checked. Please review below and write to the database.")
  #       removeModal()
  #     }
  #
  #     #After closing alert message:
  #     disable("submitchanges")
  #     disable("nochange")
  #     disable("delete")
  #
  #     #Variable is submitted (same action as submit changes button)
  #     shinyjs::show("progress_row")
  #     newvar <- input$select_var
  #
  #     ## Adding the progress bar
  #     output$progressbar <- renderPlot({
  #       #Extracting relevant variables names and making dataframe that has FALSE assigned to every other variable name
  #       reviewed_df <<- as.data.frame(var_choices())
  #       reviewed_df$reviewed <<- FALSE
  #       #Creating dataframe which  sets variable to TRUE is it is in input$select_var (variable checked and submitted)
  #       now_true <- reviewed_df %>%
  #         filter(var_choices() == newvar)
  #       now_true$reviewed <- TRUE
  #
  #       colnames(reviewed_df) <- c("var_choices", "reviewed")
  #       colnames(now_true) <- c("var_choices", "reviewed")
  #
  #       if(length(accumulated_df) == 0) {
  #         #if the dataframe exists already - replace the row in reviewed df and assign as accumulated_df
  #         reviewed_df$reviewed[which(reviewed_df$var_choices==now_true$var_choices)] <- now_true$reviewed
  #         accumulated_df <<- reviewed_df
  #       } else{
  #         #if it does exist already, add new variable to accumulated df
  #         accumulated_df$reviewed[which(accumulated_df$var_choices==now_true$var_choices)] <- now_true$reviewed
  #         accumulated_df <<- accumulated_df
  #       }
  #
  #       #some variables do not have to be included in the plot
  #       accumulated_df$reviewed <- factor(accumulated_df$reviewed, levels = c("TRUE","FALSE"))
  #
  #       progress_plot <- ggplot(accumulated_df) +
  #         geom_tile(aes(x= var_choices,y= "",fill = reviewed))+
  #         geom_text(aes(x= var_choices,y= "",label = var_choices),
  #                   color = "white", size =3,position = position_stack(vjust = 1),angle = 90)+
  #         scale_y_discrete(""
  #                          # ,expand = c(0,2)
  #         )+
  #         scale_fill_manual(breaks = c("TRUE", "FALSE"),
  #                           values = c("#2F8C1F", "#EB1A1A"))+
  #         theme(
  #           panel.background = element_blank(),
  #           axis.ticks.y =  element_blank(),
  #           axis.ticks.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_blank()
  #         )
  #       progress_plot
  #     }
  #     ,width = "auto",height = 275
  #     )
  #
  #     shinyjs::show("plotted_data")
  #     enable("reset")
  #     enable("delete")
  #     enable("nochange")
  #     y <- df_qry[, input$select_var]
  #     m <- gam(y ~ s(datect_num, bs = "cr", k = input$intslider), data = df_qry, na.action = na.exclude)
  #     df_qry$pred <<- predict(m, newdata = df_qry, na.action = na.exclude)
  #
  #
  #     ggp <- ggplot(df_qry, aes(DATECT, y = df_qry[, input$select_var])) +
  #       geom_point_interactive(aes(data_id = checked, tooltip = checked, colour = df_qry[, input$select_col]), size = 3) +
  #       geom_line(aes(y = df_qry$pred), colour = "red") +
  #       #ylim(0, NA) +
  #       xlab("Date") + ylab(paste("Your variable:", input$select_var)) + ggtitle(paste(input$select_var, "time series")) +
  #       theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
  #
  #     output$plot <- renderggiraph({
  #       x <- girafe(code = print(ggp), width_svg = 6, height_svg = 5)
  #       x <- girafe_options(x, opts_selection(
  #         type = "multiple", css = "fill:#FF3333;stroke:black;"),
  #         opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
  #       x
  #     })
  #   })
  #
  #
  #   # Write data functionality----
  #   observeEvent(input$write_data, {
  #     if(FALSE %in% accumulated_df$reviewed) {
  #       false_list <- accumulated_df[FALSE %in% accumulated_df$reviewed,]
  #       false_list <- false_list%>%
  #         select(variable_names)
  #       false_list <- as.character(false_list$variable_names)
  #       print(paste0("Not all variables have been checked. Please check ",false_list,"."))
  #     } else{
  #       print("Ready to write to database.")
  #     }
  #
  #   })
  #
  #   # Submit change button functionality-----
  #   observeEvent(input$submitchanges,{
  #     shinyjs::show("progress_row")
  #     #by assigning newvar here and not using input$select_var directly I am preventing the progress bar
  #     #being automatically updated when a new variable is selected, but only once submitchanges has been clicked
  #     newvar <- input$select_var
  #     #also adding the progress bar once the replot button has been clicked
  #     output$progressbar <- renderPlot({
  #       #extracting the relevant variables names
  #       #make a dataframe that has FALSE assigned to every variable name
  #       reviewed_df <<- as.data.frame(var_choices())
  #       reviewed_df$reviewed <<- FALSE
  #       #creating a true dataframe, makes a row that sets variable to TRUE is it is in input$select_var
  #       now_true <- reviewed_df %>%
  #         filter(var_choices() == newvar)
  #       now_true$reviewed <- TRUE
  #
  #       #just get the brackets out of the df column name to avoid non-function error.
  #       colnames(reviewed_df) <- c("var_choices", "reviewed")
  #       colnames(now_true) <- c("var_choices", "reviewed")
  #
  #       if(length(accumulated_df) == 0) {
  #         #if the dataframe exists already - replaced the row in reviewed df and assign as accumulated_df
  #         reviewed_df$reviewed[which(reviewed_df$var_choices==now_true$var_choices)] <- now_true$reviewed
  #         accumulated_df <<- reviewed_df
  #       } else{
  #         #if it does exist already, add new variable to accumulated df
  #         accumulated_df$reviewed[which(accumulated_df$var_choices==now_true$var_choices)] <- now_true$reviewed
  #         accumulated_df <<- accumulated_df
  #       }
  #
  #       #some variables do not have to be included in the plot
  #       #we'll make this variable by selection at some point
  #       accumulated_df$reviewed <- factor(accumulated_df$reviewed, levels = c("TRUE","FALSE"))
  #
  #       progress_plot <- ggplot(accumulated_df) +
  #         geom_tile(aes(x= var_choices,y= "",fill = reviewed))+
  #         geom_text(aes(x= var_choices,y= "",label = var_choices),
  #                   color = "white", size =3,position = position_stack(vjust = 1),angle = 90)+
  #         scale_y_discrete(""
  #                          # ,expand = c(0,2)
  #         )+
  #         scale_fill_manual(breaks = c("TRUE", "FALSE"),
  #                           values = c("#2F8C1F", "#EB1A1A"))+
  #         theme(
  #           panel.background = element_blank(),
  #           axis.ticks.y =  element_blank(),
  #           axis.ticks.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_blank()
  #         )
  #       progress_plot
  #     }
  #     ,width = "auto",height = 275
  #     )
  #     disable("submitchanges")
  #   })
})
