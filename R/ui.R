# Define UI for the app
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Met Data Validation"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard",icon = icon("dashboard")),
                        menuItem("Information", tabName = "information",icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      useShinyjs(),
                      tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(title = "Database controls", status = "success", solidHeader = TRUE,
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
                                             numericInput("emin", value = 00, label = "Minute", min = 0, max = 59, step = 1)),
                                      sliderInput("intslider", label = "Smoothness (number of knots in cr spline):", min = 1, max = 32, value = 10, step = 1),
                                      uiOutput("select_variables"),
                                      # selectInput("variable_check", "Variables to be checked",
                                      #             choices = c(as.list(var_choices), multiple = TRUE)),
                                      actionButton("retrieve_data", "Retrieve from database"),
                                      actionButton("restart", "Start over")
                                  ),
                                ),
                                hidden(
                                  fluidRow(id = "extracted_data",
                                           box(title = "Extracted data", status = "success", solidHeader = TRUE,
                                               tabsetPanel(id = "plotTabs",
                                                           girafeOutput("interactive_plot"),
                                                           type = "tabs"),
                                               shinyjs::disabled(actionButton("reset", label = "Reset selection")),
                                               shinyjs::disabled(actionButton("delete", label = "Delete selection")),
                                               shinyjs::disabled(actionButton("nochange", label = "No change needed")),
                                               shinyjs::disabled(actionButton("submitchanges", "Submit changes"))
                                           ),
                                           box(title = "Data Preview", status = "success", solidHeader = TRUE,
                                               tableOutput("job_table"),
                                               )
                                  )
                                ),
                        ),
                        tabItem(tabName = "information",
                                h2("Information placeholder"))
                      )
                    )
                    
                    ,busyIndicator(text = "Please wait...", wait = 500)
) #end of navbar page
