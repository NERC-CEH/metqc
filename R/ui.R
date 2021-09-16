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
                                  box(height = 475, title = "Database controls", status = "success", solidHeader = TRUE,
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
                                  #uiOutput("tabs")
                                  
                                  # box(height = 475, title = "Specify variable", status = "success", solidHeader = TRUE,
                                  #     uiOutput("submit_info"),
                                  #     uiOutput("var_filter"),
                                  #     uiOutput("var_filter_col"),
                                  #     uiOutput("landuse_filter"),
                                  #     uiOutput("var_info"),
                                  #     shinyjs::disabled(actionButton("replot", label = "Plot graph"))
                                  # )
                                ),
                                fluidRow(
                                  tabsetPanel(id = "plotTabs", type = "pills")
                                ),
                                hidden(
                                  fluidRow(id = "plotted_data",
                                           box(width = 12, title = "Plotted data", status = "success", solidHeader = TRUE,
                                               shinyjs::disabled(actionButton("reset", label = "Reset selection")),
                                               shinyjs::disabled(actionButton("delete", label = "Delete selection")),
                                               shinyjs::disabled(actionButton("nochange", label = "No change needed")),
                                               shinyjs::disabled(actionButton("submitchanges", "Submit changes")),
                                               girafeOutput("plot")
                                           )
                                  ),
                                  hidden(
                                    fluidRow(id = "progress_row",
                                             box(width = 12, title = "Review progress", status = "success", solidHeader = TRUE,
                                                 column(width = 12,
                                                        plotOutput("progressbar", width = "100%")),
                                                 column(width = 6,
                                                        div(DT::dataTableOutput("summarytable"),style = "width:75%")),
                                                 column(width = 4,
                                                        actionButton("write_data", "Write to database"))
                                             )
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "information",
                                h2("Information placeholder"))
                      )
                    )
                    
                    ,busyIndicator(text = "Please wait...", wait = 500)
) #end of navbar page
