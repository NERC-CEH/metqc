# Adding UKCEH shiny theme
devtools::source_url("https://github.com/NERC-CEH/UKCEH_shiny_theming/blob/main/theme_elements.R?raw=TRUE")

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
                                               # tabsetPanel(id = "plotTabs",
                                               #             type = "tabs",
                                               #             tabPanelBody(
                                               #               girafeOutput("interactive_plot")  
                                               #             )
                                               
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
                    ), busy_indicator(text = "Please wait...", wait = 500)
) #end of navbar page
