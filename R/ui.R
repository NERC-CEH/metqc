# Define UI for the app
ui <- shinyUI(
  navbarPage("Met Data Validation", theme=shinytheme("united"), position = "fixed-top",
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
                                                           shinyjs::disabled(actionButton("replot", label = "Plot graph"))),
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
