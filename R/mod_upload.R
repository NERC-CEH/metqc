#' Upload Module UI
#' @param id shiny id
#' @return UI element for file uploads
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Upload data"),
    fileInput(ns("csv_file"), "Upload your file(s)"),
    fluidRow(
      column(12, actionButton(ns("load_preview"), "Load and preview file(s)", class = "btn-primary")),
      # if users change their mind they can remove file
      column(12, actionButton(ns("remove_file"), "Remove file", class = "btn-danger"))
    ),
    br(), br(),
    uiOutput(ns("file_preview_ui"))
  )
}

#' Upload Module Server
#' @param id shiny id
#' @return reactive list with $data (uploaded table)
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_rv <- reactiveValues(data = NULL)

    observeEvent(input$load_preview, {
      
      # If no file selected, message will appear
      if (is.null(input$csv_file)) {
        showNotification("No file selected. Please choose a file to upload.", type = "warning")
        return()
      }
      
      #req(input$csv_file)
      
      # get the file path
      file_path <- input$csv_file$datapath

      # determine file extension
      file_ext  <- tools::file_ext(file_path)
      
      data_rv$data <- tryCatch({
        # check the first 10 lines
        header_lines <- readLines(file_path, n = 10)
        
        # check if the file looks like a Campbell datalogger file
        is_campbell <- any(grepl("^\"TOA5", header_lines)) || any(grepl("^TOA5", header_lines))
        
        if (is_campbell) {
          # find where the header starts (usually second line)
          header_line <- which.max(grepl("TIMESTAMP", header_lines))
          if (header_line == 0) header_line <- 2
          
          message("Detected Campbell Scientific .dat file")
          
          importCSdata(file_path)
          
        } else if (tolower(file_ext) %in% c("csv")) {
          readr::read_csv(
            file_path, 
            show_col_types = FALSE, 
            guess_max = 10000
          ) |> as.data.frame()
          
          # it is useful to convert to lower case as sometimes these files come as .TXT .DAT
        } else if (tolower(file_ext) %in% c("dat", "txt")) {
          
          ###### checks the delimiter type for columns in the txt or dat file
          raw_line <- readLines(file_path, n = 1)
          guessed_delim <- if (grepl("\t", raw_line)) "\t"
          else if (grepl(";", raw_line)) ";"
          else if (grepl("\\s+", raw_line)) " "
          else ","
          
          readr::read_delim(
            file_path, delim = guessed_delim, 
            show_col_types = FALSE, 
            guess_max = 10000
          ) |> as.data.frame()
          
        } else {
          stop("Unsupported file type. Please upload a .csv, .dat, or .txt file.")
        }
      },
      error = function(e) {
        showNotification(paste("❌ Failed to read file:", e$message), type = "error")
        return(NULL)
      })
      
      req(data_rv$data)
      showNotification("✅ File loaded successfully!", type = "message")
    })
    
    # if users want to remove or change the file
    observeEvent(input$remove_file, {
      data_rv$data <- NULL

      # reset the fileInput control UI using shinyjs (ensure useShinyjs() in UI)
      try({
        shinyjs::reset("csv_file")
      }, silent = TRUE)
      
      showNotification("🗑️ File removed. You can upload a new one.", type = "warning")
    })
    
    # part of the UI which gives a preview of the uploaded data
    output$file_preview_ui <- renderUI({
      req(data_rv$data)
      tagList(
        h4("Preview of uploaded data"),
        DT::dataTableOutput(ns("data_preview"))
      )
    })
    
    # data preview
    output$data_preview <- DT::renderDataTable({
      req(data_rv$data)
      DT::datatable(head(data_rv$data, 10), options = list(scrollX = TRUE))
    })
    
    # Returns the uploaded data
    return(
      reactive({
        list(data = data_rv$data)
      })
    )
  })
}
