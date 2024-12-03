library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(purrr)

#Define UI
# ui <- fluidPage(
#   titlePanel("Convert XLSX to BED Files"),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("fileInput", "Upload XLSX Files",
#                 multiple = TRUE,
#                 accept = c(".xlsx")),
#       helpText(
#         "Note: The uploaded XLSX files must contain at least the following columns: ",
#         strong("'chrom', 'start', and 'end'."),
#         "Additional columns will be ignored."
#       ),
#       actionButton("process", "Convert to BED")
#     ),
#     mainPanel(
#       uiOutput("downloadUI")
#     )
#   )
# )


# Define UI (Updated)
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),  # Add a theme for improved appearance
  titlePanel("Convert XLSX to BED Files"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),  # Add a header for the instructions section
      helpText(
        "Upload one or more XLSX files for conversion. Each file must contain the following columns:",
        tags$ul(
          tags$li(strong("'chrom'")),
          tags$li(strong("'start'")),
          tags$li(strong("'end'"))
        ),
        "Additional columns will be ignored."
      ),
      fileInput("fileInput", "Select XLSX Files",
                multiple = TRUE,
                accept = c(".xlsx")),
      actionButton("process", "Convert to BED", class = "btn btn-primary"),  # Styled button
      tags$hr(),  # Horizontal line for separation
      h5("Example File"),
      tags$a(href = "example.xlsx", "Download Example XLSX File", target = "_blank"),  # Link to example file
      width = 4  # Adjust sidebar width for better layout
    ),
    mainPanel(
      h3("Download Results"),
      uiOutput("downloadUI"),
      tags$hr(),  # Horizontal line for separation
      h5("Note: Ensure your files follow the specified format for successful conversion."),
      width = 8  # Adjust main panel width
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  processed_files <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    req(input$fileInput)
    
    files <- input$fileInput$datapath
    filenames <- input$fileInput$name
    output_dir <- tempdir()
    
    validation_failed <- FALSE

    output_files <- files |>
      set_names(filenames) |>  # Create named list for better debugging
      imap(function(file, filename) {
        tryCatch({
          # Read the file
          data <- read_excel(file)
          
          # Validate required columns
          required_cols <- c("chrom", "start", "end")
          if (!all(required_cols %in% colnames(data))) {
            validation_failed <<- TRUE
            stop(paste("File", filename, "is missing required BED columns"))
          }
          
          # Select relevant columns and convert to BED format
          bed_data <- data |>
            select(all_of(required_cols)) |>
            arrange(chrom, start, end)
          
          # Save as BED file using readr::write_delim
          bed_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(filename), ".bed"))
          write_delim(bed_data, bed_file, delim = "\t", col_names = FALSE)
          bed_file
        }, error = function(e) {
          showNotification(paste("Error processing", filename, ":", e$message), type = "error")
          NULL
        })
      }) |> 
      compact() %>%   # Remove NULL entries for failed files
      list_c()
    
    if (validation_failed || length(output_files) == 0) {
      processed_files(NULL)
      return()
    }

    # Zip all valid BED files
    zip_file <- file.path(output_dir, "output_files.zip")
    zip(zip_file, output_files, flags = '-jr9X')
    processed_files(zip_file)
    
  })
  
  output$downloadUI <- renderUI({
    req(processed_files())
    downloadLink("downloadData", "Download ZIP Archive of BED Files")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "converted_bed_files.zip"
    },
    content = function(file) {
      file.copy(processed_files(), file)
    }
  )
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     "converted_bed_files.zip"
  #   },
  #   content = function(file) {
  #     zip_path <- processed_files()
  #     req(zip_path)  # Ensure zip file exists
  #     file.copy(zip_path, file)  # Copy the zip file to the user's download location
  #     on.exit(file.remove(zip_path))  # Remove the zip file after downloading
  #   }
  # )
  
}

# Run the app
shinyApp(ui = ui, server = server)
