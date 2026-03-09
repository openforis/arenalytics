library(shiny)
library(shinyWidgets)
library(lubridate)
library(later)

ui <- fluidPage(
  titlePanel("Simulate Loading Heavy Files"),

  shinyWidgets::progressBar(
    id = "file_progress",
    value = 0,
    total = 100,
    display_pct = TRUE
  ),

  br(),
  verbatimTextOutput("console"),
  br(),
  actionButton("start", "Start Loading Files")
)

server <- function(input, output, session) {

  files <- paste0("file_", 1:5, ".csv")  # pretend file names

  rv <- reactiveValues(
    i = 0,
    log = "",
    running = FALSE,
    data_list = list()
  )

  observeEvent(input$start, {

    isolate({
      rv$i <- 0
      rv$log <- ""
      rv$running <- TRUE
      rv$data_list <- list()  # clear previous data
    })

    process_next <- function() {
      isolate({
        if (!rv$running || rv$i >= length(files)) {
          rv$running <- FALSE
          return()
        }

        rv$i <- rv$i + 1
        file_name <- files[rv$i]

        # --- Simulate loading a heavy file by creating a big data.frame ---
        data <- as.data.frame(matrix(rnorm(1e6 * 10), ncol = 10))
        rv$data_list[[file_name]] <- data  # store in reactiveValues

        # Add timestamped log
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        rv$log <- paste(rv$log, paste0("[", timestamp, "] Loaded ", file_name), sep = "\n")

        # Update progress bar
        progress_pct <- round(rv$i / length(files) * 100)
        shinyWidgets::updateProgressBar(
          session = session,
          id = "file_progress",
          value = progress_pct
        )
      })

      # Schedule next file processing in 0.5-1s to allow UI update
      later(process_next, 0.5)
    }

    process_next()  # start recursive loading
  })

  # Console output
  output$console <- renderText({
    rv$log
  })

}

shinyApp(ui, server)
