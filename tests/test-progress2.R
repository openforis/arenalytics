# library(shiny)
# library(shinyWidgets)
# library(shinyjs)
#
# filepath <- c("categories.rds", "chain_summary.json", "OLAP_bamboo.csv", "OLAP_deadwood_area.csv",
#           "OLAP_regeneration.csv",  "OLAP_stump.csv", "OLAP_tree.csv", "ReportDimensions.csv",
#           "SchemaSummary.csv", "taxonomies.rds")
# # rv <- list()
# # rv$data_list <- list()
#
# load_files <- function(.file = filepath, rv, session) {
#
#   config_files <- c("chain_summary.json", "SchemaSummary.csv", "ReportDimensions.csv")
#   cat_files <- c("categories.rds", "taxonomies.rds")
#   OLAP_files <- .file |> stringr::str_subset("OLAP_")
#
#   ## Not needed but just in case
#   #if (!all(sort(c(config_files, cat_files, OLAP_files)) == sort(.file))) stop("Missing files when reading")
#
#   i <- 0
#   ## Read config files
#   file_name <- "chain_summary.json"
#   if (file_name %in% .file) {
#     obj_name <- stringr::str_remove(file_name, "\\..*")
#     rv$data_list[[obj_name]] <- as.data.frame(matrix(rnorm(1e6 * 20), ncol = 20))
#     log <- paste("Loaded", file_name)
#   } else {
#     log <- paste("NOT FOUND:", file_name)
#   }
#   timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#   message(paste0("[", timestamp, "] ",  log))
#   i <- i + 1
#   shinyWidgets::updateProgressBar(
#     session = session,
#     id = "file_progress",
#     value = round(i/length(.file) * 100)
#   )
#
#   file_name <- "SchemaSummary.csv"
#   if (file_name %in% .file) {
#     obj_name <- stringr::str_remove(file_name, "\\..*")
#     rv$data_list[[obj_name]] <- as.data.frame(matrix(rnorm(1e6 * 20), ncol = 20))
#     log <- paste("Loaded", file_name)
#   } else {
#     log <- paste("NOT FOUND:", file_name)
#   }
#   timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#   message(paste0("[", timestamp, "] ",  log))
#   i <- i + 1
#   shinyWidgets::updateProgressBar(
#     session = session,
#     id = "file_progress",
#     value = round(i/length(.file) * 100)
#   )
#
#
# }
#
# ui <- fluidPage(
#   useShinyjs(),
#   titlePanel("Simulate Loading Heavy Files"),
#   shinyWidgets::progressBar(id = "file_progress", value = 0, total = 100, display_pct = TRUE),
#   br(),
#   tags$div(
#     id = "console",
#     style = "height: 120px; overflow-y: auto; background-color:#f7f7f7; font-family:monospace;"
#     ),
#   #verbatimTextOutput("console"),
#   br(),
#   actionButton("start", "Start Loading Files")
# )
#
# server <- function(input, output, session) {
#   #files <- paste0("file_", 1:5, ".csv")
#   rv <- reactiveValues(log = "", data_list = list())
#
#   observeEvent(input$start, {
#     shinyjs::html("console", "")  # clear on restart
#     shinyWidgets::updateProgressBar(
#       session = session,
#       id = "file_progress",
#       value = 0
#     )
#     rv$log <- ""
#     rv$data_list <- list()
#
#     withCallingHandlers(
#       {
#         load_files(.file = c("chain_summary.json", "SchemaSummary.csv"), rv = rv, session = session)
#         # load_files(files, 1, rv, session)
#         # load_files(files, 2, rv, session)
#         # load_files(files, 3, rv, session)
#         # load_files(files, 4, rv, session)
#         # load_files(files, 5, rv, session)
#       },
#       message = function(m) {
#         shinyjs::html(id = "console", html = paste0(m$message, '<br>'), add = TRUE)
#         # rv$log <- paste(rv$log, conditionMessage(m))
#         invokeRestart("muffleMessage")
#       }
#     )
#   })
#
#   output$console <- renderText({ rv$log })
# }
#
# shinyApp(ui, server)
