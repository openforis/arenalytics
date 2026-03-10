#' Tool module server function
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## Output and events: accordions ######
    ##

    ## \_ Acc1: Load data =====

    ## !!! FOR TESTING ONLY
    # rv <- list()
    # rv$inputs <- list()
    # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo_broken.zip", package = "arenalytics")
    # rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)
    ## !!!

    observeEvent(input$load_zip, {

      rv$inputs$path_zip <- input$load_zip$datapath
      rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)

      if(rv$inputs$check_zip$all_ok) {
        shinyjs::hide("msg_no_file")
        shinyjs::show("msg_file_ok")
        shinyjs::hide("msg_file_error")
        shinyjs::enable("btn_read_data")
      } else {
        shinyjs::hide("msg_no_file")
        shinyjs::hide("msg_file_ok")
        shinyjs::show("msg_file_error")
        shinyjs::disable("btn_read_data")
      }

    })

    output$file_error_detail <- renderPrint({
      req(rv$inputs$check_zip)
      # if(!rv$inputs$check_zip$all_ok) data.frame(res = unlist(rv$inputs$check_zip))
      if(!rv$inputs$check_zip$all_ok) {
        cat("Missing files:\n", paste(rv$inputs$check_zip$missing, collapse = ", "))
      }
    })

    ## \_ Acc2: dataviz ======

    ## \__ Read data ------
    observeEvent(input$btn_read_data, {

      ## Hide/Show panels
      shinyjs::hide("readdata_accordion_msg")
      shinyjs::hide("readdata_panel_msg")
      shinyjs::show("readdata_panel_progress")
      shinyjs::hide("readdata_panel_insights")
      #shinyjs::show("readdata_panel_insights")

      ## Reset progress
      rv$inputs$data <- NULL
      rv$inputs$data_ok <- FALSE
      shinyjs::html("readdata_console", "")  # clear on restart
      shinyWidgets::updateProgressBar(
        session = session,
        id = "readdata_progress",
        value = 0
      )
      shinyjs::disable("btn_data_insights")

      Sys.sleep(0.4)

      ## Read data and update progress
      rv$inputs$data <- withCallingHandlers(
        {
          fct_readzip(
            .path = rv$inputs$path_zip, .pb_session = session, .pb_id = "readdata_progress"
          )
        },
        message = function(m) {
          shinyjs::html(id = "readdata_console", html = paste0(m$message, '<br>'), add = TRUE)
          invokeRestart("muffleMessage")
        }
      )
      ## Make insight button visible (to be improved)
      if (!is.null(rv$inputs$data)) rv$inputs$data_ok <- TRUE

    })

    ## \___ Show insight button ------
    observe({
      req(rv$inputs$data_ok)
      if (rv$inputs$data_ok) {
        shinyjs::enable("btn_data_insights")
      } else {
        shinyjs::disable("btn_data_insights")
      }
    })

    ## \___ Show insights ------
    observeEvent(input$btn_data_insights, {
      req(rv$inputs$data)

      ## Hide progress and show insights
      shinyjs::hide("readdata_panel_progress")
      shinyjs::show("readdata_panel_insights")
    })



    ## \_ Acc3 : Test crosstalk ------
    observe({
      rv$test$user_iris <- datasets::iris |> # data("iris", envir = environment())
        dplyr::filter(is.null(input$species) | .data$Species %in% input$species) |>
        dplyr::filter(
          .data$Petal.Length >= min(input$petal_length),
          .data$Petal.Length<= max(input$petal_length)
        )
      rv$test$shared_iris <- crosstalk::SharedData$new(rv$test$user_iris)
    })

    observeEvent(input$btn_panel3, {
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_test"))
    })



    ## Outputs: Main panels ######

    ## \_ Data Panel ====

    ## \___ Title ------
    output$readdata_insight_title <- renderText({
      req(rv$inputs$data)
      paste(
        rv$inputs$data$chain_summary$surveyName,
        rv$inputs$data$chain_summary$surveyLabel,
        sep = " - "
      )
    })

    output$readdata_insight_subtitle <- renderText({
      req(rv$inputs$data)
      paste(names(rv$inputs$data$chain_summary), collapse = "\n")
    })

    ## \_ Test crosstalk ====
    ## \___ Virtual boxes ----

    output$vb_seplen_mean <- renderUI({
      fct_mean(.df = rv$test$user_iris, .colnum = .data$Sepal.Length, .rounding = 1)
    })

    output$vb_sepwid_mean <- renderUI({
      fct_mean(.df = rv$test$user_iris, .colnum = .data$Sepal.Width, .rounding = 1)
    })

    output$vb_nb_species <- renderUI({
      length(unique(rv$test$user_iris$Species))
    })

    ## \___ Panel cards ------
    output$scatter1 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$test$shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
    })

    output$scatter2 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$test$shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width = "100%")
    })

    output$summary <- renderPrint({
      df <- rv$test$shared_iris$data(withSelection = TRUE) |>
        dplyr::filter(.data$selected_ | is.na(.data$selected_)) |>
        dplyr::mutate(selected_ = NULL)

      cat(nrow(df), "observation(s) selected\n\n")
      summary(df)
    })

  })

}

