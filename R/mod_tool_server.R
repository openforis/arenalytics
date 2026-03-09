#' Tool module server function
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## 1. Load data ######
    ##

    ## !!! FOR TESTING ONLY
    # rv <- list()
    # rv$inputs <- list()
    # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics")
    # rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)
    ## !!!


    # observeEvent(input$load_zip, {
    #
    #   rv$inputs$path_zip <- input$load_zip
    #   rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)
    #
    #   if(rv$inputs$check_zip$all_ok) {
    #     shinyjs::hide("msg_no_file")
    #     shinyjs::show("msg_file_ok")
    #     shinyjs::hide("msg_file_error")
    #     shinyjs::enable("btn_load_data")
    #   } else {
    #     shinyjs::hide("msg_no_file")
    #     shinyjs::hide("msg_file_ok")
    #     shinyjs::show("msg_file_error")
    #     shinyjs::disable("btn_load_data")
    #   }
    #
    # })
    #
    # output$file_error_detail <- renderPrint({
    #   req(rv$inputs$check_zip)
    #   if(!rv$inputs$check_zip$all_ok) data.frame(res = unlist(rv$inputs$check_zip))
    # })

    ## Sidebar part 2 ######
    observe({
      rv$rv1$user_iris <- datasets::iris |> # data("iris", envir = environment())
        dplyr::filter(is.null(input$species) | .data$Species %in% input$species) |>
        dplyr::filter(
          .data$Petal.Length >= min(input$petal_length),
          .data$Petal.Length<= max(input$petal_length)
        )
      rv$rv1$shared_iris <- crosstalk::SharedData$new(rv$rv1$user_iris)
    })

    ## Sidebar part 2 ######
    observeEvent(input$btn_panel2, {
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab2"))
    })



    ## Main panels ######

    ## Virtual boxes ======

    output$vb_seplen_mean <- renderUI({
      fct_mean(.df = rv$rv1$user_iris, .colnum = .data$Sepal.Length, .rounding = 1)
    })

    output$vb_sepwid_mean <- renderUI({
      fct_mean(.df = rv$rv1$user_iris, .colnum = .data$Sepal.Width, .rounding = 1)
    })

    output$vb_nb_species <- renderUI({
      length(unique(rv$rv1$user_iris$Species))
    })

    ## Panel cards ======
    output$scatter1 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$rv1$shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
    })

    output$scatter2 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$rv1$shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width = "100%")
    })

    output$summary <- renderPrint({
      df <- rv$rv1$shared_iris$data(withSelection = TRUE) |>
        dplyr::filter(.data$selected_ | is.na(.data$selected_)) |>
        dplyr::mutate(selected_ = NULL)

      cat(nrow(df), "observation(s) selected\n\n")
      summary(df)
    })

  })

}

