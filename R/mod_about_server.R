#' About module server function
#'
#' @noRd
mod_about_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$dl_example <- downloadHandler(
      filename = function(){"MAU_Shiny_demo.zip"},
      content  = function(file){file.copy(system.file("extdata/MAU_Shiny_demo.zip", package = "arenalytics"), file)}
    )

  }) ## END module server function

}
