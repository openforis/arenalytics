library(shiny)
library(shinyWidgets)
library(dplyr)

ui <- fluidPage(

  fileInput("datafile","Upload data"),
  fileInput("labelfile","Upload label table"),

  pickerInput(
    "column",
    "Select variable",
    choices = NULL
  ),

  tableOutput("preview")
)

server <- function(input, output, session){

  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })

  labels <- reactive({
    req(input$labelfile)
    read.csv(input$labelfile$datapath)
  })

  observe({

    req(data(), labels())

    lab <- labels()

    # Named vector: label shown -> column returned
    choices <- setNames(lab$column, lab$label)

    updatePickerInput(
      session,
      "column",
      choices = choices
    )

  })

  output$preview <- renderTable({

    req(input$column)

    data() %>%
      select(category, all_of(input$column))

  })

}

shinyApp(ui, server)
