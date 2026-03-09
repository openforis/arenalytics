#' Tool module UI function
#'
#' @noRd
mod_tool_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ######
  ##

  ## \_ Sidebar ======

  ## \___ Acc1: Load data ------
  ac1 <- accordion_panel(
    title = i18n$t("Load OpenForis OLAP ZIP file"),
    icon = bsicons::bs_icon("1-circle"),
    value = ns("ac1"),

    ## Accordion content
    ## Input ZIP file
    div(
      p(
        "The dashboard requires a ZIP file that is produced by running the processing
        chain from your OpenForis Arena survey in Rstudio (local or online)."
      ),
      p("Once this file is produced, upload here:"),
      fileInput(
        inputId = ns("load_zip"),
        accept = ".zip",
        label = NULL
      ),
      ## TEST alternative shinyFiles
      ## !!! Package old and not maintained
      # br(),
      # p(
      #   "The dashboard requires a ZIP file that is produced by running the processing
      #   chain from your OF Arena survey in Rstudio (local or online). Once this file
      #   is produced, extract the data and point to its location here, so that the app
      #   can automatically find the data and structure files:"
      # ),
      #
      # shinyFiles::shinyDirButton(
      #   id = ns('path_to_folder'),
      #   label = 'Select a folder',
      #   title = 'Please select a folder',
      #   FALSE
      # )
    ),

    ## MESSAGES
    div(
      id = ns("msg_no_file"),
      "No data uploaded.",
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id = ns("msg_file_ok"),
      "Data structure OK.",
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id = ns("msg_file_error"),
      "Incorrect data uploaded.",
      verbatimTextOutput(ns("file_error_detail")),
      class = "text-danger",
      style = "font-style: italic;"
    )),

    ## ACTION BUTTON
    div(
      shinyjs::disabled(
        actionButton(
          inputId = ns("btn_load_data"),
          label = "Load data"
        )
      ),
      style = "margin-top: 1rem;"
    )

  ) ## END accordion_panel()


  ## \___ Acc2: Read data ======
  ac2 <-  accordion_panel(
    title = i18n$t("Data visualization"),
    icon = bsicons::bs_icon("2-circle"),
    value = ns("ac2"),

    ## Content
    ## \_____ Initial text ------
    div_data_init <- div(
      id = ns("readdata_accordion_msg"),
      bsicons::bs_icon("arrow-up"), " Start with uploading your data and run: '",
      i18n$t("Load data"), "'.",
      class = "text-warning",
      style = "font-style: italic;"
    ),

    ## \___




  )

  ## \___ Acc3: Test crosstalk -------------------------------------------------
  ac3 <-  accordion_panel(
    title = i18n$t("Test crosstalk"),
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac3"),

    ## Content
    h4("coming soon"),
    div(
      id = ns("msg_no_check"),
      p("Suspendisse suscipit accumsan sagittis. Aliquam ut scelerisque mauris."),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    selectInput(ns("species"), "Species", levels(datasets::iris$Species), multiple = TRUE),
    sliderInput(
      ns("petal_length"), "Petal Length",
      min = min(datasets::iris$Petal.Length), max = max(datasets::iris$Petal.Length),
      value = c(min(datasets::iris$Petal.Length), max(datasets::iris$Petal.Length))
    ),
    div(
      actionButton(
        inputId = ns("btn_panel3"),
        label = "To Test Panel"
      ),
      style = "margin-top: 1rem;"
    )
  )

  ## \_ Panels ======
  ## \___ Panel: data ======
  ## Data descriptors


  ## \___ Panel: analysis ==========
  ## Statistical analysis


  ## \___ Panel: test -----

  ## \_____ Value boxes -----
  vb1 <- value_box(
    title = "Sepal Mean length",
    value = htmlOutput(ns("vb_seplen_mean")),
    showcase = bsicons::bs_icon("calendar3", size = "40px"),
    theme = "primary"
  )

  vb2 <- value_box(
    title = "Sepal Mean Width",
    value = htmlOutput(ns("vb_sepwid_mean")),
    showcase = bsicons::bs_icon("pin-map", size = "40px"),
    theme = "secondary"
  )

  vb3 <- value_box(
    title = "Number of Species",
    value = htmlOutput(ns("vb_nb_species")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme = "warning"
  )


  ## \_____ cards -----
  card1 <- card(
    full_screen = T,
    h5(i18n$t("Scatter 1")),
    d3scatter::d3scatterOutput(ns("scatter1"))
  )

  card2 <- card(
    full_screen = T,
    h5(i18n$t("Scatter 2")),
    d3scatter::d3scatterOutput(ns("scatter2"))
  )

  card3 <- card(
    h5(i18n$t("Summary of selected data")),
    verbatimTextOutput(ns("summary"))
  )



  ##
  ## UI Layout #################################################################
  ##

  tagList(

    h2(i18n$t("TOOL")),

    br(),

    navset_card_tab(
      id = ns("tool_tabs"),

      ## \_ Sidebar =======

      sidebar = sidebar(
        width = "300px",
        accordion(
          open = TRUE,
          multiple = TRUE,
          ac1, ac2, ac3
        )
      ),

      ## Spacer to right align menu items
      nav_spacer(),

      ## \_ Panel Data Layout ======

      nav_panel(
        title = i18n$t("Panel 1"),
        value = "tab1",
        icon = icon("circle-check"),
        ## CONTENT
      ),

      ## \_ Panel analysis layout =======

      nav_panel(
        title = i18n$t("Panel 2"),
        value = "tab2",
        icon = icon("chart-simple"),
        ## CONTENT
      ),

      ## \_ Panel test layout============

      nav_panel(
        title = i18n$t("Test crosstalk"),
        value = "tab3",
        icon = icon("magnifying-glass"),
        ## CONTENT
        ## Value boxes
        div(
          id = ns("vb_section"),
          layout_column_wrap(
            #width = "200px",
            fill = FALSE,
            vb1, vb2, vb3
          )
        ),
        ## Cards
        div(
          id = ns("card_section"),
          layout_column_wrap(card1, card2, width = "300px"),
          card3
        )

      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
