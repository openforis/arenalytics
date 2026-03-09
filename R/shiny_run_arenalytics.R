#' TITLE
#'
#' @description Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec vehicula
#'              imperdiet finibus. Sed urna sem, molestie at sodales non, viverra vitae
#'              mauris. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque
#'              convallis tristique mauris, nec volutpat ligula dapibus eget.
#'
#' @param ... arguments to pass to shinyApp
#'
#' @import shiny
#' @import bslib
#' @importFrom rlang .data
#'
#' @examples
#' if (interactive()) {
#'
#' shiny_run_APPNAME()
#'
#' }
#'
#' @export
shiny_run_arenalytics <- function(...) {

  ##
  ## GLOBAL ####################################################################
  ##

  ## + Initiate translation ====================================================

  ## !!! In a package the translation folder needs to be directed to the package location
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("assets/translations.json", package = "arenalytics")
  )
  i18n$set_translation_language('en')

  ## + Javascript ===============================
  ## Script moved to files in inst/assets and called in the header


  ## + UI Elements =============================================================

  ## App title with logo (as function because i18n$t() needs to be inside page_navbar())
  app_title <- function(
    .title = "Analytical Dashboard for OpenForis Arena",
    .alt = "Arena Dashboard",
    .logo = "assets/logo.png",
    .logo_height = '40px' ## CANNOT EXCEED 40px to avoid resizing issues (minor)
    ) {
    div(
      tags$a(
        href = "./", ## Send back to home page
        alt = .alt,
        tags$img(src = .logo, height = .logo_height),
        .noWS = "before-end"
      ),
      i18n$t(.title),
      style = 'display:inline;font-color: black !important; font-family: "Inter"'
    )
  }

  app_window_title <- "Arena Dashboard - OpenForis"

  ## App colors
  app_primary_color   <- "#4991B0"
  app_secondary_color <- "#77AB16"

  ## App theme
  app_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "yeti",
    base_font = bslib::font_collection(
      "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue",
      "Arial", "Noto Sans", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
      "Segoe UI Symbol","Noto Color Emoji"
    ),
    code_font = bslib::font_google("Fira Code"),
    heading_font = bslib::font_google("Inter"),
    #heading_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Lato", "Inter", "Arial"),
    primary = app_primary_color,
    secondary = app_secondary_color,
  )

  ## Dropdown list for language selection
  language_selector <- div(
    style = "margin-top: 4px;",
    shinyWidgets::pickerInput(
      inputId = "language",
      label = NULL,
      #choices = c("en"),
      #choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN')),
      choices = c("en", "fr", "sp"),
      choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-fr"></i> FR', '<i class="fi fi-es"></i> ES')),
      selected = "en",
      width = "auto"
    )
  )

  ## Footer div
  app_footer <- div(
    class = "container footer text-center",
    tags$small(
      "(c) 2026 Arena Analytics - Developed by the",
      tags$strong("OpenForis team"),
      " - With the support of the Aim4Forest programme"
    )
  )


  ##
  ## UI ########################################################################
  ##

  ui <- bslib::page_navbar(

    ## + Navbar setup ------
    id = "navbar",
    title = app_title(),
    window_title = app_window_title,
    theme = app_theme,
    navbar_options = navbar_options(
      bg = "#f8f9fa",
      position = "fixed-top",
      class = "navbar-expand-md" ## "navbar-expand-sm"
    ),
    fillable = FALSE, ## Not needed for now, make a tab fill the whole browser, cool for leaflets
    # inverse = FALSE, ## Not working well with yeti, overridden in assets/styles.css

    ## + Header scripts ------
    header = shiny::tagList(
      shiny::withMathJax(),
      shinyjs::useShinyjs(),
      shinyWidgets::useSweetAlert(),
      shiny.i18n::usei18n(i18n),
      tags$head(
        ## JS custom Handler to updateTabsetPanel()
        tags$script(src = "assets/js_activate_tab.js"),
        ## CSS Style
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css"),
        ## Favicon
        tags$link(rel="icon", type="image/png", href="/assets/favicon/favicon-96x96.png", sizes="96x96"),
        tags$link(rel="icon", type="image/svg+xml", href="/assets/favicon/favicon.svg"),
        tags$link(rel="shortcut icon", href="/assets/favicon/favicon.ico"),
        tags$link(rel="apple-touch-icon", sizes="180x180", href="/assets/favicon/apple-touch-icon.png"),
        tags$meta(name="apple-mobile-web-app-title", content="Arena Analytics"),
        tags$link(rel="manifest", href="/assets/favicon/site.webmanifest")
      ),
      ## HTML Dependency - flag icons used for translation selector
      htmltools::htmlDependency(
        name = "flag-icons",
        version = "6.6.6",
        src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"),
        stylesheet = "css/flag-icons.min.css"
      )
    ),
    footer = app_footer,

    ## Panels ------
    nav_spacer(), ## align menu to the right

    nav_panel(
      title = i18n$t("Home"),
      value = "home",
      #icon = icon("campground"),
      mod_home_UI("tab_home", i18n = i18n)
    ),

    nav_panel(
      title = i18n$t("Tool"),
      value = "tool",
      #icon = icon("mug-hot"),
      mod_tool_UI("tab_tool", i18n = i18n)
    ),

    nav_panel(
      title = i18n$t("About"),
      value = "about",
      #icon = icon("info"),
      mod_about_UI("tab_about", i18n = i18n)
    ),

    nav_item(language_selector)

  ) ## END page_navbar



  ##
  ## Server ####################################################################
  ##

  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules ####
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      inputs  = reactiveValues(),
      rv2     = reactiveValues(),
      actions = reactiveValues(),
      test    = reactiveValues()
    )

    ## Save language value to show/hide divs with shinyjs
    r_lang <- reactive({ input$language })

    ## + Module server functions ####
    mod_home_server("tab_home", rv = rv)

    mod_tool_server("tab_tool", rv = rv)

    mod_about_server("tab_about", rv = rv)

    ## + Trans modules events ####
    observeEvent(input$language, {
      shiny.i18n::update_lang(language = input$language)
    })

    observeEvent(rv$actions$to_tool, {
      nav_select(id = "navbar", selected = "tool")
    })

    observeEvent(rv$actions$to_about, {
      nav_select(id = "navbar", selected = "about")
    })


  } ## END server

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function
