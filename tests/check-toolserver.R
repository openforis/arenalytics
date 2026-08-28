#
#
# devtools::load_all()
#
# ## Recreate reactive values and inputs from the app ######
# rv <- list(
#   inputs   = list(),
#   insights = list(),
#   # ct       = list(),
#   analysis = list(),
#   actions  = list()
# )
# rv
#
# input <- list(load_zip = list())
#
# input$analysis_sel_entity <- "tree"
# rv$analysis$dims_sel <- c("cluster_district", "plot_forest_type")
#
# input$analysis_p_value      <- 0.9
# input$analysis_compute_mode <- "safe"
# input$analysis_lonely_psu   <- "adjust"
#
#
# ## Mini functions ######
# replace_dim_labels <- function(df, dim_meta, categories, lang = "en", taxonomies = list()) {
#   label_col <- paste0("label_", lang)
#   dim_cols  <- intersect(
#     dplyr::filter(dim_meta, .data$report_type == "dimension") |> dplyr::pull("name"),
#     names(df)
#   )
#   purrr::reduce(dim_cols, \(acc, col) {
#     cat_name <- dim_meta |>
#       dplyr::filter(.data$name == col) |>
#       dplyr::pull("categoryName") |>
#       dplyr::first()
#     if (is.na(cat_name) || !nzchar(cat_name)) return(acc)
#
#     ## Taxonomy dimensions: use scientific_name instead of a category label
#     taxo_tbl <- taxonomies[[cat_name]]
#     if (!is.null(taxo_tbl) && "scientific_name" %in% names(taxo_tbl)) {
#       lookup <- stats::setNames(
#         as.character(taxo_tbl$scientific_name),
#         as.character(taxo_tbl$code)
#       )
#       return(dplyr::mutate(acc, !!col := dplyr::coalesce(
#         unname(lookup[as.character(.data[[col]])]),
#         as.character(.data[[col]])
#       )))
#     }
#
#     cat_tbl <- categories[[cat_name]]
#     if (is.null(cat_tbl)) return(acc)
#     lbl_col <- if (label_col %in% names(cat_tbl)) label_col else "label"
#     lookup  <- stats::setNames(
#       as.character(cat_tbl[[lbl_col]]),
#       as.character(cat_tbl$code)
#     )
#     dplyr::mutate(acc, !!col := dplyr::coalesce(unname(lookup[as.character(.data[[col]])]),
#                                                 as.character(.data[[col]])))
#   }, .init = df)
# }
#
#
# ## Load data ######
# input$load_zip$datapath <- "inst/extdata/MAU_Shiny_demo.zip"
#
# ## +++
# tmp_out <- fct_readzip(
#   .path = rv$inputs$pathzip
# )
# ## +++
#
# for (nm in names(tmp_out)) {
#   rv$inputs[[nm]] <- tmp_out[[nm]]
# }
#
# ## Get entity names and labels ######
# rv$insights$entities  <- names(rv$inputs$data) |>
#   stringr::str_subset(rv$inputs$checkzip$entity_prefix) |>
#   stringr::str_remove(rv$inputs$checkzip$entity_prefix)
#
# entity_lang <- rv$inputs$data$schema_summary |>
#   dplyr::as_tibble() |>
#   dplyr::filter(.data$type == "entity") |>
#   dplyr::select("name", dplyr::starts_with("label"))
#
# rv$insights$entities_labs  <- utils_find_label(
#   .df = entity_lang,
#   .name = rv$insights$entities,
#   .lang = rv$inputs$data$chain_summary$selectedLanguage
# )
# rv$insights$entities_named <- stats::setNames(
#   # c("area", rv$insights$entities), c("Area", rv$insights$entities_labs)
#   rv$insights$entities, rv$insights$entities_labs
# )
#
#
# ## Get metadata from selected entity ######
# rv$analysis$dim_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]]
#
# ## !!! TO INSPECT FURTHER !!!
# strat_row <-rv$analysis$dim_meta |> dplyr::filter(.data$stratum)
# rv$analysis$strat_label <- if (nrow(strat_row) > 0) strat_row$label[1] else NULL
#
#
# ## Get measures meta-data ######
# rv$analysis$measures_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]] |>
#   dplyr::filter(.data$report_type == "measure")
#
# ## RUN ANALYSIS ######
# ## +++
# result <- fct_arenalyse(
#   .zip    = rv$inputs$data,
#   .entity = input$analysis_sel_entity,
#   .dim    = rv$analysis$dims_sel,
#   .pvalue = as.numeric(input$analysis_p_value),
#   .cm     = input$analysis_compute_mode,
#   .lonely = input$analysis_lonely_psu
# )
# ## +++
#
#
# ## Results options ######
# lang      <- rv$inputs$data$chain_summary$selectedLanguage
# dim_meta  <- rv$analysis$dim_meta
# cats      <- rv$inputs$data$categories
# taxos     <- rv$inputs$data$taxonomies %||% list()
#
#
# ## RESULTS ######
# result$MEANS  <- replace_dim_labels(result$MEANS,  dim_meta, cats, lang, taxos)
# result$TOTALS <- replace_dim_labels(result$TOTALS, dim_meta, cats, lang, taxos)
