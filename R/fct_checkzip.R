#' Check OpenForis Arena OLAP ZIP file integrity
#'
#' @description fct_checkzip() checks if the ZIP files uploaded in the master ShinyApp has
#'              the core files required to run the analysis.
#'
#' @param .path a path to the zip file from
#'
#' @returns A list with TRUE/FALSE.
#'
#' @examples
#' zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
#' zip_check <- fct_checkzip(.path = zipfile)
#' zip_check$all_ok
#'
#' @export
fct_checkzip <- function(.path){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # !!!

  checklist <- data.frame(
    item = c("chain_summary.json", "SchemaSummary.csv", "ReportDimensions.csv", "categories.rds", "taxonomies.rds"),
    check = c("chain", "schema", "dimensions", "categories", "taxonomies")
  )

  ## Get file names
  zip_content <- zip::zip_list(.path)$filename |> sort()

  ## Check is files names match the checklist
  zipcheck <- purrr::map(seq_along(checklist$item), function(x){
    if (checklist$item[x] %in% zip_content) TRUE else FALSE
  })

  names(zipcheck) <- paste0("has_", checklist$check)

  ## Check number of entity tables
  nb_entities <- stringr::str_subset(zip_content, pattern = "OLAP_.*\\.csv") |> length()
  if (nb_entities > 0) zipcheck$has_entities <- TRUE else zipcheck$has_entities <- FALSE

  ## Summary
  zipcheck$all_ok <- all(unlist(zipcheck))

  zipcheck

}

## TEST
# fct_checkzip(.path = "inst/extdata/OLAP_shiny_demo.zip")

