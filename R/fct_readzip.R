

fct_readzip <- function(.path){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # !!!

  ## Get file names
  zip_content <- zip::zip_list(.path)$filename |> sort()

  file_names <- zip_content |> stringr::str_remove("\\..*")

  ## NO TEMP DIR needed however may yield error with loading RDS files
  out <- purrr::map(zip_content, function(x){

    ext <- stringr::str_remove(x, pattern = ".*\\.")
    if (ext == "csv") {
      tt <- read.csv(unz(.path, x))
    } else if (ext == "rds") {
      tt <- readRDS(gzcon(unz(.path, x)))
      #tt <- purrr::map(tt, dplyr::as_tibble)
    } else if (ext == "json") {
      tt <- jsonlite::fromJSON(unz(.path, x))
    } else {
      tt <- NULL
    }
    tt

  })

  names(out) <- file_names
  out

  # TEMP DIR solution - TEST GIVES SAME DATA
  # temp_dir <- file.path(tempdir(), paste0("arenalytics-", Sys.time()))
  # dir.create(temp_dir)
  # utils::unzip(zipfile = .path, exdir = temp_dir)
  #
  # file_paths <- sort(list.files(temp_dir, full.names = T))
  #
  # out1 <- purrr::map(file_paths, function(x){
  #
  #   ext <- stringr::str_remove(x, pattern = ".*\\.")
  #   if (ext == "csv") {
  #     tt <- read.csv(x)
  #   } else if (ext == "rds") {
  #     tt <- readRDS(x)
  #     #tt <- purrr::map(tt, dplyr::as_tibble)
  #   } else if (ext == "json") {
  #     tt <- jsonlite::fromJSON(x)
  #   } else {
  #     tt <- NULL
  #   }
  #   tt
  # })
  #
  # unlink(temp_dir)
  #
  # names(out1) <- file_names
  #
  # ## Check
  # identical(out, out1)
  # purrr::map2_lgl(out, out1, identical)
  # identical(out$OLAP_bamboo, out1$OLAP_bamboo)
  # all.equal(out$OLAP_bamboo, out1$OLAP_bamboo)

}




