## ============================================================================
## make_offline_bundle.R
##
## Run this script ONCE on a machine with internet access to download all
## packages required by arenalytics and bundle them into a zip file that can
## be shared with users who have no internet connection.
##
## Output: inst/extdata/arenalytics_offline_pkgs.zip
##
## Requirements: R >= 4.1, the `zip` package must already be installed.
## Run from the arenalytics project root:
##   source("inst/extdata/make_offline_bundle.R")
## ============================================================================

# ── Configuration ------------------------------------------------------------

## Packages declared in DESCRIPTION Imports (excluding base packages)
direct_pkgs <- c(
  "bsicons",
  "bslib",
  "DT",
  "dplyr",
  "ggplot2",
  "htmltools",
  "jsonlite",
  "purrr",
  "quarto",
  "rlang",
  "scales",
  "shiny",
  "shiny.i18n",
  "shinyjs",
  "shinyWidgets",
  "srvyr",
  "stringr",
  "tibble",
  "tidyr",
  "zip"
)

## CRAN mirror to use
cran_mirror <- "https://cran.r-project.org"

## Packages are downloaded into the standard CRAN repo layout:
##   arenalytics_offline_pkgs/src/contrib/
## This lets install.packages() use repos = "file:///path/to/arenalytics_offline_pkgs"
## directly, without any special contriburl tricks.
pkg_dir  <- "inst/extdata/tmp/arenalytics_offline_pkgs/src/contrib"
zip_root <- "inst/extdata/tmp"

## Final zip destination — built from getwd() so it is always absolute,
## even when zip internally changes the working directory.
zip_name <- file.path(getwd(), "inst/extdata/arenalytics_offline_pkgs.zip")

# ── Resolve full recursive dependency set -----------------------------------

message("Resolving recursive dependencies ...")

ap <- available.packages(repos = cran_mirror, type = "source")

all_deps <- tools::package_dependencies(
  packages  = direct_pkgs,
  db        = ap,
  recursive = TRUE,
  which     = c("Imports", "Depends", "LinkingTo")
)

all_pkgs <- unique(c(direct_pkgs, unlist(all_deps, use.names = FALSE)))

## Drop packages that ship with base R (they are always available)
base_pkgs <- rownames(installed.packages(priority = c("base", "recommended")))
all_pkgs  <- setdiff(all_pkgs, base_pkgs)

message(sprintf("Downloading %d packages (including dependencies) ...", length(all_pkgs)))

# ── Download source tarballs ------------------------------------------------

dir.create(pkg_dir, showWarnings = FALSE, recursive = TRUE)

result <- download.packages(
  pkgs    = all_pkgs,
  destdir = pkg_dir,
  repos   = cran_mirror,
  type    = "source"
)

## Write the PACKAGES index into src/contrib/ — R expects it there
tools::write_PACKAGES(pkg_dir, type = "source", verbose = FALSE)

n_downloaded <- nrow(result)
message(sprintf("Downloaded %d / %d packages.", n_downloaded, length(all_pkgs)))

if (n_downloaded < length(all_pkgs)) {
  missing_pkgs <- setdiff(all_pkgs, result[, 1])
  warning(
    "The following packages could not be downloaded and will be missing from the bundle:\n",
    paste(" -", missing_pkgs, collapse = "\n")
  )
}

# ── Build and add the arenalytics package itself ----------------------------

## NOTE: `.Rbuildignore` must exclude `inst/extdata/tmp` and the output zip,
## otherwise R CMD build sweeps the downloaded tarballs (living under inst/)
## into the arenalytics tarball and the bundle size roughly doubles.
message("Building arenalytics tarball ...")
devtools::build(path = pkg_dir, vignettes = FALSE, quiet = TRUE)

## Refresh the PACKAGES index to include arenalytics
tools::write_PACKAGES(pkg_dir, type = "source", verbose = FALSE)

# ── Create the zip bundle ---------------------------------------------------

message("Creating zip bundle ...")

## Temporarily change into zip_root so "arenalytics_offline_pkgs" is the
## top-level entry in the zip. zip_name is already absolute so it is
## unaffected by the directory change.
old_wd <- setwd(zip_root)
zip::zip(zipfile = zip_name, files = "arenalytics_offline_pkgs")
setwd(old_wd)

# ── Clean up temporary folder -----------------------------------------------

unlink(zip_root, recursive = TRUE)
message("Temporary folder removed.")

message(sprintf(
  "Done!  Bundle saved to: %s  (%.1f MB)",
  zip_name,
  file.size(zip_name) / 1024^2
))
message(
  "\nShare '", zip_name, "' together with 'tools/install_offline.md' with your users."
)

