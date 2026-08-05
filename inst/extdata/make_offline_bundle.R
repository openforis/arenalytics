## ============================================================================
## make_offline_bundle.R
##
## Run this script ONCE on a machine with internet access to download all
## packages required by arenalytics and bundle them into a zip file that can
## be shared with users who have no internet connection.
##
## Outputs (both created in inst/extdata/):
##   - arenalytics_offline_pkgs.zip      source packages  (any platform, needs a compiler)
##   - arenalytics_offline_pkgs_WIN.zip  Windows binaries (R 4.6.x, no compiler/Rtools needed)
##
## Requirements: R >= 4.1, the `zip` and `devtools` packages installed.
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

## Target R version for the Windows binary bundle (major.minor only).
## Windows binaries live at <mirror>/bin/windows/contrib/<win_ver> on CRAN.
## The end user's installed R must match this minor version (e.g. 4.6.x).
win_ver <- "4.6"

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
arenalytics_tarball <- devtools::build(path = pkg_dir, vignettes = FALSE, quiet = TRUE)

## Refresh the PACKAGES index to include arenalytics
tools::write_PACKAGES(pkg_dir, type = "source", verbose = FALSE)

# ── Create the source zip bundle --------------------------------------------

message("Creating source zip bundle ...")

## Temporarily change into zip_root so "arenalytics_offline_pkgs" is the
## top-level entry in the zip. zip_name is already absolute so it is
## unaffected by the directory change.
old_wd <- setwd(zip_root)
zip::zip(zipfile = zip_name, files = "arenalytics_offline_pkgs")
setwd(old_wd)

message(sprintf("Source bundle: %s  (%.1f MB)", zip_name, file.size(zip_name) / 1024^2))

# ── Windows binary bundle ---------------------------------------------------

message(sprintf("Downloading Windows binaries for R %s ...", win_ver))

## Windows binaries and the arenalytics source tarball are placed in a
## CRAN-style repo so install.packages(type = "both") finds the pre-compiled
## dependencies and falls back to source only for arenalytics (pure R).
win_root    <- "inst/extdata/tmp/arenalytics_offline_pkgs_WIN"
win_bin_dir <- file.path(win_root, "bin/windows/contrib", win_ver)
win_src_dir <- file.path(win_root, "src/contrib")
zip_name_win <- file.path(getwd(), "inst/extdata/arenalytics_offline_pkgs_WIN.zip")

dir.create(win_bin_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(win_src_dir, showWarnings = FALSE, recursive = TRUE)

## Point at the target R version's contrib path explicitly, since this
## session's R version may differ from win_ver.
win_contriburl <- paste0(cran_mirror, "/bin/windows/contrib/", win_ver)
ap_win <- available.packages(contriburl = win_contriburl, type = "win.binary")

result_win <- download.packages(
  pkgs       = all_pkgs,
  destdir    = win_bin_dir,
  available  = ap_win,
  contriburl = win_contriburl,
  type       = "win.binary"
)
tools::write_PACKAGES(win_bin_dir, type = "win.binary", verbose = FALSE)

n_win <- nrow(result_win)
message(sprintf("Downloaded %d / %d Windows binaries.", n_win, length(all_pkgs)))
if (n_win < length(all_pkgs)) {
  missing_win <- setdiff(all_pkgs, result_win[, 1])
  warning(
    "Missing Windows binaries (are they built for R ", win_ver, " yet?):\n",
    paste(" -", missing_win, collapse = "\n")
  )
}

## Reuse the source tarball already built above (arenalytics has no compiled
## code, so a source install on Windows is fast and needs no Rtools).
file.copy(arenalytics_tarball, win_src_dir, overwrite = TRUE)
tools::write_PACKAGES(win_src_dir, type = "source", verbose = FALSE)

message("Creating Windows zip bundle ...")
old_wd <- setwd(zip_root)
zip::zip(zipfile = zip_name_win, files = "arenalytics_offline_pkgs_WIN")
setwd(old_wd)

message(sprintf("Windows bundle: %s  (%.1f MB)", zip_name_win, file.size(zip_name_win) / 1024^2))

# ── Clean up temporary folder -----------------------------------------------

unlink(zip_root, recursive = TRUE)
message("Temporary folder removed.")

message("\nDone! Share the appropriate bundle with your users:")
message("  - arenalytics_offline_pkgs.zip      (source, any OS, needs a compiler)")
message("  - arenalytics_offline_pkgs_WIN.zip  (Windows, R ", win_ver, ".x, no compiler)")

