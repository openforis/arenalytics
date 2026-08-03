# Read OpenForis Arena OLAP zipfile data (error-safe version)

A fault-tolerant version Open Foris Arena ZIP reading function. Each
file inside the ZIP is read inside a
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) so that a single
corrupted file does not abort the entire load. Files that fail to read
are returned as `NULL` in the output list, and the error messages are
attached to the result as the `.errors` attribute. The caller can
inspect that attribute to decide whether to surface a warning in the UI.

The outer listing of the ZIP archive itself is also guarded: if
[`zip::zip_list()`](https://r-lib.github.io/zip/reference/zip_list.html)
fails (e.g. the file is not a valid ZIP), the function returns `NULL`
immediately with a descriptive error message.

## Usage

``` r
fct_readzip(.path, .pb_ss = NULL, .pb_id = NULL)
```

## Arguments

- .path:

  A path to the data ZIP file.

- .pb_ss:

  A Shiny session for
  [`shinyWidgets::updateProgressBar()`](https://dreamrs.github.io/shinyWidgets/reference/progress-bar.html).
  Default `NULL` (progress bar is only updated inside a Shiny context).

- .pb_id:

  The widget ID for
  [`shinyWidgets::updateProgressBar()`](https://dreamrs.github.io/shinyWidgets/reference/progress-bar.html).
  Default `NULL`.

## Value

A named list of entity data frames and survey descriptors, or `NULL` if
the archive itself cannot be opened. Each element is either the parsed
file content or `NULL` when reading failed. Failed files are recorded in
`attr(result, ".errors")` as a named character vector
(`name = error message`).

## Examples

``` r
zipfile <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics")
if (nzchar(zipfile) && file.exists(zipfile)) {
  zipdata <- fct_readzip(.path = zipfile)

  ## Check for any read errors
  errs <- attr(zipdata, ".errors")
  if (length(errs) > 0) {
    message("Files with read errors: ", paste(names(errs), collapse = ", "))
  }

  summary(zipdata$data$OLAP_tree$tree_biomass_ag)
}
#> [2026-08-03 08:28:10] Loaded OLAP_bamboo.csv
#> [2026-08-03 08:28:10] Loaded OLAP_deadwood_area.csv
#> [2026-08-03 08:28:10] Loaded OLAP_regeneration.csv
#> [2026-08-03 08:28:11] Loaded OLAP_stump.csv
#> [2026-08-03 08:28:11] Loaded OLAP_tree.csv
#> [2026-08-03 08:28:11] Loaded ReportDimensions.csv
#> [2026-08-03 08:28:11] Loaded SchemaSummary.csv
#> [2026-08-03 08:28:11] Loaded categories.rds
#> [2026-08-03 08:28:11] Loaded chain_summary.json
#> [2026-08-03 08:28:11] Loaded taxonomies.rds
#> [2026-08-03 08:28:11] ✓ All 10 file(s) loaded successfully and variables metadata tables built.
#> Length  Class   Mode 
#>      0   NULL   NULL 
```
