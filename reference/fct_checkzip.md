# Check OpenForis Arena pre-processed ZIP file integrity

fct_checkzip() checks if the ZIP files uploaded in the master ShinyApp
has the core files required to run the analysis.

## Usage

``` r
fct_checkzip(.path)
```

## Arguments

- .path:

  a path to the zip file from

## Value

A list with TRUE/FALSE.

## Examples

``` r
zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
if (nzchar(zipfile) && file.exists(zipfile)) {
  zip_check <- fct_checkzip(.path = zipfile)
  zip_check$all_ok
}
```
