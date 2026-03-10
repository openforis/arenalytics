<!-- badges: start -->
[![R-CMD-check](https://github.com/openforis/arenalytics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openforis/arenalytics/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# arenalytics

Analytics and visualizations for OpenForis Arena data

Run at: https://openforis-shiny.shinyapps.io/arenalytics/

or install and run locally in a Rstudio instance with:

```         
if (!require(remotes)) install.packages("remotes")
if (!require(arenalytics)) remotes::install_github("openforis/arenalytics")

arenalytics::shiny_run_arenalytics()
```
