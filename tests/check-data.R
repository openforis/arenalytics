#
# library(tidyverse)
# library(arenalytics)
#
# zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
# if (nzchar(zipfile) && file.exists(zipfile)) {
#   zipdata <- fct_readzip(.path = zipfile)
#   summary(zipdata$OLAP_tree$tree_biomass_ag)
# }
#
# names(zipdata)
#
# chain <- zipdata$chain_summary
# chain_var <- as_tibble(chain$resultVariables)
#
# schema <- as_tibble(zipdata$SchemaSummary)
#
# tree <- as_tibble(zipdata$OLAP_tree)
# tree
# names(tree)
# summary(tree)
#
# tree |> summarise(sum(area))
#
# tree_bu <- tree |> filter(OLAP_baseunit_total)
