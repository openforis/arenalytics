
library(tidyverse)
library(arenalytics)

zipfile <- system.file("extdata/MAU_Shiny_(testi_4_chain).zip", package = "arenalytics")
if (nzchar(zipfile) && file.exists(zipfile)) {
  zipdata <- fct_readzip(.path = zipfile)
  summary(zipdata$OLAP_tree$tree_biomass_ag)
}

names(zipdata$data)

chain <- zipdata$data$chain_summary
chain_var <- as_tibble(chain$resultVariables)

table(chain_var$active)

n_var <- chain_var |>
  filter(active) |>
  group_by(entity, areaBased) |>
  summarise(n_var = n(), .groups = "drop") |>
  mutate(areaBased = if_else(areaBased, "areaBased", "notAreaBased")) |>
  pivot_wider(names_from = areaBased, values_from = n_var) |>
  mutate(
    areaBased = if_else(is.na(areaBased), 0, areaBased),
    notAreaBased = if_else(is.na(notAreaBased), 0, notAreaBased)
  )

schema <- as_tibble(zipdata$SchemaSummary)

dims <- as_tibble(zipdata$ReportDimensions)

tree <- as_tibble(zipdata$data$MAU_tree)
tree

bamboo <- as_tibble(zipdata$data$MAU_bamboo)
names(tree)
summary(tree)

tree_bu <- tree |> filter(mau_baseunit_total)

taxo <- zipdata$data$taxonomies

taxo
