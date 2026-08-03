# Run survey estimation for a given entity and set of reporting dimensions

Computes weighted means and totals per hectare for all measures of a
selected entity, stratified or clustered according to the sampling
design described in the chain summary. Supports simple random sampling
(SRS), stratified SRS, and cluster sampling designs.

## Usage

``` r
fct_arenalyse(
  .zip,
  .entity,
  .dim,
  .pvalue = 0.95,
  .cm,
  .lonely = "adjust",
  .pb_ss = NULL,
  .pb_id = NULL
)
```

## Arguments

- .zip:

  Named list produced by
  [`fct_readzip()`](https://openforis.github.io/arenalytics/reference/fct_readzip.md).
  Must contain:

  chain_summary

  :   Survey chain metadata (sampling strategy, base unit, result
      variables, language, etc.).

  schema_summary

  :   Data dictionary describing all input dimensions.

  report_dimensions

  :   Available reporting dimensions.

  MAU\_

  :   Minimal Area Unit tables for the target entity.

- .entity:

  Character scalar. Name of the entity to analyse (e.g. `"tree"`,
  `"plot"`).

- .dim:

  Character vector. Names of the reporting dimensions to include (e.g.
  `c("plot_forest_type", "plot_province")`).

- .pvalue:

  P-value for the standard error estimation.

- .cm:

  Compute mode, One of `"fast"` or `"safe"`. `"fast"` computes all
  measures in a single survey summary call; `"safe"` computes each
  measure separately and keeps partial results when some measures fail.

- .lonely:

  Lonely-PSU handling strategy. One of:

  `"adjust"` (default)

  :   Substitutes the grand mean across strata for the single-PSU
      stratum's variance contribution, extended to domain (grouped)
      estimates via `survey.adjust.domain.lonely = TRUE`. Produces a
      conservative (upward-biased) but valid SE. Recommended when
      fine-grained cross-tabulations are expected.

  `"remove"`

  :   Drops the lone stratum from variance computation
      (`survey.adjust.domain.lonely = FALSE`). Faster and less
      conservative, but can silently underestimate SE for affected
      groups. May still error on domain estimates if the lonely PSU
      falls within a reporting group — safe only when base-unit
      dimensions are few and strata are well-populated.

- .pb_ss:

  A Shiny session for
  [`shinyWidgets::updateProgressBar()`](https://dreamrs.github.io/shinyWidgets/reference/progress-bar.html).
  Default `NULL`.

- .pb_id:

  The widget ID for
  [`shinyWidgets::updateProgressBar()`](https://dreamrs.github.io/shinyWidgets/reference/progress-bar.html).
  Default `NULL`.

## Value

A named list with two elements:

- MEANS:

  Tibble of per-hectare survey means with standard errors and confidence
  intervals for each measure × dimension combination.

- TOTALS:

  Tibble of estimated totals derived from `MEANS` by multiplying by
  expansion area.

Both tibbles include `area`, `item_count`, `base_unit_count`, and
optionally `cluster_count` columns.
