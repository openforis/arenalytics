# FUNCTION TITLE

Take a numeric column from a dataframe and return it's mean rounded to
the specified number of digits.

## Usage

``` r
fct_mean(.df, .colnum, .rounding = 0)
```

## Arguments

- .df:

  A table with at least a numeric column.

- .colnum:

  A column name of the input table.

- .rounding:

  Rounding number of digits, defaults to 0.

## Value

the mean of the input column values.

## Examples

``` r
fct_mean(.df = iris, .colnum = Sepal.Width, .rounding = 1)
#> [1] 3.1
```
