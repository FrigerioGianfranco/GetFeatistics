# Create some simple boxplots.

Given a table containing data, it create boxplots for each desired
variable.

## Usage

``` r
getBoxplots(df, v, f, col_pal = NULL)
```

## Arguments

- df:

  dataframe. Containing data to plot.

- v:

  character. The names of the columns of df containing numerical data to
  plot

- f:

  character of length 1. The names of the factor variable used to group
  the boxplots.

- col_pal:

  NULL or a character vector containing colors. If NULL, colors from the
  pals package will be used (see function build_long_vector_of_colors).

## Value

A ggplot object (if v is only 1), or a list of ggplots (if v contains 2
or more elements).
