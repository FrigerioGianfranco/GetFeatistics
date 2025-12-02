# Test normality by generating a density plot

Given a dataframe and a set of numerical variables of that dataframe, it
creates a density plot for each desired variable.

## Usage

``` r
test_normality_density_plot(df, v)
```

## Arguments

- df:

  a dataframe.

- v:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values.

## Value

a list containing a number of element equal to the length of v. Each
element is a density plot.
