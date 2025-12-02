# Test normality with a Shapiro–Wilk test

Given a dataframe and a set of numerical variables of that dataframe, it
tests the normality of those data by performing a Shapiro–Wilk test to
each desired variable.

## Usage

``` r
test_normality_Shapiro_table(df, v, pvalcutoff = 0.05, cutpval = FALSE)
```

## Arguments

- df:

  a dataframe.

- v:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values.

- pvalcutoff:

  a numeric of length 1, must be between 0 and 1, indicating the p-value
  cut-off.

- cutpval:

  logical. Do you want to cut the P-value using the function cutP of the
  present package?

## Value

A tibble with a number of rows equal to elements specified in v. For
each of those, it reports the results of the Shapiro test and whether
those data are normally distributed considering the given p-value
cutoff.
