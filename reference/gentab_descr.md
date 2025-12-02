# Generate a table with descriptive statistics

Given a dataframe and a set of numerical variables of that dataframe, it
generate a table with descriptive statistics for each of those
variables.

## Usage

``` r
gentab_descr(
  df,
  v,
  f = NA,
  type = "mean",
  ROUND = FALSE,
  dig = NA,
  unit_mes = NA,
  missing = "only if >0"
)
```

## Arguments

- df:

  a dataframe.

- v:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values.

- f:

  character of length 1 or a missing value. If you pass here a
  character, that must be a column of the df containing a factor: if so,
  the function will perform the descriptive statistics grouped by that
  factor.

- type:

  one of the following: "mean", "mean SD", "median", "median (min;
  max)", "median (5th; 95th percentile)", or "median (25th; 75th
  percentile)".

- ROUND:

  logical. Should the values be rounded? if TRUE, you could indicate the
  number of decimal places in the dig argument.

- dig:

  numeric of length 1 or a missing value. If ROUND is TRUE, you can
  indicate here the number of decimal places; if missing, the function
  round_the_result of the present package will be used.

- unit_mes:

  character of length 1 or a missing value. You can specify here a unit
  of measure to add to each cell of the table (I'd suggest to do it only
  if is actually the same for each variable indicated with v).

- missing:

  one of the following: "always", "never", or "only if \>0". Do you also
  want to report in the table how many missing values are present?

## Value

A tibble (a data frame) in which the first row is the number of
observations, each other row reports the desired descriptive statistics
for each considered variable.
