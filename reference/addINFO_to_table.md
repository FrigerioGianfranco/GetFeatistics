# Add the information from featINFO to a table.

Given a table containing data related to features (for example the
output of a statistical analyses) and the related featINFO table, it
combines everything in a single table.

## Usage

``` r
addINFO_to_table(
  df1,
  colfeat_df1,
  dfINFO,
  colfeat_dfINFO,
  add_char_to_INFO = FALSE,
  char_to_add_to_INFO = "_INFO"
)
```

## Arguments

- df1:

  dataframe. The table containing some results (e.g. from statistical
  analyses) for each feature.

- colfeat_df1:

  character of length 1. It is the name of the column in df1 that
  contains the unique names of features.

- dfINFO:

  dataframe. The featINFO table.

- colfeat_dfINFO:

  character of length 1. It is the name of the column in dfINFO that
  contains the unique names of features.

- add_char_to_INFO:

  logical. Should a string of character to be added to each column from
  the dfINFO?

- char_to_add_to_INFO:

  character of length 1. If add_char_to_INFO is TRUE, pass here the
  string of character that will be added to each column from dfINFO.

## Value

A tibble. The combined table containing also the information on each
feature.
