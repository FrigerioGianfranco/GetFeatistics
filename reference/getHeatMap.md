# Get an Heat Map.

Given a table containing data, it creates an heat map graph.

## Usage

``` r
getHeatMap(
  df,
  v,
  s = NULL,
  f = NULL,
  dfv = NULL,
  sv = NULL,
  fv = NULL,
  order_df_by = NULL,
  order_dfv_by = NULL,
  trnsp = TRUE,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  name_rows = FALSE,
  name_columns = FALSE,
  rotate_name_columns = TRUE,
  three_heat_colors = c("red", "white", "blue"),
  set_heat_colors_limits = FALSE,
  heat_colors_limits = NULL,
  col_pal_list = NULL
)
```

## Arguments

- df:

  dataframe. Containing data to plot.

- v:

  character. The names of the columns of df containing numerical data
  (ideally, they should be already centered and scaled!!).

- s:

  NULL or a character of length 1. The name of the column of df
  containing the sample names. You need to pass it only if you want
  sample names on the heat map.

- f:

  NULL or character. The name(s) of the column(s) of df containing the
  factor variable. Pass it only if you want the additional rectangles
  for those groups on the heat map.

- dfv:

  NULL or a dataframe. The first column must contain v. Additional
  columns can be used for the ordering or for grouping related to v.

- sv:

  NULL or a character of length 1. The name of the column of dfv
  containing the names. You need to pass it only if you want names on
  the heat map and if you want to replace existing column names.

- fv:

  NULL or character. The name(s) of the column(s) of dfv containing the
  factor variable. Pass it only if you want the additional rectangles
  for those groups on the heat map.

- order_df_by:

  NULL or character. The name(s) of the column(s) of df that you want to
  use to order the values.

- order_dfv_by:

  NULL or character. The name(s) of the column(s) of dfv that you want
  to use to order the values.

- trnsp:

  logical. If TRUE, the matrix of created from the columns v of df will
  be transposed before being used for the heat map.

- cluster_rows:

  logical. If TRUE, a dendrogram built from the hierarchical clustering
  of distances of row values will be added to the left side of the heat
  map.

- cluster_columns:

  logical. If TRUE, a dendrogram built from the hierarchical clustering
  of distances of column values will be added above the heat map.

- name_rows:

  logical. If TRUE, names will be added to rows, on the right of the
  heat map.

- name_columns:

  logical. if TRUE, names will be added to column, below the heat map.

- rotate_name_columns:

  logical. if TRUE, column names will be rotated vertically (this
  argument is meaningless if name_columns is FALSE).

- three_heat_colors:

  character of length 3, each specifying a color. These 3 colors will be
  used as color scale for values of the heat map.

- set_heat_colors_limits:

  logical. If TRUE, the absolute of the minimum or the absolute of the
  maximum value (which is higher) will be set in positive as the upper
  limit and in negative as the lower limit for the color gradients of
  values of the heat map (this will also set the middle color exactly to
  zero).

- heat_colors_limits:

  NULL or a numeric of length 2. If set_heat_colors_limits is FALSE, you
  can specify here the limits for the color gradients of values of the
  heat map (if NULL, the maximum and the minimum values will be used).

- col_pal_list:

  NULL of a list containing elements named as f and fv; each element has
  to be a character vector containing colors. Those colors will be used
  for the rectangles of the group classifications. For each element, if
  NULL, colors will be taken from the pals package (see the function
  build_long_vector_of_colors).

## Value

a ggplot object.
