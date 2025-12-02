# Get Principal components analysis.

Given a table containing data, it performs the Principal components
analysis.

## Usage

``` r
getPCA(
  df,
  v,
  s = NULL,
  f = NULL,
  dfv = NULL,
  sv = NULL,
  fv = NULL,
  labels_on_loading = TRUE,
  center = FALSE,
  scale. = FALSE,
  col_pal = NULL,
  col_pal_fv = NULL,
  PC_to_plot = c("PC1", "PC2"),
  ellipses_on_score = TRUE,
  ellipses_on_loading = FALSE
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
  containing the sample names. Pass it only if you want sample names on
  the score plot.

- f:

  NULL or character of length 1. The name of the column of df containing
  the factor variable. Pass it only if you want colored points and
  ellipses on the score plot.

- dfv:

  NULL or a dataframe. The first column must contain v. To this
  dataframe the lodgings table will be added.

- sv:

  NULL or character. The name of the column of dfv containing the names
  you want to put on the loading plot. Pass it only if you want those
  names on the loading plot.

- fv:

  NULL or character. The name of the column of dfv containing the factor
  variable. Pass it only if you want colored points and ellipses on the
  loading plot.

- labels_on_loading:

  logical. Even if dfv and/or sv are NULL, if this argument is set to
  TRUE, the loading plot will report v as labels.

- center:

  logical. Whether the variables should be shifted to be zero centered
  (as in the prcomp function).

- scale.:

  logical. whether the variables should be scaled to have unit variance
  before the analysis takes place (as in prcomp function).

- col_pal:

  a character vector containing colors for f. If NULL, colors from the
  pals package will be used (see function build_long_vector_of_colors).

- col_pal_fv:

  a character vector containing colors for fv. If NULL, colors from the
  pals package will be used (see function build_long_vector_of_colors).

- PC_to_plot:

  character of length 2. The principal components to plots on the score
  and loading plots.

- ellipses_on_score:

  logical. If you specified f and this is TRUE, ellipses will be added
  to the score plot.

- ellipses_on_loading:

  logical. If you specified fv and this is TRUE, ellipses will be added
  to the score plot.

## Value

A list with 4 objects:

- `df_with_scores_table`: the df with additional columns containing the
  scores.

- `dfv_with_loadings_table`: the dfv with additional columns containing
  containing the loadings.

- `score_plot`: a ggplot object with the score plot.

- `loading_plot`: a ggplot object with the loading plot.
