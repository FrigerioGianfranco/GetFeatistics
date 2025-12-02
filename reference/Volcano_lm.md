# Generate a Volcano plot from the table of linear regression models

Given a table generated with the function gentab_lm_long, it create a
Volcano plot.

## Usage

``` r
Volcano_lm(
  tab,
  ind_main,
  x_values = "variation_perc",
  y_values = "negative_log10fdr",
  dep_cat = FALSE,
  category = NULL,
  cut_off_names = -log10(1e-04),
  line1 = TRUE,
  line1_position = -log10(0.05),
  line2 = TRUE,
  line2_position = -log10(1e-04),
  col_pal = NULL
)
```

## Arguments

- tab:

  a dataframe created with the function gentab_lm_long.

- ind_main:

  character of length 1. The independent variable to visualise in the
  Volcano plot. Important: for factor variables, it must me the name of
  the factor and the name of the level combined without space. e.g.: if
  the variable is "Sex" and the level is "Female", you must pass here
  "SexFemale".

- x_values:

  character of length 1. The name of the column in tab containing the
  value to plot as x.

- y_values:

  character of length 1. The name of the column in tab containing the
  value to plot as y.

- dep_cat:

  logical. Do you want to color the dependent variable based on a
  certain category?

- category:

  character of length 1. If dep_cat is TRUE, pass here the name of the
  column in tab that contains the categorisation.

- cut_off_names:

  numerical of length 1. Names of dependent variables will be shown only
  if above this cut-off.

- line1:

  logical. Do you want to show a horizontal dotted line?

- line1_position:

  numerical of length 1. If line1 is TRUE, pass here the y-value of the
  horizontal dotted line.

- line2:

  logical. Do you want to show a second horizontal dotted line?

- line2_position:

  numerical of length 1. If line2 is TRUE, pass here the y-value of the
  second horizontal dotted line.

- col_pal:

  a character vector containing colors. If NULL, colors from the pals
  package will be used (see function build_long_vector_of_colors).

## Value

a ggplot object, with the Volcano plot.
