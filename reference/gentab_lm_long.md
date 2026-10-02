# Generate a long table with complete results of linear regression models

Given a dataframe and a set of dependent and independent variables from
that dataframe, it generates a linear regression model for each single
dependent variable and creates a new table with all slopes and p-values.

## Usage

``` r
gentab_lm_long(
  df,
  dep,
  form_ind,
  mdl = "lm",
  left_cens = NULL,
  right_cens = NULL,
  var_perc = FALSE,
  base = exp(1),
  FDR = FALSE,
  filter_sign = FALSE,
  pcutoff = 0.05,
  cutPval = FALSE,
  allpairs = FALSE,
  pairwise_adjust = "none"
)
```

## Arguments

- df:

  a dataframe.

- dep:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values.

- form_ind:

  character vector of length 1. Write here the part of the formual of
  the linear model the ~ (for example: for the linear model dependent ~
  independent1 + independent2 + independent3, you should pass here:
  "independent1 + independent2 + independent3").

- mdl:

  one of the following: "lm", "lmer", or "tobit". For linear model with
  fixed effects choose "lm" (it uses the function lm); for linear model
  with mixed effects choose "lmer" (function lmer of the lmerTest
  package); for linear model with censored variable choose "tobit"
  (function tobit of the AER package)

- left_cens:

  NULL or a named numeric vector. If mdl is "tobit", indicate here the
  left-censored values (e.g.: the limit of detection values). In
  particular, this argument should be set to NULL (no left-censored
  values) or a named numeric vector whose names must correspond to
  variables passed in dep.

- right_cens:

  NULL or a named numeric vector. If mdl is "tobit", indicate here the
  right-censored values (e.g.: the upper limit of detection values). In
  particular, this argument should be set to NULL (no right-censored
  values) or a named numeric vector whose names must correspond to
  variables passed in dep.

- var_perc:

  logical. Besides the beta slopes, do you also want to know the
  variation percentage? (if so, please, ensure your data are
  log-transformormed and scaled; consider using the function data_transf
  of the present package to do this). The variation percentage is
  calculated as follow: (((base^beta)-1)\*100).

- base:

  numerical of length one. If var_perc is TRUE, it is the base of the
  logarithm used to log-transform the dependent variables.

- FDR:

  logical. If TRUE, P-values are corrected across the different
  dependent variables using the false discovery rate method ("fdr") of
  p.adjust, separately for each independent-variable coefficient or
  contrast.

- filter_sign:

  logical. If TRUE, the table will be filtered and only the p-values
  lower than the value specified in pcutoff will be considered.

- pcutoff:

  a numeric of length 1, must be between 0 and 1. If filter_sign is
  TRUE, cut-off value of the p-values.

- cutPval:

  logical. If TRUE, it cut the p-values using the cutP function of the
  present package.

- allpairs:

  logical. If TRUE, pairwise contrasts are calculated for categorical
  fixed-effect variables. Marginal pairwise contrasts are returned for
  categorical variables included as standalone fixed-effect terms; for
  variables involved in categorical interactions, conditional pairwise
  contrasts are also returned; and pairwise interaction contrasts are
  returned for categorical interaction terms.

- pairwise_adjust:

  character of length one. The multiple-comparison adjustment method
  applied to inference for pairwise contrasts, including P-values and,
  where applicable, confidence intervals. Default is "none".

## Value

A tibble the results of the regression models.
