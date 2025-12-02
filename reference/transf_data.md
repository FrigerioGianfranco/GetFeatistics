# Transform the data

Given a dataframe and a set of numerical variables of that dataframe, it
can replace missing values, log-transform, and/or scale the data.

## Usage

``` r
transf_data(
  df,
  v,
  missing_replace = TRUE,
  missing_repl_type = "divide",
  missing_repl_value = 5,
  log_transf = TRUE,
  log_base = exp(1),
  scaling = TRUE,
  scaling_type = "pareto_scale",
  vect_names_transf = FALSE,
  name_vect_names = "vars_ready"
)
```

## Arguments

- df:

  a dataframe.

- v:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values.

- missing_replace:

  logical. Should missing value be replaced? if TRUE, each missing value
  will be replaced with the minimum value of that variable, modified
  with the operation indicated in the missing_repl_type and
  missing_repl_value arguments. If a variable have all missing values,
  it will not processed further.

- missing_repl_type:

  one of the following: "divide", "multiply", or "exponentiate". If
  missing_replace is TRUE, each missing values will be replaced by the
  minimum value of that variable divided by ("divide"), multiplied by
  ("multiply"), or raised to power of ("exponentiate") the value
  indicated in the missing_repl_value argument. Suggestion: if you want
  to replace missing values with the squared root of the minimum value,
  select here "exponentiate" and in missing_repl_value put 1/2 (raising
  to the power of 1/2 is equal to applying the square root).

- missing_repl_value:

  numerical of length 1. If missing_replace is TRUE, this is the value
  of the operation indicated in missing_repl_type that will be applied
  to the the minimum values, the result of which will replace the
  missing values.

- log_transf:

  logical. Should log-transformation be applied to each data?

- log_base:

  numerical of length 1. If log_transf is TRUE, this is the base of the
  logarithm applied to transform the data.

- scaling:

  logical. Should the data be scaled? If TRUE, set the scaling_type
  argument properly.

- scaling_type:

  one of the following: "mean_scale", "auto_scale", "pareto_scale", or
  "range_scale". If scaling is TRUE, for each variable, data will be
  subtracted by the mean ("mean_scale"), subtracted by the mean and
  divided by the standard deviation ("auto_scale"), subtracted by the
  mean and divided by the squared root of the standard deviation
  ("pareto_scale"), or subtracted by the mean and divided by the
  difference between the maximum and the minimum values ("range_scale").

- vect_names_transf:

  logical. If TRUE, it also create in the global environment character
  vectors containing the names of the variables transformed. It is
  useful to pass these names to the v arguments of furhter statistical
  functions of this package.

- name_vect_names:

  character of length 1. If vect_names_transf is TRUE, specify here the
  main name of the character vectors containing the names of the
  transformed variables.

## Value

df with new columns containing the transformed values. In particular:
the new columns that end in "\_mr" refer to missing values replaced,
"\_ln", "\_Log", or "\_log" to log-transformed, "\_meansc", "\_autosc",
"\_paretosc", or "\_rangesc" to the scaling. As example, the new columns
that end with "\_mr_ln_paretosc" refer to data missing value replaced,
natural-logarithm transformed, and pareto scaled. Check also the name of
those column by setting vect_names_transf as TRUE and check in the
global environment the objects whose names start with what you set in
name_vect_names.
