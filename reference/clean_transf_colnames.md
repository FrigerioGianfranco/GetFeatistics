# Clean colnames after transformation

Right after the transf_data function, column names are added with
additional names such as "\_transf_mr_ln_paretosc", (which means
transformed missing values replaced, natural-log-transformed,
pareto-scaled). With this function you clean that and reduce the
dataframe in order to contain only transformed variable of interest.

## Usage

``` r
clean_transf_colnames(df, v, suffix_to_consider = "_mr_ln_paretosc")
```

## Arguments

- df:

  a dataframa after the transf_data function.

- v:

  a character vector. Each element must correspond to original column
  names of the df and ideally this is the same that you passed
  previously to the function transf_data.

- suffix_to_consider:

  character of length 1. The suffix of the columns of the variable to
  use.

## Value

df with only transformed column of interest, with cleaned names.
