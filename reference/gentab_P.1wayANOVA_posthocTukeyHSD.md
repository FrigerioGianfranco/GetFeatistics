# Generate a Table with P-values from one-way ANOVA with TukeyHSD posthoc

Given a dataframe and a set of numerical variables of that dataframe, it
performs one-way ANOVA, with also TukeyHSD posthoc tests for between
groups comparison, to each desired variable and creates a new table with
the p-values.

## Usage

``` r
gentab_P.1wayANOVA_posthocTukeyHSD(
  DF,
  v,
  f,
  FDR = FALSE,
  groupdiff = FALSE,
  pcutoff = 0.05,
  filter_sign = FALSE,
  cutPval = FALSE
)
```

## Arguments

- DF:

  a dataframe.

- v:

  a character vector. Each element must correspond to a column name of
  the df, each of which must contain numeric values. Moreover, missing
  values are not allowed (if any, consider before replacing them using
  the function transf_data of the present package).

- f:

  character vector of length 1. Name of the column of df containing the
  factor variables for performing the ANOVA.

- FDR:

  logical. If TRUE, after performing the ANOVA, it also correct p-values
  across the different variables with a false discovery rate multiple
  comparison correction (method "fdr" of the function p.adjust).

- groupdiff:

  logical. Do you also what to add an additional column indicating which
  group is higher?

- pcutoff:

  a numeric of length 1, must be between 0 and 1. If groupdiff is TRUE,
  the difference between groups will be reported only if the p-values is
  below the cut-off reported here.

- filter_sign:

  logical. If TRUE, the table will be filtered and only the p-values
  lower than the value specified in pcutoff will be considered.

- cutPval:

  logical. If TRUE, it cut the p-values using the cutP function of the
  present package.

## Value

A tibble the results of the t-tests.
