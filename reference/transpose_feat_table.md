# Transpose features table.

Given a feature table, it transposes it, so it is more suitable for
following elaborations and statistical analyses.

## Usage

``` r
transpose_feat_table(feat_table, name_first_column = "samples")
```

## Arguments

- feat_table:

  a dataframe: first column the featname, each other column is a sample
  with feature intensities. Each row is a feature. It can be obtained
  with the get_feat_table_from_MSDial function of the present package,
  but ideally it underwent the cleaning with QCs using the QCs_process
  function.

- name_first_column:

  character of length 1. You can specify here the name of the first
  column, which will be referred to the column of samples.

## Value

A tibble with samples as rows and features as columns.
