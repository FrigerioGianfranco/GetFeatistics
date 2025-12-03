# Get a table of feature intensities from the MSDIAL export file.

Given a table exported from MSDIAL (exporting the Area from MS-Dial
5.1.230912), it creates a table with only the name of features in the
first column (same names created in the one of the featINFO) and the
intensities of features in each other column.

## Usage

``` r
get_feat_table_from_MSDial(
  MSDIAL_raw_table = NULL,
  MSDIAL_raw_table_file_name = NULL,
  n_last_coloums_to_delete = NULL
)
```

## Arguments

- MSDIAL_raw_table:

  NULL or a dataframe. Load on R the exported table using write_tsv or
  write_csv, then directly pass it in this argument. This argument will
  be ignored if the following one is not NULL.

- MSDIAL_raw_table_file_name:

  NULL or a character vector of length 1. The name of the .txt file of
  the MSDIAL exported table to import. It is preferred to use this
  argument instead of the previous, so the table will be also imported
  with the correct column types and information.

- n_last_coloums_to_delete:

  NULL or a numeric of length 1. Usually in the exported file from
  MSDIAL, the last columns are for descriptive statistics of the
  features. Pass here the number of the last columns that you need to
  remove. If NULL, these removal will be performed automatically. Also
  if 0, all the columns will be kept and the sample names modified
  accordingly.

## Value

A tibble with the feature intensities.
