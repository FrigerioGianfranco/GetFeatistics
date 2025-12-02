# Get a featINFO table from the MSDIAL export file.

Given a table exported from MSDIAL (exporting the Area from MS-Dial
5.1.230912), it creates the featINFO table, i.e. the table with the full
information on each feature.

## Usage

``` r
get_feat_info_from_MSDial(
  MSDIAL_raw_table = NULL,
  MSDIAL_raw_table_file_name = NULL,
  add_AnnoLevels = FALSE
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

- add_AnnoLevels:

  logical. If TRUE, an additional column named "AnnoLevels" will be
  added, and the annotation levels will be calculated considering the
  cut-offs reported in https://doi.org/10.1007/s00216-022-04207-z.

## Value

A tibble with the information for each feature.
