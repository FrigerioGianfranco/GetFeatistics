# Get a table of feature intensities from the patRoon output.

Given the featureGroups table obtained from patRoon, it creates a table
with only the name of features in the first column and the intensities
of features in each other column.

## Usage

``` r
get_feat_table_from_patRoon(
  patRoon_featureGroups = NULL,
  patRoon_featureGroups_file_name = NULL
)
```

## Arguments

- patRoon_featureGroups:

  NULL or a dataframe. Features groups and related intensities obtained
  after XCMS. This table should contain the feature group names in a
  column named "group", the retention times a column named "ret", the
  m/z ratio in a column named "mz", and the intensities in all other
  columns.

- patRoon_featureGroups_file_name:

  NULL or a character vector of length 1. If patRoon_featureGroups is
  NULL, this can be used and directly import the table with the .csv or
  .txt file name passed.

## Value

A tibble with the feature intensities.
