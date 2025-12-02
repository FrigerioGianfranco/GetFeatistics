# Combining three tables of feature intensities in a combined table.

Given three different tables of feature intensities and the related
featINFO tables, it combines the feature intensities in a single table.

## Usage

``` r
GetCombined_featTable_MSDial(
  featTable1,
  featINFO1,
  name1 = "table1",
  featTable2,
  featINFO2,
  name2 = "table2",
  featTable3,
  featINFO3,
  name3 = "table3",
  mz_widow = 0.005,
  rt_window = 2
)
```

## Arguments

- featTable1:

  a dataframe, containing feature intensities.

- featINFO1:

  a dataframe, containing the featINFO related to featTable1.

- featTable2:

  a dataframe, containing feature intensities.

- featINFO2:

  a dataframe, containing the featINFO related to featTable2.

- featTable3:

  a dataframe, containing feature intensities.

- featINFO3:

  a dataframe, containing the featINFO related to featTable3.

- mz_widow:

  numeric of length 1. The mass-to-charge ratio window within which
  features could be grouped as the same feature.

- rt_window:

  numeric of length 1. The retention time window within within features
  could be grouped as the same feature.

## Value

A tibble. The combined table of feature intensities.
