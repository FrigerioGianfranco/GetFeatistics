# Check molecules in the feature table.

Given a featINFO, a feature intensities table, and a table with a list
of molecules with known mass, it looks for those molecule in the feature
table.

## Usage

``` r
checkmolecules_in_feat_table(
  featmatrix,
  featinfo,
  molecules_list,
  mz_to_search = NULL,
  error = 20,
  error_type = "ppm",
  check_rt = FALSE,
  rt_to_search = NULL,
  rt_window = 12,
  return_as_featinfo_lev1 = FALSE
)
```

## Arguments

- featmatrix:

  a dataframe: first column the featname, each other column is a sample
  with feature intensities. Each row is a feature. (It can be obtained
  with the get_feat_table_from_MSDial or get_feat_table_from_patRoon
  function).

- featinfo:

  a dataframe. A table with at least three columns, in this order:
  feature names, retention times, and mass-to-charge ratios. (It can be
  obtained with the get_feat_info_from_MSDial or
  get_feat_info_from_patRoon function).

- molecules_list:

  a dataframe containing the molecules to search. The first column must
  contain the names of the molecule. Also, it should contain at least
  another column with the m/z to look for.

- mz_to_search:

  character vector of length 1. It is the column name of the
  molecules_list data-frame with the m/z values to look for (If not
  provided the third column will be used).

- error:

  numeric of length 1. Is is the tolerance for the search of m/z in Da
  or ppm (specified in the error_type argument)

- error_type:

  one of the following: "ppm", or "Da". It is referred to the value
  provided in the error argument.

- check_rt:

  logical. Should also the retention times be checked?

- rt_to_search:

  if check_rt is TRUE, it is the column name of the molecules_list
  data-frame with the rt values to look for. (If not provided the second
  column will be used)

- rt_window:

  numeric of length 1. It is the retention time window to consider
  centered to the known molecules to check the experimental ones. Please
  check that the unit (seconds or minutes) is the same in all the
  tables.

- return_as_featinfo_lev1:

  logical. If TRUE, and if also check_rt is TRUE, the function will
  return the featinfo with the found molecules as level 1 in the
  AnnoLevel column.

## Value

A tibble with the matching feature intensities. If
return_as_featinfo_lev1 is TRUE, the output is instead the featinfo with
the compounds found as level 1 in the AnnoLevel column.
