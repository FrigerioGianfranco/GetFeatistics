# Fix duplicate names

It adds sequential numbers to solve duplicates in a character vector.

## Usage

``` r
fix_duplicated(
  x,
  zeros = TRUE,
  define_highest_for_zeros = NULL,
  start_with_zero = FALSE,
  exclude_the_first = TRUE,
  NA_as_character = FALSE
)
```

## Arguments

- x:

  a character vector.

- zeros:

  logical. Should leading zero be added?

- define_highest_for_zeros:

  NULL or an numeric integer of length 1. If zeros is TRUE, you can
  decide the highest number to define the number of zeros. If this
  argument is NULL, the highest number will be defined based on the
  highest number of duplicated.

- start_with_zero:

  logical. If TRUE, the first number added for solving the duplicated
  will be 0, if FALSE will be 1.

- exclude_the_first:

  logical. If TRUE, the suffix will not be added only from the second
  duplicated on.

- NA_as_character:

  logical. If FALSE, the missing values will not be affected by this
  function, if TRUE, any missing values duplicated will be considered as
  a character "NA".

## Value

a single character with duplicated solved, thus with only unique
elements.
