# Fix names

It removes special characters from a character vector, useful to modify
column names. Spaces and hyphens will be replaced with the underscore,

## Usage

``` r
fix_names(x, if_start_with_number_add = "m")
```

## Arguments

- x:

  a character vector.

- if_start_with_number_add:

  character of length 1. If the name start with a number, a character
  indicated in this argument will be added at the beginning.

## Value

a single character with special character replaced.
