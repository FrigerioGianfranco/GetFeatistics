# Zero prefixing numbers

It converts a number to a character with zeros as prefix, useful for
having them ordered by name correctly.

## Usage

``` r
zero_prefixing(
  numbers,
  highest = max(numbers),
  additional_prefix = NULL,
  additional_suffix = NULL
)
```

## Arguments

- numbers:

  numeric. Numbers to be converted.

- highest:

  numeric of length 1. The highest number, used to define the numbers of
  zeros. By default, it uses the max of the vector passed to numbers.

- additional_prefix:

  NULL or character of length 1. An additional prefix to add at the
  beginning of each number.

- additional_suffix:

  NULL or character of length 1. An additional suffix to add at the
  beginning of each number.

## Value

A character vector with the added zeros as prefix.
