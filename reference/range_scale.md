# Range-scale values

From a vector of numbers, to each element it subtracts the mean and
divide by the difference between the maximum and the minimum values.

## Usage

``` r
range_scale(x, na.rm = FALSE)
```

## Arguments

- x:

  a numeric vector.

- na.rm:

  logical. Should missing value be stripped before the computation?
  (Same as for mean, max and min)

## Value

a numeric vector with the scaled values.
