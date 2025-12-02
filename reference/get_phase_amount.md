# Get the amount of mobile phases needed for a sequence.

Given a dataframe containing the gradient steps, it calculates the
amount of each phase needed for the sequence.

## Usage

``` r
get_phase_amount(grad, ambient = 2, an = 1)
```

## Arguments

- grad:

  a dataframe with 4 columns. The first column contains the minute of
  each gradient step. The second column is the flow in mL/min of each
  gradient step. The third column is the percentage of the A-phase at
  each step. The four column is the percentage of the B-phase at each
  step.

- ambient:

  numeric of length 1. The number of minutes the system stays at the t0
  conditions between each analysis.

- an:

  numeric of length 1. The number of analyses in the sequence.

## Value

It prints the information on the console.
