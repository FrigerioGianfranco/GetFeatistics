# Plotting calibration curves of a targeted elaboration

It creates plots of the calibration curves from a targeted elaboration.

## Usage

``` r
plot_calibration_curves(targeted_elaboration)
```

## Arguments

- targeted_elaboration:

  list, an output of the function get_targeted_elaboration.

## Value

a list with ggplots, one for each linear model computed. You can print
it to the console to see the plots, or export them using the function
export_figures.
