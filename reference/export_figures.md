# Export plots

It exports a plot or a list of plots created with ggplot or ggplot-like
functions into the current working directory.

## Usage

``` r
export_figures(
  plots,
  exprtname_figures = NULL,
  exprt_fig_type = "png",
  plot_sizes = c(NA, NA),
  plot_unit = "in"
)
```

## Arguments

- plots:

  a plot or a list of plots of the types "gg" and "ggplot".

- exprtname_figures:

  NULL or a character of length 1. The desired name for the file to
  create (do not add here the file extension as it will be added
  automatically based on the next argument). If NULL, the name of the
  object passed to the argument plots will be used.

- exprt_fig_type:

  one of the following: "pdf", "eps", "ps", "tex", "jpeg", "tiff",
  "png", "bmp", "svg", "wmf". The desired type of file to create: if
  "pdf" and there are multuple plots, it will create a single file with
  each plot in a different page; otherwise, it will use ggsave
  considering the plot_sizes and plot_unit arguments, and it will create
  a different picture for each plot in plots.

- plot_sizes:

  a numeric vector of length 2. It should contains the width and height
  for saving the figures in png format. If not specified, it will use
  the size of current graphics device (as in the ggsave function)

- plot_unit:

  one of the following: "in", "cm", "mm", or "px". It is referred to the
  with and height of the plot_sizes argument (as in the ggsave function)

## Value

It creates file(s) in the current working director.
