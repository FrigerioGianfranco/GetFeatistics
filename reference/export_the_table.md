# Export the table

It exports a data-frame or a matrix into the current working directory.
Do not use row names, as they will not be exported (but do use column
names!).

## Usage

``` r
export_the_table(tab, exprtname = NULL, exprt_type = "txt")
```

## Arguments

- tab:

  a dataframe or a matrix.

- exprtname:

  NULL or a character of length 1. The desired name for the file to
  create (do not add here the file extension as it will be added
  automatically based on the next argument). If NULL, the name of the
  object passed to the argument tab will be used.

- exprt_type:

  one of the following: "txt", "csv", "xlsx". The desired type of file
  to create: a text file with the values tab_separated (txt), a text
  file with the values comma separated (csv), or an Excel file (xlsx).

## Value

It creates a file in the current working director.
