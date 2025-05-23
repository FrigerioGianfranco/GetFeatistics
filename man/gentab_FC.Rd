% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gentab_FC.R
\name{gentab_FC}
\alias{gentab_FC}
\title{Generate a Table with Fold Change}
\usage{
gentab_FC(
  df,
  v,
  f,
  second_to_first_ratio = TRUE,
  paired = FALSE,
  are_log_transf = FALSE,
  log_base = 2,
  filter_sign = FALSE,
  FCcutoff = 2
)
}
\arguments{
\item{df}{a dataframe.}

\item{v}{a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.}

\item{f}{character vector of length 1. Name of the column of df containing the factor variable (that must have exactly 2 levels) considered for performing the Fold Change analysis.}

\item{second_to_first_ratio}{logical. If TRUE the second group/first group ratio will be computed, if FALSE the first group/second group ratio will be computed.}

\item{paired}{logical. If FALSE it performs FC on mean of the two groups. If TRUE it performs FC for each pair and then compute the mean.}

\item{are_log_transf}{logical. If you really need to perform this FC analysis on already log-transformed data, specify here as TRUE, and the subtraction will be performed instead of the ratio.}

\item{log_base}{numeric of length 1. Specify here the base of the logarithm to calculate the logFC or, if are_log_transf is TRUE; the base of the logarithm that were used to transform the data.}

\item{filter_sign}{logical. If TRUE, the table will be filtered and only those that passed the FCcutoff will be retained.}

\item{FCcutoff}{numeric of length 1. If filter_sign is TRUE, the value of the FCcutoff to consider a feature difference as significant.}
}
\value{
A tibble the results of the Fold Change analysis
}
\description{
Given a dataframe and a set of numerical variables of that dataframe, it performs fold changes analysis to each desired variable and creates a new table.
}
