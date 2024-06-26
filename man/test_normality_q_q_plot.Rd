% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_normality_q_q_plot.R
\name{test_normality_q_q_plot}
\alias{test_normality_q_q_plot}
\title{Test normality by generating a Q-Q plot}
\usage{
test_normality_q_q_plot(df, v)
}
\arguments{
\item{df}{a dataframe.}

\item{v}{a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.}
}
\value{
a list containing a number of element equal to the length of v. Each element is a Q-Q plot.
}
\description{
Given a dataframe and a set of numerical variables of that dataframe, it creates a Q-Q plot for each desired variable.
}
