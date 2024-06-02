\name{distributionR}
\alias{distributionR}
\title{Distribution Plots}
\usage{
distributionR(data, plot_bars = TRUE, plot_histograms = TRUE)
}
\arguments{
  \item{data}{The input data frame containing variables to be plotted.}
  \item{plot_bars}{Logical. Whether to plot bar charts for categorical variables. Default is \code{TRUE}.}
  \item{plot_histograms}{Logical. Whether to plot histograms for numeric variables. Default is \code{TRUE}.}
}
\description{
Generates distribution plots for the variables in the input data frame.

\itemize{
  \item \code{Category}: A categorical variable representing the categories in the data.
  \item \code{Frequency}: The frequency of occurrences for each category in the data.
}
}
\examples{
# Example usage
library(InquisitR)
data <- iris
distributionR(data)
}
