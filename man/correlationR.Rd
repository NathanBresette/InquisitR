\name{correlationR}
\alias{correlationR}
\title{Create a pairwise plot matrix with custom upper triangle, lower triangle, and diagonal plots}
\usage{
correlationR(df)
}
\arguments{
  \item{df}{
    The data frame containing the variables to be plotted
  }
}
\description{
This function creates a pairwise plot matrix using the GGally package, with customization options for the upper triangle, lower triangle, and diagonal plots.
}
\details{
The upper triangle plots display correlation coefficients between variables, while the lower triangle plots show scatter plots with optional smoothing. The diagonal plots display density plots for each variable.
}
\examples{
correlationR(iris)
}
