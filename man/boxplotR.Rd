\name{boxplotR}
\alias{boxplotR}
\title{Create Boxplots for Factor and Numeric Variable Combinations}
\usage{
boxplotR(data)
}
\arguments{
  \item{data}{A data frame containing the variables to be plotted.}
}
\description{
Generates boxplots for each combination of factor and numeric variables in the input data frame.
}
\details{
This function loops through each combination of factor and numeric variables in the provided data frame and creates a list of boxplots using \code{ggplot2}. The factor variables are used for the x-axis and the numeric variables are used for the y-axis.
}
\examples{
# Example usage
library(ggplot2)
data(iros)
boxplots <- boxplotR(iris)
print(boxplots)
}
