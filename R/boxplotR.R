


library(ggplot2)

boxplotR <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Check if input contains only one column
  if (length(data) == 1 && is.vector(data)) {
    stop("Input must be a data frame, not a single vector or column.")
  }

  # Check if data frame contains at least one numeric or categorical variable
  contains_valid_vars <- any(sapply(data, function(x) is.factor(x)))
  if (!contains_valid_vars) {
    stop("Data frame must contain at least one numeric and one factor variable.")
  }

  factor_vars <- names(data)[sapply(data, is.factor)]  # Get factor variable names
  numeric_vars <- names(data)[sapply(data, is.numeric)]  # Get numeric variable names

  plots <- list()

  # Loop through each combination of factor and numeric variable
  for (x in factor_vars) {
    for (y in numeric_vars) {
      plot_title <- paste("Boxplot of", y, "by", x)
      plot <- ggplot(data, aes_string(x = x, y = y)) +
        geom_boxplot(fill = "skyblue", color = "black") +
        labs(title = plot_title) +
        theme_minimal()
      plots[[paste(x, y, sep = "_")]] <- plot
    }
  }

  return(plots)
}

