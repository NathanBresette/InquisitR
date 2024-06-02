
utils::globalVariables(c("Category", "Frequency"))

library(ggplot2)

distributionR <- function(data, plot_bars = TRUE, plot_histograms = TRUE) {
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Check if input contains only one column
  if (length(data) == 1 && is.vector(data)) {
    stop("Input must be a data frame, not a single vector or column.")
  }

  # Check if data frame contains at least one numeric or categorical variable
  contains_valid_vars <- any(sapply(data, function(x) is.numeric(x) || is.character(x) || is.factor(x)))
  if (!contains_valid_vars) {
    stop("Data frame must contain at least one numeric or categorical (character/factor) variable.")
  }

  plots <- list()

  # Create bar plots for categorical variables
  if (plot_bars) {
    char_factor_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
    for (col in char_factor_cols) {
      freq_table <- table(data[[col]])
      plot_data <- as.data.frame(freq_table)
      names(plot_data) <- c("Category", "Frequency")
      plot <- ggplot(plot_data, aes(x = Category, y = Frequency)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = paste("Bar Plot of", col), x = col, y = "Frequency") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_minimal()
      plots[[paste("barplot", col, sep = "_")]] <- plot
    }
  }

  # Create histograms for numeric variables
  if (plot_histograms) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    for (var in numeric_vars) {
      plot <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 10, fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal()
      plots[[paste("histogram", var, sep = "_")]] <- plot
    }
  }

  return(plots)
}
