library(GGally)
library(ggplot2)
library(dplyr)

upperFn <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  cor_value <- round(cor(x, y), 2)
  cor_label <- format(cor_value, nsmall = 2)

  data.frame(x = 1, y = 1, cor = cor_value) %>%
    ggplot(aes(x = x, y = y)) +
    geom_tile(aes(fill = cor), color = "white", width = 1, height = 1) +
    geom_text(aes(label = cor_label), color = "black", size = 5, vjust = 0.5) +
    scale_fill_gradient2(low = "blue2", high = "red2", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme_minimal()
}

lowerFn <- function(data, mapping, method = "lm", ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(colour = "skyblue3") +
    geom_smooth(method = method, color = "black", ...) +
    theme_minimal()
}

diagFn <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(aes(y = ..density..), colour = "red", fill = "red", alpha = 0.2) +
    geom_histogram(aes(y = ..density..), colour = "blue", fill = "skyblue3", alpha = 0.5) +
    theme_minimal()
}

correlationR <- function(df) {
  numeric_cols <- df %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()

  if (length(numeric_cols) < 2) {
    stop("The dataframe must contain at least two numeric columns.")
  }

  ggpairs(
    df[, numeric_cols],
    lower = list(continuous = wrap(lowerFn, method = "lm")),
    diag = list(continuous = wrap(diagFn)),
    upper = list(continuous = wrap(upperFn))
  ) + theme_minimal()
}
