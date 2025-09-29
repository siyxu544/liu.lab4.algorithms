#' Print method for linreg objects
#' @description Prints coefficients of a `linreg` model object.
#' @param x An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... further arguments passed to or from other methods (ignored in this method).
#' @return The original `linreg` object (returned invisibly).
#' @examples
#' linreg_mod <- linreg(formula = Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' print(linreg_mod)
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$beta_hat[, 1])
  invisible(x)
}

#' @import ggplot2
#' @importFrom quantreg rq
#' @import patchwork
#' @export
plot.linreg <- function(x, ...) {
  plot_data <- data.frame(
    residuals = x$e_hat,
    sqrt_std_res = sqrt(abs((x$e_hat / (
      sqrt(x$v_hat)
    )))),
    fitted_values = x$y_hat,
    row_id = 1:length(x$e_hat)
  )
  # Identify top 3 outliers for labeling
  top_outliers1 <- plot_data[order(abs(plot_data$residuals), decreasing = TRUE), ][1:3, ]
  top_outliers2 <- plot_data[order(abs(plot_data$sqrt_std_res), decreasing = TRUE), ][1:3, ]
  # --- Dynamically determine axis breaks ---
  # 1. Get the range of the x and y data, ignoring any NAs
  x_axis_range <- range(plot_data$fitted_values, na.rm = TRUE)
  y_axis_range1 <- range(plot_data$residuals, na.rm = TRUE)
  y_axis_range2 <- range(plot_data$sqrt_std_res, na.rm = TRUE)
  # 2. Use the pretty() function to generate a nice sequence of breaks
  x_breaks <- pretty(x_axis_range)
  y_breaks1 <- pretty(y_axis_range1)
  y_breaks2 <- pretty(y_axis_range2)
  # Create the plot AND ASSIGN IT TO A VARIABLE
  plot1 <- ggplot(data = plot_data, aes(x = fitted_values, y = residuals)) +
    geom_point(shape = 1, size = 3) +
    geom_hline(yintercept = 0,
               linetype = "dotted",
               color = "grey40") +
    geom_smooth(
      method = "loess",
      se = FALSE,
      color = "red",
      linewidth = 0.1,
      span = 2 / 3
    ) +
    geom_smooth(
      method = "rq",
      se = FALSE,
      color = "purple",
      linewidth = 0.1
    ) +
    geom_text(
      data = top_outliers1,
      aes(label = row_id),
      nudge_x = 0.1,
      hjust = 0
    ) + # Add outlier labels
    labs (
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals",
      caption = deparse(x$call)
    ) +
    # --- Use the dynamically generated breaks ---
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks1) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      # Remove major grid lines
      panel.grid.minor = element_blank(),
      # Remove minor grid lines
      panel.border = element_rect(color = "black", fill = NA),
      # Add a black border
      plot.title = element_text(hjust = 0.5),
      # Center the main title
      plot.caption = element_text(hjust = 0.5)  # Center the caption
    )

  plot2 <- ggplot(data = plot_data, aes(x = fitted_values, y = sqrt_std_res)) +
    geom_point(shape = 1, size = 3) +
    geom_hline(yintercept = 0,
               linetype = "dotted",
               color = "grey40") +
    geom_smooth(
      method = "loess",
      se = FALSE,
      color = "red",
      linewidth = 0.1,
      span = 2 / 3
    ) +
    geom_smooth(
      method = "rq",
      se = FALSE,
      color = "purple",
      linewidth = 0.1
    ) +
    geom_text(
      data = top_outliers2,
      aes(label = row_id),
      nudge_x = 0.1,
      hjust = 0
    ) +
    labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt(abs(
        "Standardized residuals"
      ))),
      caption = deparse(x$call)
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks2) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)
    )
  suppressMessages(print(plot1 + plot2 + plot_layout(ncol = 1)))
  invisible(x)
}
