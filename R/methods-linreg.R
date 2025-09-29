#' Print method for linreg objects
#' @description Prints coefficients of a `linreg` model object.
#' @param x An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... further arguments passed to or from other methods (ignored in this method).
#' @return The original `linreg` object (returned invisibly).
#' @examples
#' model <- linreg(formula = Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' print(model)
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$beta_hat[, 1])
  invisible(x)
}

#' Extract Model Residuals
#' @description Extracts the vector of residuals from a model object of class "linreg".
#' @param object An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... Further arguments passed to or from other methods (ignored).
#' @return A named numeric vector containing the residual values (y - ŷ).
#' @examples
#' # Create a linreg model
#' model <- linreg(Petal.Length ~ Sepal.Length, data = iris)
#' # Extract the residuals
#' residuals(model)
#' @export
residuals.linreg <- function(object, ...) {
  return(object$e_hat[, 1])
}

#' Generic function for model predictions
#' This is a generic function to extract predictions from various model objects.
#' @param object A model object.
#' @param ... Additional arguments passed to specific methods.
#' @return The predictions from the model.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Extract Model Predictions
#' @description
#' This is a method for the generic function \code{pred} to extract model
#' predictions from an object of class "linreg". It returns the fitted values from the regression.
#' @param object An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... Further arguments passed to or from other methods (ignored).
#' @return A named numeric vector containing the predicted values (ŷ).
#' @examples
#' # Create a linreg model
#' model <- linreg(Petal.Length ~ Sepal.Length, data = iris)
#' # Extract the predicted values
#' pred(model)
#' @export
pred.linreg <- function(object, ...) {
  return(object$y_hat[, 1])
}

#' Extract Model Coefficients
#' @description
#' A method for the generic function \code{coef} to extract model coefficients
#' from an object of class "linreg".
#' @param object An object of class \code{linreg}, usually the result of a call to \code{linreg()}.
#' @param ... Further arguments passed to or from other methods (ignored).
#' @return A named numeric vector containing the regression coefficients.
#' @examples
#' model <- linreg(Petal.Length ~ Sepal.Length, data = iris)
#' coef(model)
#' @export
coef.linreg <- function(object, ...) {
  return(object$beta_hat[, 1])
}

#' Summarize a Linear Model Fit
#' @description
#' Provides a detailed summary of a linear regression model fit, mimicking the
#' output of `summary.lm()`.
#' @details
#' This function prints the model call, a table of coefficients with their standard errors,
#' t-values, and p-values (with significance stars), along with the residual standard error
#' and the degrees of freedom for the model.
#' @param object An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... Further arguments passed to or from other methods (ignored).
#' @return The original `linreg` object (returned invisibly). The primary purpose
#' of this function is its side effect: printing the summary to the console.
#' @examples
#' model <- linreg(Petal.Length ~ Sepal.Length, data = iris)
#' summary(model)
#' @export
summary.linreg <- function(object, ...) {
  cat("Call:\n")
  print(object$call)
  coefficients_df <- data.frame(matrix(nrow = length(object$beta_hat[, 1]), ncol = 5))
  colnames(coefficients_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
  rownames(coefficients_df) <- names(object$beta_hat[, 1])
  sig_stars <- ifelse(object$p_beta[, 1] < 0.001,
                      "***",
                      ifelse(object$p_beta[, 1] < 0.01, "**", ifelse(
                        object$p_beta[, 1] < 0.05, "*", ifelse(object$p_beta[, 1] < 0.1, ".", " ")
                      )))
  coefficients_df[, 1] <- object$beta_hat[, 1] # Estimate
  coefficients_df[, 2] <- sqrt(diag(object$v_beta_hat)) # Std. Error
  coefficients_df[, 3] <- object$t_beta[, 1] # t-value
  coefficients_df[, 4] <- object$p_beta[, 1] # p-value
  coefficients_df[, 5] <- sig_stars # sig_stars
  cat("\nCoefficients:\n")
  print(coefficients_df)
  cat("---\n")
  cat("Signif. codes:\n0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("Residual standard error:",
      round(sqrt(object$v_hat), 4),
      "on",
      object$df,
      "degrees of freedom\n")
  invisible(object)
}

#' Plot Diagnostics for a Linear Model Fit
#' @description
#' Generates two standard diagnostic plots for an object of class "linreg"
#' using the `ggplot2` framework and arranges them in a single column using `patchwork`.
#' @details
#' This function produces two plots to help assess the validity of the linear model assumptions:
#' \enumerate{
#'   \item **Residuals vs. Fitted**: Plots the model residuals against the fitted (predicted) values. This is used to detect non-linear patterns in the residuals.
#'   \item **Scale-Location**: Plots the square root of the absolute value of the standardized residuals against the fitted values. This is used to check for non-constant error variance (heteroscedasticity).
#' }
#' Both plots include a standard LOESS smoother (red line) and a robust, median-based quantile regression smoother (purple line) as suggested in the lab assignment. The top 3 most extreme data points are labeled with their row IDs.
#' @param x An object of class `linreg`, usually the result of a call to `linreg()`.
#' @param ... Further arguments passed to or from other methods (ignored).
#' @return The original `linreg` object (returned invisibly). The function's primary
#' purpose is its side effect: generating and printing the plots.
#' @examples
#' library(quantreg)
#' library(ggplot2)
#' library(patchwork)
#' model <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' plot(model)
#' @import ggplot2
#' @importFrom quantreg rq
#' @import patchwork
#' @export
plot.linreg <- function(x, ...) {
  # Check for residuals (e_hat)
  if (is.null(x$e_hat) || !is.numeric(x$e_hat)) {
    stop("The linreg object 'x' is missing or has a non-numeric 'e_hat' component.")
  }

  # Check for fitted values (y_hat)
  if (is.null(x$y_hat) || !is.numeric(x$y_hat)) {
    stop("The linreg object 'x' is missing or has a non-numeric 'y_hat' component.")
  }

  # Check for residual variance (v_hat)
  if (is.null(x$v_hat) ||
      !is.numeric(x$v_hat) || length(x$v_hat) != 1 || x$v_hat < 0) {
    stop(
      "The linreg object 'x' must have a 'v_hat' component that is a single, non-negative number."
    )
  }
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
  plot1 <- ggplot(data = plot_data, aes(x = .data$fitted_values, y = .data$residuals)) +
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
      aes(label = .data$row_id),
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

  plot2 <- ggplot(data = plot_data, aes(x = .data$fitted_values, y = .data$sqrt_std_res)) +
    geom_point(shape = 1, size = 3) +
    #    geom_hline(yintercept = 0,
    #               linetype = "dotted",
    #          color = "grey40") +
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
      aes(label = .data$row_id),
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
