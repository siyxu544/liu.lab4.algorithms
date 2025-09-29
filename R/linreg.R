#' Perform Linear Regression using Ordinary Least Squares
#' @description
#' This function fits a multiple linear regression model using Ordinary Least Squares (OLS)
#' based on a provided formula and data frame. It computes regression coefficients,
#' fitted values, residuals, and key statistical metrics for inference.
#' @param formula A `formula` object specifying the model, for example, `Petal.Length ~ Sepal.Length + Species`.
#' @param data A `data.frame` containing the variables specified in the formula. The function
#' includes a pre-processing step to automatically convert numeric-like character or
#' factor columns into a numeric type.
#' @return
#' An object of S3 class `linreg`. This object is a list containing the following components:
#' \item{coefficients}{A matrix of the estimated regression coefficients (`beta_hat`).}
#' \item{fitted.values}{A matrix of the model's fitted values (`y_hat`).}
#' \item{residuals}{A matrix of the model's residuals (`e_hat`).}
#' \item{df}{An integer representing the degrees of freedom.}
#' \item{residual_variance}{A numeric scalar of the residual variance (`v_hat`).}
#' \item{variance_coefficients}{The variance-covariance matrix of the regression coefficients (`v_beta_hat`).}
#' \item{t_values}{A matrix of the t-values for each coefficient (`t_beta`).}
#' \item{p_values}{A matrix of the p-values for each coefficient (`p_beta`).}
#' @examples
#' # Fit a linear model on the iris dataset
#' linreg_model <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' @importFrom stats model.matrix pt
#' @export

linreg <- function(formula, data) {
  current_call <- match.call()
  if (!is.data.frame(data)) {
    stop("Invalid input! Argument \"data\" should be a dataframe.")
  }
  if (!inherits(formula, "formula")) {
    stop("Invalid input! Argument \"formula\" should be a formula, e.g. y ~ x + z")
  }
  formula_vars <- all.vars(formula)
  #  if(!(attr(terms(formula),"variables") %in% colnames(data))){
  if (!all(formula_vars %in% colnames(data))) {
    stop("Formula contains variable(s) not belonging to data's column names.")
  }

  if (!is.numeric(data[[all.vars(formula)[1]]])) {
    stop(paste(
      "The dependent variable (",
      all.vars(formula)[1],
      ") must be numeric."
    ))
  }
  # Comment the following code that causes a bug
  # for(var_name in formula_vars){
  #   if(!is.numeric(data[,var_name])){
  #     converted_column_value <- suppressWarnings(as.numeric(as.character(data[,var_name])))
  #     if (any(is.na(converted_column_value))){
  #       stop(paste0("Column '", var_name,"' contains non-numeric value(s) that could not be converted."))}
  #     else{
  #       data[,var_name] <- converted_column_value
  #     }
  #   }
  # }
  X <- model.matrix(formula, data)
  y <- data[, formula_vars[1]]
  if (nrow(X) <= ncol(X)) {
    if (nrow(X) == ncol(X)) {
      stop(
        paste0(
          "Degrees of freedom are 0. Cannot perform statistical inference when the number of observations (",
          nrow(X),
          ") is not greater than the number of parameters (",
          ncol(X),
          ")."
        )
      )
    } else{
      stop(
        paste0(
          "The number of observations (n=",
          nrow(X),
          ") is less than the number of parameters to estimate (p=",
          ncol(X),
          "). You need at least ",
          ncol(X) + 1,
          " data points to fit this model and perform statistical inference."
        )
      )
    }
  }
  # Regressions coefficients:
  #   beta_hat <- (solve(t(X) %*% X)) %*% t(X) %*% y
  QR <- qr(X) # Perform the QR decomposition on the design matrix X
  p <- QR$rank # The number of parameters
  Qty <- qr.qty(QR, y) # Calculate Q^T * y
  beta_hat_vector <- backsolve(qr.R(QR), Qty[1:p]) # Solve R * beta_hat = Q^T * y using back substitution
  names(beta_hat_vector) <- colnames(X)
  beta_hat <- as.matrix(beta_hat_vector)
  # The fitted values:
  y_hat <- X %*% beta_hat
  # The residuals:
  e_hat <- y - (X  %*% beta_hat)
  # The degrees of freedom:
  df <- nrow(X) - ncol(X)
  # The residual of variance:
  v_hat <- (t(e_hat) %*% e_hat) / df
  v_hat <- v_hat[1, 1]
  # The variance of the regression coefficients:
  #  v_beta_hat <- v_hat * solve(t(X) %*% X)
  R_matrix <- qr.R(QR)
  R_inverse <- backsolve(R_matrix, diag(ncol(X)))
  XtX_inverse <- R_inverse %*% t(R_inverse)
  v_beta_hat <- v_hat * XtX_inverse
  # The t-values for each coefficient:
  t_beta <- (beta_hat / (sqrt(diag(v_beta_hat))))
  # The p-values for each coefficient:
  p_beta <- (2 * pt(abs(t_beta), df, lower.tail = FALSE))

  result <- list(
    beta_hat = beta_hat,
    y_hat = y_hat,
    e_hat = e_hat,
    df = df,
    v_hat = v_hat,
    v_beta_hat = v_beta_hat,
    t_beta = t_beta,
    p_beta = p_beta,
    call = current_call
  )
  class(result) <- "linreg"
  return(result)
}
