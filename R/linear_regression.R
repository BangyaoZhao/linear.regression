#'linear_regression
#'
#'@importFrom stats pf pt
#'
#'@param y a vector, outcomes.
#'
#'@param x a numerical matrix, each row represents an observation and each column represents a covariate.
#'
#'@param intercept a logical value, TRUE by default, indicating should the function include a intercept for the user.
#'
#'@param y_bars a logical value, indicating should the function output the estimated y's.
#'It is FALSE by default, but will be set TRUE if either
#'ss, variances, t_test, f_test or residuals
#'is set TRUE
#'
#'@param ss a logical value, indicating should the function output sum of squares.
#'It is FALSE by default, but will be set TRUE if either
#'variances, f_test or t_test
#'is set TRUE
#'
#'@param variances a logical value, indicating should the function output variances.
#'It is FALSE by default, but will be set TRUE if
#'t_test
#'is set TRUE
#'
#'@param t_test a logical value, indicating should the function output results of t test.
#'
#'@param f_test a logical value, indicating should the function output results of f test.
#'
#'@param hat_matrix a logical value, indicating should the function output the hat matrix.
#'
#'@param residuals a logical value, indicating should the function output residuals.
#'
#'@return the regression result, a list
#'
#'@examples
#'linear_regression(c(3,5,6),c(1,2,3))
#'
#'@export
#'




linear_regression = function(y,
                             x,
                             intercept = T,
                             y_bars = F,
                             ss = F,
                             variances = F,
                             t_test = F,
                             f_test = F,
                             hat_matrix = F,
                             residuals = F) {
  #the covariates matrix
  x = as.matrix(x)
  #the outcomes
  y = as.matrix(y)
  #the number of coefficients
  q = ncol(x)
  #the number of observations
  n = nrow(x)

  #if colnames are missing, add colnames
  if (length(colnames(x)) == 0) {
    variable_names = character(q)
    for (i in 1:q)
      variable_names[i] = paste0("q", i, collapse = "")
    colnames(x) = variable_names
  }

  #add the intercept if requested
  if (intercept == T) {
    x = cbind(intercept = rep(1, n), x)
    q = q + 1
  }

  #solve the estimated coefficients
  coefs = solve(t(x) %*% x,t(x) %*% y)
  coefs = as.matrix(coefs)


  #res is the returned result list
  res <- list(coefs = coefs)

  #y_bars are needed for calculating residuals
  if (residuals == T)
    y_bars = T

  #y_bars are needed for calculating sum of squares
  if (ss == T)
    y_bars = T

  #y_bars and sum of squares are needed for calculating variances
  if (variances == T) {
    y_bars = T
    ss = T
  }

  #variances, y_bars and sum of squares are needed for t_test
  if (t_test == T) {
    y_bars = T
    ss = T
    variances = T
  }

  #y_bars and sum of squares are needed for f_test
  if (f_test == T) {
    y_bars = T
    ss = T
  }

  #calculate y_bars
  if (y_bars == T) {
    y_bars = x %*% coefs
    res$y_bars = y_bars
  }

  #calculate r_square, sum of squares and their df's
  if (ss == T) {
    if (intercept) ssy = sum((y - sum(y) / n) ^ 2)
    else ssy=sum(y^2)
    dfy = n - intercept
    sse = sum((y - y_bars) ^ 2)
    dfe = n - q
    ssr = ssy - sse
    dfr = q - intercept
    r_square = ssr / ssy
    ss = list(
      ssy = ssy,
      dfy = dfy,
      sse = sse,
      dfe = dfe,
      ssr = ssr,
      dfr = dfr,
      r_square = r_square
    )
    res$ss = ss
  }

  #calculate variances
  if (variances == T) {
    mse = sse / dfe
    v_coefs = solve(t(x) %*% x) * mse
    variances = list(mse = mse, v_coefs = v_coefs)
    res$variances = variances
  }

  #calculate t_statistics, df's, and p_values
  if (t_test == T) {
    dfe = n - q
    t_statistics = coefs / sqrt(v_coefs[(q + 1) * (1:q) - q])
    p_values = 2 * pt(-abs(t_statistics), n - q)
    t_test = list(t_statistics = t_statistics,
                  p_values = p_values,
                  df = dfe)
    res$t_test = t_test
  }

  #calculate f_statistics, df, and p_value
  if (f_test == T) {
    dfr = q - intercept
    dfe = n - q
    f_statistics = (r_square / (1 - r_square)) * (dfe / dfr)
    p_value = 1 - pf(f_statistics, dfr, dfe)
    f_test = list(
      f_statistics = f_statistics,
      p_value = p_value,
      df = c(dfr, dfe)
    )
    res$f_test = f_test
  }

  #calculate the hat matrix
  if (hat_matrix == T) {
    hat_matrix = x %*% solve(t(x) %*% x) %*% t(x)
    res$hat_matrix = hat_matrix
  }

  #calculate residuals
  if (residuals == T) {
    residuals = y - y_bars
    res$residuals = residuals
  }

  #the return step
  return(res)
}
