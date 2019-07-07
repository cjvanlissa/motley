#' @title Conduct t-test for coefficients of a model
#' @description Conduct t-test for coefficients of a model
#' @param x A model.
#' @param par1 Numeric or character. Name or position of the first parameter.
#' @param par2 Numeric or character. Name or position of the second parameter.
#' @return Named vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname t_test_coef
#' @export
t_test_coef <- function(x, par1, par2){
  UseMethod("t_test_coef", x)
}

#' @method t_test_coef rma
#' @export
t_test_coef.rma <- function(x, par1, par2){
  estimate <- coef(x)
  Sigma <- vcov(x)

  Beta1 <- estimate[par1]
  Beta2 <- estimate[par2]
  Var_Beta1 <- Sigma[par1, par1]
  Var_Beta2 <- Sigma[par2, par2]
  Cov_Beta1_Beta2 <- Sigma[par1, par2]

  t_stat <- (Beta1 - Beta2) / sqrt(Var_Beta1 + Var_Beta2 - 2 * Cov_Beta1_Beta2)
  df <- x$k - 2 # Check if this is correct
  p <- 2*pt(abs(t_stat), df, lower.tail = FALSE)
  cat("t(", df, ") = ", formatC(t_stat, digits = 2, format = "f"), ", p ", report(p), sep = "")
  c("t" = as.vector(t_stat), "df" = df, "p" = as.vector(p))
}

#' @title Conduct z-test for coefficients of a model
#' @description Conduct z-test for coefficients of a model
#' @param x A model.
#' @param par1 Numeric or character. Name or position of the first parameter.
#' @param par2 Numeric or character. Name or position of the second parameter.
#' @return Named vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname z_test_coef
#' @export
z_test_coef <- function(x, par1, par2){
  UseMethod("z_test_coef", x)
}

#' @method z_test_coef rma
#' @export
z_test_coef.rma <- function(x, par1, par2){
  estimate <- coef(x)
  Sigma <- vcov(x)

  Beta1 <- estimate[par1]
  Beta2 <- estimate[par2]
  Var_Beta1 <- Sigma[par1, par1]
  Var_Beta2 <- Sigma[par2, par2]
  Cov_Beta1_Beta2 <- Sigma[par1, par2]

  t_stat <- (Beta1 - Beta2) / sqrt(Var_Beta1 + Var_Beta2 - 2 * Cov_Beta1_Beta2)
  p <- 2*pnorm(abs(t_stat), df, lower.tail = FALSE)
  cat("z = ", formatC(t_stat, digits = 2, format = "f"), ", p ", report(p), sep = "")
  c("z" = as.vector(t_stat), "p" = as.vector(p))
}
