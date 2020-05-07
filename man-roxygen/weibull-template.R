#' @param shape shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#'
#' @details
#'  The Weibull distribution with shape parameter \eqn{\tau}{t} and rate parameter
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \beta \tau \left( \beta x \right) ^{\tau -1} %
#'    \mathrm{e}^{-\left( \beta x\right) ^{\tau }}}{f(x) = b t (b x)^(t - 1) %
#'    e^{(-b x)^t}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}, \eqn{\tau > 0}{t > 0}
#'
#' @family Weibull Distribution
#'
