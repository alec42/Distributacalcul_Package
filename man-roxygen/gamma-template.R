#' @param shape shape parameter \eqn{\alpha}{alpha}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Gamma distribution with shape parameter \eqn{\alpha}{a} and rate
#'    parameter \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1}%
#'    \textrm{e}^{-\beta x}}{f(x) = b^a / \Gamma(a) x^{a - 1} e^{-b x}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta, \alpha > 0}{b, a > 0}.
#'
#' @family Gamma Distribution
#'
