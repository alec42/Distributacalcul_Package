#' @param shape shape parameter \eqn{n}, must be positive integer.
#' @param scale \eqn{\beta}{beta} is the scale parameter, must be positive.
#' @param rate alternative parameterization to scale parameter, rate = 1 / scale.
#'
#' <%=ifelse(exists("k") && k, "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x") && x, "@param x quantile.", "") %>
#' <%=ifelse(exists("q") && q, "@param q quantile.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kappa") && kappa, "@param kappa probability.", "") %>
#' <%=ifelse(exists("vark") && vark, "@param vark Value-at-Risk (VaR) calculated at the given probability kappa.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are \\eqn{\\text{Pr}(M \\leq k)}{Pr(M <= k)}, otherwise, \\eqn{\\text{Pr}(M > k)}{Pr(M > k)}.", "") %>
#'
#' @details
#'  The Erlang distribution with shape parameter \eqn{n} and rate parameter
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{n}}{\Gamma(n)} x^{n - 1}%
#'    \mathrm{e}^{-\beta x}}{f(x) = b^n / \Gamma(n) x^{n - 1} e^{-b x}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}, \eqn{n \in \mathcal{N}^+}{n = 1, 2, 3, ...}
#'
#' @family Erlang Distribution
#'
