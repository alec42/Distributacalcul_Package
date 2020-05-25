#' @param shape shape parameter \eqn{n}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x"), "@param x quantile.", "") %>
#' <%=ifelse(exists("q", inherits = F), "@param q quantile.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("lower.tail"), "@param lower.tail logical; if \\code{TRUE} (default) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
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
