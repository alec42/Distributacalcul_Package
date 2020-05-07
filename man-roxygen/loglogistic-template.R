#' @param shape shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate alternative parameterization the scale parameter, rate = 1 / scale.
#' @param scale \eqn{\lambda}{lambda} rate parameter, must be positive.
#'
#' <%=ifelse(exists("k") && k, "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x") && x, "@param x quantile.", "") %>
#' <%=ifelse(exists("q") && q, "@param q quantile.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#'
#' @details
#'  The Loglogistic distribution with shape parameter \eqn{\tau}{t} and scale parameter
#'   \eqn{\lambda}{lam} has density:
#'   \deqn{\frac{\tau \lambda^\tau x^{\tau -1}}{(\lambda^{\tau }+x^{\tau })^{2}}}{f(x) = t lam^t x^(t - 1) / (lam^t + x^t)^2}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\lambda, \tau > 0}{lam, t > 0}.
#'
#' @family Loglogistic Distribution
#'
