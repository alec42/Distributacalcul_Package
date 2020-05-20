#' @param shape shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate alternative parameterization the scale parameter, rate = 1 / scale.
#' @param scale \eqn{\lambda}{lambda} rate parameter, must be positive.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Loglogistic distribution with shape parameter \eqn{\tau}{t} and scale parameter
#'   \eqn{\lambda}{lam} has density:
#'   \deqn{\frac{\tau \lambda^\tau x^{\tau -1}}{(\lambda^{\tau }+x^{\tau })^{2}}}{f(x) = t lam^t x^(t - 1) / (lam^t + x^t)^2}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\lambda, \tau > 0}{lam, t > 0}.
#'
#' @family Loglogistic Distribution
#'
