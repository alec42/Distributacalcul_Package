#' @param shape shape parameter \eqn{\alpha}{alpha}, must be positive.
#' @param rate \eqn{\lambda}{lambda} rate parameter, must be positive.
#' @param scale alternative parameterization to the rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Pareto distribution with rate parameter \eqn{\lambda}{lam} as well as shape
#'   parameter \eqn{\alpha}{a} has density: \deqn{f\left(x\right) = \frac{\alpha%
#'   \lambda^{\alpha}}{(\lambda + x)^{\alpha + 1}}}{f(x) = (a lam^a)/ (lam + x)^(a + 1)}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\alpha, \lambda > 0}{a, lam > 0}.
#'
#' @family Pareto Distribution
#'
