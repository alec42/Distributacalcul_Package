#' @param shape1 first shape parameter \eqn{\alpha}{alpha}, must be positive integer.
#' @param shape2 second shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate \eqn{\lambda}{lambda} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k") && k, "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kappa") && kappa, "@param kappa probability.", "") %>
#'
#' @details
#'  The Burr distribution with rate parameter \eqn{\lambda}{lam} as well as shape
#'   parameters \eqn{\alpha}{a} and \eqn{\tau}{t} has density:
#'   \deqn{f\left(x\right) = \frac{\alpha \tau \lambda^{ \alpha } x^{ \tau - 1 }}{(\lambda + x^{ \tau })^{\alpha +1}}}{f(x) = (a t lam^a x^(t - 1))/ (lam + x^t)^(a + 1)}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\alpha, \tau, \lambda > 0}{a, t, lam > 0}.
#'
#' @family Burr Distribution
#'
