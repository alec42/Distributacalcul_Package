#' @param shape1 first shape parameter \eqn{\alpha}{alpha}, must be positive integer.
#' @param shape2 second shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate \eqn{\lambda}{lambda} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Burr distribution with rate parameter \eqn{\lambda}{lam} as well as shape
#'   parameters \eqn{\alpha}{a} and \eqn{\tau}{t} has density:
#'   \deqn{f\left(x\right) = \frac{\alpha \tau \lambda^{ \alpha } x^{ \tau - 1 }}{(\lambda + x^{ \tau })^{\alpha +1}}}{f(x) = (a t lam^a x^(t - 1))/ (lam + x^t)^(a + 1)}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\alpha, \tau, \lambda > 0}{a, t, lam > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{E_burr}}}{ gives the expected value.}
#'  \item{\code{\link{V_burr}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_burr}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_burr}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_burr}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_burr}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_burr}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_burr}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_burr}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Burr Distribution
#'
