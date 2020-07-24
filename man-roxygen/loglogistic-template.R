#' @param shape shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate alternative parameterization the scale parameter, rate = 1 / scale.
#' @param scale \eqn{\lambda}{lambda} rate parameter, must be positive.
#'
#' @details
#'  The Loglogistic distribution with shape parameter \eqn{\tau}{t} and scale parameter
#'   \eqn{\lambda}{lam} has density:
#'   \deqn{\frac{\tau \lambda^\tau x^{\tau -1}}{(\lambda^{\tau }+x^{\tau })^{2}}}{f(x) = t lam^t x^(t - 1) / (lam^t + x^t)^2}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\lambda, \tau > 0}{lam, t > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{dllogis}}}{ gives the density function.}
#'  \item{\code{\link{pllogis}}}{ gives the cumulative density function.}
#'  \item{\code{\link{E_llogis}}}{ gives the expected value.}
#'  \item{\code{\link{V_llogis}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_llogis}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_llogis}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_llogis}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_llogis}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_llogis}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_llogis}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_llogis}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Loglogistic Distribution
#'
