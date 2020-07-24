#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' @details
#'  The Exponential distribution with rate parameter \eqn{\beta}{b} has %
#'   density: \deqn{f\left(x\right) = \frac{1}{\beta}%
#'   \textrm{e}^{-\beta x}}{f(x) = b^a e^{-b x}} for %
#'   \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_exp}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{E_exp}}}{ gives the expected value.}
#'  \item{\code{\link{V_exp}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_exp}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_exp}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_exp}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_exp}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_exp}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_exp}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_exp}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Exponential Distribution
#'
