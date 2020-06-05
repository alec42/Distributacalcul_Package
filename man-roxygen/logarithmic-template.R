#' @param prob probability parameter \eqn{\gamma}{gamma}.
#'
#' @details
#'  The Logarithmic distribution with probability parameter \eqn{\gamma}{gam}
#'  has probability mass function
#'  \deqn{Pr(X = k) = \frac{-\gamma^{k}}{\ln(1 - \gamma)k}}{-gam^k / (ln(1 - k) k)},
#'  for \eqn{k = 0, 1, 2, \dots}{k = 0, 1, 2, ...}, and \eqn{\gamma  \in (0, 1)}{0 < gam < 1}]
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_logarithmic}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{PGF_logarithmic}}}{ gives the probability generating function (PGF).}
#'  \item{\code{\link{E_logarithmic}}}{ gives the expected value.}
#'  \item{\code{\link{V_logarithmic}}}{ gives the variance.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Logarithmic Distribution
#'
