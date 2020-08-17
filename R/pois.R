#' Poisson Distribution
#'
#' @description
#' Poisson distribution with rate parameter \eqn{\lambda}{lambda}.
#'
#' @details
#' The Poisson distribution with rate parameter \eqn{\lambda}{lam}
#' has probability mass function :
#'   \deqn{Pr(X = k) = \frac{\lambda^k \textrm{e}^{-\lambda}}{k!}}{Pr(X = k) = (lam^k e^(-lam)) / k!}
#' for \eqn{k = 0, 1, 2, \dots}{k = 0, 1, 2, ...}, and \eqn{\lambda > 0}{lam > 0}
#'
#' @template lambda-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValPois}}}{ gives the expected value.}
#'     \item{\code{\link{varPois}}}{ gives the variance.}
#'     \item{\code{\link{expValTruncPois}}}{ gives the truncated mean.}
#'     \item{\code{\link{TVatRPois}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{mgfPois}}}{ gives the moment generating function (MGF).}
#'     \item{\code{\link{pgfPois}}}{ gives the probability generating function (PGF).}
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Pois
#'
NULL

#' @rdname Pois
#'
#' @export
#'
#' @examples
#' expValPois(lambda = 3)
#'
expValPois <- function(lambda) {
    stopifnot(lambda > 0)

    lambda
}

#' @rdname Pois
#'
#' @export
#'
#' @examples
#' varPois(lambda = 3)
#'
varPois <- function(lambda) {
    stopifnot(lambda > 0)

    lambda
}

#' @rdname Pois
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' expValTruncPois(d = 0, lambda = 2, k0 = 2E2, less.than.d = FALSE)
#' expValTruncPois(d = 2, lambda = 2, k0 = 2E2, less.than.d = TRUE)
#'
expValTruncPois <- function(d, lambda, k0, less.than.d = TRUE) {
    stopifnot(lambda > 0)
    k <- 0:k0 # valeurs possibles
    fx <- stats::dpois(x = k, lambda = lambda)

    if (less.than.d) {
        expValTrunc.approx <- sum((k * fx)[k <= d])
    } else {
        expValTrunc.approx <- sum((k * fx)[k > d])
    }

    message("This is an approximation")
    return(expValTrunc.approx)
}

#' @rdname Pois
#'
#' @template kap-template
#' @template k0-template
#'
#' @importFrom stats ppois qpois dpois
#' @export
#'
#' @examples
#' TVatRPois(kap = 0.8, lambda = 3, k0 = 2E2)
#'
TVatRPois <- function(kap, lambda, k0) {
    stopifnot(kap >= 0, kap < 1, lambda > 0, k0 > 0)

    k <- 0:k0 # valeurs possibles
    fx <- stats::dpois(x = k, lambda = lambda)
    vatrk <- stats::qpois(p = kap, lambda = lambda)

    TVatR.approx <- (
        expValTruncPois(vatrk, lambda, k0, less.than.d = FALSE) +
            vatrk * (stats::ppois(q = vatrk, lambda = lambda) - kap)
    ) / (1 - kap)

    return(TVatR.approx)
}

#' @rdname Pois
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfPois(t = 1, lambda = 3)
#'
mgfPois <- function(t, lambda) {
    stopifnot(lambda > 0)

    exp(lambda * (exp(t) - 1))
}

#' @rdname Pois
#'
#' @template t-template
#' @export
#'
#' @examples
#' pgfPois(t = 1, lambda = 3)
#'
pgfPois <- function(t, lambda) {
    stopifnot(lambda > 0)

    exp(lambda * (t - 1))
}
