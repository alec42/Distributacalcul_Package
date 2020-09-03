#' Gamma Distribution
#'
#' @description
#' Gamma distribution with shape parameter \eqn{\alpha}{alpha} and rate
#' parameter \eqn{\beta}{beta}.
#'
#' @details
#' The Gamma distribution with shape parameter \eqn{\alpha}{a} and rate
#' parameter \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1}%
#'   \textrm{e}^{-\beta x}}{f(x) = b^a / \Gamma(a) x^{a - 1} e^{-b x}}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta, \alpha > 0}{b, a > 0}.
#'
#' @template shape-template
#' @template rate-template
#' @template scale-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValGamma}}}{ gives the expected value.}
#'     \item{\code{\link{varGamma}}}{ gives the variance.}
#'     \item{\code{\link{kthMomentGamma}}}{ gives the kth moment.}
#'     \item{\code{\link{expValLimGamma}}}{ gives the limited mean.}
#'     \item{\code{\link{expValTruncGamma}}}{ gives the truncated mean.}
#'     \item{\code{\link{stopLossGamma}}}{ gives the stop-loss.}
#'     \item{\code{\link{meanExcessGamma}}}{ gives the mean excess loss.}
#'     \item{\code{\link{VatRGamma}}}{ gives the Value-at-Risk.}
#'     \item{\code{\link{TVatRGamma}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{mgfGamma}}}{ gives the moment generating function (MGF).}
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Gamma
#'
NULL

#' @rdname Gamma
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' expValGamma(shape = 3, scale = 4)
#'
#' # With rate parameter
#' expValGamma(shape = 3, rate = 0.25)
#'
expValGamma <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    shape / rate
}

#' @rdname Gamma
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' varGamma(shape = 3, scale = 4)
#'
#' # With rate parameter
#' varGamma(shape = 3, rate = 0.25)
#'
varGamma <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    shape / (rate^2)
}

#' @rdname Gamma
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthMomentGamma(k = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' kthMomentGamma(k = 2, shape = 3, rate = 0.25)
#'
kthMomentGamma <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0) # k?

    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}

#' @rdname Gamma
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' expValLimGamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' expValLimGamma(d = 2, shape = 3, rate = 0.25)
#'
expValLimGamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    expValGamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate) +
        d * stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE)
}

#' @rdname Gamma
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' expValTruncGamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' expValTruncGamma(d = 2, shape = 3, rate = 0.25)
#'
#' # values greather than d
#' expValTruncGamma(d = 2, shape = 3, rate = 0.25, less.than.d = FALSE)
#'
expValTruncGamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    if (less.than.d) {
        expValTrunc.gamma <- expValGamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate)
    } else {
        expValTrunc.gamma <- expValGamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE)
    }

    return(expValTrunc.gamma)
}

#' @rdname Gamma
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' stopLossGamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' stopLossGamma(d = 2, shape = 3, rate = 0.25)
#'
stopLossGamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    expValGamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) -
        d * stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE)
}

#' @rdname Gamma
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' meanExcessGamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' meanExcessGamma(d = 2, shape = 3, rate = 0.25)
#'
meanExcessGamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    expValGamma(shape, rate) *
        stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) /
        stats::pgamma(d, q = shape, shape = rate, lower.tail = FALSE) -
        d
}

#' @rdname Gamma
#'
#' @note Function VatRGamma is a wrapper for the \code{\link[stats]{qgamma}}
#' function stats package.
#'
#' @template kap-template
#'
#' @importFrom stats qgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VatRGamma(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VatRGamma(kap = .2, shape = 3, rate = 0.25)
#'
VatRGamma <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape > 0, rate > 0)

    stats::qgamma(p = kap, shape = shape, rate = rate)
}

#' @rdname Gamma
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVatRGamma(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVatRGamma(kap = .2, shape = 3, rate = 0.25)
#'
TVatRGamma <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape > 0, rate > 0)

    (expValGamma(shape, rate) / (1 - kap)) *
        stats::pgamma(q = VatRGamma(kap, shape, rate), shape = shape + 1, rate = rate, lower.tail = FALSE)
}

#' @rdname Gamma
#'
#' @template t-template
#' @export
#'
#' @examples
#'
#' mgfGamma(t = 1, shape = 3, rate = 5)
#'
mgfGamma <- function(t, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, t < rate) # domain for t where non-neg?

    (rate / (rate - t))^shape
}
