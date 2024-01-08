#' Weibull Distribution
#'
#' @description
#' Weibull distribution with shape parameter \eqn{\tau}{tau} and rate parameter
#' \eqn{\beta}{beta}.
#'
#' @details
#' The Weibull distribution with shape parameter \eqn{\tau}{t} and rate parameter
#' \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \beta \tau \left( \beta x \right) ^{\tau -1} %
#'   \mathrm{e}^{-\left( \beta x\right) ^{\tau }}}{f(x) = b t (b x)^(t - 1) %
#'   e^{(-b x)^t}}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}, \eqn{\tau > 0}{t > 0}
#'
#' @template shape-tau-template
#' @template rate-template
#' @template scale-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{expValWeibull}}  gives the expected value.
#'     \item \code{\link{varWeibull}}  gives the variance.
#'     \item \code{\link{kthMomentWeibull}}  gives the kth moment.
#'     \item \code{\link{expValLimWeibull}}  gives the limited mean.
#'     \item \code{\link{expValTruncWeibull}}  gives the truncated mean.
#'     \item \code{\link{stopLossWeibull}}  gives the stop-loss.
#'     \item \code{\link{meanExcessWeibull}}  gives the mean excess loss.
#'     \item \code{\link{VatRWeibull}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRWeibull}}  gives the Tail Value-at-Risk.
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Weibull
#'
NULL

#' @rdname Weibull
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' expValWeibull(shape = 2, scale = 5)
#'
#' # With rate parameter
#' expValWeibull(shape = 2, rate = 0.2)
#'
expValWeibull <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    1 / (rate) *
        gamma(1 + 1 / shape)
}

#' @rdname Weibull
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' varWeibull(shape = 2, scale = 5)
#'
#' # With rate parameter
#' varWeibull(shape = 2, rate = 0.2)
#'
varWeibull <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    kthMomentWeibull(shape, rate, k = 2) -
        kthMomentWeibull(shape, rate, k = 1)^2
}

#' @rdname Weibull
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' kthMomentWeibull(k = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' kthMomentWeibull(k = 2, shape = 2, rate = 0.2)
#'
kthMomentWeibull <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0) # restricton on k?

    1 / (rate^k) *
        gamma(1 + k / shape)
}

#' @rdname Weibull
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' expValLimWeibull(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' expValLimWeibull(d = 2, shape = 2, rate = 0.2)
#'
expValLimWeibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    expValWeibull(shape, rate) *
        stats::pgamma(
            q = d^shape,
            shape = 1 + 1/shape,
            scale = rate^shape
        ) +
        d * exp(-(rate * d)^shape)
}

#' @rdname Weibull
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' expValTruncWeibull(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' expValTruncWeibull(d = 2, shape = 2, rate = 0.2)
#'
#' # Mean of values greater than d
#' expValTruncWeibull(d = 2, shape = 2, rate = 0.2, less.than.d = FALSE)
#'
expValTruncWeibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    if (less.than.d) {
        Etrunc.weibull <- expValWeibull(shape, rate) *
            stats::pgamma(
                q = d^shape,
                shape = 1 + 1/shape,
                scale = rate^shape
            )
    } else {
        Etrunc.weibull <- expValWeibull(shape, rate) *
            stats::pgamma(
                q = d^shape,
                shape = 1 + 1/shape,
                scale = rate^shape,
                lower.tail = FALSE
            )
    }

    return(Etrunc.weibull)
}

#' @rdname Weibull
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' stopLossWeibull(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' stopLossWeibull(d = 2, shape = 3, rate = 0.25)
#'
stopLossWeibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    expValWeibull(shape, rate) *
        stats::pgamma(
            q = d^shape,
            shape = 1 + 1/shape,
            scale = rate^shape,
            lower.tail = FALSE
        ) -
        d * exp(-(rate * d)^shape)
}

#' @rdname Weibull
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' meanExcessWeibull(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' meanExcessWeibull(d = 2, shape = 3, rate = 0.25)
#'
meanExcessWeibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    exp((rate * d)^shape) *
        expValWeibull(shape, rate) *
        stats::pgamma(
            q = d^shape,
            shape = 1 + 1/shape,
            scale = rate^shape,
            lower.tail = FALSE
        ) -
        d
}

#' @rdname Weibull
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' VatRWeibull(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VatRWeibull(kap = .2, shape = 3, rate = 0.25)
#'
VatRWeibull <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap <= 1, kap >= 0)

    1 / rate * (-log(1 - kap))^(1/shape)
}

#' @rdname Weibull
#'
#' @template kap-template
#'
#' @importFrom stats qgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' TVatRWeibull(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVatRWeibull(kap = .2, shape = 3, rate = 0.25)
#'
TVatRWeibull <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap < 1, kap >= 0)

    expValWeibull(shape, rate) /
        (1 - kap) *
        stats::pgamma(
            q = -log(1 - kap),
            shape = 1 + 1/shape,
            scale = 1,
            lower.tail = FALSE
        )
}
