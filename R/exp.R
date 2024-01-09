#' Exponential Distribution
#'
#' @description
#' Exponential distribution with rate parameter \eqn{\beta}{beta}.
#'
#' @details
#' The Exponential distribution with rate parameter \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{1}{\beta}\textrm{e}^{-\beta x}}{f(x) = b^a e^{-b x}}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}.
#'
#' @template rate-template
#' @template scale-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{expValExp}}  gives the expected value.
#'     \item \code{\link{varExp}}  gives the variance.
#'     \item \code{\link{kthMomentExp}}  gives the kth moment.
#'     \item \code{\link{expValLimExp}}  gives the limited mean.
#'     \item \code{\link{expValTruncExp}}  gives the truncated mean.
#'     \item \code{\link{stopLossExp}}  gives the stop-loss.
#'     \item \code{\link{meanExcessExp}}  gives the mean excess loss.
#'     \item \code{\link{VatRExp}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRExp}}  gives the Tail Value-at-Risk.
#'     \item \code{\link{mgfExp}}  gives the moment generating function (MGF).
#'   }
#' Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Exp
#'
NULL

#' @rdname Exp
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' expValExp(scale = 4)
#'
#' # With rate parameter
#' expValExp(rate = 0.25)
#'
expValExp <- function(rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0)

    1/rate
}

#' @rdname Exp
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' varExp(scale = 4)
#'
#' # With rate parameter
#' varExp(rate = 0.25)
#'
varExp <- function(rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0)

    (1/rate)^2
}

#' @rdname Exp
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' kthMomentExp(k = 2, scale = 4)
#'
#' # With rate parameter
#' kthMomentExp(k = 2, rate = 0.25)
#'
kthMomentExp <- function(k, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0, k >= 0) #domain k?

    (1/rate)^k * factorial(k)
}

#' @rdname Exp
#'
#' @template d-template
#'
#' @importFrom stats pexp
#' @export
#'
#' @examples
#' # With scale parameter
#' expValLimExp(d = 2, scale = 4)
#'
#' # With rate parameter
#' expValLimExp(d = 2, rate = 0.25)
#'
expValLimExp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, rate > 0)

    expValExp(rate) * stats::pexp(d, rate)
}

#' @rdname Exp
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pexp
#' @export
#'
#' @examples
#' # With scale parameter
#' expValTruncExp(d = 2, scale = 4)
#'
#' # With rate parameter, values greater than d
#' expValTruncExp(d = 2, rate = 0.25, less.than.d = FALSE)
#'
expValTruncExp <- function(d, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, rate > 0)

    if (less.than.d) {
        expValTrunc.Exp <- expValExp(rate) * stats::pexp(d, rate) -
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    } else {
        expValTrunc.Exp <- expValExp(rate) * stats::pexp(d, rate, lower.tail = FALSE) +
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    }

    return(expValTrunc.Exp)
}

#' @rdname Exp
#'
#' @template d-template
#'
#' @importFrom stats pexp
#' @export
#'
#' @examples
#' # With scale parameter
#' stopLossExp(d = 2, scale = 4)
#'
#' # With rate parameter
#' stopLossExp(d = 2, rate = 0.25)
#'
stopLossExp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, rate > 0)

    expValExp(rate) * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
}

#' @rdname Exp
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' meanExcessExp(d = 2, scale = 4)
#'
#' # With rate parameter
#' meanExcessExp(d = 5, rate = 0.25)
#'
meanExcessExp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, rate > 0)

    expValExp(rate)
}

#' @rdname Exp
#'
#' @note Function VatRExp is a wrapper of the \code{\link[stats]{qexp}}
#' function from the stats package.
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' VatRExp(kap = .99, scale = 4)
#'
#' # With rate parameter
#' VatRExp(kap = .99, rate = 0.25)
#'
VatRExp <- function(kap, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, rate > 0)

    expValExp(rate) * -log(1 - kap)
}

#' @rdname Exp
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' TVatRExp(kap = .99, scale = 4)
#'
#' # With rate parameter
#' TVatRExp(kap = .99, rate = 0.25)
#'
TVatRExp <- function(kap, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, rate > 0)

    VatRExp(kap = kap, rate) + expValExp(rate)
}

#' @rdname Exp
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfExp(t = 1, rate = 5)
#'
mgfExp <- function(t, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0, t < rate) # domain for t where non-neg?

    rate / (rate - t)
}

