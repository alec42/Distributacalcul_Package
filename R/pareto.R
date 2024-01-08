#' Pareto Distribution
#'
#' @description
#' Pareto distribution with shape parameter \eqn{\alpha}{alpha} and rate
#' parameter \eqn{\lambda}{lambda}.
#'
#' @details
#' The Pareto distribution with rate parameter \eqn{\lambda}{lam} as well as shape
#' parameter \eqn{\alpha}{a} has density:
#'   \deqn{f\left(x\right) = \frac{\alpha\lambda^{\alpha}}%
#'   {(\lambda + x)^{\alpha + 1}}}{f(x) = (a lam^a)/ (lam + x)^(a + 1)}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\alpha, \lambda > 0}{a, lam > 0}.
#'
#' @template shape-template
#' @template rate-lambda-template
#' @template scale-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{dPareto}}  gives the probability density function (PDF).
#'     \item \code{\link{pPareto}}  gives the cumulative density function (CDF).
#'     \item \code{\link{expValPareto}}  gives the expected value.
#'     \item \code{\link{varPareto}}  gives the variance.
#'     \item \code{\link{kthMomentPareto}}  gives the kth moment.
#'     \item \code{\link{expValLimPareto}}  gives the limited mean.
#'     \item \code{\link{expValTruncPareto}}  gives the truncated mean.
#'     \item \code{\link{stopLossPareto}}  gives the stop-loss.
#'     \item \code{\link{meanExcessPareto}}  gives the mean excess loss.
#'     \item \code{\link{VatRPareto}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRPareto}}  gives the Tail Value-at-Risk.
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Pareto
#'
NULL

#' @rdname Pareto
#'
#' @template x-template
#' @export
#'
#' @examples
#' # With scale parameter
#' dPareto(x = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' dPareto(x = 2, shape = 2, rate = .20)
#'
dPareto <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(x >= 0, shape > 1, rate > 0)

    shape * rate^shape / (rate + x)^(shape + 1)
}

#' @rdname Pareto
#'
#' @template q-template
#' @template lower.tail-template
#' @export
#'
#' @examples
#' # With scale parameter
#' pPareto(q = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' pPareto(q = 2, shape = 2, rate = 0.20)
#'
#' # Survival function
#' pPareto(q = 2, shape = 2, rate = 0.20, lower.tail = FALSE)
#'
pPareto <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(q >= 0, shape > 1, rate > 0)

    Sx <- (rate / (rate + q))^shape

    return(ifelse(lower.tail, 1 - Sx, Sx))
}

#' @rdname Pareto
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' expValPareto(shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' expValPareto(shape = 5, rate = 2)
#'
expValPareto <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0)

    rate / (shape - 1)
}

#' @rdname Pareto
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' varPareto(shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' varPareto(shape = 5, rate = 2)
#'
varPareto <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0)

    (shape * rate^2) / ((shape - 1)^2 * (shape - 2))
}

#' @rdname Pareto
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' kthMomentPareto(k = 4, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' kthMomentPareto(k = 4, shape = 5, rate = 2)
#'
kthMomentPareto <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, shape > k)

    if (k %% 1 == 0 && k > 0) {
        kthMoment.pareto <- (rate^k * factorial(k)) / prod(shape - seq(from = 1, to = k, by = 1))
    } else if (k > -1) {
        kthMoment.pareto <- (rate^k * gamma(k + 1) * gamma(shape - k))/gamma(shape)
    }

    return(kthMoment.pareto)
}

#' @rdname Pareto
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' expValLimPareto(d = 4, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' expValLimPareto(d = 4, shape = 5, rate = 2)
#'
expValLimPareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    expValPareto(shape, rate) * pPareto(q = d, shape - 1, rate)
}

#' @rdname Pareto
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' expValTruncPareto(d = 4, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' expValTruncPareto(d = 4, shape = 5, rate = 2)
#'
expValTruncPareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    if (less.than.d) {
        expValTrunc.pareto <- expValPareto(shape, rate) * pPareto(q = d, shape - 1, rate) -
            d * pPareto(q = d, shape, rate, lower.tail = FALSE)
    } else {
        expValTrunc.pareto <- expValPareto(shape, rate) * pPareto(q = d, shape - 1, rate, lower.tail = FALSE) +
            d * pPareto(q = d, shape, rate, lower.tail = FALSE)
    }

    return(expValTrunc.pareto)
}

#' @rdname Pareto
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' stopLossPareto(d = 2, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' stopLossPareto(d = 2, shape = 5, rate = 2)
#'
stopLossPareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    expValPareto(shape, rate) * pPareto(q = d, shape - 1, rate, lower.tail = FALSE)
}

#' @rdname Pareto
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' meanExcessPareto(d = 6, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' meanExcessPareto(d = 6, shape = 5, rate = 2)
#'
meanExcessPareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    expValPareto(shape, rate + d)
}

#' @rdname Pareto
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' VatRPareto(kap = .99, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' VatRPareto(kap = .99, shape = 5, rate = 2)
#'
VatRPareto <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap >= 0, kap <= 1)

    rate * ((1 - kap)^(-1 / shape) - 1)
}

#' @rdname Pareto
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' TVatRPareto(kap = .99, shape = 5, scale = 0.5)
#'
#' # With rate parameter
#' TVatRPareto(kap = .99, shape = 5, rate = 2)
#'
TVatRPareto <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, kap >= 0, kap <= 1)

    rate * ((shape / (shape - 1)) * (1 - kap)^(-1/shape) - 1)
}

