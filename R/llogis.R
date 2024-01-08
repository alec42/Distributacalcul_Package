#' Loglogistic Distribution
#'
#' @description
#' Loglogistic distribution with shape parameter \eqn{\tau}{tau} and scale
#' parameter \eqn{\lambda}{lambda}.
#'
#' @details
#' The loglogistic distribution with shape parameter \eqn{\tau}{t} and scale parameter
#'   \eqn{\lambda}{lam} has density:
#'   \deqn{\frac{\tau \lambda^\tau x^{\tau -1}}{(\lambda^{\tau }+x^{\tau })^{2}}}{f(x) = t lam^t x^(t - 1) / (lam^t + x^t)^2}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\lambda, \tau > 0}{lam, t > 0}.
#'
#'
#' @template shape-tau-template
#' @template rate-template
#' @template scale-template
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item \code{\link{dLlogis}}  gives the probability density function (PDF).
#'  \item \code{\link{pLlogis}}  gives the cumulative density function (CDF).
#'  \item \code{\link{expValLlogis}}  gives the expected value.
#'  \item \code{\link{varLlogis}}  gives the variance.
#'  \item \code{\link{kthMomentLlogis}}  gives the kth moment.
#'  \item \code{\link{expValLimLlogis}}  gives the limited mean.
#'  \item \code{\link{expValTruncLlogis}}  gives the truncated mean.
#'  \item \code{\link{stopLossLlogis}}  gives the stop-loss.
#'  \item \code{\link{meanExcessLlogis}}  gives the mean excess loss.
#'  \item \code{\link{VatRLlogis}}  gives the Value-at-Risk.
#'  \item \code{\link{TVatRLlogis}}  gives the Tail Value-at-Risk.
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name llogis
#'
NULL

#' @rdname llogis
#'
#' @template x-template
#' @export
#'
#' @examples
#' dLlogis(x = 2, shape = 2, scale = 4)
#'
dLlogis <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        x >= 0,
        shape > 1,
        rate > 0
    )

    (shape * rate^shape * x^(shape - 1)) / (rate^shape + x^shape)^(2)
}


#' @rdname llogis
#'
#' @template q-template
#' @template lower.tail-template
#' @export
#'
#' @examples
#' # With scale parameter
#' pLlogis(q = 3, shape = 3, scale = 5)
#'
#' # With rate parameter
#' pLlogis(q = 3, shape = 3, rate = 0.2)
#'
#' # Survival function
#' pLlogis(q = 3, shape = 3, rate = 0.2, lower.tail = FALSE)
#'
pLlogis <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(
        q >= 0,
        shape > 1,
        rate > 0
    )

    Sx <- rate^shape / (rate^shape + q^shape)
    return(ifelse(lower.tail, 1 - Sx, Sx))
}


#' @rdname llogis
#'
#' @export
#'
#' @examples
#' expValLlogis(shape = 2, scale = 4)
#'
expValLlogis <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape > 1,
        scale > 0
    )

    scale * gamma(1 + 1/shape) * gamma(1 - 1/shape)
}

#' @rdname llogis
#'
#' @export
#'
#' @examples
#' varLlogis(shape = 3, scale = 4)
#'
varLlogis <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape > 2,
        scale > 0
    )

    scale^2 * (
        gamma(1 + 2 / shape) * gamma(1 - 2 / shape) -
            (gamma(1 + 1 / shape) * gamma(1 - 1 / shape))^2
    )
}

#' @rdname llogis
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' kthMomentLlogis(k = 3, shape = 5, scale = 4)
#'
kthMomentLlogis <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape > 0, k > -shape, k < shape,
        scale > 0)

    scale^k * gamma(1 + k/shape) * gamma(1 - k/shape)
}

#' @rdname llogis
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' expValLimLlogis(d = 2, shape = 2, scale = 4)
#'
expValLimLlogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape > 1,
        scale > 0
    )

    expValLlogis(shape, rate) *
        stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape
        ) +
        d * ((scale^shape) / (scale^shape + d^shape))
}

#' @rdname llogis
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' # With rate parameter
#' expValTruncLlogis(d = 2, shape = 2, scale = 4)
#'
#' # Values greater than d
#' expValTruncLlogis(d = 2, shape = 2, scale = 4, less.than.d = FALSE)
#'
expValTruncLlogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(
        d >= 0,
        shape > 1,
        scale > 0
    )

    if (less.than.d) {
        expValTrunc.llogis <- expValLlogis(shape, rate) * stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape
        )
    } else {
        expValTrunc.llogis <- expValLlogis(shape, rate) * stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        )
    }

    return(expValTrunc.llogis)
}

#' @rdname llogis
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' stopLossLlogis(d = 2, shape = 2, scale = 4)
#'
stopLossLlogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape > 1,
        scale > 0
    )

    expValLlogis(shape, rate) *
        stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        ) -
        d * ((scale^shape) / (scale^shape + d^shape))
}

#' @rdname llogis
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' meanExcessLlogis(d = 3, shape = 2, scale = 4)
#'
meanExcessLlogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape > 1,
        scale > 0
    )

    expValLlogis(shape, rate) *
        ((d^shape + scale^shape) / (scale^shape)) *
        stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        ) -
        d
}

#' @rdname llogis
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' # With scale parameter
#' VatRLlogis(kap = .2, shape = 2, scale = 4)
#'
#' # With rate parameter
#' VatRLlogis(kap = .2, shape = 2, rate = 0.25)
#'
VatRLlogis <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        kap >= 0, kap <= 1,
        shape > 0,
        scale > 0
    )

    scale * (kap^(-1) - 1)^(-1/shape)
}

#' @rdname llogis
#'
#' @template kap-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' # With scale parameter
#' TVatRLlogis(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVatRLlogis(kap = .2, shape = 3, rate = 0.25)
#'
TVatRLlogis <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        kap >= 0, kap < 1,
        shape > 1,
        scale > 0
    )

    (expValLlogis(shape, rate) / (1 - kap)) *
        stats::pbeta(
            q = kap,
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        )
}
