#' Beta Distribution
#'
#' @description
#' Beta distribution with shape parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @details
#' The Beta distribution with shape parameters \eqn{\alpha}{a} and
#' \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\Gamma(\alpha + \beta)}{\Gamma(\alpha) %
#'   \Gamma(\beta)} x^{\alpha - 1} (1 - x)^(\beta - 1)}{f(x) = Γ(a+b) / %
#'   (Γ(a)Γ(b))x^(a - 1)(1 - x)^(b - 1)}
#' for \eqn{x \in [0, 1]}{0 ≤ x ≤ 1}, \eqn{\alpha, \beta > 0}{a, b > 0}.
#'
#' @template shape1-template-beta
#' @template shape2-template-beta
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValBeta}}}{ gives the expected value.}
#'     \item{\code{\link{varBeta}}}{ gives the variance.}
#'     \item{\code{\link{kthMomentBeta}}}{ gives the kth moment.}
#'     \item{\code{\link{expValLimBeta}}}{ gives the limited mean.}
#'     \item{\code{\link{expValTruncBeta}}}{ gives the truncated mean.}
#'     \item{\code{\link{stopLossBeta}}}{ gives the stop-loss.}
#'     \item{\code{\link{meanExcessBeta}}}{ gives the mean excess loss.}
#'     \item{\code{\link{VatRBeta}}}{ gives the Value-at-Risk.}
#'     \item{\code{\link{TVatRBeta}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{mgfBeta}}}{ gives the moment generating function (MGF).}
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Beta
#'
NULL

#' @rdname Beta
#'
#' @export
#'
#' @examples
#' expValBeta(shape1 = 3, shape2 = 5)
#'
expValBeta <- function(shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0)

    shape1 / (shape1 + shape2)
}

#' @rdname Beta
#'
#' @export
#'
#' @examples
#' varBeta(shape1 = 4, shape2 = 5)
#'
varBeta <- function(shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0)

    (shape1 * shape2) /
        (
            (shape1 + shape2)^2 * (shape1 + shape2 + 1)
        )
}

#' @rdname Beta
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' kthMomentBeta(k = 3, shape1 = 4, shape2 = 5)
#'
kthMomentBeta <- function(k, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0) # condition for k?

    (gamma(shape1 + k) * gamma(shape1 + shape2)) /
        (gamma(shape1) * gamma(shape1 + shape2 + k))
}

#' @rdname Beta
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' expValLimBeta(d = 0.3, shape1 = 4, shape2 = 5)
#'
expValLimBeta <- function(d, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    expValBeta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2) +
        shape2 * stats::pbeta(q = d, shape1, shape2, lower.tail = FALSE)
}

#' @rdname Beta
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' expValTruncBeta(d = 0.4, shape1 = 4, shape2 = 5)
#'
#' # Values less than d
#' expValTruncBeta(d = 0.4, shape1 = 4, shape2 = 5, less.than.d = FALSE)
#'
expValTruncBeta <- function(d, shape1, shape2, less.than.d = TRUE) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    if (less.than.d) {
        Etrunc.beta <- expValBeta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2)
    } else {
        Etrunc.beta <- expValBeta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE)
    }

    return(Etrunc.beta)
}

#' @rdname Beta
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' stopLossBeta(d = 0.3, shape1 = 4, shape2 = 5)
#'
stopLossBeta <- function(d, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    expValBeta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE) +
        d * stats::pbeta(q = d, shape1, shape2, lower.tail = FALSE)
}

#' @rdname Beta
#'
#' @template d-template
#'
#' @importFrom stats pbeta
#' @export
#'
#' @examples
#' meanExcessBeta(d = .3, shape1 = 4, shape2 = 5)
#'
meanExcessBeta <- function(d, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    (expValBeta(shape1, shape2) *
            (
                stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE) /
                    stats::pbeta(q = d, shape1, shape2, lower.tail = FALSE)
            )
    ) - d
}

#' @rdname Beta
#'
#' @note Function VatRBeta is a wrapper for the \code{\link[stats]{qbeta}}
#' function from the stats package.
#'
#' @template kap-template
#'
#' @importFrom stats qbeta
#' @export
#'
#' @examples
#' VatRBeta(kap = .99, shape1 = 4, shape2 = 5)
#'
VatRBeta <- function(kap, shape1, shape2) {
    stopifnot(kap >= 0, kap <= 1, shape1 > 0, shape2 > 0)

    stats::qbeta(p = kap, shape1, shape2)
}

#' @rdname Beta
#'
#' @template k-template
#'
#' @importFrom stats pbeta qbeta
#' @export
#'
#' @examples
#' TVatRBeta(kap = .99, shape1 = 4, shape2 = 5)
#'
TVatRBeta <- function(kap, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, kap >= 0, kap < 1)

    (expValBeta(shape1, shape2) / (1 - kap)) *
        stats::pbeta(
            q = stats::qbeta(p = kap, shape1 = shape1, shape2 = shape2),
            shape1 = shape1 + 1,
            shape2 = shape2,
            lower.tail = FALSE
        )
}

#' @rdname Beta
#'
#' @template t-template
#' @template k0-template
#' @export
#'
#' @examples
#' mgfBeta(t = 1, shape1 = 3, shape2 = 5, k0 = 1E2)
#'
mgfBeta <- function(t, shape1, shape2, k0) {
    stopifnot(shape1 > 0, shape2 > 0, k0 > 0) # domain for t?

    MGF.beta <- 1 + sum(
        sapply(1:k0, function(k) {
            prod(sapply(0:(k - 1), function(j) (shape1 + j) / (shape1 + shape2 + j)),
                 (t^k) / factorial(k))
        })
    )
    warning("This is an approximation")
    return(MGF.beta)
}

