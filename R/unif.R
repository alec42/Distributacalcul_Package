#' Uniform Distribution
#'
#' @description
#' Uniform distribution with min \eqn{a} and max \eqn{b}.
#'
#' @details
#' The (continuous) uniform distribution with min and max parameters \eqn{a}
#' and \eqn{b} respectively has density:
#'   \deqn{f(x) = \frac{1}{b - a} \times \bm{1}_{\{x \in [a, b] \}}}{f(x) = 1 / (b - a) x 1_\{a <= x <= b\}}
#' for \eqn{x \in [a, b]}{a <= x <= b}.
#'
#' @template minmax-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValUnif}}}{ gives the expected value.}
#'     \item{\code{\link{varUnif}}}{ gives the variance.}
#'     \item{\code{\link{kthMomentUnif}}}{ gives the kth moment.}
#'     \item{\code{\link{expValLimUnif}}}{ gives the limited mean.}
#'     \item{\code{\link{expValTruncUnif}}}{ gives the truncated mean.}
#'     \item{\code{\link{stopLossUnif}}}{ gives the stop-loss.}
#'     \item{\code{\link{meanExcessUnif}}}{ gives the mean excess loss.}
#'     \item{\code{\link{VatRUnif}}}{ gives the Value-at-Risk.}
#'     \item{\code{\link{TVatRUnif}}}{ gives the Tail Value-at-Risk.}
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Unif
#'
NULL

#' @rdname Unif
#'
#' @export
#'
#' @examples
#' expValUnif(min = 3, max = 4)
#'
expValUnif <- function(min = 0, max = 1) {
    stopifnot(min < max)

    (min + max) / 2
}

#' @rdname Unif
#'
#' @export
#'
#' @examples
#' varUnif(min = 3, max = 4)
#'
varUnif <- function(min = 0, max = 1) {
    stopifnot(min < max)

    ((max - min)^2) / 12
}

#' @rdname Unif
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' kthMomentUnif(k = 2, min = 3, max = 4)
#'
kthMomentUnif <- function(k, min = 0, max = 1) {
    stopifnot(min < max, k > -1) # condition for k strictly positive?

    (max^(k + 1) - min^(k + 1)) / ((k + 1) * (max - min))
}

#' @rdname Unif
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' expValLimUnif(d = 3, min = 2, max = 4)
#'
expValLimUnif <- function(d, min = 0, max = 1) {
    stopifnot(min < max, d >= min, d <= max)

    (d^2 - min^2) / (2 * (max - min)) + d * ((max - d) / (max - min))
}

#' @rdname Unif
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @export
#'
#' @examples
#' expValTruncUnif(d = 3, min = 2, max = 4)
#'
#' # Values greather than d
#' expValTruncUnif(d = 3, min = 2, max = 4, less.than.d = FALSE)
#'
expValTruncUnif <- function(d, min = 0, max = 1, less.than.d = TRUE) {
    stopifnot(min < max, d >= min, d <= max)

    if (less.than.d) {
        expValTrunc.unif <- (d^2 - min^2) / (2 * (max - min))
    } else {
        expValTrunc.unif <- (max^2 - d^2) / (2 * (max - min))
    }

    return(expValTrunc.unif)
}

#' @rdname Unif
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' stopLossUnif(d = 3, min = 2, max = 4)
#'
stopLossUnif <- function(d, min = 0, max = 1) {
    stopifnot(min < max, d >= min, d <= max)

    ((max - d)^2) / (2 * (max - min))
}

#' @rdname Unif
#'
#' @template d-template
#'
#' @export
#'
#' @examples
#' meanExcessUnif(d = 2, min = 2, max = 4)
#'
meanExcessUnif <- function(d, min = 0, max = 1) {
    stopifnot(
        # min < max,
        # d >= min,
        d <= max
    )

    (max - d) / 2
}

#' @rdname Unif
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' VatRUnif(kap = .99, min = 3, max = 4)
#'
VatRUnif <- function(kap, min = 0, max = 1) {
    stopifnot(kap <= 1, kap >= 0, min < max)

    min + (max - min) * kap
}

#' @rdname Unif
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' TVatRUnif(kap = .99, min = 3, max = 4)
#'
TVatRUnif <- function(kap, min = 0, max = 1) {
    stopifnot(kap <= 1, kap >= 0, min < max)

    min + ((max - min)/2) * (1 + kap)
}

#' @rdname Unif
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfUnif(t = 2, min = 0, max = 1)
#'
mgfUnif <- function(t, min = 0, max = 1) {
    stopifnot(min < max)

    (exp(max * t) - exp(min * t)) / ((max - min) * t)
}
