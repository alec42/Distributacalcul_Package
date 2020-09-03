#' Binomial Distribution
#'
#' @description
#' Binomial distribution with size \eqn{n}{n} and probability of
#' success \eqn{p}{p}.
#'
#' @details
#' The binomial distribution with probability of success \eqn{p}{p} for \eqn{n}{n} trials
#' has probability mass function :
#'   \deqn{Pr(X = k) = \left(\frac{n}{k}\right) p^n (1 - p)^{n - k}}{Pr(X = k) = n!/(k!(n - k)!) p^n(1 - p)^(n - k)}
#' for \eqn{k = 0, 1, 2, \dots, n}{k = 0, 1, 2, ..., n}, \eqn{p \in [0, 1]}{0 <= p <= 1}, and \eqn{n > 0}{n > 0}
#'
#' @template size-prob-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{mgfBinom}}}{ gives the moment generating function (MGF).}
#'     \item{\code{\link{pgfBinom}}}{ gives the probability generating function (PGF).}
#'     \item{\code{\link{expValBinom}}}{ gives the expected value.}
#'     \item{\code{\link{varBinom}}}{ gives the variance.}
#'     \item{\code{\link{expValTruncBinom}}}{ gives the truncated mean.}
#'     \item{\code{\link{TVatRBinom}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{VatRBinom}}}{ gives the Value-at-Risk.}
#'   }
#' Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name binom
NULL

#' @rdname binom
#'
#' @export
#'
#' @examples
#' expValBinom(size = 3, prob = 0.5)
#'
expValBinom <- function(size, prob) {
    stopifnot(
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    size * prob
}

#' @rdname binom
#'
#' @export
#'
#' @examples
#' varBinom(size = 3, prob = 0.5)
#'
varBinom <- function(size, prob) {
    stopifnot(
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    size * prob * (1 - prob)
}

#' @rdname binom
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats dbinom
#' @export
#'
#' @examples
#'
#' expValTruncBinom(d = 2, size = 3, prob = 0.5)
#' expValTruncBinom(d = 0, size = 3, prob = 0.5, less.than.d = FALSE)
#'
expValTruncBinom <- function(d, size, prob, less.than.d = TRUE) {
    stopifnot(
        d >= 0, d %% 1 == 0, d <= size,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)

    if (less.than.d) {
        expValTrunc.binom <- sum((k * fx)[k <= d])
    } else {
        expValTrunc.binom <- sum((k * fx)[k > d])
    }

    return(expValTrunc.binom)
}

#' @rdname binom
#'
#' @template kap-template
#'
#' @importFrom stats qbinom
#' @export
#'
#' @note Function VatRBinom is a wrapper of the \code{\link[stats]{qbinom}}
#' function from the stats package.
#'
#' @examples
#'
#' VatRBinom(kap = 0.8, size = 5, prob = 0.2)
#'
VatRBinom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    stats::qbinom(p = kap, size = size, prob = prob)
}

#' @rdname binom
#'
#' @template kap-template
#'
#' @importFrom stats dbinom qbinom pbinom
#' @export
#'
#' @examples
#' TVatRBinom(kap = 0.8, size = 5, prob = 0.2)
#'
TVatRBinom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)
    vark <- stats::qbinom(p = kap, size, prob)

    (
        expValTruncBinom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (stats::pbinom(q = vark, size = size, prob = prob) - kap)
    ) /
        (1 - kap)
}

#' @rdname binom
#'
#' @template t-template
#' @export
#'
#' @examples
#'
#' pgfBinom(t = 1, size = 3, prob = 0.5)
#'
pgfBinom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * t + (1 - prob))^size
}

#' @rdname binom
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfBinom(t = 1, size = 3, prob = 0.5)
#'
mgfBinom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * exp(t) + (1 - prob))^size
}

