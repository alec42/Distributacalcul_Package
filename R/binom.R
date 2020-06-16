#' Binomial distribution
#'
#' @description
#' Binomial distribution with size \eqn{n}{n} and probability of
#' success \eqn{p}{p}.
#'
#' @details
#' The Binomial distribution with probability of success \eqn{p}{p} for \eqn{n}{n} trials
#' has probability mass function :
#'   \deqn{Pr(X = k) = \left(\frac{n}{k}\right) p^n (1 - p)^{n - k}}{Pr(X = k) = n!/(k!(n - k)!) p^n(1 - p)^(n - k)}
#' for \eqn{k = 0, 1, 2, \dots, n}{k = 0, 1, 2, ..., n}, \eqn{p \in [0, 1]}{0 <= p <= 1}, and \eqn{n > 0}{n > 0}
#'
#' @template size-prob-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{MGF_binom}}}{ gives the moment generating function (MGF).}
#'     \item{\code{\link{E_binom}}}{ gives the expected value.}
#'     \item{\code{\link{V_binom}}}{ gives the variance.}
#'     \item{\code{\link{Etrunc_binom}}}{ gives the truncated mean.}
#'     \item{\code{\link{TVaR_binom}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{VaR_binom}}}{ gives the Value-at-Risk.}
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
#' E_binom(size = 3, prob = 0.5)
#'
E_binom <- function(size, prob) {
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
#' V_binom(size = 3, prob = 0.5)
#'
V_binom <- function(size, prob) {
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
#' Etrunc_binom(d = 2, size = 3, prob = 0.5)
#' Etrunc_binom(d = 0, size = 3, prob = 0.5, less.than.d = FALSE)
#'
Etrunc_binom <- function(d, size, prob, less.than.d = TRUE) {
    stopifnot(
        d >= 0, d %% 1 == 0, d <= size,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)

    if (less.than.d) {
        Etrunc.binom <- sum((k * fx)[k <= d])
    } else {
        Etrunc.binom <- sum((k * fx)[k > d])
    }

    return(Etrunc.binom)
}

#' @rdname binom
#'
#' @template kap-template
#'
#' @importFrom stats qbinom
#' @export
#'
#' @note Function VaR_binom is a wrapper of the qbinom function from the
#' stats package.
#'
#' @examples
#'
#' VaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
VaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    stats::qbinom(p = kap, size = size, prob = prob)
}

#' @rdname binom
#'
#' @importFrom stats dbinom qbinom pbinom
#' @export
#'
#' @examples
#' TVaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
TVaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)
    vark <- stats::qbinom(p = kap, size, prob)

    (
        Etrunc_binom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (stats::pbinom(q = vark, size = size, prob = prob) - kap)
    ) /
        (1 - kap)
}

#' @rdname binom
#'
#' @template t-template
#'
#' @export
#'
#' @examples
#'
#' PGF_binom(t = 1, size = 3, prob = 0.5)
#'
PGF_binom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * t + (1 - prob))^size
}

#' @rdname binom
#'
#' @export
#'
#' @examples
#' MGF_binom(t = 1, size = 3, prob = 0.5)
#'
MGF_binom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * exp(t) + (1 - prob))^size
}

