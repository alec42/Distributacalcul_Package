#' Logarithmic Distribution
#'
#' @description
#' Logarithmic distribution with probability parameter \eqn{\gamma}{gamma}.
#'
#' @details
#' The Logarithmic distribution with probability parameter \eqn{\gamma}{gam}
#' has probability mass function :
#'   \deqn{Pr(X = k) = \frac{-\gamma^{k}}{\ln(1 - \gamma)k}}{-gam^k / (ln(1 - k) k)},
#' for \eqn{k = 0, 1, 2, \dots}{k = 0, 1, 2, ...},
#' and \eqn{\gamma  \in (0, 1)}{0 < gam < 1}].
#'
#' @template prob-template-logarithmic
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{dLogarithmic}}  gives the probability density function (PDF).
#'     \item \code{\link{pLogarithmic}}  gives the cumulative density function (CDF).
#'     \item \code{\link{expValLogarithmic}}  gives the expected value.
#'     \item \code{\link{varLogarithmic}}  gives the variance.
#'     \item \code{\link{VatRLogarithmic}}  gives the Value-at-Risk.
#'     \item \code{\link{mgfLogarithmic}}  gives the moment generating function (MGF).
#'     \item \code{\link{pgfLogarithmic}}  gives the probability generating function (MGF).
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Logarithmic
#'
NULL

#' @rdname Logarithmic
#'
#' @template x-template
#' @export
#'
#' @examples
#' dLogarithmic(x = 3, prob = 0.2)
#'
dLogarithmic <- function(x, prob) {
    stopifnot(
        x > 0, x %% 1 == 0,
        prob >= 0, prob <= 1
    )

    -prob^(x) / (log(1 - prob) * x)
}

#' @rdname Logarithmic
#'
#' @template q-template
#' @template lower.tail-template
#' @export
#'
#' @examples
#' pLogarithmic(q = 3, prob = 0.2)
#'
pLogarithmic <- function(q, prob, lower.tail = TRUE) {
    stopifnot(
        q > 0, q %% 1 == 0,
        prob >= 0, prob <= 1
    )

    Fx <- sum(dLogarithmic(1:q, prob))

    return(ifelse(lower.tail == TRUE, Fx, 1 - Fx))
}

#' @rdname Logarithmic
#'
#' @export
#'
#' @examples
#' expValLogarithmic(prob = 0.50)
#'
expValLogarithmic <- function(prob) {
    stopifnot(prob > 0, prob < 1)

    (-prob) / (log(1 - prob) * (1 - prob))
}

#' @rdname Logarithmic
#'
#' @export
#'
#' @examples
#' varLogarithmic(prob = 0.50)
#'
varLogarithmic <- function(prob) {
    stopifnot(prob > 0, prob < 1)

    (prob + log(1 - prob)) / ((1 - prob)^2 * (log(1 - prob))^2)
}

#' @rdname Logarithmic
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' VatRLogarithmic(kap = 0.99, prob = 0.2)
#'
VatRLogarithmic <- function(kap, prob) {
    stopifnot(
        kap >= 0, kap <= 1,
        prob >= 0, prob <= 1
    )

    optimisationMaxValue <- 1
    while(pLogarithmic(optimisationMaxValue, 0.2) < kap) {
        optimisationMaxValue <- optimisationMaxValue + 1
    }

    optimisationMaxValue
}

#' @rdname Logarithmic
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfLogarithmic(t = .2, prob = 0.50)
#'
mgfLogarithmic <- function(t, prob) {
    stopifnot(prob > 0, prob < 1, prob * exp(t) < 1)

    log(1 - prob * exp(t)) / log(1 - prob)
}


#' @rdname Logarithmic
#'
#' @template t-template
#' @export
#'
#' @examples
#' pgfLogarithmic(t = .2, prob = 0.50)
#'
pgfLogarithmic <- function(t, prob) {
    stopifnot(prob > 0, prob < 1, prob * t < 1)

    log(1 - prob * t) / log(1 - prob)
}
