#' Discrete Uniform Distribution
#'
#' @description
#' Discrete uniform distribution with min \eqn{a} and max \eqn{b}.
#'
#' @details
#' The (discrete) uniform distribution with min and max parameters \eqn{a}
#' and \eqn{b} respectively has density:
#'   \deqn{\textrm{Pr}\left(X = x \right) = \frac{1}{b - a + 1}}{Pr(X = x) = 1 / (b - a + 1)}
#' for \eqn{x \in \{a, a + 1, \dots, b - 1, b\}}{x = a, a + 1, ..., b - 1, b}.
#'
#' @template minmax-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{dUnifD}}}{ gives the probability density function (PDF).}
#'     \item{\code{\link{pUnifD}}}{ gives the cumulative density function (CDF).}
#'     \item{\code{\link{expValUnifD}}}{ gives the expected value.}
#'     \item{\code{\link{varUnifD}}}{ gives the variance.}
#'   }
#' Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name unifDiscr
NULL

#' @rdname unifDiscr
#'
#' @template q-template
#' @export
#'
#' @examples
#' pUnifD(q = 0.2, min = 0, max = 1)
#'
pUnifD <- function(q, min = 0, max = 1){
    stopifnot(
        min < max,
        min <= q, q <= max
        # , q %% 1 == 0 # round
    )

    if (q < min) {
        return(0)
    } else if (min <= q & q < max) {
        (round(q) - min + 1) / (max - min + 1)
    } else {
        return(1)
    }
}

#' @rdname unifDiscr
#'
#' @template x-template
#' @export
#'
#' @examples
#' dUnifD(min = 0, max = 1)
#'
dUnifD <- function(x, min = 0, max = 1){
    stopifnot(
        min < max
        # ,x %% 1 == 0 # not part of the equation
    )

    1 / (max - min + 1)
}

#' @rdname unifDiscr
#'
#' @export
#'
#' @examples
#' varUnifD(min = 0, max = 1)
#'
varUnifD <- function(min = 0, max = 1){
    stopifnot(min < max)

    ((max - min + 1)^2 - 1) / 12
}

#' @rdname unifDiscr
#'
#' @export
#'
#' @examples
#' expValUnifD(min = 0, max = 1)
#'
expValUnifD <- function(min = 0, max = 1){
    stopifnot(min < max)

    (min + max) / 2
}
