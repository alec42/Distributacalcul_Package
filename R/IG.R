#' Inverse Gaussian Distribution
#'
#' @description
#' Inverse Gaussian distribution with mean \eqn{\mu}{mu} and shape parameter
#' \eqn{\beta}{beta}.
#'
#' @details
#' The Inverse Gaussian distribution with
#'
#' @template mean-location-template
#' @template shape-beta-template
#' @template dispersion-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{expValIG}}  gives the expected value.
#'     \item \code{\link{varIG}}  gives the variance.
#'     \item \code{\link{expValLimIG}}  gives the limited mean.
#'     \item \code{\link{expValTruncIG}}  gives the truncated mean.
#'     \item \code{\link{stopLossIG}}  gives the stop-loss.
#'     \item \code{\link{meanExcessIG}}  gives the mean excess loss.
#'     \item \code{\link{VatRIG}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRIG}}  gives the Tail Value-at-Risk.
#'     \item \code{\link{mgfIG}}  gives the moment generating function (MGF).
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name IG
#'
NULL

#' @rdname IG
#'
#' @export
#'
#' @examples
#' expValIG(mean = 2, shape = 5)
#'
expValIG <- function(mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(
        mean >= 0
        # , shape >= 0 # not in function
    )

    mean
}

#' @rdname IG
#'
#' @export
#'
#' @examples
#' varIG(mean = 2, shape = 5)
#'
varIG <- function(mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape >= 0)

    mean * shape
}

#' @rdname IG
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' expValLimIG(d = 2, mean = 2, shape = 5)
#'
expValLimIG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    d -
        (d - mean) * stats::pnorm(q = (d - mean) * sqrt(1 / (shape * d))) -
        (d + mean) * exp(2 * mean / shape) * stats::pnorm(q = -(d + mean) * sqrt(1 / (shape * d)))
}

#' @rdname IG
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' expValTruncIG(d = 2, mean = 2, shape = 5)
#'
expValTruncIG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2, less.than.d = TRUE) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    expValTruncIG.IG.less.than.d <- d -
        (2 * d - mean) *
        stats::pnorm(q = (d - mean) *
                         sqrt(1 / (shape * d))) -
        (2 * d + mean) *
        exp(2 * mean / shape) *
        stats::pnorm(q = - (d + mean) *
                         sqrt(1 / (shape * d)))

    if (less.than.d) {
        return(expValTruncIG.IG.less.than.d)
    } else {
        return(mean - expValTruncIG.IG.less.than.d)
    }
}

#' @rdname IG
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' stopLossIG(d = 2, mean = 2, shape = 5)
#'
stopLossIG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape >= 0, d >= 0)

    (mean - d) * stats::pnorm(q = (d - mean) * sqrt(1 / (shape*d)), lower.tail = FALSE) +
        (mean + d) * exp(2*mean / shape) * stats::pnorm(q = - (d + mean) * sqrt(1 / (shape*d)))
}

#' @rdname IG
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' meanExcessIG(d = 2, mean = 2, shape = 5)
#'
meanExcessIG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    (
        (mean - d) * stats::pnorm((d - mean) * sqrt(1 / (shape*d)), lower.tail = FALSE) +
            (d + mean) * exp(2*mean / shape) * stats::pnorm(q = -(d + mean) * sqrt(1 / (shape*d)))
    ) /
        (1 - (
            stats::pnorm(q = (d - mean) * sqrt(1 / (shape*d))) +
                exp(2*mean / shape) * stats::pnorm(q = (d + mean) * -sqrt(1 / (shape*d)))
        )
        )
}

#' @rdname IG
#'
#' @note Function VatRIG is a wrapper for the \code{\link[statmod]{qinvgauss}}
#' function from the statmod package.
#'
#' @template kap-template
#'
#' @importFrom statmod qinvgauss
#' @export
#'
#' @examples
#' VatRIG(kap = 0.99, mean = 2, shape = 5)
#'
VatRIG <- function(kap, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(kap >= 0, kap < 1, mean >= 0, shape >= 0)

    statmod::qinvgauss(p = kap, mean = mean, dispersion = dispersion)
}

#' @rdname IG
#'
#' @template kap-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' TVatRIG(kap = 0.99, mean = 2, shape = 5)
#'
TVatRIG <- function(kap, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape > 0, kap >= 0, kap < 1)

    vark <- VatRIG(kap = kap, mean, shape)

    (mean - vark +
            (2*vark + mean) * exp(2*mean / shape) +
            (2*vark - mean) * stats::pnorm(q = ((vark - mean) * sqrt(1 / (shape*vark))))
    ) / (1 - kap)
}

#' @rdname IG
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfIG(t = 1, mean = 2, shape = .5)
#'
mgfIG <- function(t, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape >= 0, 2 * t * shape <= 1)

    exp((mean/shape) * (1 - sqrt(1 - 2 * shape * t)))
}
