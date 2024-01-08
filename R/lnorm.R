#' Lognormal Distribution
#'
#' @description
#' Lognormal distribution with mean \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @details
#' The Log-normal distribution with mean \eqn{\mu}{mu} and standard deviation
#' \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma x}\textrm{e}^{-\frac{1}{2}\left(\frac{\ln(x) - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((ln(x) - mu)/sigma)^2) / ((2 pi)^(1/2) sigma x}
#' for \eqn{x \in \mathcal{R}^{+}}{x >= 0}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @template sdlog-template
#' @template meanlog-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{expValLnorm}}  gives the expected value.
#'     \item \code{\link{varLnorm}}  gives the variance.
#'     \item \code{\link{kthMomentLnorm}}  gives the kth moment.
#'     \item \code{\link{expValLimLnorm}}  gives the limited mean.
#'     \item \code{\link{expValTruncLnorm}}  gives the truncated mean.
#'     \item \code{\link{stopLossLnorm}}  gives the stop-loss.
#'     \item \code{\link{meanExcessLnorm}}  gives the mean excess loss.
#'     \item \code{\link{VatRLnorm}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRLnorm}}  gives the Tail Value-at-Risk.
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Lnorm
#'
NULL

#' @rdname Lnorm
#'
#' @export
#'
#' @examples
#' expValLnorm(meanlog = 3, sdlog = 5)
#'
expValLnorm <- function(meanlog, sdlog) {
    exp(meanlog + (sdlog^2) / 2)
}

#' @rdname Lnorm
#'
#' @export
#'
#' @examples
#' varLnorm(meanlog = 3, sdlog = 5)
#'
varLnorm <- function(meanlog, sdlog) {
    stopifnot(sdlog > 0)

    exp(2 * meanlog + (sdlog^2)) *
        (exp(sdlog^2) - 1)
}

#' @rdname Lnorm
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' kthMomentLnorm(k = 2, meanlog = 3, sdlog = 5)
#'
kthMomentLnorm <- function(k, meanlog, sdlog) {
    stopifnot(sdlog > 0) # k?

    exp(meanlog * k + k^2 * (sdlog^2) / 2)
}

#' @rdname Lnorm
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' expValLimLnorm(d = 2, meanlog = 2, sdlog = 5)
#'
expValLimLnorm <- function(d, meanlog, sdlog) {
    stopifnot(d >= 0, sdlog > 0)

    expValLnorm(meanlog, sdlog) *
        stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog) +
        d *
        stats::pnorm(q = log(d), mean = meanlog, sd = sdlog, lower.tail = FALSE)
}

#' @rdname Lnorm
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' expValTruncLnorm(d = 2, meanlog = 2, sdlog = 5)
#'
#' # Values greater than d
#' expValTruncLnorm(d = 2, meanlog = 2, sdlog = 5, less.than.d = FALSE)
#'
expValTruncLnorm <- function(d, meanlog, sdlog, less.than.d = TRUE) {
    stopifnot(d >= 0, sdlog > 0)

    if (less.than.d) {
        expValTrunc.lnorm <- expValLnorm(meanlog, sdlog) *
            stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog)
    } else {
        expValTrunc.lnorm <- expValLnorm(meanlog, sdlog) *
            stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog, lower.tail = FALSE)
    }

    return(expValTrunc.lnorm)
}

#' @rdname Lnorm
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' stopLossLnorm(d = 2, meanlog = 2, sdlog = 5)
#'
stopLossLnorm <- function(d, meanlog, sdlog) {
    stopifnot(d >= 0, sdlog > 0)

    expValLnorm(meanlog, sdlog) *
        stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog, lower.tail = FALSE) -
        d *
        stats::pnorm(q = log(d), mean = meanlog, sd = sdlog, lower.tail = FALSE)
}

#' @rdname Lnorm
#'
#' @template d-template
#'
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' meanExcessLnorm(d = 2, meanlog = 2, sdlog = 5)
#'
meanExcessLnorm <- function(d, meanlog, sdlog) {
    stopifnot(d >= 0, sdlog > 0)

    expValLnorm(meanlog, sdlog) *
        stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog, lower.tail = FALSE) /
        stats::pnorm(q = log(d), mean = meanlog, sd = sdlog, lower.tail = FALSE) -
        d
}

#' @rdname Lnorm
#'
#' @note Function VatRLnorm is a wrapper of the \code{\link[stats]{qlnorm}}
#' function from the stats package.
#'
#' @template kap-template
#'
#' @importFrom stats qlnorm
#' @export
#'
#' @examples
#' VatRLnorm(kap = 0.8, meanlog = 3, sdlog = 5)
#'
VatRLnorm <- function(kap, meanlog, sdlog) {
    stopifnot(kap >= 0, kap <= 1, sdlog > 0)

    stats::qlnorm(p = kap, meanlog = meanlog, sdlog = sdlog)
}

#' @rdname Lnorm
#'
#' @template kap-template
#'
#' @importFrom stats qnorm pnorm
#' @export
#'
#' @examples
#' TVatRLnorm(kap = 0.8, meanlog = 2, sdlog = 5)
#'
TVatRLnorm <- function(kap, meanlog, sdlog) {
    stopifnot(kap < 1, kap >= 0, sdlog > 0)

    expValLnorm(meanlog, sdlog) *
        stats::pnorm(q = stats::qnorm(p = kap) - sdlog, lower.tail = FALSE) /
        (1 - kap)
}
