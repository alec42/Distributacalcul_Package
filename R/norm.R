#' Normal Distribution
#'
#' @description
#' Normal distribution
#'
#' @details
#' The Normal distribution with mean \eqn{\mu}{mu} and standard deviation
#' \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma}\textrm{e}^{-\frac{1}{2}\left(\frac{x - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((x - mu)/sigma)^2) / ((2 pi)^(1/2) sigma}
#' for \eqn{x \in \mathcal{R}}{x real}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @template mean-mu-template
#' @template sd-sigma-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{expValNorm}}  gives the expected value.
#'     \item \code{\link{varNorm}}  gives the variance.
#'     \item \code{\link{expValLimNorm}}  gives the limited mean.
#'     \item \code{\link{expValTruncNorm}}  gives the truncated mean.
#'     \item \code{\link{stopLossNorm}}  gives the stop-loss.
#'     \item \code{\link{meanExcessNorm}}  gives the mean excess loss.
#'     \item \code{\link{VatRNorm}}  gives the Value-at-Risk.
#'     \item \code{\link{TVatRNorm}}  gives the Tail Value-at-Risk.
#'     \item \code{\link{mgfNorm}}  gives the moment generating function (MGF).
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Norm
#'
NULL

#' @rdname Norm
#'
#' @export
#'
#' @examples
#' expValNorm(mean = 3, sd = 5)
#'
expValNorm <- function(mean, sd) {
    stopifnot(sd > 0)

    mean
}

#' @rdname Norm
#'
#' @export
#'
#' @examples
#' varNorm(mean = 3, sd = 5)
#'
varNorm <- function(mean, sd) {
    stopifnot(sd > 0)

    sd^2
}

#' @rdname Norm
#'
#' @template d-template
#'
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' expValLimNorm(d = 2, mean = 2, sd = 5)
#'
expValLimNorm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    mean *
        stats::pnorm(q = d, mean = mean, sd = sd) -
        sd^2 *
        stats::dnorm(x = d, mean = mean, sd = sd) +
        d *
        stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE)
}

#' @rdname Norm
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pnorm dnorm
#' @export
#'
#' @examples
#' expValTruncNorm(d = 2, mean = 2, sd = 5)
#'
expValTruncNorm <- function(d, mean = 0, sd = 1, less.than.d = TRUE) {
    stopifnot(sd > 0)

    if (less.than.d) {
        expValTrunc.norm <- mean * stats::pnorm(q = d, mean = mean, sd = sd) -
            sd^2 * stats::dnorm(x = d, mean = mean, sd = sd)
    } else {
        expValTrunc.norm <- mean * stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE) +
            sd^2 * stats::dnorm(x = d, mean = mean, sd = sd)
    }

    return(expValTrunc.norm)
}

#' @rdname Norm
#'
#' @template d-template
#'
#' @importFrom stats pnorm dnorm
#' @export
#'
#' @examples
#' stopLossNorm(d = 2, mean = 2, sd = 5)
#'
stopLossNorm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    (mean + d) *
        stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE) -
        (sd^2) *
        stats::dnorm(x = d, mean = mean, sd = sd)
}

#' @rdname Norm
#'
#' @template d-template
#'
#' @importFrom stats pnorm dnorm
#' @export
#'
#' @examples
#' meanExcessNorm(d = 2, mean = 2, sd = 5)
#'
meanExcessNorm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    mean +
        d -
        (sd^2) *
        stats::dnorm(x = d, mean = mean, sd = sd) /
        stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE)
}

#' @rdname Norm
#'
#' @note Function VatRNorm is a wrapper of the \code{\link[stats]{qnorm}}
#' function from the stats package.
#'
#' @template kap-template
#'
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' VatRNorm(kap = 0.8, mean = 3, sd = 5)
#'
VatRNorm <- function(kap, mean = 0, sd = 1) {
    stopifnot(kap <= 1, kap >= 0, sd > 0)

    stats::qnorm(p = kap, mean = mean, sd = sd)
}

#' @rdname Norm
#'
#' @template kap-template
#'
#' @importFrom stats qnorm dnorm
#' @export
#'
#' @examples
#' TVatRNorm(kap = 0.8, mean = 2, sd = 5)
#'
TVatRNorm <- function(kap, mean = 0, sd = 1) {
    stopifnot(kap >= 0, kap < 1, sd > 0)

    mean +
        (sd / (1 - kap)) *
        stats::dnorm(x = stats::qnorm(p = kap))
}

#' @rdname Norm
#'
#' @template t-template
#'
#' @export
#'
#' @examples
#' mgfNorm(t = 1, mean = 3, sd = 5)
#'
mgfNorm <- function(t, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    exp(mean * t + t^2 * (sd^2) / 2)
}
