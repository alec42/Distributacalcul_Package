#' Erlang Distribution
#'
#' @description
#' Erlang distribution with shape parameter \eqn{n} and rate parameter
#' \eqn{\beta}{beta}.
#'
#' @details
#' The Erlang distribution with shape parameter \eqn{n} and rate parameter
#' \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{n}}{\Gamma(n)} x^{n - 1}%
#'   \mathrm{e}^{-\beta x}}{f(x) = b^n / \Gamma(n) x^{n - 1} e^{-b x}}
#' for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0},%
#' \eqn{n \in \mathcal{N}^+}{n = 1, 2, 3, ...}.
#'
#' @template shape-n-template
#' @template rate-template
#' @template scale-template
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item \code{\link{dErlang}}  gives the probability density function (PDF).
#'  \item \code{\link{pErlang}}  gives the cumulative density function (CDF).
#'  \item \code{\link{expValErlang}}  gives the expected value.
#'  \item \code{\link{varErlang}}  gives the variance.
#'  \item \code{\link{kthMomentErlang}}  gives the kth moment.
#'  \item \code{\link{expValLimErlang}}  gives the limited mean.
#'  \item \code{\link{expValTruncErlang}}  gives the truncated mean.
#'  \item \code{\link{stopLossErlang}}  gives the stop-loss.
#'  \item \code{\link{meanExcessErlang}}  gives the mean excess loss.
#'  \item \code{\link{VatRErlang}}  gives the Value-at-Risk.
#'  \item \code{\link{TVatRErlang}}  gives the Tail Value-at-Risk.
#'  \item \code{\link{mgfErlang}}  gives the moment generating function (MGF).
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name erlang
#'
NULL

#' @rdname erlang
#'
#' @template x-template
#' @export
#'
#' @examples
#' dErlang(x = 2, shape = 2, scale = 4)
#'
dErlang <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        x >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    ((rate^shape) / gamma(shape)) *
        (x^(shape - 1)) *
        exp(-rate * x)
}

#' @rdname erlang
#'
#' @template q-template
#' @template lower.tail-template
#' @export
#'
#' @examples
#' pErlang(q = 2, shape = 2, scale = 4)
#'
pErlang <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(
        q >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    if (lower.tail) {
        FSx <- 1 - exp(-rate * q) *
            sum(sapply(0:(shape - 1), function(j) ((rate * q)^j) / factorial(j)))
    } else {
        FSx <- exp(-rate * q) *
            sum(sapply(0:(shape - 1), function(j) ((rate * q)^j) / factorial(j)))
    }

    return(FSx)
}

#' @rdname erlang
#'
#' @export
#'
#' @examples
#' expValErlang(shape = 2, scale = 4)
#'
expValErlang <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    shape / rate
}

#' @rdname erlang
#'
#' @export
#'
#' @examples
#' varErlang(shape = 2, scale = 4)
#'
varErlang <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    shape / rate^2
}

#' @rdname erlang
#'
#' @template k-template
#'
#' @export
#'
#' @examples
#' kthMomentErlang(k = 3, shape = 2, scale = 4)
#'
kthMomentErlang <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        k >= 1,  # k?
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}

#' @rdname erlang
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' expValLimErlang(d = 2, shape = 2, scale = 4)
#'
expValLimErlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    expValErlang(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate) +
        d * stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE)
}

#' @rdname erlang
#'
#' @template d-template
#' @template less.than.d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' # With rate parameter
#' expValTruncErlang(d = 2, shape = 2, scale = 4)
#'
#' # Values greater than d
#' expValTruncErlang(d = 2, shape = 2, scale = 4, less.than.d = FALSE)
#'
expValTruncErlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(
        d >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    if (less.than.d) {
        expValTrunc.gamma <- expValErlang(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate)
    } else {
        expValTrunc.gamma <- expValErlang(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE)
    }

    return(expValTrunc.gamma)
}

#' @rdname erlang
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' stopLossErlang(d = 2, shape = 2, scale = 4)
#'
stopLossErlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    expValErlang(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) -
        d * stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE)
}

#' @rdname erlang
#'
#' @template d-template
#'
#' @importFrom stats pgamma
#' @export
#'
#' @examples
#' meanExcessErlang(d = 3, shape = 2, scale = 4)
#'
meanExcessErlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        d >= 0,
        shape %% 1 == 0, shape > 0,
        rate > 0
    )

    expValErlang(shape, rate) *
        stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) /
        stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE) -
        d
}

#' @rdname erlang
#'
#' @note Function VatRErlang is a wrapper of the \code{\link[stats]{qgamma}}
#' function from the stats package.
#'
#' @template kap-template
#'
#' @importFrom stats qgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' VatRErlang(kap = .2, shape = 2, scale = 4)
#'
#' # With rate parameter
#' VatRErlang(kap = .2, shape = 2, rate = 0.25)
#'
VatRErlang <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        kap >= 0, kap < 1,
        shape > 0,
        rate > 0
    )

    stats::qgamma(p = kap, shape = shape, rate = rate)
}

#' @rdname erlang
#'
#' @template kap-template
#'
#' @importFrom stats qgamma
#' @export
#'
#' @examples
#' # With scale parameter
#' TVatRErlang(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVatRErlang(kap = .2, shape = 3, rate = 0.25)
#'
TVatRErlang <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        kap >= 0, kap < 1,
        shape > 0,
        rate > 0
    )

    vark <- stats::qgamma(p = kap, shape = shape, rate = rate)

    (expValErlang(shape, rate) / (1 - kap)) *
        exp(-rate * vark) *
        sum(sapply(0:shape, function(j) ((rate * vark)^j) / factorial(j)))
}

#' @rdname erlang
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfErlang(t = 2, shape = 2, scale = .25)
#'
mgfErlang <- function(t, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(
        shape %% 1 == 0, shape > 0,
        rate > 0,
        t < rate # domain for t where non-neg?
    )

    (rate / (rate - t))^shape
}
