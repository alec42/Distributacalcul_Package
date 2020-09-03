#' Compound Binomial Distribution
#'
#' @description
#' Computes various risk measures (mean, variance, Value-at-Risk (VaR),
#' and Tail Value-at-Risk (TVaR)) for the compound Binomial distribution.
#'
#' @details
#' The compound binomial distribution has density ....
#'
#' @template size-prob-template
#' @template distr_severity-template
#' @template k0-template
#' @template x-template
#' @template shape-template
#' @template rate-template
#' @template scale-template
#' @template vark-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{pCompBinom}}}{ gives the cumulative density function.}
#'     \item{\code{\link{expValCompBinom}}}{ gives the expected value.}
#'     \item{\code{\link{varCompBinom}}}{ gives the variance.}
#'     \item{\code{\link{TVatRCompBinom}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{VatRCompBinom}}}{ gives the Value-at-Risk.}
#'   }
#' Returned values are approximations for the cumulative density function,
#' TVaR, and VaR.
#'
#' @name CompBinom
NULL

#' @rdname CompBinom
#'
#' @importFrom stats dbinom pgamma
#' @export
#'
#' @examples
#' pCompBinom(x = 2, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
pCompBinom <- function(x, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        size %% 1 == 0,
        rate > 0,
        k0 >= 0
    )
    stopifnot(grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE))

    if(grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        stats::dbinom(x = 0, size = size, prob = prob) + sum(
            stats::dbinom(x = 1:k0, size = size, prob = prob) *
                stats::pgamma(q = x, shape = shape * 1:k0, rate = rate)
        )
    }
}

#' @rdname CompBinom
#'
#' @export
#'
#' @examples
#' expValCompBinom(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
expValCompBinom <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        size %% 1 == 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        expValGamma(shape, rate) * expValBinom(size, prob)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        expValLnorm(shape, sqrt(rate)) * expValBinom(size, prob)
    }
}

#' @rdname CompBinom
#'
#' @export
#'
#' @examples
#' varCompBinom(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
varCompBinom <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        size %% 1 == 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        (shape / rate)^2 * size * prob * (1 - prob) + size * prob * varGamma(shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        expValLnorm(shape, sqrt(rate))^2 * size * prob * (1 - prob) + size * prob * varLnorm(shape, sqrt(rate))
    }
}

#' @rdname CompBinom
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' VatRCompBinom(kap = 0.9, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VatRCompBinom <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap <= 1,
        prob >= 0, prob <= 1,
        size > 0,
        size %% 1 == 0,
        rate > 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= pCompBinom(x = 0, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity)) {
        VaR.CompBinom <- 0
    } else {
        stopifnot(shape > 0)
        VaR.CompBinom <- stats::optimize(function(i) abs(pCompBinom(x = i, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }

    return(VaR.CompBinom)
}

#' @rdname CompBinom
#'
#' @importFrom stats dbinom pgamma
#' @export
#'
#' @examples
#' vark_calc <- VatRCompBinom(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVatRCompBinom(kap = 0.9, size = 1, prob = 0.2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#'
TVatRCompBinom <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, vark, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap <= 1,
        prob >= 0, prob <= 1,
        size > 0,
        size %% 1 == 0,
        rate > 0,
        vark >= 0
    )

    if (vark == 0) {
        TVaR.BNCOMP <- expValCompBinom(size, prob, shape, rate, distr_severity = distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        TVaR.BNCOMP <- sum(
            stats::dbinom(x = 1:k0, size = size, prob = prob) *
                expValGamma(shape, rate) * 1:k0 *
                stats::pgamma(q = vark, shape = shape * 1:k0 + 1, rate = rate, lower.tail = FALSE)
        ) / (1 - kap)
    }

    return(TVaR.BNCOMP)
}
