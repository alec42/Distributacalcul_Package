#' Compound Negative Binomial Distribution
#'
#' @description
#' Computes various risk measures (mean, variance, Value-at-Risk (VatR),
#' and Tail Value-at-Risk (TVatR)) for the compound Negative Binomial
#' distribution.
#'
#' @details
#' The compound negative binomial distribution has density ....
#'
#' @template size-prob-negbinom-template
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
#'     \item{\code{\link{pCompNBinom}}}{ gives the cumulative density function.}
#'     \item{\code{\link{expValCompNBinom}}}{ gives the expected value.}
#'     \item{\code{\link{varCompNBinom}}}{ gives the variance.}
#'     \item{\code{\link{TVatRCompNBinom}}}{ gives the Tail Value-at-Risk.}
#'     \item{\code{\link{VatRCompNBinom}}}{ gives the Value-at-Risk.}
#'   }
#' Returned values are approximations for the cumulative density function,
#' TVatR, and VatR.
#'
#' @name CompNBinom
NULL

#' @rdname CompNBinom
#'
#' @importFrom stats dbinom pgamma
#' @export
#'
#' @examples
#' pCompNBinom(x = 2, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
pCompNBinom <- function(x, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        rate > 0,
        k0 >= 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        stats::dnbinom(x = 0, size = size, prob = prob) + sum(
            stats::dnbinom(x = 1:k0, size = size, prob = prob) *
                stats::pgamma(q = x, shape = shape * 1:k0, rate = rate)
        )
    }
}

#' @rdname CompNBinom
#'
#' @export
#'
#' @examples
#'
#' expValCompNBinom(size = 4, prob = 0.2, shape = 0, scale = 1,
#'          distr_severity = "Lognormal")
#'
expValCompNBinom <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        expVal.NBCOMP <- expValNBinom(size, prob, nb_tries = FALSE) * kthMomentGamma(k = 1, shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        expVal.NBCOMP <- expValNBinom(size, prob, nb_tries = FALSE) * expValLnorm(shape, sqrt(rate))
    }

    return(expVal.NBCOMP)
}


#' @rdname CompNBinom
#'
#' @export
#'
#' @examples
#' varCompNBinom(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
varCompNBinom <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        prob >= 0, prob <= 1,
        size > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        V.CompNBinom <- expValGamma(shape, rate)^2 * varNBinom(size, prob, nb_tries = FALSE) + varGamma(shape, rate) * expValNBinom(size, prob, nb_tries = FALSE)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        V.CompNBinom <- expValNBinom(size, prob, nb_tries = FALSE) * (expValLnorm(shape, sqrt(rate)) / prob + varLnorm(shape, sqrt(rate)))
    }

    return(V.CompNBinom)
}

#' @rdname CompNBinom
#'
#' @template kap-template
#'
#' @export
#'
#' @examples
#' VatRCompNBinom(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VatRCompNBinom <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap < 1,
        prob >= 0, prob <= 1,
        size > 0,
        rate > 0,
        k0 >= 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= pCompNBinom(x = 0, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity)) {
        VatR.CompNBinom <- 0
    } else {
        stopifnot(shape > 0)
        VatR.CompNBinom <- stats::optimize(function(i) abs(pCompNBinom(x = i, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }

    return(VatR.CompNBinom)
}

#' @rdname CompNBinom
#'
#' @importFrom stats dbinom pgamma
#' @export
#'
#' @examples
#' vark_calc <- VatRCompNBinom(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVatRCompNBinom(kap = 0.9, size = 1, prob = 0.2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#'
TVatRCompNBinom <- function(kap, vark, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap < 1,
        prob >= 0, prob <= 1,
        rate > 0,
        k0 >= 0,
        vark >= 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (vark == 0) {
        TVatR.CompNBinom <- expValCompNBinom(size, prob, shape, rate, distr_severity = distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        TVatR.CompNBinom <- sum(
            stats::dnbinom(x = 1:k0, size = size, prob = prob) *
                expValGamma(shape, rate) * 1:k0 *
                stats::pgamma(q = vark, shape = shape * 1:k0 + 1, rate = rate, lower.tail = FALSE)
        ) / (1 - kap)
    }

    return(TVatR.CompNBinom)
}
