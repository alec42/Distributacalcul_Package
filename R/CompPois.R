#' Compound Poisson Distribution
#'
#' @description
#' Computes various risk measures (mean, variance, Value-at-Risk (VaR),
#' and Tail Value-at-Risk (TVaR)) for the compound Poisson distribution.
#'
#' @details
#' The compound Poisson distribution with parameters ... has density ....
#'
#' @param x vector of quantiles
#' @template lambda-template
#' @template distr_severity-template
#' @template k0-template
#' @template shape-template
#' @template rate-template
#' @template scale-template
#' @template vark-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{pCompPois}}  gives the cumulative density function.
#'     \item \code{\link{expValCompPois}}  gives the expected value.
#'     \item \code{\link{varCompPois}}  gives the variance.
#'     \item \code{\link{TVatRCompPois}}  gives the Tail Value-at-Risk.
#'     \item \code{\link{VatRCompPois}}  gives the Value-at-Risk.
#'   }
#' Returned values are approximations for the cumulative density function,
#' TVaR, and VaR.
#'
#' @name CompPois
NULL

#' @rdname CompPois
#'
#' @importFrom stats dpois pgamma
#' @export
#'
#' @examples
#' pCompPois(x = 2, lambda = 2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
pCompPois <- function(x, lambda, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        x >= 0,
        lambda > 0,
        rate > 0,
        k0 >= 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        stats::dpois(x = 0, lambda = lambda) + sum(
            stats::dpois(x = 1:k0, lambda = lambda) *
                stats::pgamma(q = x, shape = shape * 1:k0, rate = rate)
        )
    }
}

#' @rdname CompPois
#'
#' @export
#'
#' @examples
#' expValCompPois(lambda = 2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
expValCompPois <- function(lambda, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        lambda > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        expValGamma(shape, rate) * lambda
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        expValLnorm(shape, sqrt(rate)) * lambda
    }
}

#' @rdname CompPois
#'
#' @export
#'
#' @examples
#' varCompPois(lambda = 2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
varCompPois <- function(lambda, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        lambda > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        lambda * kthMomentGamma(k = 2, shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        lambda * kthMomentLnorm(k = 2, shape, sqrt(rate))
    }
}

#' @rdname CompPois
#'
#' @template kap-template
#'
#' @importFrom stats optimize dpois
#' @export
#'
#' @examples
#' VatRCompPois(kap = 0.9, lambda = 2, shape = log(1000) - 0.405,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VatRCompPois <- function(kap, lambda, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap <= 1,
        lambda > 0,
        rate > 0,
        shape > 0,
        k0 >= 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= pCompPois(x = 0, lambda = lambda, shape = shape, rate = rate, k0 = k0)) {
        VaR.CompPois <- 0
    } else {
        VaR.CompPois <- stats::optimize(function(i) abs(pCompPois(x = i, lambda = lambda, shape = shape, rate = rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }

    return(VaR.CompPois)
}

#' @rdname CompPois
#'
#' @importFrom stats pgamma dpois
#' @export
#'
#' @examples
#' vark_calc <- VatRCompPois(kap = 0.9, lambda = 2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVatRCompPois(kap = 0.9, lambda = 2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#'
TVatRCompPois <- function(kap, lambda, shape, rate = 1 / scale, scale = 1 / rate, vark, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap < 1,
        lambda > 0,
        rate > 0,
        k0 >= 0,
        vark >= 0
    )

    if (vark == 0) {
        TVaR.CompPois <- expValCompPois(rate, shape, lambda, distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        TVaR.CompPois <- sum(
            stats::dpois(x = 1:k0, lambda) *
                expValGamma(shape, rate) * 1:k0 *
                stats::pgamma(q = vark, shape = shape * 1:k0 + 1, rate, lower.tail = FALSE)
        ) / (1 - kap)
    }

    return(TVaR.CompPois)
}
