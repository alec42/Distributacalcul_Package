#' Compound Poisson Distribution
#'
#' @description
#'  Computes various risk measures (mean, variance, Value-at-Risk (VaR),
#'   and Tail Value-at-Risk (TVaR)) for the compound Poisson distribution.
#'
#' @details
#'  The compound Binomial Distribution with parameters ... has density ....
#'
#' @template lambda-template
#' @template distr_severity-template
#' @template k0-template
#' @template x-template
#' @template shape-template
#' @template rate-template
#' @template scale-template
#' @template vark-template
#'
#' @importFrom stats dpois pgamma
#' @export
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{p_PCOMP}}}{ gives the cumulative density function.}
#'  \item{\code{\link{E_PCOMP}}}{ gives the expected value.}
#'  \item{\code{\link{V_PCOMP}}}{ gives the variance.}
#'  \item{\code{\link{TVaR_PCOMP}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_PCOMP}}}{ gives the Value-at-Risk.}
#'  }
#'  Returned values are approximations for the cumulative density function,
#'  TVaR, and VaR.
#'
#' @examples
#' p_PCOMP(x = 2, lambda = 2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
p_PCOMP <- function(x, lambda, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        x >= 0,
        lambda > 0,
        rate > 0,
        k0 >= 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        stats::dpois(x = 0, lambda = lambda) +
            sum(sapply(1:k0,
                       function(k)
                           stats::dpois(x = k, lambda = lambda) *
                           stats::pgamma(q = x, shape = shape * k, rate = rate)
                       )
            )
    }
}

#' @rdname p_PCOMP
#' @export
#'
#' @examples
#' E_PCOMP(lambda = 2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
E_PCOMP <- function(lambda, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        lambda > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        E_gamma(shape, rate) * lambda
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        E_lnorm(shape, sqrt(rate)) * lambda
    }
}

#' @rdname p_PCOMP
#' @export
#'
#' @examples
#' V_PCOMP(lambda = 2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
V_PCOMP <- function(lambda, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(
        lambda > 0,
        rate > 0
    )

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        lambda * kthmoment_gamma(k = 2, shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        lambda * kthmoment_lnorm(k = 2, shape, sqrt(rate))
    }
}


#' @rdname p_PCOMP
#' @export
#'
#' @importFrom stats optimize dpois
#'
#' @template kap-template
#'
#' @examples
#' VaR_PCOMP(kap = 0.9, lambda = 2, shape = log(1000) - 0.405,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VaR_PCOMP <- function(kap, lambda, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap <= 1,
        lambda > 0,
        rate > 0,
        shape > 0,
        k0 >= 0
    )
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= p_PCOMP(x = 0, lambda = lambda, shape = shape, rate = rate, k0 = k0)) {
        VaR.PCOMP <- 0
    } else {
        VaR.PCOMP <- stats::optimize(function(i) abs(p_PCOMP(x = i, lambda = lambda, shape = shape, rate = rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }

    return(VaR.PCOMP)
}


#' @rdname p_PCOMP
#' @export
#'
#' @importFrom stats pgamma dpois
#'
#' @examples
#' vark_calc <- VaR_PCOMP(kap = 0.9, lambda = 2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVaR_PCOMP(kap = 0.9, lambda = 2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#'
TVaR_PCOMP <- function(kap, lambda, shape, rate = 1 / scale, scale = 1 / rate, vark, k0, distr_severity = "Gamma") {
    stopifnot(
        kap >= 0, kap < 1,
        lambda > 0,
        rate > 0,
        k0 >= 0,
        vark >= 0
    )

    if (vark == 0) {
        TVaR.PCOMP <- E_PCOMP(rate, shape, lambda, distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stopifnot(shape > 0)
        TVaR.PCOMP <- sum(sapply(1:k0, function(k) stats::dpois(x = k, lambda) * ( shape * k )/rate * stats::pgamma(q = vark, shape = shape * k + 1, rate, lower.tail = FALSE)))/(1 - kap)
    }

    return(TVaR.PCOMP)
}
