#' Compound Negative Binomial Distribution
#'
#' @description
#'  Computes various risk measures (mean, variance, Value-at-Risk (VaR),
#'   and Tail Value-at-Risk (TVaR)) for the compound Negative Binomial
#'   distribution.
#'
#' @details
#'  The compound Negative Binomial Distribution has density ....
#'
#' @template size-prob-negbinom-template
#' @template distr_severity-template
#' @template k0-template
#' @template x-template
#' @template shape-template
#' @template rate-template
#' @template scale-template
#'
#' @export
#'
#' @importFrom stats dbinom pgamma
#'
#' @examples
#' p_BNCOMP(x = 2, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
p_BNCOMP <- function(x, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stats::dnbinom(x = 0, size = size, prob = prob) +
            sum(sapply(1:k0, function(i) stats::dnbinom(x = i, size = size, prob = prob) * stats::pgamma(q = x, shape = shape * i, rate = rate)))
    }
}

#' @rdname p_BNCOMP
#' @export
#'
#' @examples
#'
#' E_BNCOMP(size = 4, prob = 0.2, shape = 0, rate = 1,
#'          distr_severity = "Lognormal")
#'
E_BNCOMP <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma")
{
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        if (missing(rate)) { # checks if user specified rate
            rate = 1 / scale
        }
        stopifnot(shape > 0, rate | scale > 0)
        E.NBCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * kthmoment_gamma(k = 1, shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        E.NBCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * E_lnorm(shape, sqrt(rate))
    } else {
        stop("Please enter a valid distribution choice. Either 'Lognormal' or 'Gamma'")
    }

    return(E.NBCOMP)
}


#' @rdname p_BNCOMP
#' @export
#'
#' @examples
#' V_BNCOMP(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
V_BNCOMP <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma") {
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        V.BNCOMP <- E_gamma(shape, rate)^2 * V_negbinom(size, prob, nb_tries = FALSE) + V_gamma(shape, rate) * E_negbinom(size, prob, nb_tries = FALSE)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        V.BNCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * (E_lnorm(shape, sqrt(rate)) / prob + V_lnorm(shape, sqrt(rate)))
    } else {
        stop("Please enter a valid distribution choice. Either 'Lognormal' or 'Gamma'")
    }

    return(V.BNCOMP)
}

#' @rdname p_BNCOMP
#' @export
#'
#' @template kap-template
#'
#' @examples
#' VaR_BNCOMP(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VaR_BNCOMP <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma")
{
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= p_BNCOMP(x = 0, size, prob, shape, rate, distr_severity = distr_severity, k0 = k0)) {
        0
    } else {
        stats::optimize(function(i) abs(p_BNCOMP(x = i, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }
}

#' @rdname p_BNCOMP
#' @export
#'
#' @importFrom stats dbinom pgamma
#'
#' @examples
#' \dontrun{
#' vark_calc <- VaR_BNCOMP(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVaR_BNCOMP(kap = 0.9, size = 1, prob = 0.2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#' }
#'
.TVaR_BNCOMP <- function(kap, vark, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma") {
    warning("This funciton is not ready for usage")
    stopifnot(prob >= 0, prob <= 1, rate > 0, kap < 1, kap >= 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (vark == 0) {
        TVaR.BNCOMP <- E_BNCOMP(size, prob, shape, rate, distr_severity = distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        TVaR.BNCOMP <- sum(sapply(1:k0, function(i)
            stats::dnbinom(x = i, size = size, prob = prob) *
                E_gamma(shape, rate) *
                stats::pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = FALSE))
        ) / (1 - kap)
    } else {
        stop("Please enter a valid distribution choice. 'Gamma'")
    }

    return(TVaR.BNCOMP)
}
