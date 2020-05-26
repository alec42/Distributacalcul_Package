#' Compound Binomial Distribution
#'
#' @description
#'  Computes various risk measures (mean, variance, Value-at-Risk (VaR),
#'   and Tail Value-at-Risk (TVaR)) for the compound Binomial distribution.
#'
#' @details
#'  The compound Binomial Distribution has density ....
#'
#' @template size-prob-template
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
#' p_BINCOMP(x = 2, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'           rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
p_BINCOMP <- function(x, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        stats::dbinom(x = 0, size = size, prob = prob) + sum(sapply(1:k0, function(i) stats::dbinom(x = i, size = size, prob = prob) * stats::pgamma(q = x, shape = shape * i, rate = rate)))
    }
}

#' @rdname p_BINCOMP
#' @export
#'
#' @examples
#' E_BINCOMP(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
E_BINCOMP <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma")
{
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    if(distr_severity == "Gamma")
    {
        E_gamma(shape, rate) * E_binom(size, prob)
    }
    else if (distr_severity == "Lognormale")
        E_lnorm(shape, sqrt(rate)) * E_binom(size, prob)
}

#' @rdname p_BINCOMP
#' @export
#'
#' @examples
#' V_BINCOMP(size = 1, prob = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
V_BINCOMP <- function(size, prob, shape, rate = 1 / scale, scale = 1 / rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * size * prob * (1 - prob) + size * prob * V_gamma(shape, rate)
    }
    else if (distr_severity == "Lognormale")
        E_lnorm(shape, sqrt(rate))^2 * size * prob * (1 - prob) + size * prob * V_lnorm(shape, sqrt(rate))
}

#' @rdname p_BINCOMP
#' @export
#'
#' @template kap-template
#'
#' @examples
#' VaR_BINCOMP(kap = 0.9, size = 1, prob = 0.2, shape = log(1000) - 0.405,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#'
VaR_BINCOMP <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, k0, distr_severity = "Gamma")
{
    stopifnot(kap <= 1, kap >= 0, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= p_BINCOMP(x = 0, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity)) {
        0
    } else {
        stats::optimize(function(i) abs(p_BINCOMP(x = i, size, prob, shape, rate, k0 = k0, distr_severity = distr_severity) - kap), c(0, k0))$minimum
    }
}

#' @rdname p_BINCOMP
#' @export
#'
#' @importFrom stats dbinom pgamma
#'
#' @examples
#' vark_calc <- VaR_BINCOMP(kap = 0.9, size = 1, prob = 0.2, shape = 0.59,
#'             rate = 0.9^2, k0 = 1E2, distr_severity = "Gamma")
#' TVaR_BINCOMP(kap = 0.9, size = 1, prob = 0.2, shape = 0.59, rate = 0.9^2,
#'             vark = vark_calc, k0 = 1E2, distr_severity = "Gamma")
#'
TVaR_BINCOMP <- function(kap, size, prob, shape, rate = 1 / scale, scale = 1 / rate, vark, k0, distr_severity = "Gamma") {
    stopifnot(kap < 1, kap >= 0)

    if (vark == 0)
    {
        E_BINCOMP(size, prob, shape, rate, distr_severity = distr_severity) / (1 - kap)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:k0, function(i) stats::dbinom(x = i, size = size, prob = prob) * (shape * i / rate) * stats::pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = FALSE))) / (1 - kap))
    }
}
