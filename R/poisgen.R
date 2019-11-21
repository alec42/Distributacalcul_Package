#' Generalized Poisson-mixture
#'
#' @description Probability mass function, cumulative probability mass function,
#'  expected value and variance of the Generalized Poisson-mixture with
#'  shape parameters \eqn{\lambda}{lambda} and \eqn{\tau}{tau}.
#'
#' @param shape1 shape parameter \eqn{\lambda}{lambda}, must be positive.
#' @param shape2 shape parameter \eqn{\tau}{tau}, where \eqn{\tau \in [0, )}{0 <= tau < 1}
#' @param k probability.
#' @param q quantile, \eqn{k \in \mathcal{N}}{k is a positive integer}.
#' @export
#'
#' @family Mixture distributions
#'
#' @examples
#' dpoisGen(k = 2, shape1 = 0.25, shape2 = 0.5)
#' ppoisGen(q = 3, shape1 = 0.25, shape2 = 0.5)
#' E_poisGen(shape1 = 0.25, shape2 = 0.5)
#' V_poisGen(shape1 = 0.25, shape2 = 0.5)
#'
dpoisGen <- function(k, shape1, shape2) {
    (shape1 * (1 - shape2) * (shape1 * (1 - shape2) + shape2 * k)^(k - 1)) /
        factorial(k) *
        exp(-(shape1 * (1 - shape2) + shape2 * k))
}

ppoisGen <- function(q, shape1, shape2, lower.tail = TRUE) {
    if (lower.tail) {
        sum(sapply(0:q, function(x) dpoisgen(x, shape1, shape2)))
    } else {
        1 - sum(sapply(0:q, function(x) dpoisgen(x, shape1, shape2)))
    }
}

E_poisGen <- function(shape1, shape2) {
    shape1
}

V_poisGen <- function(shape1, shape2) {
    shape1 / (1 - shape2)^2
}

#' Compound Generalized Poisson-mixture
#'
#' @description Expected value and variance of the Compound Generalized Poisson-mixture
#'  with shape parameters \eqn{\lambda}{lambda} and \eqn{\tau}{tau}.
#'  Additionnaly, approximations of the cumulative density function (CDF), Value-at-Risk (VaR),
#'  Tail Value-at-Risk (TVaR) are included.
#'
#' @param paramSev vector of severity distribution parameters.
#' @param distribution severity distribution choice. Valid arguments include "Gamma".
#' @param upper Upper bound for the summation of the cumulative density function approximation. In the case of the TVaR, it is a vector to optimize (in order) the VaR and the TVaR.
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#'
#' @export
#'
#' @family Compound mixture distributions
#'
#' @examples
#' E_poisGenComp(shape1 = 0.25, shape2 = 0.5, paramSev = c(2, 1/1000))
#' V_poisGenComp(shape1 = 0.25, shape2 = 0.5, paramSev = c(2, 1/1000))
#' Etronq_poisGenComp(d = 10193.43, shape1 = 0.25, shape2 = 0.5, paramSev = c(2, 1/1000))
#' ppoisGenComp(q = 10193.43, shape1 = 0.25, shape2 = 0.50, paramSev = c(2, 1/1000))
#' VaR_poisGenComp(kappa = 0.99, shape1 = 0.25, shape2 = 0.50, paramSev = c(2, 1/1000), upper = 1e5)
#' Etronq_poisGenComp(VaR_k = 10193.43, shape1 = 0.25, shape2 = 0.50, paramSev = c(2, 1/1000))
#'
E_poisGenComp <- function(shape1, shape2, paramSev, distribution = "Gamma") {
    if (distribution == "Gamma") {
        E_poisGen(shape1, shape2) * E_gamma(paramSev[1], paramSev[2])
    }
}

V_poisGenComp <- function(shape1, shape2, paramSev, distribution = "Gamma") {
    if (distribution == "Gamma") {
        E_poisGen(shape1, shape2) * V_gamma(paramSev[1], paramSev[2]) +
            V_poisGen(shape1, shape2) * E_gamma(paramSev[1], paramSev[2])^2
    }
}

ppoisGenComp <- function(q, shape1, shape2, paramSev, upper = 1e2, distribution = "Gamma", lower.tail = TRUE) {
    if (distribution == "Gamma") {
        p <- dpoisGen(0, shape1, shape2) +
            sum(sapply(1:upper, function(x) dpoisGen(x, shape1, shape2) * pgamma(q, paramSev[1] * x, paramSev[2])))
    }
    if (lower.tail) {
        p
    } else {
        1 - p
    }
}

VaR_poisGenComp <- function(kappa, shape1, shape2, paramSev, upper = 1e2, distribution = "Gamma") {
    optimise(function(q)
        abs(kappa -
                ppoisGenComp(q, shape1, shape2, paramSev, upper = 1e2, distribution = "Gamma")
        ), c(0, upper))$minimum
}

Etronq_poisGenComp <- function(VaR_k, shape1, shape2, paramSev, upper = 1e2, distribution = "Gamma") {
    sum(sapply(1:upper, function(x) x *
                   E_gamma(paramSev[1] * x, paramSev[2]) *
                   dpoisGen(k, shape1, shape2) *
                   pgamma(VaR_k, paramSev[1] * k, paramSev[2], lower.tail = F)
    )
    )
}


# TVaR_poisGenComp <- function(kappa, shape1, shape2, paramSev, upper = c(1e2, 1e3), distribution = "Gamma") {
#     (1 / kappa) *
#         sum(sapply(1:upper,
#                    function(x)
#                        dpoisGen(x, shape1, shape2) *
#                        E_gamma(paramSev[1] * x, paramSev[2]) *
#                        pgamma(VaR_poisGenComp(kappa, shape1, shape2, paramSev, upper = upper, distribution =distribution), paramSev[1] * x, paramSev[2], lower.tail = FALSE)))
# }

#' @description Capital allocation for Compound Generalized Poisson-mixture distribution.
#'
#' @param kappa probability.
#' @param TVaR_k Tail Value-at-Risk to calculate allocation.
#' @param VaR_k Value-at-Risk to calculate allocation.
#' @param rule rule by which to calculate allocation. Options include "variance" and "TVaR".
#'
#' @details returns list object with the proportion per frequency (M) and severity (B).
#'
#' @examples
#' alloc_poisGenComp(shape1 = 0.25, shape2 = 0.50, paramSev = c(2, 1/1000), TVaR_k = 16274.93)
#' alloc_poisGenComp(shape1 = 0.25, shape2 = 0.50, paramSev = c(2, 1/1000), TVaR_k = 16274.93, VaR_k = 10193.43, kappa = 0.99, rule = "TVaR")
#'
alloc_poisGenComp <- function(kappa, shape1, shape2, paramSev, TVaR_k = NULL, VaR_k = NULL, rule = "variance", distribution = "Gamma") {
    if (rule == "variance") {
        C_M <- ((V_poisGen(shape1, shape2) * E_gamma(paramSev[1], paramSev[2])^2)/V_poisGenComp(shape1, shape2, paramSev, distribution)) * TVaR_k
        C_B <- ((E_poisGen(shape1, shape2) * V_gamma(paramSev[1], paramSev[2]))/V_poisGenComp(shape1, shape2, paramSev, distribution)) * TVaR_k
    } else if (rule == "TVaR") {
        C_M <- Etronq_poisGenComp(VaR_k, shape1, shape2, paramSev, upper, distribution) / (1 - kappa)
        C_B <- TVaR_k - C_M
    }
    list("Explained by frequency (M)" = C_M,"Explained by severity (B)" =  C_B)
}
