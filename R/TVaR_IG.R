#' Tail Value-at-Risk of the Inverse Gaussian distribution
#'
#' @description Tail Value-at-Risk of the Inverse Gaussian distribution with
#'  mean \eqn{\mu}{mu} and shape parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template IG-template
#'
#' @export
#'
#' @examples
#'
#' TVaR_IG(kap = 0.99, mean = 2, shape = 5)
#'
TVaR_IG <- function(kap, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape > 0, kap >= 0, kap < 1)

    vark <- VaR_IG(kap = kap, mean, shape)

    (mean - vark +
            (2*vark + mean) * exp(2*mean / shape) +
            (2*vark - mean) * stats::pnorm(q = ((vark - mean) * sqrt(1 / (shape*vark))))
    ) / (1 - kap)
}
