#' Tail Value-at-Risk of the Poisson distribution
#'
#' @description Tail Value-at-Risk of the Poisson distribution with rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar kap TRUE
#' @templateVar k0 TRUE
#' @template pois-template
#'
#' @export
#' @importFrom stats ppois qpois dpois
#'
#' @examples
#'
#' TVaR_pois(kap = 0.8, lambda = 3, k0 = 2E2)
#'
TVaR_pois <- function(kap, lambda, k0) {
    stopifnot(kap >= 0, kap < 1, lambda > 0, k0 > 0)

    k <- 0:k0 # valeurs possibles
    fx <- stats::dpois(x = k, lambda = lambda)
    vark <- stats::qpois(p = kap, lambda = lambda)

    TVaR.approx <- (
        Etronq_pois(vark, lambda, k0, less.than.d = FALSE) +
            vark * (stats::ppois(q = vark, lambda = lambda) - kap)
        ) / (1 - kap)

    return(TVaR.approx)
}
