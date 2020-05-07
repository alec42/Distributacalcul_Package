#' Tail Value-at-risk of the Poisson distribution
#'
#' @description Tail Value-at-Risk of the Poisson distribution with rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar q FALSE
#' @templateVar kap TRUE
#' @template pois-template
#'
#' @export
#'
#' @examples
#'
#' TVaR_pois(kappa = 0.8, lambda = 3, k0 = 2E2)
#'
TVaR_pois <- function(kappa, lambda, k0) {
    k <- 0:k0 # valeurs possibles
    fx <- dpois(x = k, lambda = lambda)
    vark <- qpois(p = kappa, lambda = lambda)

    TVaR.approx <- (
        Etronq_pois(vark, lambda, k0, less.than.d = FALSE) +
            vark * (ppois(q = vark, lambda = lambda) - kappa)
        ) / (1 - kappa)
    # message("This is an approximation") already returned by Etronq_pois
    return(TVaR.approx)
}
