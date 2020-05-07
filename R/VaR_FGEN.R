#' Value-at-Risk of the F-Generalized distribution
#'
#' @param x x
#' @param kap niveau de confiance désiré
#' @param lambda lambda, > 0
#' @param alpha alpha, >0
#' @param tau tau, >0
#'
#' @export
#'
VaR_FGEN <- function(x, kap, lambda, alpha, tau) {
    abs(pFGEN(x, lambda, alpha, tau) - kap)
}
