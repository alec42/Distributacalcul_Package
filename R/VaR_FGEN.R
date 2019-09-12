#' VaR de la F-Généralisé
#' @param x x
#' @param kappa niveau de confiance désiré
#' @param lambda lambda, > 0
#' @param alpha alpha, >0
#' @param tau tau, >0
#' @export
VaR_FGEN <- function(x, kappa, lambda, alpha, tau) {
    abs(pFGEN(x, lambda, alpha, tau) - kappa)
}
