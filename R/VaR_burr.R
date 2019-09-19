#' Value-at-risk d'une loi Burr
#' @param kappa niveau de confiance désiré
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
VaR_burr <- function(kappa, alpha, lam, tau) {
    (lam * ((1 - kappa)^(-1/alpha) - 1))^(1/tau)
}
