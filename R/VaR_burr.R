#' Value-at-risk de la loi Burr
#' @param k niveau de confiance désiré
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
VaR_burr <- function(k, alpha, lam, tau) {
    (lam * ((1 - k)^(-1/alpha) - 1))^(1/tau)
}
