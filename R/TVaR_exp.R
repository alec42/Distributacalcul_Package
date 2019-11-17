#' Tail Value-at-risk d'une loi exponentielle
#' @param b beta
#' @param kappa pourcentage de confiance désiré
#' @export
TVaR_exp <- function(kappa, b) VaR_exp(b, kappa) + E_exp(b)
