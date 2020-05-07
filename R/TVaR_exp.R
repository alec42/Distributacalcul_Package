#' Tail Value-at-risk d'une loi exponentielle
#' @param b beta
#' @param kap pourcentage de confiance désiré
#' @export
TVaR_exp <- function(kap, b) VaR_exp(b, kap) + E_exp(b)
