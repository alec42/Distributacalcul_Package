#' Stop-loss d'une loi exponentielle
#' @param b beta
#' @param d montant d de la franchise
#' @export
SL_exp <- function(d, b) exp(-b*d) / b
