#' Stop-loss d'une loi uniforme
#' @param a minimum
#' @param b maximum
#' @export
SL_unif <- function(d, a, b) (b-d)^2 / (2*(b-a))
