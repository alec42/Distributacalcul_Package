#' Espérance limités d'une loi uniforme
#' @param a minimum
#' @param b maximum
#' @param d cut-off value
#' @export
Elim_unif <- function(d, a, b) (d^2 - a^2) / (2*(b-a)) + d*((b-d) / (b-a))
