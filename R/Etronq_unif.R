#' Espérance tronquée d'une loi uniforme
#' @param a minimum
#' @param b maximum
#' @export
Etronq_unif <- function(d,a, b) (d^2 -a^2) / (2*(b-a))
