#' Espérance tronquée d'une loi uniforme
#' @param a minimum
#' @param b maximum
#' @param d cut-off value
#' @export
Etronq_unif <- function(d,a, b) {
    (d^2 -a^2) / (2*(b-a))
}
