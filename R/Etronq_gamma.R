#' Espérance tronquée d'une loi Gamma
#' @param a alpha
#' @param b beta (ou lambda dans certains livres)
#' @param d Deductible
#' @export
Etronq_gamma <- function(d, a, b) (a * pgamma(d, a+1, b)) / b

