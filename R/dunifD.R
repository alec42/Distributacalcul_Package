#' Densité d'une loi uniforme discrète
#' @param x x n'a aucun impact pour une loi uniforme par définition
#' @param a borne inférieur
#' @param b borne supérieur
#' @details loi uniforme discrète
#' @export
dunifD <- function(x, a, b){
    1 / (b - a + 1)
}
