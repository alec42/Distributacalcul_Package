#' Variance d'une loi uniforme discrète
#' @param a borne inférieur
#' @param b borne supérieur
#' @details loi uniforme discrète
#' @export
V_unifD <- function(a, b){
    ((b - a + 1)^2 - 1)/12
}
