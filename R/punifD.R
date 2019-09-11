#' Répartition d'une loi uniforme discrète
#' @param q quantile
#' @param a borne inférieur
#' @param b borne supérieur
#' @details loi uniforme discrète
#' @export
punifD <- function(q, a, b){
    if(q < a){
        return(0)
    }
    else if(a <= q & q < b){
        (round(q) - a + 1)/(b - a + 1)
    }
    else{
        return(1)
    }
}
