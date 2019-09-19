#' Répartition d'une loi uniforme discrète
#' @param k k
#' @param a borne inférieur
#' @param b borne supérieur
#' @details loi uniforme discrète
#' @export
punifD <- function(k, a, b){
    if(k < a){
        return(0)
    }
    else if(a <= k & k < b){
        (round(k) - a + 1)/(b - a + 1)
    }
    else{
        return(1)
    }
}
