#' Fonction de répartition de la loi binomiale négative
#' @param k Nombre de échecs (ou succès) jusqu'où sommer.
#' @param r Nombre de succès visé.
#' @param p Probabilité de succès.
#' @param beta Deuxième paramétrisation de la loi binomiale négative tel que beta = (1 - p) / p.
#' @param nb_tries Stipule si la binomiale est une du nombre d'essais pour le r-ème succès, ou du nombre d'échecs pour le r-ème succès (par défaut).
#' @param lower.tail Si vrai (défaut) alors fonction de répartition. Si faux, de survie.
#' @export
p_negbinom <- function(k, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = FALSE, lower.tail = T)
    {
    start_FX <- ifelse(nb_tries, r, 0)

    if((nb_tries = T) & (k < r)){
        return(NULL)}
    else{
        if(lower.tail){
            sum(sapply(start_FX:k, function(i) d_negbinom(k = i, r = r, p = p, nb_tries = nb_tries)))
        }
        else{
            1 - sum(sapply(start_FX:k, function(i) d_negbinom(k = i, r = r, p = p, nb_tries = nb_tries)))
        }
    }
}
