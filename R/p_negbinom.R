#' Fonction de répartition de la loi binomiale négative
#' @param k Nombre de échecs (ou succès) jusqu'où sommer.
#' @param r Nombre de succès visé.
#' @param p Probabilité de succès.
#' @param beta Deuxième paramétrisation de la loi binomiale négative tel que beta = (1 - p) / p.
#' @param nb_tries Stipule si la binomiale est une du nombre d'essais pour le r-ème succès, ou du nombre d'échecs pour le r-ème succès (par défaut).
#' @export
p_negbinom <- function(k, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    sum(sapply(0:k, function(i) d_nbinom(k = i, r, p, nb_tries)))
}
