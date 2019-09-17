#' Fonction de densité de la loi binomiale négative
#' @param k Nombre de échecs (ou succès) visé.
#' @param r Nombre de succès visé.
#' @param p Probabilité de succès.
#' @param beta Deuxième paramétrisation de la loi binomiale négative tel que beta = (1 - p) / p.
#' @param nb_tries Stipule si la binomiale est une du nombre d'essais pour le r-ème succès, ou du nombre d'échecs pour le r-ème succès (par défaut).
#' @export
d_negbinom <- function(k, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    if(nb_tries)
    {
        choose((k - 1), (r - 1)) * p^r * (1 - p)^(k - r)
    }
    else
    {
        choose((r + k - 1), k) * p^r * (1 - p)^k
    }

}
