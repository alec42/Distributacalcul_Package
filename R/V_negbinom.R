#' Variance d'une loi binomiale négative
#' @param r Nombre de succès visé.
#' @param p Probabilité de succès.
#' @param beta Deuxième paramétrisation de la loi binomiale négative tel que beta = (1 - p) / p.
#' @param nb_tries Stipule si la binomiale est une du nombre d'essais pour le r-ème succès, ou du nombre d'échecs pour le r-ème succès (par défaut).
#' @export
V_negbinom <- function(r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    r * ((1 - p) / (p^2))
}
