#' Espérance tronquée d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @export
Etronq_pareto <- function(d, alph, lam)
{
    un <- E_pareto(alph, lam) * ppareto(d, alph - 1, lam)
    deux <- d * (1 - ppareto(d, alph, lam))
    un - deux
}
