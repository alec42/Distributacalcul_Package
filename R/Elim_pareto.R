#' Espérance limitée d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @param d déductible
#' @export
Elim_pareto <- function(d, alph, lam)
{
    E_pareto(alph, lam) * ppareto(d, alph - 1, lam)
}
