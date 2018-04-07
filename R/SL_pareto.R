#' Stop-Loss d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @param d déductible
#' @export
SL_pareto <- function(d, alph, lam)
{
    E_pareto(alph, lam) * (1 - ppareto(d, alph - 1, lam))
}
