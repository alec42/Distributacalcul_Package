#' Variance d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @export
V_pareto <- function(alph, lam)
{
    (alph*lam^2) / ((alph - 1)^2 * (alph -2))
}
