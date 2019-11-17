#' Tail Value-at-risk d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @param kappa kappa
#' @export
TVaR_pareto <- function(kappa, alph, lam)
{
    fact_tvar <- alph / (alph - 1)
    lam * (fact_tvar * (1-kappa)^(-1/alph) - 1)
}
