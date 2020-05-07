#' Tail Value-at-risk d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @param kap kap
#' @export
TVaR_pareto <- function(kap, alph, lam)
{
    fact_tvar <- alph / (alph - 1)
    lam * (fact_tvar * (1-kap)^(-1/alph) - 1)
}
