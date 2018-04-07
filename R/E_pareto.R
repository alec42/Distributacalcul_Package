#' Espérance d'une loi Pareto
#' @param alph alpha
#' @param lam lambda
#' @export
E_pareto <- function(alph, lam) lam / (alph - 1)
