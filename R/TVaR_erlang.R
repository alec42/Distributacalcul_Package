#' Tail Value-at-risk d'une loi Erlang
#' @param kappa Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kappa
#' @param n n
#' @param b beta
#' @details Cette formule nécessite la formule de la VaR_erlang (déjà installée avec le package tvarPackage)
#' @export
TVaR_erlang <- function(kappa, n, b, vark)
{
    (n / ((1 - kappa) * b)) * (exp(-b * vark) * sum(sapply(0:n, function(j) ((b * vark)^j)/factorial(j) )))
}
