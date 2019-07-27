#' Tail Value-at-risk d'une loi Erlang
#' @param k Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance k
#' @param alpha alpha
#' @param lam lam
#' @param tau tau
#' @details Cette formule nécessite la formule de la VaR_erlang (déjà installée avec le package tvarPackage)
#' @export
TVaR_erlang <- function(vark, k, n, b)
{
    (n / ((1 - k) * b)) * (exp(-b * vark) * sum(sapply(0:n, function(j) ((b * vark)^j)/factorial(j) )))
}
