#' Fonction de répartition de la loi Erlang
#' @param x x
#' @param n n
#' @param b beta
#' @param lower.tail Si vrai (défaut) alors fonction de répartition. Si faux, de survie.
#' @export
perlang <- function(x, n, b, lower.tail = T)
{
    Fx <- exp(-b * x) * sum(sapply(0:(n - 1), function(j) ((b * x)^j)/factorial(j)))
    if(lower.tail == T)
        (1 - Fx)
    else
        Fx
}
