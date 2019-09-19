#' Fonction de densité de la loi erlang
#' @param x x
#' @param n n
#' @param b beta
#' @export
derlang <- function(x, n, b)
{
    ((b ^ n) / gamma(n)) * (x^(n - 1)) * exp(-b * x)
}
