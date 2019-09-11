#' Variance d'une loi Erlang
#' @param n tau
#' @param b beta
#' @export
V_erlang <- function(n, b)
{
    n / b^2
}
