#' Espérance limitée d'une loi Erlang
#' @param d déductible
#' @param n n
#' @param b beta
#' @export
Elim_erlang <- function(d, n, b)
{
    (n/b) * pgamma(q = d, shape = n + 1, rate = b) + d * pgamma(q = d, shape = n, rate = b, lower.tail = F)
}
