#' Espérance tronquée d'une loi Erlang
#' @param d Deductible
#' @param n n
#' @param b beta
#' @export
Etronq_erlang <- function(d, n, b)
{
    n/b * (1 - exp(-b * d) * sum(sapply(0:n, function(j) ((b * d)^j)/factorial(j) )))
}
