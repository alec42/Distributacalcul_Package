#' Mean-Excess loss d'une loi erlang
#' @param d déductible
#' @param n n
#' @param b beta
#' @export
Mexcess_erlang <- function(d, n, b)
{
    (n / b) * pgamma(q = d, shape = n + 1, rate = b, lower.tail = F)/pgamma(q = d, shape = n, rate = b, lower.tail = F) - d
}
