#' Stop-loss d'une loi exponentielle
#' @param d montant d de la franchise
#' @param n n
#' @param b beta
#' @export
SL_erlang <- function(d, n, b)
{
    (n/b) * pgamma(q = d, shape = n + 1, rate = b, lower.tail = F) - d * pgamma(q = d, shape = n, rate = b, lower.tail = F)
}
