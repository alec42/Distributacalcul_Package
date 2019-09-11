#' k-ème moment de la loi Erlang
#' @param k k-ème moment
#' @param n n
#' @param b beta
#' @export
kthmoment_erlang <- function(k, n, b)
{
    prod(sapply(0:(k - 1), function(i) (n + i)))/(b^k)
}
