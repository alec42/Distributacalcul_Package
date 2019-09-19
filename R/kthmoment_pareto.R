#' k-ème moment d'une loi Pareto
#' @param k k-ème moment, -1 < k < alpha ou alpha > k
#' @param alpha alpha
#' @param lam lambda
#' @export
kthmoment_pareto <- function(alpha, lam, k)
{
    if(-1 < k & k < alpha)
        (lam^k * gamma(k + 1) * gamma(alpha - k))/gamma(alpha)
    else if(k < alpha)
        (lam^k * factorial(k))/prod(alpha - seq(from = 1, to = k, by = 1))
}
