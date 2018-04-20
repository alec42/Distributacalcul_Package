#' Mean-Excess Loss de la loi normale
#' @param mu moyenne mu
#' @param sig sigma
#' @param d déductible
#' @export
Mexcess_norm <- function(d, mu = 0, sig = 1)
{
    phi1 <- (d - mu) / sig
    fact_norm <- (sig / sqrt(2*pi)) * exp(-phi1^2 / 2)
    mu + d - fact_norm / (1 - pnorm(phi1))
}
