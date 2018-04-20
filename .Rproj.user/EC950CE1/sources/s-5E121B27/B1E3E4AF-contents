#' Espérance tronquée de la loi normale
#' @param mu moyenne mu
#' @param sig sigma
#' @param d déductible
#' @export
Etronq_norm <- function(d, mu = 0, sig = 1)
{
    phi1 <- (d - mu) / sig
    mu * pnorm(phi1) - (sig/sqrt(2*pi)) * exp(-phi1^2 / 2)
}
