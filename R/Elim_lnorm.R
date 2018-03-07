#' Espérance limitée d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @param d déductible
#' @export
Elim_lnorm <- function(d, mu, sig)
{
    esp <- exp(mu + (sig^2) / 2)
    phi1 <- (log(d) - mu - sig^2) / sig
    phi2 <-  (log(d) - mu) / sig
    (esp * pnorm(phi1)) + (d * (1 - pnorm(phi2)))
}



