#' Mean Excess-Loss d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @param d déductible
#' @export
Mexcess_lnorm <- function(d, mu, sig)
{
    esp <- exp(mu + (sig^2) / 2)
    phi1 <- (log(d) - mu - sig^2) / sig
    phi2 <-  (log(d) - mu) / sig
    ((1 / (1 - pnorm(phi1))) * esp * (1 - pnorm(phi2))) - d
}



