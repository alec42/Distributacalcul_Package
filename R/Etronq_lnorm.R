#' Espérance tronquée d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @param d déductible
#' @export
Etronq_lnorm <- function(d, mu, sig)
{
    fact_lnorm <- (log(d) - mu  - sig^2) / sig
    exp(mu  + (sig^2) / 2) * pnorm(fact_lnorm)
}



