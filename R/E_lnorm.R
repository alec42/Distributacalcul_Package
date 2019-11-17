#' Espérance d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @export
E_lnorm <- function(mu, sig) exp(mu + (sig^2) / 2)


