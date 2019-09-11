#' Mean-Excess loss d'une loi erlang
#' @param x x
#' @param d déductible
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
Mexcess_IG <- function(x, d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    ((mu - d) * (1 - pnorm((d - mu) * sqrt(1 / (beta * d)))) + (d + mu) * exp((2 * mu) / beta) * pnorm(-(d + mu) * sqrt(1 / (beta * d))))/ (1 - (pnorm(sqrt(1/(beta * x)) * (d - mu)) + exp((2 * mu) / beta) * pnorm(-sqrt(1/(beta * x)) * (d + mu))))
}
