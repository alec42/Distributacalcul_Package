#' Mean excess loss (excès-moyen) d'une loi inverse gaussienne
#' @param d déductible
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
#' @importFrom stats pnorm
Mexcess_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    stopifnot(d >= 0, mu >= 0, beta >= 0)

    ((mu - d) * stats::pnorm((d - mu) * sqrt(1 / (beta * d)), lower.tail = F) +
         (d + mu) * exp((2 * mu) / beta) * stats::pnorm(-(d + mu) * sqrt(1 / (beta * d)))) /
        (1 - (stats::pnorm(sqrt(1/(beta * d)) * (d - mu)) +
                  exp((2 * mu) / beta) *
                  stats::pnorm(-sqrt(1/(beta * d)) * (d + mu))
              )
         )
}
