#' Value-at-Risk of the Logarithmic Distribution
#'
#' @description Value-at-Risk of the Logarithmic distribution with
#'  probability parameter \eqn{\gamma}{gam}.
#'
#' @template kap-template
#' @template logarithmic-template
#'
#' @export
#' @importFrom stats optimise
#'
#' @examples
#'
#' # With rate parameter
#' VaR_logarithmic(kap = 0.99, prob = 0.2)
#'
VaR_logarithmic <- function(kap, prob) {
    stopifnot(
        kap >= 0, kap <= 1,
        prob >= 0, prob <= 1
    )

    optimisationMaxValue <- 1
    while(plogarithmic(optimisationMaxValue, 0.2) < 1) {
        optimisationMaxValue <- optimisationMaxValue + 1
    }

    ceiling(stats::optimise(function(q) abs(plogarithmic(ceiling(q), prob) - kap), c(1, optimisationMaxValue))$minimum)
}
