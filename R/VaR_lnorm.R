#' Value-at-Risk of the Lognormal distribution
#'
#' @description Value-at-Risk of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}. Wrapper of qlnorm.
#'
#' @templateVar kap TRUE
#' @template lnorm-template
#'
#' @export
#' @importFrom stats qlnorm
#'
#' @examples
#'
#' VaR_lnorm(kap = 0.8, meanlog = 3, sdlog = 5)
#'
VaR_lnorm <- function(kap, meanlog, sdlog) {
    stopifnot(kap >= 0, kap <= 1, sdlog > 0)

    stats::qlnorm(p = kap, meanlog = meanlog, sdlog = sdlog)
}
