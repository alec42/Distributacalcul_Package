#' Value-at-Risk of the Beta distribution
#'
#' @description Value-at-Risk of the Beta distribution with shape
#'  parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'  Wrapper of qbeta.
#'
#' @templateVar kap TRUE
#' @template beta-template
#'
#' @examples
#'
#' VaR_beta(kap = .99, shape1 = 4, shape2 = 5)
#'
VaR_beta <- function(kap, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, kap >= 0, kap <= 1)

    qbeta(p = kap, shape1, shape2)
}


