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

    if (kap < 0 | kap > 1) {
        stop("kap must be between 0 and 1")
    }

    qbeta(p = kap, shape1, shape2)
}


