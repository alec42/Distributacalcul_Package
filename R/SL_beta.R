#' Stop-loss of the Beta distribution
#'
#' @description Stop-loss of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{alpha}.
#'
#' @templateVar d TRUE
#' @template beta-template
#'
#' @examples
#'
#' SL_beta(d = .3, shape1 = 4, shape2 = 5)
#'
SL_beta <- function(d, shape1, shape2) {

    if (d < 0 | d > 1) {
        stop("d must be between 0 and 1")
    }

    E_beta(shape1, shape2) * pbeta(q = d, shape1 + 1, shape2, lower.tail = F) +
        d * pbeta(q = d, shape1, shape2, lower.tail = F)
}


