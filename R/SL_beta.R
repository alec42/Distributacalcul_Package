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
#' SL_beta(d = 0.3, shape1 = 4, shape2 = 5)
#'
SL_beta <- function(d, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    E_beta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE) +
        d * stats::pbeta(q = d, shape1, shape2, lower.tail = FALSE)
}


