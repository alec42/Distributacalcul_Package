#' Expected value of the Beta distribution
#'
#' @description Expected value of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @template beta-template
#'
#' @examples
#'
#' E_beta(shape1 = 3, shape2 = 5)
#'
E_beta <- function(shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0)

    shape1 / (shape1 + shape2)
}
