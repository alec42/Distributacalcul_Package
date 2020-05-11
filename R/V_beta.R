#' Variance of the Beta distribution
#'
#' @description Variance of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @template beta-template
#'
#' @examples
#'
#' V_beta(shape1 = 4, shape2 = 5)
#'
V_beta <- function(shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0)

    (shape1 * shape2) /
        (
            (shape1 + shape2)^2 * (shape1 + shape2 + 1)
        )
}
