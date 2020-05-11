#' kth moment of the Beta distribution
#'
#' @description kth moment of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @template beta-template
#'
#' @export
#'
#' @examples
#'
#' kthmoment_beta(k = 3, shape1 = 4, shape2 = 5)
#'
kthmoment_beta <- function(k, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0) # condition for k?

    (gamma(shape1 + k) * gamma(shape1 + shape2)) /
        (gamma(shape1) * gamma(shape1 + shape2 + k))
}
