#' Moment Generating Function of the Beta distribution
#'
#' @description Moment Generating Function (MGF) of the Beta distribution
#'  with shape parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @param t t.
#' @templateVar k0 TRUE
#' @template beta-template
#'
#' @export
#'
#' @examples
#'
#' MGF_beta(t = 1, shape1 = 3, shape2 = 5, k0 = 1E2)
#'
MGF_beta <- function(t, shape1, shape2, k0) {
    stopifnot(shape1 > 0, shape2 > 0, k0 > 0) # domain for t?

    MGF.beta <- 1 + sum(
            sapply(1:k0, function(k) {
                prod(sapply(0:(k - 1), function(j) (shape1 + j) / (shape1 + shape2 + j)),
                     (t^k) / factorial(k))
            })
        )
    warning("This is an approximation")
    return(MGF.beta)
}
