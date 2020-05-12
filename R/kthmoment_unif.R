#' kth moment of the Uniform distribution
#'
#' @description kth moment of the Uniform distribution with shape parameters
#'  \eqn{a}{a} and \eqn{b}{b}.
#' @param min minimum
#' @param max maximum
# @templateVar k TRUE
# @template beta-template
#' @export
#'
#' @examples
#'
#' kthmoment_unif(k = 3, min = 4, max = 5)
#'
kthmoment_unif <- function(k, min, max) {
    stopifnot(min < max) # condition for k?

    (max^(k + 1) - min^(k + 1)) / ((k + 1) * (max - min))
}
