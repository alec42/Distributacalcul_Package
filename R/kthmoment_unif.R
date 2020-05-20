#' kth moment of the Uniform distribution
#'
#' @description kth moment of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar k TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' kthmoment_unif(k = 2, min = 3, max = 4)
#'
kthmoment_unif <- function(k, min = 0, max = 1) {
    stopifnot(min < max, k > -1) # condition for k stricly positive?

    (max^(k + 1) - min^(k + 1)) / ((k + 1) * (max - min))
}
