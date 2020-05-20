#' Variance of the Uniform distribution
#'
#' @description Variance of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' V_unif(min = 3, max = 4)
#'
V_unif <- function(min = 0, max = 1) {
    stopifnot(min < max)

    ((max - min)^2) / 12
}
