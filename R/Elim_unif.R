#' Limited mean of the Uniform distribution
#'
#' @description Limited mean of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar d TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' Elim_unif(d = 3, min = 2, max = 4)
#'
Elim_unif <- function(d, min = 0, max = 1) {
    stopifnot(min < max, d >= min, d <= max)

    (d^2 - min^2) / (2 * (max - min)) + d * ((max - d) / (max - min))
}
