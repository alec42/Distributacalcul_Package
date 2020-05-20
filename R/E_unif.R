#' Expected value of the Uniform distribution
#'
#' @description Expected value of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' E_unif(min = 3, max = 4)
#'
E_unif <- function(min = 0, max = 1) {
    stopifnot(min < max)

    (min + max) / 2
}
