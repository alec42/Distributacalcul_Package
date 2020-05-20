#' Stop-loss of the Uniform distribution
#'
#' @description Stop-loss of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar d TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' SL_unif(d = 3, min = 2, max = 4)
#'
SL_unif <- function(d, min = 0, max = 1) {
    stopifnot(min < max, d >= min, d <= max)

    ((max - d)^2) / (2 * (max - min))
}
