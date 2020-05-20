#' Mean excess loss of the Uniform distribution
#'
#' @description Mean excess loss of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar d TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' Mexcess_unif(d = 2, min = 2, max = 4)
#'
Mexcess_unif <- function(d, min = 0, max = 1) {
    stopifnot(
        # min < max,
        # d >= min,
        d <= max
    )

    (max - d) / 2
}
