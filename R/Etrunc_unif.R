#' Truncated mean of the Uniform distribution
#'
#' @description Truncated mean of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @template d-template
#' @template less.than.d-template
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' Etrunc_unif(d = 3, min = 2, max = 4)
#'
#' # Values greather than d
#' Etrunc_unif(d = 3, min = 2, max = 4, less.than.d = FALSE)
#'
Etrunc_unif <- function(d, min = 0, max = 1, less.than.d = TRUE) {
    stopifnot(min < max, d >= min, d <= max)

    if (less.than.d) {
        Etrunc.unif <- (d^2 - min^2) / (2 * (max - min))
    } else {
        Etrunc.unif <- (max^2 - d^2) / (2 * (max - min))
    }

    return(Etrunc.unif)
}
