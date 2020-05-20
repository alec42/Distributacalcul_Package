#' Truncated mean of the Uniform distribution
#'
#' @description Truncated mean of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' Etronq_unif(d = 3, min = 2, max = 4)
#'
#' # Values greather than d
#' Etronq_unif(d = 3, min = 2, max = 4, less.than.d = FALSE)
#'
Etronq_unif <- function(d, min = 0, max = 1, less.than.d = TRUE) {
    stopifnot(min < max, d >= min, d <= max)

    if (less.than.d) {
        Etronq.unif <- (d^2 - min^2) / (2 * (max - min))
    } else {
        Etronq.unif <- (max^2 - d^2) / (2 * (max - min))
    }

    return(Etronq.unif)
}
