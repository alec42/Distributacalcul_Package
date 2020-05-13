#' Moment Generating Function of the Normal distribution
#'
#' @description Moment Generating Function (MGF) of the Normal distribution
#'   with mean \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @param t t.
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' MGF_norm(t = 1, mean = 3, sd = 5)
#'
MGF_norm <- function(t, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    exp(mean * t + t^2 * (sd^2) / 2)
}
