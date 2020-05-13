#' Moment Generating Function of the Gamma distribution
#'
#' @description Moment Generating Function (MGF) of the Gamma distribution
#'  with shape parameter \eqn{\alpha}{alpha} and rate parameter
#'  \eqn{\beta}{beta}.
#'
#' @param t t.
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' MGF_gamma(t = 1, shape = 3, rate = 5)
#'
MGF_gamma <- function(t, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, t < rate) # domain for t where non-neg?

    (rate / (rate - t))^shape
}
