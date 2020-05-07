#' Limited Mean of the Gamma distribution
#'
#' @description Limited Mean of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Elim_gamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' Elim_gamma(d = 2, shape = 3, rate = 0.25)
#'
Elim_gamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    E_gamma(shape, rate) * pgamma(q = d, shape + 1, rate) +
        d * pgamma(q = d, shape, rate, lower.tail = F)
}
