#' Tail Value-at-Risk of the Uniform distribution
#'
#' @description Tail Value-at-Risk of the Uniform distribution with shape
#'  parameters \eqn{a}{a} and \eqn{b}{b}.
#'
#' @param kap probability.
#' @param min minimum a
#' @param max maximum b
# @templateVar kap TRUE
# @template uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_unif(kap = .2, min = 3, max = 4)
#'
TVaR_unif <- function(kap, min, max) {
    stopifnot(kap <= 1, kap >= 0, min < max)

    min + (max - min)/2 * (1 + kap)
}
