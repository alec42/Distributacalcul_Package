#' Value-at-Risk of the Uniform distribution
#'
#' @description Value-at-Risk of the Uniform distribution with shape
#'  parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @param kap probability.
#' @param min minimum
#' @param max maximum
# @templateVar kap TRUE
# @template uniform-template
#'
#' @export
#'
# @examples
#
# # With scale parameter
# VaR_unif(kap = .2, min = 3, max = 4)
#'
VaR_unif <- function(kap, min, max) {
    stopifnot(kap <= 1, kap >= 0, min < max)

    min + (max - min) * kap
}
