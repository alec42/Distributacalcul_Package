#' Probability mass function of the (discrete) Uniform Distribution
#'
#' @description Probability mass function of the (discrete) Uniform
#'  distribution with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar x TRUE
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' d_unifD(x = 2, min = 2, max = 5)
#'
d_unifD <- function(x, min = 0, max = 1){
    stopifnot(
        min < max
        # ,q %% 1 == 0 # not part of the equation
    )

    1 / (max - min + 1)
}
