#' Cumulative probability mass function of the (discrete) Uniform distribution
#'
#' @description Cumulative probability mass function of the (discrete) Uniform
#'  distribution with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar q TRUE
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' p_unifD(q = 2, min = 2, max = 5)
#'
p_unifD <- function(q, min = 0, max = 1){
    stopifnot(
        min < max
        # , q %% 1 == 0 # round
    )

    if (q < min) {
        return(0)
    } else if (min <= q & q < max) {
        (round(q) - min + 1) / (max - min + 1)
    } else {
        return(1)
    }
}
