#' Cumulative probability mass function of the (discrete) Uniform Distribution
#'
#' @description Cumulative probability mass function of the (discrete) Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar q TRUE
#' @templateVar kappa FALSE
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' punifD(q = 2, min = 2, max = 5)
#'
punifD <- function(q, min, max){
    if (q < min) {
        return(0)
    } else if (min <= q & q < max) {
        (round(q) - min + 1) / (max - min + 1)
    } else {
        return(1)
    }
}
