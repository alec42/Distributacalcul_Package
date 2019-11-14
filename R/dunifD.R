#' Probability mass function of the (discrete) Uniform Distribution
#'
#' @description Probability mass function of the (discrete) Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar x TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' dunifD(x = 2, min = 2, max = 5)
#'
dunifD <- function(x = 1, min, max){
    1 / (max - min + 1)
}
