#' Variance of the (discrete) Uniform Distribution
#'
#' @description Variance of the (discrete) Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_unifD(min = 2, max = 5)
#'
V_unifD <- function(min, max){
    ((max - min + 1)^2 - 1)/12
}
