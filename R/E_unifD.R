#' Expected value of the (discrete) Uniform Distribution
#'
#' @description Expected value of the (discrete) Uniform distribution
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
#' E_unifD(min = 2, max = 5)
#'
E_unifD <- function(min, max){
    (min + max)/2
}
