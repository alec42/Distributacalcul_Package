#' Variance of the (discrete) Uniform distribution
#'
#' @description Variance of the (discrete) Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_unifD(min = 2, max = 5)
#'
V_unifD <- function(min = 0, max = 1){
    stopifnot(min < max)

    ((max - min + 1)^2 - 1)/12
}
