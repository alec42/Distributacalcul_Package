#' Expected value of the (discrete) Uniform distribution
#'
#' @description Expected value of the (discrete) Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @template discrete-uniform-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_unifD(min = 2, max = 5)
#'
E_unifD <- function(min = 0, max = 1){
    stopifnot(min < max)

    (min + max)/2
}
