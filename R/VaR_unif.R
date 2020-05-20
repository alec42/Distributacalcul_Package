#' Value-at-Risk of the Uniform distribution
#'
#' @description Value-at-Risk of the Uniform distribution
#'  with min \eqn{a} and max \eqn{b}.
#'
#' @templateVar kap TRUE
#' @template continuous-uniform-template
#'
#' @export
#'
#' @examples
#'
#' VaR_unif(kap = .99, min = 3, max = 4)
#'
VaR_unif <- function(kap, min = 0, max = 1) {
    stopifnot(kap <= 1, kap >= 0, min < max)

    min + (max - min) * kap
}
