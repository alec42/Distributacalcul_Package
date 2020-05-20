#' Expected value of the Logarithmic distribution
#'
#' @description Expected value of the Logarithmic distribution with
#'  probability parameter \eqn{\gamma}{gamma}.
#'
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' E_logarithmique(prob = 0.50)
#'
E_logarithmique <- function(prob) {
    stopifnot(prob > 0, prob < 1)

    (-prob) / (log(1 - prob) * (1 - prob))
}
