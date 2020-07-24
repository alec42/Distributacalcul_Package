#' Probability Mass Function of the Logarithmic Distribution
#'
#' @description Probability mass function of the Logarithmic distribution with
#'  probability parameter \eqn{\gamma}{gam}.
#'
#' @template x-template
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' # With rate parameter
#' dlogarithmic(x = 3, prob = 0.2)
#'
dlogarithmic <- function(x, prob) {
    stopifnot(
        x > 0, x %% 1 == 0,
        prob >= 0, prob <= 1
    )

    -prob^(x) / (log(1 - prob) * x)
}
