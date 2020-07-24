#' Cumulative Mass Function of the Logarithmic Distribution
#'
#' @description Cumulative mass function of the Logarithmic distribution with
#'  probability parameter \eqn{\gamma}{gam}.
#'
#' @template q-template
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' # With rate parameter
#' plogarithmic(q = 3, prob = 0.2)
#'
plogarithmic <- function(q, prob) {
    stopifnot(
        q > 0, q %% 1 == 0,
        prob >= 0, prob <= 1
    )

    sum(dlogarithmic(1:q, prob))
}
