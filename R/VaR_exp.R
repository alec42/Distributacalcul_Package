#' Value-at-risk of the Exponential distribution
#'
#' @description Value-at-Risk of the Exponential distribution with rate
#'   parameter \eqn{\beta}{beta}.
#'
#' @param kap pourcentage de confiance désiré.
#' @param rate beta.
#' @param scale alternate parametrization, scale = 1 / rate.
#'
# @templateVar kap TRUE
# @template exponential-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_exp(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VaR_exp(kap = .2, shape = 3, rate = 0.25)
#'
VaR_exp <- function(kap, rate = 1 / scale, scale = 1 / rate) {
    -log(1 - kap) / rate
}
