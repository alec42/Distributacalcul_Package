#' Expected value of the Normal distribution
#'
#' @description Expected value of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' E_norm(mean = 3, sd = 5)
#'
E_norm <- function(mean, sd) {
    stopifnot(sd > 0)

    mean
}
