#' Truncated mean of the Poisson distribution
#'
#' @description Truncated mean of the Poisson distribution with rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @templateVar k0 TRUE
#' @template pois-template
#'
#' @export
#'
#' @examples
#'
#' Etronq_pois(d = 0, lambda = 2, k0 = 2E2, less.than.d = FALSE)
#' Etronq_pois(d = 2, lambda = 2, k0 = 2E2, less.than.d = TRUE)
#'
Etronq_pois <- function(d, lambda, k0, less.than.d = TRUE) {
    stopifnot(lambda > 0)
    k <- 0:k0 # valeurs possibles
    fx <- stats::dpois(x = k, lambda = lambda)

    if (less.than.d) {
        Etronq.approx <- sum((k * fx)[k <= d])
    } else {
        Etronq.approx <- sum((k * fx)[k > d])
    }

    message("This is an approximation")
    return(Etronq.approx)
}
