#' Fréchet Lower Bound Copula
#'
#' @description
#' Computes CDF and simulations of the Fréchet lower bound copula.
#'
#' @details
#'  The Fréchet lower bound copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = \max(u_{1} + u_{2} - 1, 0)}{C(u1, u2) = max(u1 + u2 - 1, 0)}
#'   for \eqn{u_{1}, u_{2} \in [0, 1]}{0 <= u1, u2 <= 1}.
#'
#' @template u1u2-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{cFrechetLowerBound}}  returns the value of the copula.
#'     \item \code{\link{crFrechetLowerBound}}  returns simulated values of the copula.
#'   }
#'
#'
#' @name frechetLowerBound
NULL

#' @rdname frechetLowerBound
#'
#' @export
#'
#' @examples
#' cFrechetLowerBound(u1 = .76, u2 = 0.4)
#'
cFrechetLowerBound <- Vectorize(function(u1, u2, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1
    )

    max(u1 + u2 - 1, 0)
})

#' @rdname frechetLowerBound
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' crFrechetLowerBound(numberSimulations = 10, seed = 42)
#'
crFrechetLowerBound <- function(numberSimulations = 1E4, seed = 42) {
    stopifnot(
        numberSimulations > 0
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )

    cbind(simulatedUniforms[, 1], 1 - simulatedUniforms[, 1])
}
