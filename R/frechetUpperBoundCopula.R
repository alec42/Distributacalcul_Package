#' Fréchet Upper Bound Copula
#'
#' @description
#' Computes CDF and simulations of the Fréchet upper bound copula.
#'
#' @details
#'  The Fréchet upper bound copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = \min(u_{1}, u_{2})}{C(u1, u2) = min(u1, u2)}
#'   for \eqn{u_{1}, u_{2} \in [0, 1]}{0 <= u1, u2 <= 1}.
#'
#' @template u1u2-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cFrechetUpperBound}}}{ returns the value of the copula.}
#'     \item{\code{\link{crFrechetUpperBound}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name frechetUpperBound
NULL

#' @rdname frechetUpperBound
#'
# @importFrom stats dbinom pgamma
#' @export
#'
#' @examples
#' cFrechetUpperBound(u1 = .56, u2 = 0.4)
#'
cFrechetUpperBound <- Vectorize(function(u1, u2, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1
    )

    min(u1, u2)
})

#' @rdname frechetUpperBound
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' crFrechetUpperBound(numberSimulations = 10, seed = 42)
#'
crFrechetUpperBound <- function(numberSimulations = 1E4, seed = 42) {
    stopifnot(
        numberSimulations > 0
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )

    cbind(simulatedUniforms[, 1], simulatedUniforms[, 1])
}
