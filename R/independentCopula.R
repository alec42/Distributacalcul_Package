#' Independence Copula
#'
#' @description
#' Computes CDF and simulations of the independence copula.
#'
#' @details
#'  The independence copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = u_{1} \times u_{2}}{C(u1, u2) = u1 * u2}
#'   for \eqn{u_{1}, u_{2} \in [0, 1]}{0 <= u1, u2 <= 1}.
#'
#' @template u1u2-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cIndependent}}}{ returns the value of the copula.}
#'     \item{\code{\link{crIndependent}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name independent
NULL

#' @rdname independent
#'
#' @export
#'
#' @examples
#' cIndependent(u1 = .76, u2 = 0.4)
#'
cIndependent <- Vectorize(function(u1, u2, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1
    )

    u1 * u2
})

#' @rdname independent
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' crIndependent(numberSimulations = 10, seed = 42)
#'
crIndependent <- function(numberSimulations = 1E4, seed = 42) {
    stopifnot(
        numberSimulations > 0
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )

    simulatedUniforms
}
