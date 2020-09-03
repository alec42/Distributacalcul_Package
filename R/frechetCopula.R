#' Fréchet Copula
#'
#' @description
#' Computes CDF and simulations of the Fréchet copula.
#'
#' @details
#'  The Fréchet copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = (1 - \alpha - \beta)(u_{1} \times u_{2}) + \alpha\min(u_{1}, u_{2})%
#'     + \beta\max(u_{1} + u_{2} - 1, 0)}{C(u1, u2) = (1 - alpha - beta) (u1 * u2) %
#'     + alpha min(u1, u2) + beta max(u1 + u2 - 1, 0)}
#'   for \eqn{u_{1}, u_{2}, \alpha, \beta \in [0, 1]}{0 <= u1, u2, alpha, beta <= 1} %
#'    and \eqn{\alpha + \beta \leq 1}{alpha + beta <= 1}.
#'
#' @template u1u2-template
#' @template dependencyParameter-template-two
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cFrechet}}}{ returns the value of the copula.}
#'     \item{\code{\link{crFrechet}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name frechet
NULL

#' @rdname frechet
#'
#' @export
#'
#' @examples
#' cFrechet(u1 = .76, u2 = 0.4, dependencyParameter = c(0.2, 0.3))
#'
cFrechet <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= 0, dependencyParameter <= 1, sum(dependencyParameter) <= 1
    )

    (1 - dependencyParameter[1] - dependencyParameter[2]) * (u1 * u2) +
        dependencyParameter[1] * min(u1, u2) +
        dependencyParameter[2] * max(u1 + u2 - 1, 0)
}

#' @rdname frechet
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' crFrechet(numberSimulations = 10, seed = 42, dependencyParameter = c(0.2, 0.3))
#'
crFrechet <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter >= 0, dependencyParameter <= 1, sum(dependencyParameter) <= 1
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    vUpperBound <- cbind(simulatedUniforms[,2], simulatedUniforms[,2])
    vLowerBound <- cbind(simulatedUniforms[,1], 1 - simulatedUniforms[,1])
    set.seed(seed*10)
    vIndependant <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )

    J <- sample(c(0, 1, 2), numberSimulations, replace = TRUE, prob = c(dependencyParameter[[1]], dependencyParameter[[2]], 1 - sum(dependencyParameter)))
    simulatedCopula[, 1] <- rowSums(cbind((J == 0) * vUpperBound[, 1], (J == 1) * vLowerBound[, 1]), (J == 2) * vIndependant[, 1])
    simulatedCopula[, 2] <- rowSums(cbind((J == 0) * vUpperBound[, 2], (J == 1) * vLowerBound[, 2]), (J == 2) * vIndependant[, 2])

    simulatedCopula
}
