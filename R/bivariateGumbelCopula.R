#' Bivariate Gumbel Copula
#'
#' @description
#' Computes CDF, PDF and simulations of the bivariate Gumbel copula.
#'
#' @details
#'  The bivariate Gumbel copula has CDF :
#'
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{cBivariateGumbel}}  returns the value of the copula.
#'     \item \code{\link{cdBivariateGumbel}}  returns the value of the density function associated to the copula.
#'     \item \code{\link{crBivariateGumbel}}  returns simulated values of the copula.
#'   }
#'
#'
#' @name bivariateGumbel
NULL

#' @rdname bivariateGumbel
#'
#' @export
#'
#' @examples
#' cBivariateGumbel(u1 = .76, u2 = 0.4, dependencyParameter = 1.4)
#'
cBivariateGumbel <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= 1
    )

    exp(-((-log(u1))^(dependencyParameter) + (-log(u2))^(dependencyParameter))^(1 / dependencyParameter))
}

#' @rdname bivariateGumbel
#'
#' @export
#'
#' @examples
#' cdBivariateGumbel(u1 = .76, u2 = 0.4, dependencyParameter = 1.4)
#'
cdBivariateGumbel <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= 1
    )

    cBivariateGumbel(u1, u2, dependencyParameter, ...) *
        (((-log(u1))^(dependencyParameter - 1) * (-log(u2))^(dependencyParameter - 1)) / (u1 * u2)) *
        ((-log(u1))^(dependencyParameter) + (-log(u2))^(dependencyParameter))^(1 / dependencyParameter - 2) *
        (dependencyParameter - 1 + ((-log(u1))^(dependencyParameter) + (-log(u2))^(dependencyParameter))^(1 / dependencyParameter))
}

#' @rdname bivariateGumbel
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp qunif
#'
#' @export
#'
#' @examples
#' crBivariateGumbel(numberSimulations = 10, seed = 42, dependencyParameter = 1.2)
#'
crBivariateGumbel <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter >= 1
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    set.seed(seed * 10)
    simulatedUniformsQuadruple <- cbind(
        simulatedUniforms,
        matrix(stats::runif(2 * numberSimulations, 0, 1), nrow = numberSimulations, ncol = 2)
    )

    simulatedUnifCircleStable <- stats::qunif(simulatedUniformsQuadruple[, 3], - pi / 2, pi / 2)
    simulatedExponentialsStable <- stats::qexp(simulatedUniformsQuadruple[, 4], 1)
    simulatedStables <- sin((simulatedUnifCircleStable + (pi / 2)) / dependencyParameter) / (cos(simulatedUnifCircleStable)^(dependencyParameter)) *
        (cos(pi / (2 * dependencyParameter) + ((1 / dependencyParameter) - 1) * simulatedUnifCircleStable) / simulatedExponentialsStable)^(dependencyParameter - 1)

    simulatedExponentials <- t(sapply(1:nrow(simulatedUniformsQuadruple), function(row) stats::qexp(simulatedUniformsQuadruple[row, 1:2], simulatedStables[row])))
    simulatedCopula <- sapply(1:2, function(col) exp(-(simulatedExponentials[,col])^(1/dependencyParameter)))

    simulatedCopula
}

