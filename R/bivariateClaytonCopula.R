#' Bivariate Clayton Copula
#'
#' @description
#' Computes CDF, PDF and simulations of the bivariate Clayton copula.
#'
#' @details
#'  The bivariate Clayton copula has CDF :
#'
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cBivariateAMH}}}{ returns the value of the copula.}
#'     \item{\code{\link{cdBivariateAMH}}}{ returns the value of the density function associated to the copula.}
#'   }
#'
#'
#' @name bivariateClayton
NULL

#' @rdname bivariateClayton
#'
#' @export
#'
#' @examples
#' cBivariateClayton(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cBivariateClayton <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter > 0
    )

    (u1^(-dependencyParameter) + u2^(-dependencyParameter) - 1)^(-1/dependencyParameter)
}

#' @rdname bivariateClayton
#'
#' @export
#'
#' @examples
#' cdBivariateClayton(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cdBivariateClayton <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter > 0
    )

    ((1 + dependencyParameter) / (u1 * u2)^(dependencyParameter + 1)) * (u1^(-dependencyParameter) + u2^(-dependencyParameter) - 1)^(-2 - 1/dependencyParameter)
}

#' @rdname bivariateClayton
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp qgamma
#'
#' @export
#'
#' @examples
#' crBivariateClayton(numberSimulations = 10, seed = 42, dependencyParameter = 0.2)
#'
crBivariateClayton <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter > 0
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    set.seed(seed * 10)
    simulatedUniformsTriple <- cbind(simulatedUniforms, stats::runif(numberSimulations, 0, 1))
    simulatedGammas <- stats::qgamma(simulatedUniformsTriple[,3], 1 / dependencyParameter, 1)
    simulatedExponentials <- t(sapply(1:nrow(simulatedUniformsTriple), function(row) stats::qexp(simulatedUniformsTriple[row, 1:2], simulatedGammas[row])))
    simulatedCopula <- sapply(1:2, function(col) (1 + simulatedExponentials)^(-1/dependencyParameter))

    simulatedCopula
}

