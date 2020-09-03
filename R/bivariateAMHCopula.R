#' Bivariate Ali-Mikhail-Haq Copula
#'
#' @description
#' Computes CDF, PDF and simulations of of the bivariate Ali-Mikhail-Haq
#' copula.
#'
#' @details
#'  The bivariate Ali-Mikhail-Haq copula has CDF :
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cBivariateAMH}}}{ returns the value of the copula.}
#'     \item{\code{\link{cdBivariateAMH}}}{ returns the value of the density copula.}
#'     \item{\code{\link{crBivariateAMH}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name bivariateAMH
NULL

#' @rdname bivariateAMH
#'
#' @export
#'
#' @examples
#' cBivariateAMH(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cBivariateAMH <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= -1, dependencyParameter <= 1
    )

    (u1 * u2) / (1 - dependencyParameter * (1 - u1) * (1 - u2))
}

#' @rdname bivariateAMH
#'
#' @export
#'
#' @examples
#' cdBivariateAMH(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cdBivariateAMH <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= -1, dependencyParameter <= 1
    )

    (
        1 - dependencyParameter +
            2 * dependencyParameter * cBivariateAMH(u1, u2, dependencyParameter, ...)
    ) / (
        1 - dependencyParameter * (1 - u1) * (1 - u2)
    )^2
}

#' @rdname bivariateAMH
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp
#'
#' @export
#'
#' @examples
#' crBivariateAMH(numberSimulations = 10, seed = 42, dependencyParameter = 0.2)
#'
crBivariateAMH <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter >= -1, dependencyParameter <= 1
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    pmfGeometricValues <- c(0, dependencyParameter * (1 - dependencyParameter)^(1:(numberSimulations - 1) - 1))
    set.seed(seed * 10)
    simulatedGeometrics <- sample(0:(numberSimulations - 1), numberSimulations, replace = TRUE, prob = pmfGeometricValues)

    simulatedExponentials <- t(sapply(1:nrow(simulatedUniforms), function(row) stats::qexp(simulatedUniforms[row, 1:2], simulatedGeometrics[row])))
    simulatedCopula <- sapply(1:2, function(col) (1 - dependencyParameter) / (exp(simulatedExponentials[, col]) - dependencyParameter))

    simulatedCopula
}

