#' Bivariate Frank Copula
#'
#' @description
#' Computes CDF, PDF and simulations of the bivariate Frank copula.
#'
#' @details
#'  The bivariate Frank copula has CDF :
#'
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cBivariateFrank}}}{ returns the value of the copula.}
#'     \item{\code{\link{cdBivariateFrank}}}{ returns the value of the density function associated to the copula.}
#'     \item{\code{\link{crBivariateFrank}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name bivariateFrank
NULL

#' @rdname bivariateFrank
#'
#' @export
#'
#' @examples
#' cBivariateFrank(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cBivariateFrank <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter != 0
    )
    -log(1 + ((exp(-dependencyParameter * u1) - 1) * (exp(-dependencyParameter * u2) - 1)) / (exp(-dependencyParameter) - 1)) / dependencyParameter
}

#' @rdname bivariateFrank
#'
#' @export
#'
#' @examples
#' cdBivariateFrank(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cdBivariateFrank <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter != 0
    )

    dependencyParameter * exp(-dependencyParameter * (u1 + u2)) * (1 - exp(-dependencyParameter)) * (exp(-dependencyParameter * (u1 + u2)) - exp(-dependencyParameter * u1) - exp(-dependencyParameter * u2) + exp(-dependencyParameter))^(-2)
}

#' @rdname bivariateFrank
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp
#'
#' @export
#'
#' @examples
#' crBivariateFrank(numberSimulations = 10, seed = 42, dependencyParameter = 0.2)
#'
crBivariateFrank <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter != 0
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    set.seed(seed * 10)
    simulatedUniformsTriple <- cbind(simulatedUniforms, stats::runif(numberSimulations, 0, 1))
    gammaLogarithmicParameter <- 1 - exp(-dependencyParameter)
    simulatedLogarithmics <- sapply(simulatedUniformsTriple[, 3], function(kappa) VatRLogarithmic(kappa, gammaLogarithmicParameter))
    simulatedExponentials <- t(sapply(1:nrow(simulatedUniformsTriple), function(row) stats::qexp(simulatedUniformsTriple[row, 1:2], simulatedLogarithmics[row])))
    simulatedCopula <- sapply(1:2, function(col) log(1 - gammaLogarithmicParameter * exp(-simulatedExponentials[, col])) / log(1 - gammaLogarithmicParameter))

    simulatedCopula
}

