#' Bivariate Eyraud-Farlie-Gumbel-Morgenstern (EFGM) Copula
#'
#' @description
#' Computes CDF, PDF and simulations of the EFGM copula.
#'
#' @details
#'  The EFGM copula has CDF :
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{cBivariateEFGM}}  returns the value of the copula.
#'     \item \code{\link{cdBivariateEFGM}}  returns the value of the density function associated to the copula.
#'     \item \code{\link{crBivariateEFGM}}  returns simulated values of the copula.
#'   }
#'
#'
#' @name bivariateEFGM
NULL

#' @rdname bivariateEFGM
#'
#' @export
#'
#' @examples
#' cBivariateEFGM(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cBivariateEFGM <- Vectorize(function(u1, u2, dependencyParameter) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= -1, dependencyParameter <= 1
    )

    u1 * u2 + dependencyParameter * u1 * u2 * (1 - u1) * (1 - u2)
})

#' @rdname bivariateEFGM
#'
#' @export
#'
#' @examples
#' cdBivariateEFGM(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cdBivariateEFGM <- function(u1, u2, dependencyParameter) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= -1, dependencyParameter <= 1
    )

    1 + dependencyParameter * (1 - 2 * u1) * (1 - 2 * u2)
}

#' @rdname bivariateEFGM
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' crBivariateEFGM(numberSimulations = 10, seed = 42, dependencyParameter = 0.2)
#'
crBivariateEFGM <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
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

    simulatedCopula[,1] <- simulatedUniforms[, 1]
    W1 <- dependencyParameter * (2 * simulatedCopula[,1] - 1) - 1
    W2 <- W1^2 + 4 * dependencyParameter * simulatedUniforms[, 2] * (2 * simulatedCopula[,1] - 1)
    simulatedCopula[,2] <- (2 * simulatedUniforms[, 2]) / (sqrt(W2) - W1)

    simulatedCopula
}

