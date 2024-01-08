#' Bivariate Marshall-Olkin Copula
#'
#' @description
#' Computes CDF and simulations of the bivariate Marshall-Olkin copula.
#'
#' @details
#' The bivariate Marshall-Olkin copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = u_{1}u_{2}^{1 - \beta} \times%
#'     \textbf{1}_{\{u_{1}^{\alpha} \leq u_{2}^{\beta}\}} + %
#'     u_{1}^{1 - \alpha}u_{2} \times \textbf{1}_{\{u_{1}^{\alpha}%
#'     \geq u_{2}^{\beta}\}}}{C(u1, u2) = u1 u2^(1 - beta) * %
#'     1_(u1^alpha <= u2^beta) + u1^(1 - alpha) u2 * 1_(u1^alpha >= u2^beta}
#' for \eqn{u_{1}, u_{2}, \alpha, \beta \in [0, 1]}{0 <= u1, u2, alpha, beta <= 1}.
#' It is the geometric mean of the independance and upper Fréchet bound copulas.
#'
#' @template u1u2-template
#' @template dependencyParameter-template-two
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item \code{\link{cBivariateMO}}  returns the value of the copula.
#'     \item \code{\link{crBivariateMO}}  returns simulated values of the copula.
#'   }
#'
#'
#' @name bivariateMO
NULL

#' @rdname bivariateMO
#'
#' @export
#'
#' @examples
#' cBivariateMO(u1 = .76, u2 = 0.4, dependencyParameter = c(0.4, 0.3))
#'
cBivariateMO <- function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        length(dependencyParameter) == 2,
        dependencyParameter >= 0, dependencyParameter <= 1, sum(dependencyParameter) <= 1
    )

    min(u1 * u2^(1 - dependencyParameter[2]), u1^(1 - dependencyParameter[1]) * u2)
}

#' @rdname bivariateMO
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp pexp
#'
#' @export
#'
#' @examples
#' crBivariateMO(numberSimulations = 10, seed = 42, dependencyParameter = c(0.2, 0.5))
#'
crBivariateMO <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        length(dependencyParameter) == 2,
        dependencyParameter >= 0, dependencyParameter <= 1, sum(dependencyParameter) <= 1
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    set.seed(seed*10)
    simulatedUniformsTriple <- cbind(simulatedUniforms, stats::runif(numberSimulations, 0, 1))
    simulatedExponentials <- sapply(1:3, function(col) stats::qexp(simulatedUniformsTriple[, col], c(1, (1 - dependencyParameter[1])/dependencyParameter[1], (1 - dependencyParameter[2])/dependencyParameter[2])[col]))
    simulatedExponentialMinimums <- sapply(1:2, function(col) apply(simulatedExponentials[, c(col, 3)], 1, min))
    simulatedCopula <- sapply(1:2, function(col) stats::pexp(simulatedExponentialMinimums[, col], 1 / dependencyParameter[col]))

    simulatedCopula
}

