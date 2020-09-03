#' Bivariate Cuadras-Augé Copula
#'
#' @description
#' Computes CDF and simulations of the bivariate Cuadras-Augé copula.
#'
#' @details
#' The bivariate Cuadras-Augé copula has CDF :
#'   \deqn{C(u_{1}, u_{2}) = u_{1}u_{2}^{1 - \alpha} \times%
#'   \textbf{1}_{\{u_{1} \leq u_{2}\}} + u_{1}^{1 - \alpha}u_{2} \times%
#'   \textbf{1}_{\{u_{1} \geq u_{2}\}}}{C(u1, u2) = u1 u2^(1 - alpha) * %
#'   1_(u1 <= u2) + u1^(1 - alpha) u2 * 1_(u1 >= u2)}
#' for \eqn{u_{1}, u_{2}, \alpha \in [0, 1]}{0 <= u1, u2, alpha <= 1}.
#' It is the geometric mean of the independance and upper Fréchet bound copulas.
#'
#' @template u1u2-template
#' @template dependencyParameter-template
#' @template dots-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{cBivariateCA}}}{ returns the value of the copula.}
#'     \item{\code{\link{crBivariateCA}}}{ returns simulated values of the copula.}
#'   }
#'
#'
#' @name bivariateCA
NULL

#' @rdname bivariateCA
#'
#' @export
#'
#' @examples
#' cBivariateCA(u1 = .76, u2 = 0.4, dependencyParameter = 0.4)
#'
cBivariateCA <- Vectorize(function(u1, u2, dependencyParameter, ...) {
    stopifnot(
        u1 >= 0, u1 <= 1,
        u2 >= 0, u2 <= 1,
        dependencyParameter >= 0, dependencyParameter <= 1
    )

    min(u1 * u2^(1 - dependencyParameter), u1^(1 - dependencyParameter) * u2)
})

#' @rdname bivariateCA
#'
#' @template numberSimulations-template
#' @template seed-template
#'
#' @importFrom stats runif qexp pexp
#'
#' @export
#'
#' @examples
#' crBivariateCA(numberSimulations = 10, seed = 42, dependencyParameter = 0.2)
#'
crBivariateCA <- function(numberSimulations = 1E4, seed = 42, dependencyParameter) {
    stopifnot(
        numberSimulations > 0,
        dependencyParameter >= 0, dependencyParameter <= 1
    )

    set.seed(seed)
    simulatedUniforms <- matrix(
        stats::runif(2 * numberSimulations, 0, 1),
        nrow = numberSimulations, ncol = 2
    )
    simulatedCopula <- matrix(0, nrow = numberSimulations, ncol = 2)

    set.seed(seed*10)
    simulatedUniformsTriple <- cbind(simulatedUniforms, stats::runif(numberSimulations, 0, 1))
    simulatedExponentials <- sapply(1:3, function(col) stats::qexp(simulatedUniformsTriple[, col], c(rep(dependencyParameter, 2), 1 - dependencyParameter)[col]))
    simulatedExponentialMinimums <- sapply(1:2, function(col) apply(simulatedExponentials[, c(col, 3)], 1, min))

    stats::pexp(simulatedExponentialMinimums, 1)
}

