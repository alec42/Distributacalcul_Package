#' Mean excess loss of the Beta distribution
#'
#' @description Mean excess loss of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template beta-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' Mexcess_beta(d = .3, shape1 = 4, shape2 = 5)
#'
Mexcess_beta <- function(d, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    (E_beta(shape1, shape2) *
            (
                stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE) /
                    stats::pbeta(q = d, shape1, shape2, lower.tail = FALSE)
            )
    ) - d
}


