#' Tail Value-at-Risk of the Beta distribution
#'
#' @description Tail Value-at-Risk of the Beta distribution with shape
#'  parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template beta-template
#'
#' @export
#' @importFrom stats pbeta qbeta
#'
#' @examples
#'
#' TVaR_beta(kap = .99, shape1 = 4, shape2 = 5)
#'
TVaR_beta <- function(kap, shape1, shape2) {
    stopifnot(shape1 > 0, shape2 > 0, kap >= 0, kap < 1)

    (E_beta(shape1, shape2) / (1 - kap)) *
        stats::pbeta(q = stats::qbeta(p = kap, shape1 = shape1, shape2 = shape2),
              shape1 = shape1 + 1,
              shape2 = shape2,
              lower.tail = FALSE)
}


