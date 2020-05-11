#' Truncated mean of the Beta distribution
#'
#' @description Truncated mean of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template beta-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' Etronq_beta(d = 0.4, shape1 = 4, shape2 = 5)
#' Etronq_beta(d = 0.4, shape1 = 4, shape2 = 5, less.than.d = FALSE)
#'
Etronq_beta <- function(d, shape1, shape2, less.than.d = TRUE) {
    stopifnot(shape1 > 0, shape2 > 0, d >= 0, d <= 1)

    if (less.than.d) {
        Etronq.beta <- E_beta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2)
    } else {
        Etronq.beta <- E_beta(shape1, shape2) * stats::pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE)
    }

    return(Etronq.beta)
}


