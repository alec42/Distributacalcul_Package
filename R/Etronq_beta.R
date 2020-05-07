#' Truncated mean of the Beta distribution
#'
#' @description Truncated mean of the Beta distribution with shape parameters
#'  \eqn{\alpha}{alpha} and \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template beta-template
#'
#' @examples
#'
#' Etronq_beta(d = 0.4, shape1 = 4, shape2 = 5)
#' Etronq_beta(d = 0.4, shape1 = 4, shape2 = 5, less.than.d = FALSE)
#'
Etronq_beta <- function(d, shape1, shape2, less.than.d = TRUE) {

    if (d < 0 | d > 1) {
        stop("d must be between 0 and 1")
    }

    if (less.than.d) {
        Etronq.beta <- E_beta(shape1, shape2) * pbeta(q = d, shape1 + 1, shape2)
    } else {
        Etronq.beta <- E_beta(shape1, shape2) * pbeta(q = d, shape1 + 1, shape2, lower.tail = FALSE)
    }

    return(Etronq.beta)
}


