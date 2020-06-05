#' Truncated mean of the Lognormal distribution
#'
#' @description Truncated mean of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template lnorm-template
#'
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#'
#' Etronq_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
Etronq_lnorm <- function(d, meanlog, sdlog, less.than.d = TRUE) {
    stopifnot(d >= 0, sdlog > 0)

    if (less.than.d) {
        Etronq.lnorm <- E_lnorm(meanlog, sdlog) *
            stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog)
    } else {
        Etronq.lnorm <- E_lnorm(meanlog, sdlog) *
            stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog, lower.tail = FALSE)
    }

    return(Etronq.lnorm)
}



