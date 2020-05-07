#' Value-at-Risk of the Beta distribution
#'
#' @param shape1 alpha
#' @param shape2 beta
#' @param kap kap (doit être entre 0 et 1)
#'
#' @export
#'
VaR_beta <- function(kap, shape1, shape2)
{
    qbeta(p = kap, shape1, shape2)
}


