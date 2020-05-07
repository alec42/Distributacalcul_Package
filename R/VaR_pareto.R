#' Value-at-risk of the Pareto distribution
#' @param shape alpha
#' @param rate lambda
#' @param kap kap
#'
#' @export
#'
VaR_pareto <- function(kap, shape, rate)
{
    rate * ((1 - kap)^(-1 / shape) - 1)
}
