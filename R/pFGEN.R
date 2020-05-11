#' Fonction de répartition de la F-Généralisé
#' @param x x
#' @param lambda lambda, > 0
#' @param alpha alpha, >0
#' @param tau tau, >0
#' @export
#' @importFrom stats pbeta
pFGEN <- function(x, lambda, alpha, tau)
{
    stats::pbeta(q = (x / (lambda + x)),
          shape1 = tau,
          shape2 = alpha)
}
