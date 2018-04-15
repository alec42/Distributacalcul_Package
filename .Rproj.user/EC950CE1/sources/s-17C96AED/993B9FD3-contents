#' fonction quantile de la loi Gompertz
#' @param q quantile (va de 0 à 1)
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param Tx Paramètre Tx, si on désire avoir la distribution de Tx.
#' @export
qgompertz <- function(q, x = 0, beta, gam, Tx = T)
{
    if (Tx)
        beta <- beta * exp(gam * x)

    log(1 - (gam / beta) * log(1 - q)) / gam
}
