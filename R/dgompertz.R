#' fonction de densité de la loi Gompertz
#' @param t durée de vie T_x
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param Tx Paramètre Tx, si on désire avoir la distribution de Tx.
#' @export
dgompertz <- function(t, x = 0, beta, gam, Tx = T)
{
    if (Tx)
        hgompertz(t, x, beta, gam) * (1 - pgompertz(t, x, beta, gam))
    else
        hgompertz(x, beta, gam, Tx = F) * (1 - pgompertz(t, x, beta, gam, Tx = F))
}
