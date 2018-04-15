#' Force de mortalité de la loi Gompertz
#' @param t durée de vie T_x
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param Tx Paramètre Tx, si on désire avoir la distribution de Tx.
#' @export
hgompertz <- function(t = 0, x = 0, beta, gam, Tx = T)
{
    if (x < 0)
        stop("X doit être supérieur à 0")
    if  (Tx)
        return(beta * exp(gam * (x+t)))
    else
        beta * exp(gam * x)

}
