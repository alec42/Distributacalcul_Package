#' fonction de survie de la loi Gompertz
#' @param t durée de vie T_x
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param Tx Paramètre Tx, si on désire avoir la distribution de Tx.
#' @export
sgompertz <- function(t, x = 0, beta, gam, Tx = T)
{
    if (Tx)
        beta <- beta * exp(gam  * x)

    exp(-(beta / gam) * (exp(gam * t) - 1))
}
