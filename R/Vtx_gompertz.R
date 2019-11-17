#' Variance de Tx pour la loi Gompertz
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param floor Pour valider quelles techniques on utilise.
#' @details pour le paramètre `floor`, on peut utiliser `T` si on veut utiliser la partie entière. `F` fera usage de la fonction `integrate` de R.
#' @export
Vtx_gompertz <- function(x = 0, beta, gam, floor = T)
{
    w <- 0:300 # âge limite exagéré
    if (floor)
    {
        ## Méthode à utiliser si on prends la partie entière
        f <- function(k) k * (pgompertz(k + 1, x, beta, gam) - pgompertz(k, x, beta, gam))
        fcarre <- function(k) k^2 * (pgompertz(k + 1, x, beta, gam) - pgompertz(k, x, beta, gam))
        sum(sapply(w, fcarre)) - sum(sapply(w, f))^2
    }
    else
    {
        ## Méthode à utiliser si on veut la valeur exacte
        f <- function(t) (1 - pgompertz(t, x, beta, gam))

        fcarre <- function(t)
        {
            t^2 * beta * exp(gam*(t + x)) * exp((-beta/gam) * exp(gam * x) * (exp(gam * t) - 1))
        }

        Etx <- integrate(f, 0, max(w))$value
        Etxcarre <- integrate(fcarre, 0, max(w))$value
        Etxcarre - Etx^2
    }
}
