#' espérance de Tx pour la loi Gompertz
#' @param x âge ateint
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @param floor Pour valider quelles techniques on utilise.
#' @details pour le paramètre `floor`, on peut utiliser `T` si on veut utiliser la partie entière. `F` fera usage de la fonction `integrate` de R.
#' @export
Etx_gompertz <- function(x = 0, beta, gam, floor = T)
{
    w <- 0:300 # âge limite exagéré
    if (floor)
    {
        ## Méthode à utiliser si on prends la partie entière
        f <- function(k) k * (pgompertz(k + 1, x, beta, gam) - pgompertz(k, x, beta, gam))
        sum(sapply(w, f))
    }
    else
    {
        ## Méthode à utiliser si on veut la valeur exacte
        require(pracma)
        f <- function(t) (1 - pgompertz(t, x, beta, gam))
        # on utilise la propriété de la fonction de survie.
        quad(f, 0, max(w))
    }
}
