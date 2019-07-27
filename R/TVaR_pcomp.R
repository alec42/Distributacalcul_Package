#' Tail Value-at-risk d'une loi Poisson Composée
#' @param k Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance k
#' @param lamb lambda
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour la somme dans le calcul de la TVaR
#' @param distr_severity choix de distribution de sévérité. Gamma ou Lognormale
#' @details Cette formule nécessite la formule de la VaR_pcomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_PComp <- function(k, lamb, ko = 300, shape, rate, vark, distr_severity = "Gamma")
{
    if (vark == 0)
    {
        E_PCOMP(rate = rate, shape = shape, lambda = lamb, distr_severity = distr_severity) / (1 - k)
    }
    else if (distr_severity == "Gamma")
    {
        sum(sapply(1:ko, function(k) dpois(x = k, lambda = lamb) * (shape * k )/rate * pgamma(q = vark, shape = shape * k + 1, rate = rate, lower.tail = F)))/(1 - k)
    }
    else
    {
        sum(sapply(1:ko, function(k) dpois(x = k, lambda = lamb) * (shape * k )/rate * plnorm(q = vark, meanlog = shape * k + 1, sdlog = sqrt(rate), lower.tail = F)))/(1 - k)
    }

}
