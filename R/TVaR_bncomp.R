#' Tail Value-at-Risk d'une loi binomiale négative composée
#'
#' @details Cette formule nécessite la formule de la VaR_bncomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#'
#' @param kap Niveau de confiance désiré
#' @param size r pour la binomiale négative
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choice of severity distribution Gamma (default)ss
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @param ko borne pour la somme dans le calcul de la TVaR.
#'
#' @export
#'
TVaR_BNCOMP <- function(kap, vark, size, prob, shape, rate, distr_severity = "Gamma", ko) {
    warning("This funciton is not ready for usage")
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (vark == 0) {
        TVaR.BNCOMP <- E_BNCOMP(size, prob, shape, rate, distr_severity) / (1 - kap)
    } else if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        TVaR.BNCOMP <- sum(sapply(1:ko, function(i)
            stats::dnbinom(x = i, size = size, prob = prob) *
                E_gamma(shape, rate) *
                stats::pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))
            ) / (1 - kap)
    } else {
        stop("Please enter a valid distribution choice. 'Gamma'")
    }

    return(TVaR.BNCOMP)

}
