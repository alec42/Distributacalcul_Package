#' Fonction de densité de la loi binomiale négative
#'
#' @description Probability mass function of the negative binomial distribution
#'  with parameters \code{r} (number of successful trials) and \code{p}
#'  (probability of success).
#'
#' @template negbinom-template-lower-tail
#' @template negbinom-template
#'
#' @seealso [p_negbinom()] for the cumulative probability mass function,
#' [V_negbinom()] for the variance, and [E_negbinom()] for the mean.
#'
#' @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' d_negbinom(k = 3, r = 2, p = .4)
#'
#' # Where k is the number of failures before a rth success
#' d_negbinom(k = 3, r = 2, p = .4, nb_tries = T)
#'
#' # By definition, k must be greater than r.
#' \dontrun{
#'  d_negbinom(k = 1, r = 2, p = .4, nb_tries = T)
#' }
#'
#' # With alternative parameterization where k is the number of trials
#' d_negbinom(k = 3, r = 2, beta = 1.5)
#'
d_negbinom <- function(k, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    if (nb_tries) {
        stopifnot(k > r)
        choose((k - 1), (r - 1)) * p^r * (1 - p)^(k - r)
    } else {
        choose((r + k - 1), k) * p^r * (1 - p)^k
    }

}
