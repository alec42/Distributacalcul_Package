#' Expected value of the negative binomial distribution
#'
#' @description Expected value of the negative binomial distribution with
#'  parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @template negbinom-template
#'
#' @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' E_negbinom(r = 2, p = .4)
#'
#' # Where k is the number of failures before a rth success
#' E_negbinom(r = 2, p = .4, nb_tries = TRUE)
#'
#' # With alternative parameterization where k is the number of trials
#' E_negbinom(r = 2, beta = 1.5)
#'
E_negbinom <- function(r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    if (nb_tries) {
        r / p
    } else {
        r * ((1 - p) / p)
    }
}
