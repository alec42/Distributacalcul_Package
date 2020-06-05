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
#' E_negbinom(size = 2, prob = .4)
#'
#' # Where k is the number of failures before a rth success
#' E_negbinom(size = 2, prob = .4, nb_tries = TRUE)
#'
#' # With alternative parameterization where k is the number of trials
#' E_negbinom(size = 2, beta = 1.5)
#'
E_negbinom <- function(size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE)
{
    stopifnot(size > 0, prob > 0, prob < 1)

    if (nb_tries) {
        size / prob
    } else {
        size * ((1 - prob) / prob)
    }
}
