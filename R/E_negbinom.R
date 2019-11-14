#' Expected value of the negative binomial distribution
#'
#' @description Expected value of the negative binomial distribution with
#'  parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @details The expected value of the negative binomial where \eqn{p} is the
#'  probability of success and:
#'  \eqn{k} is the number of failures until the \eqn{r}th success is:
#'   \deqn{\textrm{E}(M) = \frac{r(1 - p)}{p}}{E(M) = r(1 - p)/p}
#'
#'  \eqn{k} is the number of trials until the \eqn{r}th success is:
#'   \deqn{\textrm{E}(M) = \frac{r}{p}}{E(M) = r/p}
#'
#'  \eqn{k} is the number of trials until the \eqn{r}th success is:
#'   \deqn{\textrm{E}(X) = r \beta}{E(M) = r beta}#'
#'
#' @template negbinom-template
#'
#' @seealso [d_negbinom()] for the probability mass function, [p_negbinom()]
#'  for the cumulative probability mass function, and [V_negbinom()]
#'  for the variance.
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
