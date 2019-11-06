#' Variance d'une loi binomiale négative
#'
#' @description Variance of the negative binomial distribution with
#'  parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @template negbinom-template
#'
#' @details The variance of the negative binomial where \eqn{p} is the
#'  probability of success and:
#'  \eqn{k} is the number of failures or the number of trials until the
#'   \eqn{r}th success is:
#'   \deqn{\text{V}(M) = \frac{r(1 - p)}{p^2}}{E(M) = r(1 - p)/(p^2)}
#'
#'  \eqn{k} is the number of trials until the \eqn{r}th success is:
#'   \deqn{\text{V}(X) = r \beta (1 + \beta)}{E(M) = r beta (1 + beta)}
#'
#'
#' @seealso [d_negbinom()] for the probability mass function, [p_negbinom()]
#'  for the cumulative probability mass function, and [E_negbinom()]
#'  for the mean.
#'
#' @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' V_negbinom(r = 2, p = .4)
#'
#' # Where k is the number of failures before a rth success
#' V_negbinom(r = 2, p = .4, nb_tries = T)
#'
#' # With alternative parameterization where k is the number of trials
#' V_negbinom(r = 2, beta = 1.5)
#'
V_negbinom <- function(r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F)
{
    r * ((1 - p) / (p^2))
}
