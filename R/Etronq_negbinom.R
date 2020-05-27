#' Truncated mean of the Negative Binomial distribution.
#'
#' @description Truncated mean of the negative binomial distribution with
#'  parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @template negbinom-template
#'
# @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' Etronq_negbinom(d = 2, size = 2, prob = .4, k0 = 2E2)
#'
#' # Where k is the number of failures before a rth success
#' Etronq_negbinom(d = 2, size = 2, prob = .4, nb_tries = TRUE, k0 = 2E2)
#'
#' # With alternative parameterization where k is the number of trials
#' Etronq_negbinom(d = 2, size = 2, beta = 1.5, k0 = 2E2)
#'
