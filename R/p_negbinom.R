#' Cumulative density function of the Negative Binomial distribution
#'
#' @description Cumulative probability mass function of the negative binomial
#'  distribution with parameters \eqn{r} (number of successful trials) and
#'  \eqn{p} (probability of success).
#'
#' @templateVar k TRUE
#' @templateVar kappa FALSE
#' @templateVar lower.tail TRUE
#' @template negbinom-template
#'
#' @seealso [d_negbinom()] for the probability mass function, [V_negbinom()]
#'  for the variance, and [E_negbinom()] for the mean.
#'
# @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' p_negbinom(k = 3, size = 2, prob = .4)
#'
#' # Where k is the number of failures before a rth success
#' p_negbinom(k = 3, size = 2, prob = .4, nb_tries = TRUE)
#'
#' # By definition, k must be greater than r.
#' \dontrun{
#'  p_negbinom(k = 1, size = 2, prob = .4, nb_tries = TRUE)
#' }
#'
#' # With alternative parameterization where k is the number of trials
#' p_negbinom(k = 3, size = 2, beta = 1.5)
#'
# p_negbinom <- function(k, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE, lower.tail = TRUE){
#
#     if (nb_tries) {
#         stopifnot(k > size)
#     }
#     stopifnot(size > 0, prob > 0, prob < 1)
#     start_FX <- ifelse(nb_tries, size, 0)
#     if (lower.tail) {
#         sum(sapply(start_FX:k, function(i) d_negbinom(k = i, size = size, prob = prob, nb_tries = nb_tries)))
#     } else {
#         1 - sum(sapply(start_FX:k, function(i) d_negbinom(k = i, size = size, prob = prob, nb_tries = nb_tries)))
#     }
# }
