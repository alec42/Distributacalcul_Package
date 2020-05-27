#' Fonction de densité de la loi binomiale négative
#'
#' @description Probability mass function of the negative binomial distribution
#'  with parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @templateVar k TRUE
#' @template negbinom-template
#'
#' @seealso [p_negbinom()] for the cumulative probability mass function,
#' [V_negbinom()] for the variance, and [E_negbinom()] for the mean.
#'
# @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' d_negbinom(k = 3, size = 2, prob = .4)
#'
#' # Where k is the number of failures before a rth success
#' d_negbinom(k = 3, size = 2, prob = .4, nb_tries = TRUE)
#'
#' # By definition, k must be greater than r.
#' \dontrun{
#'  d_negbinom(k = 1, size = 2, prob = .4, nb_tries = TRUE)
#' }
#'
#' # With alternative parameterization where k is the number of trials
#' d_negbinom(k = 3, size = 2, beta = 1.5)
#'
# d_negbinom <- function(k, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE)
# {
#     if (nb_tries) {
#         stopifnot(k > size)
#     }
#     stopifnot(size > 0, prob > 0, prob < 1)
#
#     if (nb_tries) {
#         choose((k - 1), (size - 1)) * prob^size * (1 - prob)^(k - size)
#     } else {
#         choose((size + k - 1), k) * prob^size * (1 - prob)^k
#     }
#
# }
