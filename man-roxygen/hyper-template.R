#' @param N Total number of balls (white and black) in the urn. \eqn{N = n + m}{N = n + m}
#' @param m Number of white balls in the urn.
#' @param n Number of black balls in the urn. Can specify n instead of N.
#' @param k Number of balls drawn from the urn, k = 0, 1, ..., m + n.
#'
#' @details
#'  The Hypergeometric distribution for \eqn{N}{N} total items of which
#'  \eqn{m}{m} are of one type and \eqn{n}{n} of the other and from which
#'  \eqn{k}{k} items are picked
#'   has probability mass function :
#'   \deqn{Pr(X = x) = \frac{\left(\frac{m}{k}\right)\left(\frac{n}{k - x}\right)}{\left(\frac{N}{k}\right)}}{((m)C(k) (n)C(k - x)) / ((N)C(k))}
#'   for \eqn{x = 0, 1, \dots, \min(k, m)}{x = 0, 1, ..., min(k, m)}.
#'
#' @family Hypergeometric Distribution
#'
