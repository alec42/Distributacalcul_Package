#' Hypergeometric Distribution
#'
#' @description
#' Hypergeometric distribution where we have a sample of k balls from an urn
#' containing N, of which m are white and n are black.
#'
#' @details
#' The Hypergeometric distribution for \eqn{N}{N} total items of which
#' \eqn{m}{m} are of one type and \eqn{n}{n} of the other and from which
#' \eqn{k}{k} items are picked has probability mass function :
#'   \deqn{Pr(X = x) = \frac{\left(\frac{m}{k}\right)\left(\frac{n}{k - x}\right)}{\left(\frac{N}{k}\right)}}{((m)C(k) (n)C(k - x)) / ((N)C(k))}
#' for \eqn{x = 0, 1, \dots, \min(k, m)}{x = 0, 1, ..., min(k, m)}.
#'
#' @template bigN-hypergeo-template
#' @template n-hypergeo-template
#' @template k-hypergeo-template
#' @template m-hypergeo-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValErl}}}{ gives the expected value.}
#'     \item{\code{\link{varErl}}}{ gives the variance.}
#'   }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name Erl
#'
NULL

#' @rdname Erl
#'
#' @export
#'
#' @examples
#'
#' # With total balls specified
#' expValErl(N = 5, m = 2, k = 2)
#'
#' # With number of each colour of balls specified
#' expValErl(m = 2, n = 3, k = 2)
#'
expValErl <- function(N = n + m, m, n = N - m, k) {
    stopifnot(k >= 0, k %% 1 == 0, k <= min(N - m, m))

    k * (m / N)
}

#' @rdname Erl
#'
#' @export
#'
#' @examples
#'
#' # With total balls specified
#' varErl(N = 5, m = 2, k = 2)
#'
#' # With number of each colour of balls specified
#' varErl(m = 2, n = 3, k = 2)
#'
varErl <- function(N = n + m, m, n = N - m, k) {
    stopifnot(k >= 0, k %% 1 == 0, k <= min(N - m, m))

    (k * (m / N)) * ((((k - 1) * (m - 1)) / (N - 1)) + 1 - (k * (m / N)))
}
