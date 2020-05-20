#' Variance of the Hypergeometric distribution
#'
#' @description Variance of the Hypergeometric distribution where we
#'  have a sample of k balls from an urn containing N of which m are
#'  white and n are black.
#'
#' @template hyper-template
#'
#' @export
#'
#' @examples
#'
#' # With total balls specified
#' V_hyper(N = 5, m = 2, k = 2)
#'
#' # With number of each colour of balls specified
#' V_hyper(m = 2, n = 3, k = 2)
#'
V_hyper <- function(N = n + m, m, n = N - m, k) {
    stopifnot(k >= 0, k %% 1 == 0, k <= min(N - m, m))

    (k * (m / N)) * ((((k - 1) * (m - 1)) / (N - 1)) + 1 - (k * (m / N)))
}
