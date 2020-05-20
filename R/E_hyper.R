#' Expected value of the Hypergeometric distribution
#'
#' @description Expected value of the Hypergeometric distribution where we
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
#' E_hyper(N = 5, m = 2, k = 2)
#'
#' # With number of each colour of balls specified
#' E_hyper(m = 2, n = 3, k = 2)
#'
E_hyper <- function(N = n + m, m, n = N - m, k) {
    stopifnot(k >= 0, k %% 1 == 0, k <= min(N - m, m))

    k * (m / N)
}
