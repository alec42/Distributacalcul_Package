#' Binomial Distribution
#'
#' @description
#'  Binomial distribution with size \eqn{n}{n} and probability of
#'  success \eqn{p}{p}.
#'
#' @details
#'  The Binomial distribution with ... has density ....
#'
#' @family Binomial Distribution
#' @template size-prob-template
#'
#' @export
#'
#' @examples
#'
#' E_binom(size = 3, prob = 0.5)
E_binom <- function(size, prob) {
    stopifnot(
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    size * prob
}

#' @rdname E_binom
#' @export
#'
#' @examples
#'
#' V_binom(size = 3, prob = 0.5)
#'
V_binom <- function(size, prob) {
    stopifnot(
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    size * prob * (1 - prob)
}

#' @rdname E_binom
#' @export
#'
#' @importFrom stats dbinom
#'
#' @template d-template
#' @template lessthand-template
#'
#' @examples
#'
#' Etronq_binom(d = 2, size = 3, prob = 0.5)
#' # Values greater than d
#' Etronq_binom(d = 0, size = 3, prob = 0.5, less.than.d = FALSE)
#'
Etronq_binom <- function(d, size, prob, less.than.d = TRUE) {
    stopifnot(
        d >= 0, d %% 1 == 0, d <= size,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)

    if (less.than.d) {
        Etronq.binom <- sum((k * fx)[k <= d])
    } else {
        Etronq.binom <- sum((k * fx)[k > d])
    }

    return(Etronq.binom)
}


#' @rdname E_binom
#' @export
#'
#' @importFrom stats qbinom
#'
#' @template kap-template
#'
#' @note Function VaR_binom is a wrapper of the qbinom function from the
#' stats package.
#'
#' @examples
#'
#' VaR_binom(kap = 0.8, size = 5, prob = 0.2)
VaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    stats::qbinom(p = kap, size = size, prob = prob)
}

#' @rdname E_binom
#' @export
#'
#' @importFrom stats dbinom qbinom pbinom
#'
#' @template kap-template
#'
#' @examples
#'
#' TVaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
TVaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)
    vark <- stats::qbinom(p = kap, size, prob)

    (
        Etronq_binom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (stats::pbinom(q = vark, size = size, prob = prob) - kap)
    ) /
        (1 - kap)
}

#' @rdname E_binom
#' @export
#'
#' @template t-template
#'
#' @examples
#'
#' PGF_binom(t = 1, size = 3, prob = 0.5)
PGF_binom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * t + (1 - prob))^size
}

#' @rdname E_binom
#' @export
#'
#' @template t-template
#'
#' @examples
#'
#' MGF_binom(t = 1, size = 3, prob = 0.5)
#'
MGF_binom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * exp(t) + (1 - prob))^size
}

