#' Negative Binomial Distribution
#'
#' @description
#' Negative binomial distribution with parameters \eqn{r} (number of successful
#' trials) and \eqn{p} (probability of success).
#'
#' @details
#' When \eqn{k} is the number of failures until the \eqn{r}th success,
#' with a probability \eqn{p} of a success, the negative binomial has density:
#'   \deqn{\left(\frac{r + k - 1}{k}\right) (p)^{r} (1 - p)^{k}}{Pr(M = k) = choose(r + k - 1, k) p^r (1 - p)^k}
#' for \eqn{k \in \{0, 1, \dots \}}{k = 0, 1, 2, ...}
#'
#' When \eqn{k} is the number of trials until the \eqn{r}th success,
#' with a probability \eqn{p} of a success, the negative binomial has density:
#'   \deqn{\left(\frac{k - 1}{r - 1}\right) (p)^{r} (1 - p)^{k - r}}{Pr(M = k) = choose(k - 1, r - 1) p^r (1 - p)^(k - r)}
#' for \eqn{k \in \{r, r + 1, r + 2, \dots \}}{k = r, r + 1, r + 2, ...}
#'
#' The alternative parameterization of the negative binomial with parameter
#' \eqn{\beta}{beta}, and \eqn{k} being the number of trials, has density:
#'   \deqn{\frac{\Gamma(r + k)}{\Gamma(r) k!} \left(\frac{1}{1 + \beta}\right)^{r}%
#'   \left(\frac{\beta}{1 +  \beta}\right)^{k - r}}{Pr(M = k) = %
#'   (r + k - 1)!/((r - 1)! k!) (1/(1 + beta))^r (beta/(1 + beta))^(k - r)}
#' for \eqn{k \in \{0, 1, \dots \}}{k = 0, 1, 2, ...}
#'
#' @template size-prob-negbinom-template
#' @template beta-negbinom-template
#' @template nb_tries-template
#'
#' @return
#' Function :
#'   \itemize{
#'     \item{\code{\link{expValNBinom}}}{ gives the expected value.}
#'     \item{\code{\link{varNBinom}}}{ gives the variance.}
#'     \item{\code{\link{mgfNBinom}}}{ gives the moment generating function (MGF).}
#'     \item{\code{\link{pgfNBinom}}}{ gives the probability generating function (PGF).}
#'   }
#' Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @name NBinom
#'
NULL

# @rdname NBinom
#
# @template x-template
# @export
#
# @examples
# dNBinom(x = 2, shape = 2, scale = 4)
#
# # Where k is the number of trials for a rth success
# dNBinom(k = 3, size = 2, prob = .4)
#
# # Where k is the number of failures before a rth success
# dNBinom(k = 3, size = 2, prob = .4, nb_tries = TRUE)
#
# # By definition, k must be greater than r.
# \dontrun{
#  dNBinom(k = 1, size = 2, prob = .4, nb_tries = TRUE)
# }
#
# # With alternative parameterization where k is the number of trials
# dNBinom(k = 3, size = 2, beta = 1.5)
#
# dNBinom <- function(k, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE)
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

# @rdname NBinom
#
# @template q-template
# @template lower.tail-template
# @export
#
# @examples
# pNBinom(q = 2, shape = 2, scale = 4)
#
# # Where k is the number of trials for a rth success
# pNBinom(k = 3, size = 2, prob = .4)
#
# # Where k is the number of failures before a rth success
# pNBinom(k = 3, size = 2, prob = .4, nb_tries = TRUE)
#
# # By definition, k must be greater than r.
# \donttest{
#  pNBinom(k = 1, size = 2, prob = .4, nb_tries = TRUE)
# }
#
# # With alternative parameterization where k is the number of trials
# pNBinom(k = 3, size = 2, beta = 1.5)
#
# pNBinom <- function(k, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE, lower.tail = TRUE){
#
#     if (nb_tries) {
#         stopifnot(k > size)
#     }
#     stopifnot(size > 0, prob > 0, prob < 1)
#     start_FX <- ifelse(nb_tries, size, 0)
#     if (lower.tail) {
#         sum(sapply(start_FX:k, function(i) dNBinom(k = i, size = size, prob = prob, nb_tries = nb_tries)))
#     } else {
#         1 - sum(sapply(start_FX:k, function(i) dNBinom(k = i, size = size, prob = prob, nb_tries = nb_tries)))
#     }
# }


#' @rdname NBinom
#'
#' @export
#'
#' @examples
#' # Where k is the number of trials for a rth success
#' expValNBinom(size = 2, prob = .4)
#'
#' # Where k is the number of failures before a rth success
#' expValNBinom(size = 2, prob = .4, nb_tries = TRUE)
#'
#' # With alternative parameterization where k is the number of trials
#' expValNBinom(size = 2, beta = 1.5)
#'
expValNBinom <- function(size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE) {
    stopifnot(size > 0, prob > 0, prob < 1)

    if (nb_tries) {
        size / prob
    } else {
        size * ((1 - prob) / prob)
    }
}

#' @rdname NBinom
#'
#' @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' varNBinom(size = 2, prob = .4)
#'
#' # Where k is the number of failures before a rth success
#' varNBinom(size = 2, prob = .4, nb_tries = TRUE)
#'
#' # With alternative parameterization where k is the number of trials
#' varNBinom(size = 2, beta = 1.5)
#'
varNBinom <- function(size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE) {
    stopifnot(size > 0, prob > 0, prob < 1)

    size * ((1 - prob) / (prob^2))
}

#' @rdname NBinom
#'
#' @template t-template
#' @export
#'
#' @examples
#' mgfNBinom(t = 1, size = 4, prob = 0.5)
#'
mgfNBinom <- function(t, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE) {
    stopifnot(size > 0, prob > 0, prob < 1)

    if (nb_tries == FALSE) {
        (prob / (1 - (1 - prob) * exp(t)))^size
    } else if (nb_tries == TRUE) {
        ((prob * exp(t)) / (1 - (1 - prob) * exp(t)))^size
    }
}

#' @rdname NBinom
#'
#' @template t-template
#'
#' @export
#'
#' @examples
#' pgfNBinom(t = 5, size = 3, prob = 0.3)
#'
pgfNBinom <- function(t, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE) {
    stopifnot(size > 0, prob > 0, prob < 1)

    if (nb_tries == FALSE) {
        (prob / (1 - (1 - prob) * t))^size
    } else if (nb_tries == TRUE) {
        ((prob * t) / (1 - (1 - prob) * t))^size
    }
}

