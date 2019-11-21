#' Mixture of Gamma distributions
#'
#' @description Probability mass function, cumulative probability mass function,
#'  expected value, truncated expected value, and Tail Value-at-Risk (TVaR) of a
#'  mixture of Gamma distributions with shape parameters \eqn{\alpha_i}{alpha_i}
#'  and rate parameters \eqn{\beta_i}{beta_i} (i = 1, 2).
#'
#' @param shape vector of shape parameters \eqn{\alpha_i}{alpha_i} (i = 1, 2). Vector must be of length 2. Parameters must be positive.
#' @param rate vector of rate parameters \eqn{\beta_i}{beta_i} (i = 1, 2). Vector must be of length 2.
#' @param scale alternative parametrisation of rate parameters. \eqn{\frac{1}{\beta}}{scale = 1 / rate}.
#' @param TOL Error for the finding the upper bound limit of the summation. 1E-6 by default.
#' <%=ifelse(exists("x") && x, "@param x quantile. Doit être strictement supérieur à 0.", "") %>
#' <%=ifelse(exists("q") && q, "@param q quantile.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("kappa") && kappa, "@param kappa probability.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' @export
#'
#' @family Gamma mixture
#'
#' @examples
#' pgammaMixture(q = 15, shape = c(3, 5), rate = c(1/7, 1/4))
#' pgammaMixture(q = 15, shape = c(3, 5), scale = c(7, 4))
#' pgammaMixture(q = 15, shape = c(3, 5), rate = c(1/7, 1/4), lower.tail = FALSE)
#'
#' dgammaMixture(x = 15, shape = c(3, 5), scale = c(7, 4))
#'
#'
pgammaMixture <- function(q, shape, rate = 1 / scale, scale = 1 / rate, TOL = 1E-6, lower.tail = TRUE) {
    stopifnot(length(rate) == 2,
              length(rate) == length(shape),
              shape > 0)

    param_min <- c(shape[which.min(rate)], min(rate))
    param_max <- c(shape[-which.min(rate)], rate[-min(rate)])

    upper <- qnbinom(TOL, param_min[1], param_min[2]/param_max[2], lower.tail = FALSE)

    Fs <- rep(0, length(x))
    for (i in 0:upper) {
        Fs <- Fs + dnbinom(
            x = i,
            size = param_min[1],
            prob = param_min[2] / param_max[2]
        ) * pgamma(
            q = q,
            shape = i + param_min[1] + param_max[1],
            rate = param_max[2]
        )
    }

    if (lower.tail) {
        return(Fs)
    } else {
        return(1 - Fs)
    }
}

dgammaMixture <- function(x, shape, rate = 1 / scale, scale = 1 / rate, TOL = 1E-6) {
    stopifnot(length(rate) == 2,
              length(rate) == length(shape),
              shape > 0
              )

    param_min <- c(shape[which.min(rate)], min(rate))
    param_max <- c(shape[-which.min(rate)], rate[-min(rate)])

    upper <- qnbinom(TOL, param_min[1], param_min[2]/param_max[2], lower.tail = FALSE)

    fs <- rep(0, length(x))
    for (i in 0:upper) {
        fs <- fs + dnbinom(
            x = i,
            size = param_min[1],
            prob = param_min[2] / param_max[2]
        ) * dgamma(
            x = x,
            shape = i + param_min[1] + param_max[1],
            rate = param_max[2]
        )
    }

    return(fs)
}

library(ggplot2)

ggplot(data = data.frame(x = c(1, 30)), aes(x)) +
    stat_function(fun = dgammaMixture,
                  args = list(shape = c(8, 15), rate = c(7, 8))) +
    ylab("f(x)") +
    theme_classic() +
    stat_function(
        fun = dgammaMixture,
        args = list(shape = c(8, 15), rate = c(7, 8)),
        xlim = c(0, 15),
        geom = "area",
        fill = "red",
        alpha = 0.7
    )
