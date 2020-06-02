#' @param size Number of trials (0 or more).
#' @param prob Probability of success on each trial.
#'
#' <%=ifelse(exists("k0"), "@param k0 point up to which to sum the distribution for the approximation.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x"), "@param x quantile.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("vark"), "@param vark Value-at-Risk (VaR) calculated at the given probability kap.", "") %>
#' <%=ifelse(exists("lower.tail"), "@param lower.tail logical; if \\code{TRUE} (default) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Binomial distribution with probability of success \eqn{p}{p} for \eqn{n}{n} trials
#'   has probability mass function :
#'   \deqn{Pr(X = k) = \left(\frac{n}{k}\right) p^n (1 - p)^{n - k}}{Pr(X = k) = n!/(k!(n - k)!) p^n(1 - p)^(n - k)}
#'   for \eqn{k = 0, 1, 2, \dots, n}{k = 0, 1, 2, ..., n}, \eqn{p \in [0, 1]}{0 <= p <= 1}, and \eqn{n > 0}{n > 0}
#'
#' @family Binomial Distribution
#'
