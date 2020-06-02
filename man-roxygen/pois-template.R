#' @param lambda Rate parameter \eqn{\lambda}{lambda}.
#'
#' <%=ifelse(exists("k0"), "@param k0 point up to which to sum the distribution to approximate the expected value.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("vark"), "@param vark Value-at-Risk (VaR) calculated at the given probability kap.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#' <%=ifelse(exists("lower.tail"), "@param lower.tail logical; if \\code{TRUE} (default) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#'
#' @details
#'  The Poisson distribution with rate parameter \eqn{\lambda}{lam}
#'   has probability mass function :
#'   \deqn{Pr(X = k) = \frac{\lambda^k \textrm{e}^{-\lambda}}{k!}}{Pr(X = k) = (lam^k e^(-lam)) / k!}
#'   for \eqn{k = 0, 1, 2, \dots}{k = 0, 1, 2, ...}, and \eqn{\lambda > 0}{lam > 0}
#'
#' @family Poisson Distribution
#'
