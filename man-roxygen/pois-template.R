#' @param lambda Rate parameter \eqn{\lambda}{lambda}.
#'
#' <%=ifelse(exists("k0") && k0, "@param k0 point up to which to sum the distribution to approximate the expected value.", "") %>
#' <%=ifelse(exists("k") && k, "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x") && x, "@param x quantile.", "") %>
#' <%=ifelse(exists("q") && q, "@param q quantile.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kappa") && kappa, "@param kappa probability.", "") %>
#' <%=ifelse(exists("vark") && vark, "@param vark Value-at-Risk (VaR) calculated at the given probability kappa.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("less.than.d") && less.than.d, "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'
#' @family Poisson Distribution
#'
