#' @param size Number of trials (0 or more).
#' @param prob Probability of success on each trial.
#'
#' <%=ifelse(exists("k0"), "@param k0 point up to which to sum the distribution for the approximation.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x"), "@param x quantile.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("vark"), "@param vark Value-at-Risk (VaR) calculated at the given probability kap.", "") %>
#' <%=ifelse(exists("lower.tail"), "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Binomial distribution
#'
#' @family Binomial Distribution
#'
