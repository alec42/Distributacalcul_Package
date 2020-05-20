#' @param min,max lower and upper limits of the distribution. Must be finite.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The (continuous) uniform distribution with min and max parameters \eqn{a}
#'   and \eqn{b} respectively has density:
#'   \deqn{f(x) = \frac{1}{b - a} \times \bm{1}_{\{x \in [a, b] \}}}{f(x) = 1 / (b - a) x 1_\{a <= x <= b\}}
#'   for \eqn{x \in [a, b]}{a <= x <= b}.
#'
#' @family Continuous Uniform Distribution
#'
