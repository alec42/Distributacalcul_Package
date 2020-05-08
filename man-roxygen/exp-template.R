#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Exponential distribution with rate parameter \eqn{\beta}{b} has %
#'   density: \deqn{f\left(x\right) = \frac{1}{\beta}%
#'   \textrm{e}^{-\beta x}}{f(x) = b^a e^{-b x}} for %
#'   \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}.
#'
#' @family Exponential Distribution
#'
