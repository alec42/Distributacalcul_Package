#' @param shape1 shape parameter \eqn{\alpha}{alpha}, must be positive.
#' @param shape2 shape parameter \eqn{\beta}{beta}, must be positive.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value. Recall the the domain is limited between 0 and 1.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#' <%=ifelse(exists("k0"), "@param k0 point up to which to sum the distribution for the approximation.", "") %>
#'
#' @details
#'  The Beta distribution with shape parameters \eqn{\alpha}{a} and
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\Gamma(\alpha + \beta)}{\Gamma(\alpha) %
#'    \Gamma(\beta)} x^{\alpha - 1} (1 - x)^(\beta - 1)}{f(x) = Γ(a+b) / %
#'    (Γ(a)Γ(b))x^(a - 1)(1 - x)^(b - 1)}
#'   for \eqn{x \in [0, 1]}{0 ≤ x ≤ 1}, \eqn{\alpha, \beta > 0}{a, b > 0}.
#'
#' @family Beta Distribution
#'
