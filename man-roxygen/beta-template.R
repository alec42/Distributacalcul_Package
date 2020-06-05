#' @param shape1 shape parameter \eqn{\alpha}{alpha}, must be positive.
#' @param shape2 shape parameter \eqn{\beta}{beta}, must be positive.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value. Recall the the domain is limited between 0 and 1.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k0"), "@param k0 point up to which to sum the distribution for the approximation.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Beta distribution with shape parameters \eqn{\alpha}{a} and
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\Gamma(\alpha + \beta)}{\Gamma(\alpha) %
#'    \Gamma(\beta)} x^{\alpha - 1} (1 - x)^(\beta - 1)}{f(x) = Γ(a+b) / %
#'    (Γ(a)Γ(b))x^(a - 1)(1 - x)^(b - 1)}
#'   for \eqn{x \in [0, 1]}{0 ≤ x ≤ 1}, \eqn{\alpha, \beta > 0}{a, b > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_beta}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{E_beta}}}{ gives the expected value.}
#'  \item{\code{\link{V_beta}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_beta}}}{ gives the kth moment.}
#'  \item{\code{\link{Etronq_beta}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_beta}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_beta}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_beta}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_beta}}}{ gives the Tail Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Beta Distribution
#'
