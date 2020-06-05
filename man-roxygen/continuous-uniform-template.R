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
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{E_unif}}}{ gives the expected value.}
#'  \item{\code{\link{V_unif}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_unif}}}{ gives the kth moment.}
#'  \item{\code{\link{Etronq_unif}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_unif}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_unif}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_unif}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_unif}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_unif}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Continuous Uniform Distribution
#'
