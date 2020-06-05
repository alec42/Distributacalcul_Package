#' @param mean mean (location) parameter \eqn{\mu}{mu}.
#' @param sd standard deviation \eqn{\sigma}{sigma}, must be positive.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Normal distribution with mean \eqn{\mu}{mu} and standard deviation
#'   \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma}\textrm{e}^{-\frac{1}{2}\left(\frac{x - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((x - mu)/sigma)^2) / ((2 pi)^(1/2) sigma}
#'   for \eqn{x \in \mathcal{R}}{x real}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_norm}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{E_norm}}}{ gives the expected value.}
#'  \item{\code{\link{V_norm}}}{ gives the variance.}
#'  \item{\code{\link{Etronq_norm}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_norm}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_norm}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_norm}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_norm}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_norm}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Normal distribution
#'
