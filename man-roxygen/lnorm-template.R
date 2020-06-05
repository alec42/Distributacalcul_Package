#' @param meanlog location parameter \eqn{\mu}{mu}.
#' @param sdlog standard deviation \eqn{\sigma}{sigma}, must be positive.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Log-normal distribution with mean \eqn{\mu}{mu} and standard deviation
#'   \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma x}\textrm{e}^{-\frac{1}{2}\left(\frac{\ln(x) - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((ln(x) - mu)/sigma)^2) / ((2 pi)^(1/2) sigma x}
#'   for \eqn{x \in \mathcal{R}^{+}}{x >= 0}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{E_lnorm}}}{ gives the expected value.}
#'  \item{\code{\link{V_lnorm}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_lnorm}}}{ gives the kth moment.}
#'  \item{\code{\link{Etronq_lnorm}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_lnorm}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_lnorm}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_lnorm}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_lnorm}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_lnorm}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Lognormal distribution
#'
