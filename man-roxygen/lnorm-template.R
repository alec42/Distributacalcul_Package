#' @param meanlog mean \eqn{\mu}{mu}.
#' @param sdlog standard deviation \eqn{\sigma}{sigma}
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Log-normal distribution with mean \eqn{\mu}{mu} and standard deviation
#'   \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma x}\textrm{e}^{-\frac{1}{2}\left(\frac{\ln(x) - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((ln(x) - mu)/sigma)^2) / ((2 pi)^(1/2) sigma x}
#'   for \eqn{x \in \mathcal{R}^{+}}{x >= 0}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @family Lognormal distribution
#'
