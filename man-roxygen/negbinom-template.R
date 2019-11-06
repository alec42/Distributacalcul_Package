#' @param r Number of successful trials.
#' @param p Probability of success.
#' @param beta Alternative parameterization of the negative binomial
#'  distribution where beta = (1 - p) / p.
#' @param nb_tries logical; if \code{FALSE} (default) number of trials
#'  until the \code{r}th success, otherwise, number of failures until
#'  the \code{r}th success.
#'
#' <%=ifelse(exists("k") && k, "@param k quantile.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are \\eqn{\\text{Pr}(M \\leq k)}{Pr(M <= k)}, otherwise, \\eqn{\\text{Pr}(M > k)}{Pr(M > k)}.", "") %>
#'
#' @details
#'  When \eqn{k} is the number of failures until the \eqn{r}th success,
#'   with a probability \eqn{p} of a success, the negative binomial has density:
#'   \deqn{\binom{r + k - 1}{k} (p)^{r} (1 - p)^{k}}{%
#'    Pr(M = k) = choose(r + k - 1, k) p^r (1 - p)^k}
#'   for \eqn{k \in \{0, 1, \dots \}}{k = 0, 1, 2, ...}
#'
#'  When \eqn{k} is the number of trials until the \eqn{r}th success,
#'   with a probability \eqn{p} of a success, the negative binomial has density:
#'   \deqn{\binom{k - 1}{r - 1} (p)^{r} (1 - p)^{k - r}}{%
#'    Pr(M = k) = choose(k - 1, r - 1) p^r (1 - p)^(k - r)}
#'   for \eqn{k \in \{r, r + 1, r + 2, \dots \}}{k = r, r + 1, r + 2, ...}
#'
#'  The alternative parameterization of the negative binomial with parameter
#'   \eqn{\beta}{beta}, and \eqn{k} being the number of trials, has density:
#'   \deqn{\frac{\Gamma(r + k)}{\Gamma(r) k!} \left(\frac{1}{1 + \beta}\right)^{r}%
#'    \left(\frac{\beta}{1 +  \beta}\right)^{k - r}}{Pr(M = k) = %
#'    \Gamma(r + k)/(\Gamma(r) k!) (1/(1 + beta))^r (beta/(1 + beta))^(k - r)}
#'   for \eqn{k \in \{0, 1, \dots \}}{k = 0, 1, 2, ...}
#'
#' @family Negative Binomial Distribution
#'
