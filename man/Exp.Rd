% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp.R
\name{Exp}
\alias{Exp}
\alias{expValExp}
\alias{varExp}
\alias{kthMomentExp}
\alias{expValLimExp}
\alias{expValTruncExp}
\alias{stopLossExp}
\alias{meanExcessExp}
\alias{VatRExp}
\alias{TVatRExp}
\alias{mgfExp}
\title{Exponential Distribution}
\usage{
expValExp(rate = 1/scale, scale = 1/rate)

varExp(rate = 1/scale, scale = 1/rate)

kthMomentExp(k, rate = 1/scale, scale = 1/rate)

expValLimExp(d, rate = 1/scale, scale = 1/rate)

expValTruncExp(d, rate = 1/scale, scale = 1/rate, less.than.d = TRUE)

stopLossExp(d, rate = 1/scale, scale = 1/rate)

meanExcessExp(d, rate = 1/scale, scale = 1/rate)

VatRExp(kap, rate = 1/scale, scale = 1/rate)

TVatRExp(kap, rate = 1/scale, scale = 1/rate)

mgfExp(t, rate = 1/scale, scale = 1/rate)
}
\arguments{
\item{rate}{rate parameter \eqn{\beta}{beta}, must be positive.}

\item{scale}{alternative parameterization to the rate parameter, scale = 1 / rate.}

\item{k}{kth-moment.}

\item{d}{cut-off value.}

\item{less.than.d}{logical; if \code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.}

\item{kap}{probability.}

\item{t}{t.}
}
\value{
Function :
  \itemize{
    \item \code{\link{expValExp}}  gives the expected value.
    \item \code{\link{varExp}}  gives the variance.
    \item \code{\link{kthMomentExp}}  gives the kth moment.
    \item \code{\link{expValLimExp}}  gives the limited mean.
    \item \code{\link{expValTruncExp}}  gives the truncated mean.
    \item \code{\link{stopLossExp}}  gives the stop-loss.
    \item \code{\link{meanExcessExp}}  gives the mean excess loss.
    \item \code{\link{VatRExp}}  gives the Value-at-Risk.
    \item \code{\link{TVatRExp}}  gives the Tail Value-at-Risk.
    \item \code{\link{mgfExp}}  gives the moment generating function (MGF).
  }
Invalid parameter values will return an error detailing which parameter is problematic.
}
\description{
Exponential distribution with rate parameter \eqn{\beta}{beta}.
}
\details{
The Exponential distribution with rate parameter \eqn{\beta}{b} has density:
  \deqn{f\left(x\right) = \frac{1}{\beta}\textrm{e}^{-\beta x}}{f(x) = b^a e^{-b x}}
for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}.
}
\note{
Function VatRExp is a wrapper of the \code{\link[stats]{qexp}}
function from the stats package.
}
\examples{
# With scale parameter
expValExp(scale = 4)

# With rate parameter
expValExp(rate = 0.25)

# With scale parameter
varExp(scale = 4)

# With rate parameter
varExp(rate = 0.25)

# With scale parameter
kthMomentExp(k = 2, scale = 4)

# With rate parameter
kthMomentExp(k = 2, rate = 0.25)

# With scale parameter
expValLimExp(d = 2, scale = 4)

# With rate parameter
expValLimExp(d = 2, rate = 0.25)

# With scale parameter
expValTruncExp(d = 2, scale = 4)

# With rate parameter, values greater than d
expValTruncExp(d = 2, rate = 0.25, less.than.d = FALSE)

# With scale parameter
stopLossExp(d = 2, scale = 4)

# With rate parameter
stopLossExp(d = 2, rate = 0.25)

# With scale parameter
meanExcessExp(d = 2, scale = 4)

# With rate parameter
meanExcessExp(d = 5, rate = 0.25)

# With scale parameter
VatRExp(kap = .99, scale = 4)

# With rate parameter
VatRExp(kap = .99, rate = 0.25)

# With scale parameter
TVatRExp(kap = .99, scale = 4)

# With rate parameter
TVatRExp(kap = .99, rate = 0.25)

mgfExp(t = 1, rate = 5)

}
