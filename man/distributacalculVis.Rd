% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributacalculVis.R
\name{distributacalculVis}
\alias{distributacalculVis}
\title{Interactive distribution visualization}
\usage{
distributacalculVis(law, mod)
}
\arguments{
\item{law}{Distribution to visualize, presently one of these 2 parameter
 continuous distributions :
\itemize{
 \item{"Norm": }{Normal distribution.}
 \item{"Lnorm": }{Lognormal distribution.}
 \item{"Gamma": }{Gamma distribution.}
 \item{"Beta": }{Beta distribution.}
 \item{"Unif": }{Uniform distribution.}
 \item{"Llogis": }{Log-logistic distribution.}
 \item{"Weibull": }{Weibull distribution.}
 \item{"Pareto": }{Pareto distribution.}
}}

\item{mod}{Vector of modules to visualize, one of :
\itemize{
 \item{"all": }{Renders all boxes.}
 \item{"functions": }{Density and cumulative density functions}
 \item{"moments": }{Various moments.}
 \item{"riskMeasures": }{VaR and TVaR.}
}}
}
\value{
Launches Shiny application.
}
\description{
Opens an interactive Shiny app for the selected distribution
and modules.
}
\examples{
## Only run this example in interactive R sessions
if (interactive()) {
   distributacalculVis(law = "Norm", mod = "functions")
}

}
