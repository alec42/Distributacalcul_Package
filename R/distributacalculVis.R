#' Interactive distribution visualization
#'
#' @description Opens an interactive Shiny app for the selected distribution
#' and modules.
#'
#' @param law Distribution to visualize, presently one of these 2 parameter
#'  continuous distributions :
#' \itemize{
#'  \item{"norm": }{Normal distribution.}
#'  \item{"lnorm": }{Lognormal distribution.}
#'  \item{"gamma": }{Gamma distribution.}
#  \item{"exp": }{Exponential distribution.}
#'  \item{"beta": }{Beta distribution.}
#'  \item{"unif": }{Uniform distribution.}
#'  \item{"llogis": }{Log-logistic distribution.}
#'  \item{"weibull": }{Weibull distribution.}
#'  \item{"pareto": }{Pareto distribution.}
#' }
#'
#' @param mod Vector of modules to visualize, one of :
#' \itemize{
#'  \item{"all": }{Renders all boxes.}
#'  \item{"functions": }{Density and cumulative density functions}
#'  \item{"moments": }{Various moments.}
#'  \item{"riskMeasures": }{VaR and TVaR.}
#' }
#'
#' @importFrom shiny column fluidRow tags shinyApp callModule
#' @importFrom shinydashboardPlus dashboardPagePlus dashboardHeaderPlus
#' @importFrom shinydashboard dashboardSidebar dashboardBody
#' @export
#'
#' @return Launches Shiny application.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'    distributacalculVis(law = "norm", mod = "functions")
#' }
#'
distributacalculVis <- function(law, mod) {
    stopifnot(law %in% c("norm", "lnorm",
                         "gamma", "exp",
                         # "erlang",
                         "llogis", "weibull", "pareto",
                         "beta", "unif")
    )
    stopifnot(mod %in% c("all", "functions", "moments", "riskMeasures")
    )
    shiny::shinyApp(
        ui = shinydashboardPlus::dashboardPagePlus(
            header = shinydashboardPlus::dashboardHeaderPlus(title = law),
            sidebar = shinydashboard::dashboardSidebar(width = NULL, collapsed = TRUE, disable = TRUE),
            body = shinydashboard::dashboardBody(
                shiny::tags$head(
                    shiny::tags$style(
                        type = "text/css",
                        "
                        label {
                        display: table-cell;
                        text-align: center;
                        vertical-align: middle;
                        }
                        .form-group {
                        display: table-row;
                        }
                        "
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        width = 3,
                        parametersBoxUI(toupper(law))
                    ),
                    if ("functions" %in% mod | "all" %in% mod) {
                        shiny::column(
                            width = 4,
                            functionsBoxUI(toupper(law))
                        )
                    },
                    if ("moments" %in% mod | "all" %in% mod) {
                        shiny::column(
                            width = 4,
                            momentsBoxUI(toupper(law))
                        )
                    },
                    if ("riskMeasures" %in% mod | "all" %in% mod) {
                        shiny::column(
                            width = 4,
                            riskMeasuresBoxUI(toupper(law))
                        )
                    }
                )
            )
        ),
        server = function(input, output, session) {
            shiny::callModule(
                module = parametersBox,
                id = toupper(law),
                law = law
            )
            if ("functions" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = functionsBox,
                    id = toupper(law),
                    law = law
                )
            }
            if ("moments" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = momentsBox,
                    id = toupper(law),
                    law = law
                )
            }
            if ("riskMeasures" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = riskMeasuresBox,
                    id = toupper(law),
                    law = law
                )
            }
        }
    )
}
