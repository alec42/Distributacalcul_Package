#' Interactive distribution visualization
#'
#' @description Opens an interactive Shiny app for the selected distribution
#' and modules.
#'
#' @param law Distribution to visualize, presently one of these 2 parameter
#'  continuous distributions :
#' \itemize{
#'  \item{"Norm": }{Normal distribution.}
#'  \item{"Lnorm": }{Lognormal distribution.}
#'  \item{"Gamma": }{Gamma distribution.}
#  \item{"Exp": }{Exponential distribution.}
#'  \item{"Beta": }{Beta distribution.}
#'  \item{"Unif": }{Uniform distribution.}
#'  \item{"Llogis": }{Log-logistic distribution.}
#'  \item{"Weibull": }{Weibull distribution.}
#'  \item{"Pareto": }{Pareto distribution.}
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
#' @importFrom shiny.i18n Translator
#' @export
#'
#' @return Launches Shiny application.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'    distributacalculVis(law = "Norm", mod = "functions")
#' }
#'
distributacalculVis <- function(law, mod) {
    stopifnot(law %in% c("Norm", "Lnorm",
                         "Gamma", "Exp",
                         # "Erlang",
                         "Llogis", "Weibull", "Pareto",
                         "Beta", "Unif")
    )
    stopifnot(mod %in% c("all", "functions", "moments", "riskMeasures")
    )
    shiny::shinyApp(
        ui = shinydashboardPlus::dashboardPagePlus(
            header = shinydashboardPlus::dashboardHeaderPlus(
                # title = law
                title = shiny::textOutput("mainTitle"),
                .list = list(
                    shiny::tags$li(
                        class = "dropdown",
                        shiny::uiOutput("languageSelectorUI")
                    )
                )
            ),
            sidebar = shinydashboard::dashboardSidebar(width = NULL, collapsed = TRUE, disable = TRUE),
            body = shinydashboard::dashboardBody(
                shiny::tags$head(
                    shiny::tags$style(type = "text/css",
                                      "label {
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
                law = law,
                lang = i18n
            )
            if ("functions" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = functionsBox,
                    id = toupper(law),
                    law = law,
                    lang = i18n
                )
            }
            if ("moments" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = momentsBox,
                    id = toupper(law),
                    law = law,
                    lang = i18n
                )
            }
            if ("riskMeasures" %in% mod | "all" %in% mod) {
                shiny::callModule(
                    module = riskMeasuresBox,
                    id = toupper(law),
                    law = law,
                    lang = i18n
                )
            }

            ####  Translations  ####
            translator <- shiny.i18n::Translator$new(
                translation_json_path = "man-roxygen/translations/translation.json"
            )
            i18n <- shiny::reactive({
                selected <- input$selectedLanguage
                if (length(selected) > 0 && selected %in% translator$languages) {
                    translator$set_translation_language(selected)
                }
                translator
            })
            output$languageSelectorUI <- shiny::renderUI({
                shiny::selectInput(
                    inputId = 'selectedLanguage',
                    label = "",
                    choices = c(
                        "English" = "en",
                        "Francais" = "fr"
                    ),
                    selected = input$selectedLanguage
                )
            })
            output$mainTitle <- shiny::renderText({
                i18n()$t("Probability Distributions")
            })
            output$riskMeasuresTitle <- shiny::renderText({
                i18n()$t("Risk measures")
            })
        }
    )
}
