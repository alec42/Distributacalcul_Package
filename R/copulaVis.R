#' Interactive copula visualization
#'
#' @description Opens an interactive Shiny app for the selected copula
#' and modules.
#'
#' @template copula-param-template
#' @template modules-param-template
#'
#' @return Launches Shiny application.
#'
#' @export
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'    copulaVis(copula = "BivariateEFGM", modules = "distributionPlot")
#' }
#'
copulaVis <- function(copula, modules) {
    copulaChoices <- c(
        "FrechetLowerBound", "FrechetUpperBound", "Frechet",
        "BivariateEFGM",
        "BivariateCA", "BivariateMO",
        "BivariateAMH", "BivariateClayton", "BivariateFrank", "BivariateGumbel"
    )
    ##  Copulas with two parameters
    twoParameterCopulas <- c("Frechet", "BivariateMO")
    ##  Copulas with at least one parameter
    onePlusParameterCopulas <- c(twoParameterCopulas, "BivariateEFGM", "BivariateCA", "BivariateAMH", "BivariateClayton", "BivariateFrank", "BivariateGumbel")
    ##  Copulas with a density function
    densityFunctionsCopulas <- c("BivariateEFGM", "BivariateAMH", "BivariateClayton", "BivariateFrank", "BivariateGumbel")
    boundaryFrechetCopulas <- c("BivariateEFGM", "FrechetLowerBound",
                                "FrechetUpperBound", "Frechet", "BivariateFrank")
    unboundeddependencyParameter <- c("BivariateClayton", "BivariateFrank", "BivariateGumbel")

    stopifnot(copula %in% copulaChoices)
    stopifnot(modules %in% c("all", "distributionPlot", "simulationsPlot"))
    shiny::shinyApp(
        ui = shinydashboardPlus::dashboardPagePlus(
            header = shinydashboardPlus::dashboardHeaderPlus(
                # title = shiny::textOutput("mainTitle"),
                title = NULL,
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
                        width = 2,
                        parametersBoxCopulasUI(toupper(copula))
                    ),
                    if ("distributionPlot" %in% modules | "all" %in% modules) {
                        shiny::column(
                            width = 5,
                            distributionPlotBoxCopulasUI(toupper(copula))
                        )
                    },
                    if ("simulationsPlot" %in% modules | "all" %in% modules) {
                        shiny::column(
                            width = 5,
                            simulationsPlotBoxCopulasUI(toupper(copula))
                        )
                    }
                )
                    )
            ),
        server = function(input, output, session) {
            shiny::callModule(
                module = parametersBoxCopulas,
                id = toupper(copula),
                copula = copula,
                lang = i18n
            )
            if ("distributionPlot" %in% modules | "all" %in% modules) {
                shiny::callModule(
                    module = distributionPlotBoxCopulas,
                    id = toupper(copula),
                    copula = copula,
                    lang = i18n
                )
            }
            if ("simulationsPlot" %in% modules | "all" %in% modules) {
                shiny::callModule(
                    module = simulationsPlotBoxCopulas,
                    id = toupper(copula),
                    copula = copula,
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
                i18n()$t("Copulas")
            })
            output$riskMeasuresTitle <- shiny::renderText({
                i18n()$t("Risk measures")
            })
        }
    )
}
