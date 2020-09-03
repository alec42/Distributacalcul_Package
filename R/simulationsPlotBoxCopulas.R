#' Interactive functions visualization (server side) for copulas
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @template copula-param-template
#' @template lang-template
#'
#' @return Server function for the simulations plot module of the Copula
#'   Shiny function. Should not be run directly.
#'
#' @export
#'
#' @keywords internal
#'
simulationsPlotBoxCopulas <- function(input, output, session, copula, lang) {
    twoParameterCopulas <- c("Frechet", "BivariateMO")
    ns <- session$ns

    ####    Render non-function parameters  ####
    output$seed <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("seed"),
            label = "Seed for simulation",
            value = 42
        )
    })
    output$numberSimulatedPoints <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("numberSimulatedPoints"),
            label = "Number of data points (simulations)",
            1E3, min = 1, step = 10
        )
    })
    ####    Create parameters  ####
    numberSimulatedPoints <- shiny::reactive({
        return(input$numberSimulatedPoints)
    })
    seed <- shiny::reactive({
        return(input$seed)
    })

    ####    Plot Hexagon simulations    ####
    output$plotHexagon <- plotly::renderPlotly({
        shiny::req(input$dependencyParameter1)
        if (copula %in% twoParameterCopulas) {
            shiny::req(input$dependencyParameter2)
        }
        ##  Treats cases of multiple parameters
        simulatedCopulaArgs <- dplyr::case_when(
            copula %in% twoParameterCopulas ~ list(
                copula,
                input$numberSimulatedPoints, input$seed,
                c(input$dependencyParameter1, input$dependencyParameter2)
            ),
            TRUE ~ list(
                copula,
                input$numberSimulatedPoints, input$seed,
                input$dependencyParameter1
            )
        )

        plotly::ggplotly(
            rlang::exec(
                .fn = "simulatedCopulaHexPlot",
                !!!simulatedCopulaArgs
            )
        )
    })


    ####    Render translations ####
    output$simulationsPlotTitle <- shiny::renderText({
        lang()$t("Copula Simulations")
    })
}


#' Interactive moments visualization (UI side) for copulas
#'
#' @param id id of module
#'
#' @return UI function for the moments module of the Copula Shiny
#'  function. Should not be run directly.
#'
#' @importFrom shiny NS uiOutput
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom tippy tippyOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
#' @keywords internal
#'
simulationsPlotBoxCopulasUI <- function(id) {
    ns <- shiny::NS(id)

    shinydashboardPlus::boxPlus(
        title = shiny::textOutput(ns("simulationsPlotTitle")),
        width = NULL,
        solidHeader = TRUE,
        closable = FALSE,
        status = "danger",
        plotly::plotlyOutput(ns("plotHexagon")),
        shiny::uiOutput(ns("seed")),
        shiny::uiOutput(ns("numberSimulatedPoints"))
    )
}
