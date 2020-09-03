#' Interactive parameter chooser (server side) for copulas
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @template copula-param-template
#' @template lang-template
#'
#' @return Server function for the parameter module of the Copula Shiny
#'   function. Should not be run directly.
#'
#' @export
#'
#' @keywords internal
#'
parametersBoxCopulas <- function(input, output, session, copula, lang) {
    unboundeddependencyParameter <- c("BivariateClayton", "BivariateFrank", "BivariateGumbel")
    twoParameterCopulas <- c("Frechet", "BivariateMO")
    ns <- session$ns

    ####    Render parameters   ####
    output$dependencyParameter1 <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("dependencyParameter1"),
            label = dplyr::case_when(
                copula %in% twoParameterCopulas ~ "alpha",
                TRUE ~ ""
            ),
            value = dplyr::case_when(
                copula %in% c("BivariateGumbel") ~ 1,
                TRUE ~ 0.50
            ),
            min = dplyr::case_when(
                copula %in% twoParameterCopulas ~ 0,
                copula %in% c("BivariateGumbel") ~ 1,
                TRUE ~ -1
            ),
            max = ifelse(copula %in% unboundeddependencyParameter, input$numberSimulatedPoints3D, 1),
            step = 0.10
        )
    })
    output$dependencyParameter2 <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("dependencyParameter2"),
            label = dplyr::case_when(
                copula %in% twoParameterCopulas ~ "beta",
                TRUE ~ ""
            ),
            0.5, min = 0, max = 1, step = 0.10
        )
    })


    ####    Create parameters  ####
    dependencyParameter1 <- shiny::reactive({
        return(input$dependencyParameter1)
    })
    dependencyParameter2 <- shiny::reactive({
        return(input$dependencyParameter2)
    })


    ####    Render translation  ####
    output$parametersTitle <- shiny::renderText({
        lang()$t("Parameters")
    })
}


#' Interactive parameter chooser (UI side) for copulas
#'
#' @param id id of module
#'
#' @return UI function for the parameter module of the Copula Shiny
#'  function. Should not be run directly.
#'
#' @importFrom shiny NS uiOutput textOutput
#' @importFrom shinydashboardPlus boxPlus
#' @export
#'
#' @keywords internal
#'
parametersBoxCopulasUI <- function(id) {
    twoParameterCopulas <- c("Frechet", "BivariateMO")
    ns <- shiny::NS(id)

    #### Parameters ####
    shinydashboardPlus::boxPlus(
        title = shiny::textOutput(ns("parametersTitle")),
        status = "info",
        background = "light-blue",
        solidHeader = TRUE,
        width = NULL,
        closable = FALSE,
        # shiny::splitLayout(
            shiny::uiOutput(ns("dependencyParameter1")),
            if (id %in% toupper(twoParameterCopulas)) {
                shiny::uiOutput(ns("dependencyParameter2"))
            }
        # )
    )
}
