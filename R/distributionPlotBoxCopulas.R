#' Interactive functions visualization (server side) for copulas
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @template copula-param-template
#' @template lang-template
#'
#' @return Server function for the distribution plot module of the Copula
#'   Shiny function. Should not be run directly.
#'
#'
#' @export
#'
#' @keywords internal
#'
distributionPlotBoxCopulas <- function(input, output, session, copula, lang) {
    twoParameterCopulas <- c("Frechet", "BivariateMO")
    ns <- session$ns

    ####    Render non-function parameters  ####
    output$plotFunction <- shiny::renderUI({
        shinyWidgets::switchInput(
            inputId = session$ns("plotFunction"),
            label = "Copula",
            labelWidth = "50px", handleWidth = "10px",
            onLabel = "CDF", offLabel = "PDF",
            value = TRUE
        )
    })
    output$Boundaries <- shiny::renderUI({
        shinyWidgets::switchInput(
            inputId = session$ns("Boundaries"),
            label = "Fr'echet Boundaries",
            labelWidth = "50px", handleWidth = "10px",
            onLabel = "Show", offLabel = "Hide",
            value = TRUE
        )
    })
    output$showContours <- shiny::renderUI({
        shinyWidgets::switchInput(
            inputId = session$ns("showContours"),
            label = "Contours",
            labelWidth = "100px", handleWidth = "10px",
            onLabel = "Show", offLabel = "Hide",
            value = TRUE
        )
    })
    output$numberSimulatedPoints3D <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("numberSimulatedPoints3D"),
            label = "Number of data points (3D)",
            10, min = 1, step = 10
        )
    })

    ####    Create parameters  ####
    plotFunction <- shiny::reactive({
        return(input$plotFunction)
    })
    Boundaries <- shiny::reactive({
        return(input$Boundaries)
    })
    showContours <- shiny::reactive({
        return(input$showContours)
    })
    numberSimulatedPoints3D <- shiny::reactive({
        return(input$numberSimulatedPoints3D)
    })


    ####    Toggle density function  ####
    if (copula %in% c("FrechetLowerBound", "FrechetUpperBound")) {
        shinyWidgets::updateSwitchInput(session, "plotFunction", value = TRUE)
    }


    ####    Plot 3D copula  ####
    output$plot3D <- plotly::renderPlotly({
        shiny::req(input$numberSimulatedPoints3D, input$dependencyParameter1)

        if (copula %in% twoParameterCopulas) {
            shiny::req(input$dependencyParameter2)
        }
        u <- seq(0, 1, length.out = input$numberSimulatedPoints3D)

        ##  Only calculates boundary simulations if needed
        if (input$Boundaries && input$plotFunction) {
            copulaSimDataFrechetBoundaries <- list(
                probLower = sapply(u, function(u1) sapply(u, function(u2)
                    rlang::exec(paste0("c", "FrechetLowerBound"), u1, u2, input$dependencyParameter1)
                )
                ),
                probUpper = sapply(u, function(u1) sapply(u, function(u2)
                    rlang::exec(paste0("c", "FrechetUpperBound"), u1, u2, input$dependencyParameter1)
                )
                )
            )
        }

        ##  Treat case of multiple parameters
        visualisation3DCopulaArgs <- dplyr::case_when(
            copula %in% twoParameterCopulas ~ list(c(input$dependencyParameter1, input$dependencyParameter2)),
            TRUE ~ list(input$dependencyParameter1)
        )

        simulatedCopulaData <- list(
            U1 = u, U2 = u,
            prob = sapply(u, function(u1) sapply(u, function(u2) rlang::exec(
                paste0(ifelse(input$plotFunction == TRUE, "c", "cd"), copula),
                u1, u2, !!!visualisation3DCopulaArgs
            )))
        )

        plotObject <- plotly::plot_ly(
            x = simulatedCopulaData$U1, y = simulatedCopulaData$U2,
            z = simulatedCopulaData$prob,
            lighting = list(
                specular = .3 # shiny-ness
            )
        ) %>%
            plotly::add_surface(
                contours = list(
                    z = list(
                        show = input$showContours,
                        usecolormap = TRUE,
                        highlightcolour = "#ff0000",
                        project = list(z = TRUE)
                    )),
                showscale = FALSE,
                colorscale = list(c(0, 1), c(grDevices::rgb(31, 0, 171, maxColorValue = 255), grDevices::rgb(146, 171, 247, maxColorValue = 255)))
            ) %>%
            plotly::layout(
                title = paste(
                    dplyr::case_when(
                        copula == "Independant" ~ "Independant",
                        copula == "FrechetLowerBound" ~ "Fr'echet Lower Bound",
                        copula == "FrechetUpperBound" ~ "Fr'echet Upper Bound",
                        copula == "BivariateEFGM" ~ "Eyraud-Farlie-\nGumbel-Morgenstern",
                        copula == "Frechet" ~ "Fr'echet",
                        copula == "BivariateCA" ~ "Bivariate Cuadras-\nAug'e",
                        copula == "BivariateMO" ~ "Bivariate Marhsall-\nOlkin",
                        copula == "BivariateAMH" ~ "Bivariate Ali-\nMikhail-Haq",
                        copula == "BivariateClayton" ~ "Bivariate Clayton",
                        copula == "BivariateFrank" ~ "Bivariate Frank",
                        copula == "BivariateGumbel" ~ "Bivariate Gumbel",
                        TRUE ~ ""
                    ),
                    "Copula"
                ),
                scene = list(
                    xaxis = list(title = "U1"), yaxis = list(title = "U2"),
                    zaxis = list(title = shiny::textOutput(ns("CopulaLabel"))),
                    camera = list(
                        eye = list(x = -2.5, y = -.75, z = 0.25)
                    )
                )
            )
        if (input$Boundaries && input$plotFunction) {
            plotObject %>% plotly::add_surface(
                z = copulaSimDataFrechetBoundaries$probLower,
                showscale = FALSE,
                colorscale = "reds",
                opacity = 0.7
            ) %>% plotly::add_surface(
                z = copulaSimDataFrechetBoundaries$probUpper,
                showscale = FALSE,
                colorscale = "reds",
                opacity = 0.6
            )
        } else {
            plotObject
        }
    })


    ####    Render translations ####
    output$distributionPlotTitle <- shiny::renderText({
        lang()$t("Copula Distribution")
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
distributionPlotBoxCopulasUI <- function(id) {
    ns <- shiny::NS(id)

    shinydashboardPlus::boxPlus(
        title = shiny::textOutput(ns("distributionPlotTitle")),
        width = NULL,
        solidHeader = TRUE,
        closable = FALSE,
        status = "danger",
        plotly::plotlyOutput(ns("plot3D")),
        shiny::uiOutput(ns("plotFunction")),
        shiny::uiOutput(ns("numberSimulatedPoints3D")),
        shiny::uiOutput(ns("Boundaries")),
        shiny::uiOutput(ns("showContours"))
    )
}
