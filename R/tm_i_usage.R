#' ui_i_usage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput tags fluidRow column tabsetPanel tabPanel imageOutput
#' @importFrom reactable reactableOutput
ui_i_usage <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$h1("Application Guide"),
        tags$p("The Pilot 2 Shiny Application contains five distinct interfaces, each displaying a different analysis output as described in the App Information page."),
        reactableOutput(ns("pilot1_table"))
      )
    ),
    tags$br(),
    fluidRow(
      column(
        width = 12,
        tags$h2("Dynamic Filters"),
        tags$p("The", tags$b("KM Plot for TTDE"), "module allows for filters to be applied based on variables in the", tags$b("ADSL"), "and", tags$b("ADTTE"), "data sets. Below is an example of performing subpopulation analysis for an age group within the module:"),
        tags$br(),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Step 1",
            fluidRow(
              column(
                width = 4,
                imageOutput(ns("step1_image"))
              ),
              column(
                width = 8,
                tags$p("Within the", tags$b("Add Filter Variables"), "widget, click the box with the placeholder",  tags$b("Select variables to filter"))
              )
            )
          ),
          tabPanel(
            title = "Step 2",
            column(
              width = 4,
              imageOutput(ns("step2_image"))
            ),
            column(
              width = 8,
              tags$p("Scroll up/down or use the search bar to find the variable for subpopulation. Click the desired variable, ", tags$b("AGEYR1"), "in this example")
            )
          ),
          tabPanel(
            title = "Step 3",
            column(
              width = 4,
              imageOutput(ns("step3_image"))
            ),
            column(
              width = 8,
              tags$p("In the", tags$b("Active Filter Variables"), "widget, the selected variable with its available categories or levels will display,", tags$b("AGEYR1"), "in this example, is displayed with three categories. If the selected variable in the previous step is a continuous variable, then a slider will appear for selecting a range of values."),
              tags$br(),
              tags$p("Select the target subpopulation (e.g. >80) and the analysis output displayed on the left hand side will be updated in real-time according to the selection, which in this example is equivalent to performing a filter on the", tags$b("ADSL"), "data by AGEGR1 == '>80'")
            )
          )
        )
      )
    )
  )
}

#' @importFrom reactable reactable renderReactable colGroup colDef
#' @importFrom shiny renderImage
srv_i_usage <- function(input, output, session, datasets) {
  output$pilot1_table <- renderReactable({
    # contents of table
    pilot1_table <- tibble::tribble(
      ~tab, ~output,
      "Demographic Table", "Table 14-2.01 Summary of Demographic and Baseline Characteristics",
      "KM Plot for TTDE", "Figure 14-1 Time to Dermatologic Event by Treatment Group",
      "Primary Table", "Table 14-3.01 Primary Endpoint Analysis: ADAS Cog(11) - Change from Baseline to Week 24 - LOCF",
      "Efficacy Table", "Table 14-3.02 Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 - LOCF",
      "Visit Completion Table", "Not Applicable"
    )

    reactable(pilot1_table)
  })

  output$step1_image <- renderImage({
    list(
      src = app_sys("app", "www", "app_screenshot2.png"),
      alt = "Filter Screenshot 1",
      width = "85%"
    )
  }, deleteFile = FALSE)

  output$step2_image <- renderImage({
    list(
      src = app_sys("app", "www", "app_screenshot3.png"),
      alt = "Filter Screenshot 2",
      width = "90%"
    )
  }, deleteFile = FALSE)

  output$step3_image <- renderImage({
    list(
      src = app_sys("app", "www", "app_screenshot4.png"),
      alt = "Filter Screenshot 3",
      width = "90%"
    )
  }, deleteFile = FALSE)
}