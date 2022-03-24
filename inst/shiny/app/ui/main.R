tabsetPanel(
  tabPanel(
    "Demographic Table", 
    uiOutput("tbl_demog")
  ),
  tabPanel(
    "KM-Plot for TTDE",
    plotOutput("plot_km")
  ), 
  tabPanel(
    "Primary Table",
    uiOutput("tbl_primary")
  ),
  tabPanel(
    "Efficacy Table",
    fluidPage(
      tags$br(),
      tags$br(),
      fluidRow(h4("Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 LOCF",
                  helpPopover("Table is based on participants who have observable data at Baseline and Week 20")
                  ),
               tags$br(),tags$br(),
               column(width=10,reactable::reactableOutput("tbl_efficacy_1"))),
      tags$br(),
      tags$br(),
      tags$hr(),
      fluidRow(h4("Pairwise Comparison",
                  helpPopover("Inference in this table is based on a Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates.")
                  ),
               tags$br(),tags$br(),
               column(width=10,reactable::reactableOutput("tbl_efficacy_2"))),
      tags$br(),
      tags$br(),
      tags$hr(),
      fluidRow(h6(tags$i("*Abbreviation: CI=Confidence Interval; LS=Least Squarses; SD=Standard Deviation")))
    )
  )
)
