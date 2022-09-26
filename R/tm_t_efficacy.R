#' ui_t_efficacy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow h4 h6 column fluidPage
#' @importFrom graphics pairs 
#' @importFrom stats lm sd
#' @import tippy
ui_t_efficacy <- function(id, datasets) {
  ns <- NS(id) 
  fluidPage(
    tags$br(),
    tags$br(),
    fluidRow(
      tippy::tippy(
        h4("Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 LOCF"),
        tooltip = tooltip_text("Table is based on participants who have observable data at Baseline and Week 20", 16),
        #tooltip = "<span style='font-size:18px;'>Table is based on participants who have observable data at Baseline and Week 20<span>",
        allowHTML = TRUE
      ),
      tags$br(),tags$br(),
      column(
        width=10,
        reactable::reactableOutput(ns("tbl_efficacy_1"))
      ) 
    ),
    
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      tippy::tippy(
        h4("Pairwise Comparison"),
        tooltip = tooltip_text("Inference in this table is based on a Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates.", 16),
        allowHTML = TRUE
      ),
      tags$br(),
      tags$br(),
      column(
        width=10,
        reactable::reactableOutput(ns("tbl_efficacy_2"))
      )
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      h6(tags$i("Abbreviations: CI=Confidence Interval; LS=Least Squarses; SD=Standard Deviation"))
    )
  )
}

#' srv_t_efficacy Server Functions
#'
#' @noRd
#' @importFrom shiny reactive 
#' @importFrom reactable reactable renderReactable colGroup colDef
#' @importFrom dplyr filter group_by summarise mutate select n
srv_t_efficacy <- function(input, output, session, datasets) {
  efficacy_results <- reactive({
    adlb <- datasets$get_data("ADLB", filtered = TRUE)
    gluc_lmfit <- adlb %>%
      filter(AVISITN == 20) %>%
      lm(CHG ~ BASE + TRTPN, data = .)
    
    ## Raw summary statistics
    t11 <- adlb %>%
      filter(AVISITN == 20) %>%
      group_by(TRTPN, TRTP) %>%
      summarise(
        N = n(),
        mean_bl = mean(BASE),
        sd_bl = sd(BASE),
        mean_chg = mean(CHG),
        sd_chg = sd(CHG),
        mean = mean(AVAL),
        sd = sd(AVAL)
      )
    
    ## Calculate LS mean
    t12 <- emmeans::emmeans(gluc_lmfit, "TRTPN")
    
    ## Merge and format data for reporting
    apr0ancova1 <- merge(t11, t12) %>%
      mutate(emmean_sd = SE * sqrt(df)) %>%
      mutate(
        Trt = c("Study Drug", "Placebo"),
        N1 = N,
        Mean1 = fmt_est(mean_bl, sd_bl),
        N2 = N,
        Mean2 = fmt_est(mean, sd),
        N3 = N,
        Mean3 = fmt_est(mean_chg, sd_chg),
        CI = fmt_ci(emmean, lower.CL, upper.CL)
      ) %>%
      select(Trt:CI)
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    t2 <- data.frame(pairs(t12))
    
    ## Treatment Comparison
    apr0ancova2 <- t2 %>%
      mutate(
        lower = estimate - 1.96 * SE,
        upper = estimate + 1.96 * SE
      ) %>%
      mutate(
        comp = "Study Drug vs. Placebo",
        mean = fmt_ci(estimate, lower, upper),
        p = fmt_pval(p.value)
      ) %>%
      select(comp:p)
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    ### Calculate root mean square and save data in output folder
    apr0ancova3 <- data.frame(rmse = paste0(
      "Root Mean Squared Error of Change = ",
      formatC(sd(gluc_lmfit$residuals), digits = 2, format = "f", flag = "0")
    ))
    list(
      apr0ancova1 = apr0ancova1,
      apr0ancova2 = apr0ancova2,
      apr0ancova3 = apr0ancova3
    )
  })
  output$tbl_efficacy_1 <- reactable::renderReactable({
    efficacy_results <- efficacy_results()
    apr0ancova1 <- efficacy_results$apr0ancova1
    coln =c("Treatment",
            "N","Mean (SD)",
            "N","Mean (SD)",
            "N","Mean (SD)","LS Mean (95% CI)")
    colgr=c(1,2,2,3,3,4,4,4)
    colwidths <- c(rep(100, 7), 150)
    colgrn=c("","Baseline","Week 20","Change from Baseline")
    collist = purrr::map2(1:ncol(apr0ancova1), colwidths, ~{
      colDef(name = coln[.x], minWidth = .y)
    })
    names(collist) = names(apr0ancova1)
    reactable(
      apr0ancova1,
      columns = collist,
      columnGroups = list(
        colGroup(name = colgrn[2], columns = names(apr0ancova1)[colgr==2]),
        colGroup(name = colgrn[3], columns = names(apr0ancova1)[colgr==3]),
        colGroup(name = colgrn[4], columns = names(apr0ancova1)[colgr==4])
      )
    )
  })
  output$tbl_efficacy_2 <- reactable::renderReactable({
    efficacy_results <- efficacy_results()
    apr0ancova2 <- efficacy_results$apr0ancova2
    apr0ancova3 <- efficacy_results$apr0ancova3
    coln =c("",
            "Difference in LS Mean (95% CI)",
            "p-Value")
    collist = lapply(1:ncol(apr0ancova2),function(xx){
      if(xx>1){colDef(name=coln[xx])
      }else{colDef(name=coln[xx],footer=apr0ancova3$rmse)}
    })
    names(collist) = names(apr0ancova2)
    
    reactable(
      apr0ancova2,
      columns = collist,
      defaultColDef = colDef(footerStyle = list(fontStyle = "itatlic"))
    )
  })
}