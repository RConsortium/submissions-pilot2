# Packages ---------------------------------------------------------------------

library(shiny)
library(pilot1wrappers)
#  https://github.com/RConsortium/submissions-pilot1
library(dplyr)
library(rtables)
library(ggplot2)
library(cowplot)
library(visR)
library(Tplyr)
library(huxtable)
library(emmeans)
library(r2rtf)

source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/startup.R")) 
 
adsl  <- haven::read_xpt(file.path(path$adam, "adsl.xpt"))
adsl_labels <- var_labels(adsl)
adsl <- adsl %>%
  dplyr::mutate(
    TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose")),
    AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    RACE = factor(RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
  )

adas  <- haven::read_xpt(file.path(path$adam, "adadas.xpt"))

adtte <- haven::read_xpt(file.path(path$adam, "adtte.xpt")) %>% 
  filter(PARAMCD == "TTDE", STUDYID == "CDISCPILOT01")

adlb <- haven::read_xpt(file.path(path$adam, "adlbc.xpt")) %>%
  subset(TRTPN %in% c(0, 81) & PARAMCD == "GLUC" & !is.na(AVISITN)) %>%
  mutate(TRTPN = ifelse(TRTPN == 0, 99, TRTPN)) # change treatment order for pairwise comparison
 

ui <- fluidPage(
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
      verbatimTextOutput("tbl_efficacy") # a placeholder output
    )
  )
)

server <- function(input, output, session) {
  ########################## Demographic Table ########################################
  output$tbl_demog <- renderUI({
    vars <- c("AGE", "AGEGR1", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "MMSETOT")
    adsl_demog <- adsl %>%
      dplyr::filter(
      STUDYID == "CDISCPILOT01",
      ITTFL == "Y"
    )
    
    lyt <- basic_table(title = "Protocol: CDISCPILOT01",
                       subtitles = "Population: Intent-to-Treat",
                       main_footer = paste0("Program: tlf_demographic.Rmd \n" , Sys.time())
    ) %>%
      split_cols_by("TRT01P") %>%
      add_colcounts() %>%
      analyze(vars, function(x, ...) {
        if (is.numeric(x)) {
          in_rows(
            "Mean (sd)" = c(mean(x), sd(x)),
            "Median" = median(x),
            "Min - Max" = range(x),
            .formats = c("xx.xx (xx.xx)", "xx.xx", "xx.xx - xx.xx")
          )
        } else if (is.factor(x) || is.character(x)) {
          in_rows(.list = list_wrap_x(table)(x))
        } else {
          stop("type not supproted")
        }
      },
      var_labels = adsl_labels[vars])
    
    # Table build
    tbl <- build_table(lyt, adsl_demog)
    
    rtables::as_html(tbl)
  })
  
  ##################################### KM plot for TTDE ######################################
  output$plot_km <- renderPlot({
    anl <<- adsl %>% 
      dplyr::filter(
        SAFFL == "Y",
        STUDYID == "CDISCPILOT01"
      ) %>%
      dplyr::select(STUDYID, USUBJID, TRT01A) %>%
      dplyr::inner_join(
        adtte %>% select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
        by = c("STUDYID", "USUBJID")
      ) %>%
      dplyr::mutate(
        TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose"))
      )
    surv_mod <- visR::estimate_KM(data = anl, strata = "TRT01A")
    
    # 
    # # save plot
    ggplot2::theme_set(theme_bw())
    
    KM <- visR::visr(surv_mod,
                     y_label = "Probability of event\n",
                     x_label = "Time to First Dermatologic Event (Days)",
                     y_ticks = seq(0,1,0.10)) %>%
      add_CNSR() %>%
      add_CI() 
    
    KM <- KM +
      ggplot2::geom_hline(yintercept=0.5, linetype = "dashed")
    
    KM <- KM %>%
      visR::add_risktable(group = "statlist")
    
    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        "KM plot for Time to First Dermatologic Event: Safety population\n",
        fontfamily = "sans",
        fontface = "bold",
        size=10
      )
    
    caption <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste0(Sys.time()),
        fontfamily = "sans",
        size=10
      )
    
    KM <- cowplot::plot_grid(
      title, KM, caption,
      ncol = 1,
      rel_heights = c(0.1,0.8,0.1)
    )
    KM
  })
  
  ##################################### Primary Table ################################
  output$tbl_primary <- renderUI({
    adas <- adas %>%
      filter(
        EFFFL == "Y",
        ITTFL=='Y',
        PARAMCD == 'ACTOT',
        ANL01FL == 'Y'
      )
    ## -----------------------------------------------------------------------------------------------------------------------------------
    t <- tplyr_table(adas, TRTP) %>% 
      set_pop_data(adsl) %>% 
      set_pop_treat_var(TRT01P) %>% 
      set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>% 
      set_distinct_by(USUBJID) %>% 
      set_desc_layer_formats(
        'n' = f_str('xx', n),
        'Mean (SD)' = f_str('xx.x (xx.xx)', mean, sd),
        'Median (Range)' = f_str('xx.x (xxx;xx)', median, min, max)
      ) %>% 
      add_layer(
        group_desc(AVAL, where= AVISITN ==  0, by = "Baseline")
      ) %>% 
      add_layer(
        group_desc(AVAL, where= AVISITN == 24, by = "Week 24")
      ) %>% 
      add_layer(
        group_desc(CHG,  where= AVISITN == 24, by = "Change from Baseline")
      )
    
    sum_data <- t %>% 
      build() %>% 
      nest_rowlabels() %>% 
      select(-starts_with('ord')) %>% 
      add_column_headers(
        paste0("|Placebo\\line(N=**Placebo**)| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**) ", 
               "| Xanomeline High Dose\\line(N=**Xanomeline High Dose**)"),
        header_n(t)
      )
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    model_portion <- efficacy_models(adas, 'CHG', 24)
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    final <- bind_rows(sum_data, model_portion)
    
    ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
      huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
      huxtable::set_align(1, 1:ncol(final), 'center') %>%
      huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
      huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
      huxtable::set_width(1.2) %>%
      huxtable::set_escape_contents(FALSE) %>%
      huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))
    HTML(to_html(ht))
  })
  
  ##################################### Efficacy Table ################################
  output$tbl_efficacy <- renderPrint({
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
    t12 <- emmeans(gluc_lmfit, "TRTPN")
    
    ## Merge and format data for reporting
    apr0ancova1 <- merge(t11, t12) %>%
      mutate(emmean_sd = SE * sqrt(df)) %>%
      mutate(
        Trt = c("Study Drug", "Placebo"),
        N1 = N,
        Mean1 = pilot1wrappers::fmt_est(mean_bl, sd_bl),
        N2 = N,
        Mean2 = pilot1wrappers::fmt_est(mean, sd),
        N3 = N,
        Mean3 = pilot1wrappers::fmt_est(mean_chg, sd_chg),
        CI = pilot1wrappers::fmt_ci(emmean, lower.CL, upper.CL)
      ) %>%
      select(Trt:CI)
    
    apr0ancova1
    
    
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
        mean = pilot1wrappers::fmt_ci(estimate, lower, upper),
        p = pilot1wrappers::fmt_pval(p.value)
      ) %>%
      select(comp:p)
    
    apr0ancova2
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    ### Calculate root mean square and save data in output folder
    apr0ancova3 <- data.frame(rmse = paste0(
      "Root Mean Squared Error of Change = ",
      formatC(sd(gluc_lmfit$residuals), digits = 2, format = "f", flag = "0")
    ))
    
    apr0ancova3
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    tbl_1 <- apr0ancova1 %>%
      rtf_title(
        title = "ANCOVA of Change from Baseline at Week 20",
        subtitle = c(
          "LOCF",
          "ITT Population"
        )
      ) %>%
      rtf_colheader(
        colheader = " | Baseline | Week 20 | Change from Baseline",
        col_rel_width = c(3, 4, 4, 9)
      ) %>%
      rtf_colheader(
        colheader = "Treatment | N | Mean (SD) | N | Mean (SD) | N | Mean (SD) | LS Mean (95% CI){^a}",
        col_rel_width = c(3, 1, 3, 1, 3, 1, 3, 5)
      ) %>%
      rtf_body(
        col_rel_width = c(3, 1, 3, 1, 3, 1, 3, 5),
        text_justification = c("l", rep("c", 7)),
        last_row = FALSE
      ) %>%
      rtf_footnote(
        footnote = c(
          "{^a} Based on an ANCOVA model.",
          "ANCOVA = Analysis of Covariance, CI = Confidence Interval, LS = Least Squares, SD = Standard Deviation"
        )
      ) %>%
      rtf_source(
        source = "Source: [study999: adam-adlbc]",
        text_justification = "c"
      )
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    tbl_2 <- apr0ancova2 %>%
      rtf_colheader(
        colheader = "Pairwise Comparison | Difference in LS Mean (95% CI){^a} | p-Value",
        text_justification = c("l", "c", "c"),
        col_rel_width = c(8, 7, 5)
      ) %>%
      rtf_body(
        col_rel_width = c(8, 7, 5),
        text_justification = c("l", "c", "c"),
        last_row = FALSE
      )
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    tbl_3 <- apr0ancova3 %>%
      rtf_body(
        as_colheader = FALSE,
        text_justification = "l"
      )
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    list(tbl_1, tbl_2, tbl_3) 
  })
}
 
shinyApp(ui, server)
