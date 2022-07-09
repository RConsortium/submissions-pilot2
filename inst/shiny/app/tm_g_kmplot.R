ui_g_kmplot <- function(id, datasets) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}
srv_g_kmplot <- function(input, output, session, datasets) {
  output$plot <- renderPlot({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    anl <<- adsl %>% 
      dplyr::filter(
        SAFFL == "Y",
        STUDYID == "CDISCPILOT01"
      ) %>%
      dplyr::select(STUDYID, USUBJID, TRT01A) %>%
      dplyr::inner_join(
        filter(
          adtte, STUDYID == "CDISCPILOT01"
        ) %>% select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
        by = c("STUDYID", "USUBJID")
      ) %>%
      dplyr::mutate(
        TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose")),
        AVAL = AVAL/30.4167
      )
    
     
    ## -----------------------------------------------------------------------------------------------------------------------------------
    # estimate survival
    surv_mod <- visR::estimate_KM(data = anl, strata = "TRT01A")
     
    # 
    # # save plot
    ggplot2::theme_set(theme_bw())

    KM <- visR::visr(surv_mod,
                     y_label = "Survival Probability (%)",
                     x_label = "Time (Months)", 
                     fun = "pct",
                     legend_position = "bottom" ) %>%
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
        paste(
          "The shaded areas are 95% CI of the survival probability for each group",
          "\n",
          paste0(Sys.time())
        ),
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
}