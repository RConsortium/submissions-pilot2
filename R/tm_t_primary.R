#' ui_t_primary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput
ui_t_primary <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    h4("Table 14-3.01"),
    h4("Primary Endpoint Analysis: ADAS Cog(11) - Change from Baseline to Week 24 - LOCF"),
    uiOutput(ns("table")),
    p("Statistical model and comparison p-values removed when applying data filters. Refer to the application information for additional details."),
    p("[1] Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate."),
    p("[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable."),
    p("[3] Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.")
  )
}

#' srv_t_primary Server Functions
#'
#' @noRd
#' @importFrom shiny renderUI
#' @import Tplyr
srv_t_primary <- function(input, output, session, datasets) {
  output$table <- renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = FALSE)
    ADAS_FILTERED <- datasets$get_data("ADAS", filtered = FALSE)
    adas <- ADAS_FILTERED

    ## -----------------------------------------------------------------------------------------------------------------------------------
    t <- tplyr_table(adas, TRTP) %>%
      set_pop_data(ADSL_FILTERED) %>%
      set_pop_treat_var(TRT01P) %>%
      set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>%
      set_distinct_by(USUBJID) %>%
      set_desc_layer_formats(
        'n' = f_str('xx', n),
        'Mean (SD)' = f_str('xx.x (xx.xx)', mean, sd),
        'Median (Min; Max)' = f_str('xx.x (xxx;xx)', median, min, max)
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
      dplyr::select(-starts_with('ord')) %>%
      add_column_headers(
        paste0("|Placebo</br>(N=**Placebo**)| Xanomeline High Dose</br>(N=**Xanomeline High Dose**) ",
               "| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**)"),
        header_n(t)
      )


    ## -----------------------------------------------------------------------------------------------------------------------------------
    model_portion <- efficacy_models(adas, var = 'CHG', wk = 24, show_pvalue = TRUE)


    ## -----------------------------------------------------------------------------------------------------------------------------------
    final <- dplyr::bind_rows(sum_data, model_portion)

    ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
      huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
      huxtable::set_align(1, 1:ncol(final), 'center') %>%
      huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
      huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
      huxtable::set_width(1) %>%
      huxtable::set_escape_contents(FALSE) %>%
      huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))
    htmltools::HTML(huxtable::to_html(ht))
  })

}
