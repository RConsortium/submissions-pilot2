#' ui_t_disposition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput
ui_t_disposition <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table"))
    # p("Statistical model and comparison p-values removed when applying data filters. Refer to the application information for additional details."),
    # p("[1] Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate."),
    # p("[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable."),
    # p("[3] Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.")
  )
}

#' srv_t_primary Server Functions
#'
#' @noRd
#' @importFrom shiny renderUI
#' @import Tplyr
#' @import dplyr
srv_t_disposition <- function(input, output, session, datasets) {
  output$table <- renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
    ADLB_FILTERED <- datasets$get_data("ADLB", filtered = TRUE)
    adsl <- ADSL_FILTERED
    adlb <- ADLB_FILTERED
    
    ## -----------------------------------------------------------------------------------------------------------------------------------
    visit_long <- adsl %>%
      filter(SAFFL == "Y") %>%
      select(USUBJID, TRT01P, VISNUMEN) %>%
      mutate(VIS_LONG = purrr::map2(USUBJID, VISNUMEN, ~{
        # create tibble with full visit records 
        # filter for records less than supplied VISNUMEN
        df <- tibble::tibble(
          VISITNUM = c(3, 4, 5, 7, 8, 9, 10, 11, 12)
        ) %>%
        mutate(VALUE = 1) %>%
        filter(VISITNUM <= .y)
        return(df)
      })) %>%
      tidyr::unnest(cols = VIS_LONG)

    # visit number and week lookup
    v_week_df <- tibble::tibble(
      VISITNUM = c(3, 4, 5, 7, 8, 9, 10, 11, 12),
      VISIT = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24)))
    )%>%
      mutate(VISIT = factor(VISIT, levels = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24)))))

    # build Tplyr table
    t_visit <- visit_long %>%
      left_join(v_week_df, by = "VISITNUM") %>%
      tplyr_table(TRT01P) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_total_group() %>%
      add_layer(
        group_count(VISIT) %>%
        set_distinct_by(USUBJID) %>%
        set_format_strings(
          f_str('xx (xx%)', distinct_n, distinct_pct)
        )
      )

    b_t_visit <- t_visit %>%
      build() %>%
      dplyr::select(row_label1, var1_Placebo, `var1_Xanomeline High Dose`, `var1_Xanomeline Low Dose`, var1_Total) %>%
      add_column_headers(
        paste0("|Placebo</br>(N=**Placebo**)",
               "| Xanomeline High Dose</br>(N=**Xanomeline High Dose**) ",
               "| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**) ",
               "| Total</br>(N=**Total**) "),
        header_n(t_visit)
      )

    ht <- huxtable::as_hux(b_t_visit, add_colnames = FALSE) %>%
      huxtable::set_bold(1, 1:ncol(b_t_visit), TRUE) %>%
      huxtable::set_align(1, 1:ncol(b_t_visit), 'center') %>%
      huxtable::set_valign(1, 1:ncol(b_t_visit), 'bottom') %>%
      huxtable::set_bottom_border(1, 1:ncol(b_t_visit), 1) %>%
      huxtable::set_width(0.9) %>%
      huxtable::set_escape_contents(FALSE) %>%
      huxtable::set_col_width(c(.5, 1/8, 1/8, 1/8, 1/8))
    htmltools::HTML(huxtable::to_html(ht))
  })

}
