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
    h4("Table 14-4.01"),
    h4("Visit Completion"),
    uiOutput(ns("table")),
    p("Table is based on participants within the ITT population")
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
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = FALSE)
    ADLB_FILTERED <- datasets$get_data("ADLB", filtered = FALSE)
    adsl <- ADSL_FILTERED
    adlbc <- ADLB_FILTERED
    
    # use adlbc data set to remain consistent with efficacy table input data
    visit_df <- adlbc %>%
      filter(PARAMCD == "GLUC") %>% 
      filter(AVISITN != 98) %>%
      filter(!is.na(AVISITN)) %>%
      select(USUBJID, AVISITN) %>%
      distinct() %>%
      left_join(
        select(adsl, USUBJID, TRT01P),
        by = "USUBJID"
      )
    
    # visit number and week lookup
    v_week_df <- tibble::tibble(
      AVISITN = c(0, 2, 4, 6, 8, 12, 16, 20, 24, 26, 99),
      VISIT = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)), "End of Treatment")
    )%>%
      mutate(VISIT = factor(VISIT, levels = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)), "End of Treatment")))
    
    # build Tplyr table
    t_visit <- visit_df %>%
      left_join(v_week_df, by = "AVISITN") %>%
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
