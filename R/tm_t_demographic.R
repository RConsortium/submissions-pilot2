#' ui_t_demographic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput
#' @importFrom stats median sd
 ui_t_demographic <- function(id, datasets) {
   ns <- NS(id)
   tagList(
    h4("Table 14-2.01"),
    h4("Summary of Demographic and Baseline Characteristics"),
    uiOutput(ns("table"))
   )
 }
 
 #' srv_t_demographic Server Functions
 #'
 #' @noRd
 #' @importFrom shiny renderUI
 #' @import rtables
 srv_t_demographic <- function(input, output, session, datasets) {
   output$table <- renderUI({
     ADSL_FILTERED <- datasets$get_data("ADSL", filtered = FALSE)
     vars <- c("AGE", "AGEGR1", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL")
     labels <- datasets$get_varlabels("ADSL", vars)
     labels <- vapply(vars, function(x) ifelse(is.na(labels[[x]]), 
                                               x, labels[[x]]), character(1))
     labels["AGEGR1"] <- "Age group"
     labels["AGE"] <- "Age (year)"
     labels["RACE"] <- "Race"
     lyt <- basic_table(title = "",
                        subtitles = character(),
                        main_footer = paste("Program: tm_t_demographic.R", Sys.time())
     ) %>%
       split_cols_by("TRT01P") %>%
       add_colcounts() %>%
       analyze(vars, function(x, ...) {
         if (is.numeric(x)) {
           in_rows(
             "Mean (SD)" = c(mean(x), sd(x)),
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
       var_labels = labels)
     tbl <- build_table(lyt, ADSL_FILTERED)
     as_html(tbl)
     
   })
   
 }