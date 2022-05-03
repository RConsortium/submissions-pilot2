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