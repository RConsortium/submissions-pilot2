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
  select(-starts_with('ord')) %>% 
  add_column_headers(
    paste0("|Placebo</br>(N=**Placebo**)| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**) ", 
           "| Xanomeline High Dose</br>(N=**Xanomeline High Dose**)"),
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
  huxtable::set_width(0.45) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(c(1/3, 1/6, 1/6, 1/6))
