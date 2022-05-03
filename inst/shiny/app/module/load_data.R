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