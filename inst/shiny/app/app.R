# Packages ---------------------------------------------------------------------
library(teal)
library(rtables)
library(haven)
library(dplyr)
library(Tplyr)
library(tidyr)
library(glue)
library(stringr)
library(huxtable)
library(ggplot2)
library(cowplot)
library(visR)
library(emmeans)
library(reactable)
devtools::load_all()

# Preamble ---------------------------------------------------------------------
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/startup.R"))
#source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/pilot1.R"))

# Data loading and analysis ----------------------------------------------------
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/tm_g_kmplot.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/tm_t_demographic.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/tm_t_efficacy.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/tm_t_primary.R"))

# App --------------------------------------------------------------------------
adsl  <- haven::read_xpt(file.path(rprojroot::find_root("DESCRIPTION"), "adam/adsl.xpt"))
adsl <- adsl %>%
  dplyr::mutate(
    TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose")),
    AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    RACE = factor(RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
  )
adas  <- haven::read_xpt(file.path(rprojroot::find_root("DESCRIPTION"), "adam/adadas.xpt")) %>%
  filter(
    EFFFL == "Y",
    ITTFL == 'Y',
    PARAMCD == 'ACTOT',
    ANL01FL == 'Y'
  )
adtte <- haven::read_xpt(file.path(rprojroot::find_root("DESCRIPTION"), "adam/adtte.xpt")) %>%
  filter(PARAMCD == "TTDE")
adlb <- haven::read_xpt(file.path(rprojroot::find_root("DESCRIPTION"), "adam/adlbc.xpt")) %>%
  subset(TRTPN %in% c(0, 81) & PARAMCD == "GLUC" & !is.na(AVISITN)) %>%
  mutate(TRTPN = ifelse(TRTPN == 0, 99, TRTPN)) # change treatment order for pairwise comparison
app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl),
    cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
    cdisc_dataset("ADTTE", adtte),
    cdisc_dataset("ADLB", adlb)
  ),
  modules = root_modules(
    module(
      label = "App Information",
      server = function(input, output, session, datasets){},
      ui = function(id, ...) {
        shiny::includeMarkdown("about.md")
      },
      filters = NULL
    ),
    module(
      label = "Demographic Table",
      ui = ui_t_demographic,
      server = srv_t_demographic,
      filters = "ADSL"
    ),
    module(
      label = "KM plot for TTDE",
      ui = ui_g_kmplot,
      server = srv_g_kmplot,
      filters = c("ADSL", "ADTTE")
    ),
    module(
      label = "Primary Table",
      ui = ui_t_primary,
      server = srv_t_primary,
      filters = c("ADSL", "ADAS")
    ),
    module(
      label = "Efficacy Table",
      ui = ui_t_efficacy,
      server = srv_t_efficacy,
      filters = c("ADSL", "ADLB")
    )

  ),
  header = "Pilot2 app (draft)",
  footer = tags$p(class="text-muted", "Source: R Consortium")
)
shinyApp(app$ui, app$server)
