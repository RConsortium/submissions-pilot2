#' @importFrom teal init modules module
#' @importFrom shiny tags
create_teal <- function() {
  adam_path <- get_golem_config("adam_path")
  adsl  <- haven::read_xpt(file.path(adam_path, "adsl.xpt"))
  adsl <- adsl %>%
    dplyr::mutate(
      TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose")),
      AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
      RACE = factor(RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
    )
  adas  <- haven::read_xpt(file.path(adam_path, "adadas.xpt")) %>%
    dplyr::filter(
      EFFFL == "Y",
      ITTFL == 'Y',
      PARAMCD == 'ACTOT',
      ANL01FL == 'Y'
    )
  adtte <- haven::read_xpt(file.path(adam_path, "adtte.xpt")) %>%
    dplyr::filter(PARAMCD == "TTDE") %>%
    select(., -TRTDUR, -TRTP, -TRTA, -TRTAN)
  adlb <- haven::read_xpt(file.path(adam_path, "adlbc.xpt")) %>%
    filter(PARAMCD == "GLUC" & !is.na(AVISITN))
  
  app <- teal::init(
    data = teal.data::cdisc_data(
      teal.data::cdisc_dataset("ADSL", adsl),
      teal.data::cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
      teal.data::cdisc_dataset("ADTTE", adtte),
      teal.data::cdisc_dataset("ADLB", adlb)
    ),
    modules = modules(
      module(
        label = "App Information",
        server = function(input, output, session, datasets){},
        ui = function(id, ...) {
          shiny::includeMarkdown(app_sys("app", "docs", "about.md"))
        },
        filters = NULL
      ),
      module(
        label = "Usage Guide",
        ui = ui_i_usage,
        server = srv_i_usage,
        filters = NULL
      ),
      module(
        label = "Demographic Table",
        ui = ui_t_demographic,
        server = srv_t_demographic,
        filters = NULL
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
        filters = NULL
      ),
      module(
        label = "Efficacy Table",
        ui = ui_t_efficacy,
        server = srv_t_efficacy,
        filters = NULL
      ),
      module(
        label = "Visit Completion Table",
        ui = ui_t_disposition,
        server = srv_t_disposition,
        filters = NULL
      )
      
    ),
    header = "Pilot 2 Shiny Application",
    footer = tags$p(class="text-muted", "Source: R Consortium")
  )
  
  return(app)
}