# Packages ---------------------------------------------------------------------
library(stringr)
library(tidyr)
library(glue)
library(reactable)
library(shiny)
library(dplyr)
library(rtables)
library(ggplot2)
library(cowplot)
library(visR)
library(Tplyr)
library(huxtable)
library(emmeans)
library(r2rtf)
devtools::load_all()

# Preamble ---------------------------------------------------------------------
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/startup.R")) 
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/aux_popover.R"))

# Data loading and analysis ----------------------------------------------------
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/load_data.R")) 
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/cal_demograph.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/plot_km.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/cal_primary.R"))
source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/cal_efficacy.R"))

# App --------------------------------------------------------------------------
ui <- fluidPage(
  source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/ui/main.R"))$value
)

server <- function(input, output, session) {
  output$tbl_demog <- renderUI({
    source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/out_demograph.R"))$value
  })
  
  output$plot_km <- renderPlot({ KM })
  
  output$tbl_primary <- renderUI({
    source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/out_primary.R"))$value
  })
  
  output$tbl_efficacy_1 <- reactable::renderReactable({
    source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/out_eff_1.R"))$value
  })

  output$tbl_efficacy_2 <- reactable::renderReactable({
    source(file.path(rprojroot::find_root("DESCRIPTION"), "inst/shiny/app/module/out_eff_2.R"))$value
  })
}
 
shinyApp(ui, server)
