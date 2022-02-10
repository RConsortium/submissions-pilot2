# Packages ---------------------------------------------------------------------

library("shiny")

# Global modules and functions -------------------------------------------------

# Custom UI components
source("modules/xxx.R")

# UI ---------------------------------------------------------------------------

ui <- function(request) {
  source("ui/main.R", local = TRUE)$value
}

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  source("server/main.R", local = TRUE)$value
}

shinyApp(ui, server)
