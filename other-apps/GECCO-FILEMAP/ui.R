library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GECCO FileMaps",
        tabPanel("About"
        ),
        navbarMenu("Tables",
            tabPanel("FileMap",
                mainPanel(
                    tableOutput('table')
                )
            )
        )
    )
)
