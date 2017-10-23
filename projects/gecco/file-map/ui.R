library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GECCO FileMaps",
        tabPanel("About"
        ),
        navbarMenu("Tables",
            tabPanel("FileMap - Single Sample",
                mainPanel(
                    tableOutput('table_single')
                )
            ),
            tabPanel("FileMap - Paired Samples",
                mainPanel(
                    tableOutput('table_paired')
                )
            )
        )
    )
)
