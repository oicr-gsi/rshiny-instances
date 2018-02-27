library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GBS-551 - DYS analysis of off-target pileup",
        tabPanel("About"
        ),
        navbarMenu("Hist Plots",
            tabPanel("DYS_1001_Es_P_PE_360_TS_150916_D00353_0107_AC7HH0ANXX_AAACATCG_L005_001",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("chr1")
                    )
                )
            )
        )
    )
)
