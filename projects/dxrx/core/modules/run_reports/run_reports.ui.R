library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(
    navbarPage(
        "DxRx Run Reports",
        navbarMenu("Plots",
	
            tabPanel("Total Reads(Pass filter)",
                fluidPage(
                    bsButton("showpanel", "Show/hide sidebar", type = "toggle", value = TRUE),
                    uiOutput('ui')
                )
            )
        )
    )
)

