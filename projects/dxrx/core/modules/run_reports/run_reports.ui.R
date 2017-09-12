library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(
    navbarPage(
        "DxRx Run Reports",
        navbarMenu("Plots",
	
            tabPanel("Total Reads(Pass filter)",
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("runReports_totalReadsFailThreshold",
                            "fail threshold:",
                            min = 0,
                            max = max(as.numeric(gsub(",","",rrdf$PF.Reads))) * 1.10,
                            value = 0
                        ),       
                        selectInput("runReports_totalReadsGroup", "Group by:",
                            c("None" = "none",
                            "Run" = "Run",
                            "Lane" = "Lane",
                            "Run and Lane" = "Run_Lane",
                            "Barcode" = "Barcode",
                            "Study" = "Study",
                            "Subject" = "Subject",
                            "Sample" = "Sample",
                            "Tissue Type and Origin" = "Tissue_type_origin",
                            "Date" = "Date",
                            "Instrument" = "Instrument",
                            "Increment" = "Increment",
                            "Flowcell" = "Flowcell")
                        )
                    ),
                    mainPanel(
                        plotlyOutput("runReports_totalReadsPlot", height = "80vh")
                    )
                )
            )
        )
    )
)

