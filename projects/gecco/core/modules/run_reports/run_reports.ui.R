library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("/home/ubuntu/data/gecco/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(
    navbarPage(
        "GECCO Run Reports",
        navbarMenu("Plots",

                    tabPanel("total reads (pass filter)",
                        fluidPage(
                            bsButton("runReports_totalReadsShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('runReports_totalReadsUI')
                        )
                    ),

                    tabPanel("mean insert size",
                        fluidPage(
                            bsButton("runReports_insertMeanShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('runReports_insertMeanUI')
                        )
                    ),

                    tabPanel("percent of reads mapped to hg19",
                        fluidPage(
                            bsButton("runReports_percentMappedShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('runReports_percentMappedUI')
                        )
                    ),

                    tabPanel("percent of mapped reads on target",
                        fluidPage(
                            bsButton("runReports_percentOntShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('runReports_percentOntUI')
                        )
                    ),

                    tabPanel("mean coverage",
                        fluidPage(
                            bsButton("runReports_meanCoverageShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('runReports_meanCoverageUI')
                        )
                    )

		)
	)
)

