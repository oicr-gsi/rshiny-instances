library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(
	navbarPage(
		"DxRx Run Reports",
		navbarMenu("Plots",
			tabPanel("Total Reads (Pass filter)",
				fluidPage(
					bsButton("runReports_totalReadsShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('runReports_totalReadsUI')
				)
			),
			tabPanel("Mean Insert Size",
				fluidPage(
					bsButton("runReports_insertMeanShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('runReports_insertMeanUI')
				)
			),
			tabPanel("Percent Mapped",
				fluidPage(
					bsButton("runReports_percentMappedShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('runReports_percentMappedUI')
				)
			),
			tabPanel("Percent On Target",
				fluidPage(
					bsButton("runReports_percentOntShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('runReports_percentOntUI')
				)
			),
			tabPanel("Mean Coverage",
				fluidPage(
					bsButton("runReports_meanCoverageShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('runReports_meanCoverageUI')
				)
			)
		)
	)
)
