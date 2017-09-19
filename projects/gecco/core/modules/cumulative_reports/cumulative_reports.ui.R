library(shiny)
library(ggplot2)
library(plotly)
df <- read.table("/home/ubuntu/data/gecco/core/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\t")

shinyUI(
	navbarPage(
		"GECCO Cumulative Reports",
		navbarMenu("Plots",
			tabPanel("Aggregate Reads (Pass filter)",
				fluidPage(
					bsButton("cumlativeReports_AggregateReadsShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_AggregateReadsUI')
				)
			),
			tabPanel("Percent Aggregate Excluded Reads",
				fluidPage(
					bsButton("cumlativeReports_percentAggregateExcludedReadsShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_percentAggregateExcludedReadsUI')
				)
			),
			tabPanel("Average Read Length",
				fluidPage(
					bsButton("cumlativeReports_averageReadLengthShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_averageReadLengthUI')
				)
			),
			tabPanel("Percent On Target",
				fluidPage(
					bsButton("cumlativeReports_percentOntShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_percentOntUI')
				)
			),
			tabPanel("Mean Coverage",
				fluidPage(
					bsButton("cumlativeReports_meanCoverageShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_meanCoverageUI')
				)
			),
			tabPanel("Percent of Target Covered at 8x or Higher",
				fluidPage(
					bsButton("cumlativeReports_percentCoverageEightXShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_percentCoverageEightXUI')
				)
			),
			tabPanel("Minimum Coverage for 90% of Target",
				fluidPage(
					bsButton("cumlativeReports_minCoverageForNinetyPercentOfTargetShowPanel","show/hide sidebar",type="toggle",value=TRUE),
					uiOutput('cumlativeReports_minCoverageForNinetyPercentOfTargetUI')
				)
			)
		)
	)
)
