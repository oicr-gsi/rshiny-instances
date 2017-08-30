library(shiny)
library(ggplot2)
library(plotly)
df <- read.table("/home/ubuntu/data/gecco/core/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
	headerPanel("GECCO Cumulative Report Analysis"),
	hr(), 

	titlePanel("Aggregate Reads (Pass filter)"),
	plotlyOutput("cumlativeReports_AggregateReadsPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_AggregateReadsFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub(",","",df$Aggregate.PF.Reads))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_AggregateReadsGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Percent Aggregate Excluded Reads"),
	plotlyOutput("cumlativeReports_percentAggregateExcludedReadsPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentAggregateExcludedReadsFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentAggregateExcludedReadsGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Average Read Length"),
	plotlyOutput("cumlativeReports_averageReadLengthPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_averageReadLengthFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(df$Average.Read.Length),
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_averageReadLengthGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Percent On Target"),
	plotlyOutput("cumlativeReports_percentOntPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentOntFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentOntGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Mean Coverage"),
	plotlyOutput("cumlativeReports_meanCoveragePlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_meanCoverageFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(df$Average.Coverage),
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_meanCoverageGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Percent of Target Covered at 8x or Higher"),
	plotlyOutput("cumlativeReports_percentCoverageEightXPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentCoverageEightXFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentCoverageEightXGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr(),
	titlePanel("Minimum Coverage for 90% of Target"),
	plotlyOutput("cumlativeReports_minCoverageForNinetyPercentOfTargetPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_minCoverageForNinetyPercentOfTargetFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_minCoverageForNinetyPercentOfTargetGroup", "Group by:",
				c("None" = "none",
				"Last Modified" = "Last.Modified",
				"Sample" = "Sample",
				"Study" = "Study",
				"Subject" = "Subject",
				"Sample without GroupID" = "SampleNoGID",
				"Tissue Type and Origin" = "Tissue_type_origin",
				"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
			)
		)
	),
	hr()
))

