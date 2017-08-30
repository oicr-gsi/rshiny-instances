library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("/home/ubuntu/data/gecco/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
	headerPanel("GECCO Run Report Analysis"),
	hr(), 

	titlePanel("Total Reads (Pass filter)"),
	plotlyOutput("runReports_totalReadsPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_totalReadsFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub(",","",rrdf$PF.Reads))) * 1.10,
				value = 0
			)
		),
		column(3,
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
		)
	),
	hr(),
	titlePanel("Mean Insert Size"),
	plotlyOutput("runReports_insertMeanPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_insertMeanFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(rrdf$Insert_Mean),
				value = 0
			)
		),
		column(3,
			selectInput("runReports_insertMeanGroup", "Group by:",
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
		)
	),
	hr(),
	titlePanel("Percent Mapped"),
	plotlyOutput("runReports_percentMappedPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_percentMappedFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_percentMappedGroup", "Group by:",
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
		)
	),
	hr(),
	titlePanel("Percent On Target"),
	plotlyOutput("runReports_percentOntPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_percentOntFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_percentOntGroup", "Group by:",
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
		)
	),
	hr(),
	titlePanel("Mean Coverage"),
	plotlyOutput("runReports_meanCoveragePlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_meanCoverageFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub("x","",rrdf$Coverage))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_meanCoverageGroup", "Group by:",
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
		)
	),
	hr(),
	titlePanel("Percent on Target vs Total Reads (Pass filter)"),
	plotOutput("runReports_onTargetVSTotalReadsPlot", height = "80vh"),
	hr()
))

