library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/gecco/core/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\t")

output$cumlativeReports_AggregateReadsUI <- renderUI ({
	if (input$cumlativeReports_AggregateReadsShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_AggregateReadsFailThreshold",
					"fail threshold:",
					min = 0,
					max = max(as.numeric(gsub(",","",df$Aggregate.PF.Reads))) * 1.10,
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_AggregateReadsPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_AggregateReadsPlot" , height="120vh")
	}
})

output$cumlativeReports_AggregateReadsPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_AggregateReadsFailThreshold

	sortby <- input$cumlativeReports_AggregateReadsGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub(",","",df$Aggregate.PF.Reads)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="aggregate reads (pass filter)")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_percentAggregateExcludedReadsUI <- renderUI ({
	if (input$cumlativeReports_percentAggregateExcludedReadsShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_percentAggregateExcludedReadsFailThreshold",
					"fail threshold:",
					min = 0,
					max = 100.0,
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_percentAggregateExcludedReadsPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_percentAggregateExcludedReadsPlot" , height="120vh")
	}
})

output$cumlativeReports_percentAggregateExcludedReadsPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentAggregateExcludedReadsFailThreshold

	sortby <- input$cumlativeReports_percentAggregateExcludedReadsGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Aggregate.PF.Excluded.Reads..)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="percent of aggregate reads excluded from alignment")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_averageReadLengthUI <- renderUI ({
	if (input$cumlativeReports_averageReadLengthShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_averageReadLengthFailThreshold",
					"fail threshold:",
					min = 0,
					max = max(df$Average.Read.Length),
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_averageReadLengthPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_averageReadLengthPlot" , height="120vh")
	}
})

output$cumlativeReports_averageReadLengthPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_averageReadLengthFailThreshold

	sortby <- input$cumlativeReports_averageReadLengthGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample, df$record, df$Average.Read.Length)
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="average read length")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_percentOntUI <- renderUI ({
	if (input$cumlativeReports_percentOntShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_percentOntFailThreshold",
					"fail threshold:",
					min = 0,
					max = 100.0,
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_percentOntPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_percentOntPlot" , height="120vh")
	}
})

output$cumlativeReports_percentOntPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentOntFailThreshold

	sortby <- input$cumlativeReports_percentOntGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Merged.Mapped.Reads.On.Target..)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="percent of mapped reads on target")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_meanCoverageUI <- renderUI ({
	if (input$cumlativeReports_meanCoverageShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_meanCoverageFailThreshold",
					"fail threshold:",
					min = 0,
					max = max(df$Average.Coverage),
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_meanCoveragePlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_meanCoveragePlot" , height="120vh")
	}
})

output$cumlativeReports_meanCoveragePlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_meanCoverageFailThreshold

	sortby <- input$cumlativeReports_meanCoverageGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample, df$record, df$Average.Coverage)
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="mean coverage")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_percentCoverageEightXUI <- renderUI ({
	if (input$cumlativeReports_percentCoverageEightXShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_percentCoverageEightXFailThreshold",
					"fail threshold:",
					min = 0,
					max = 100.0,
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_percentCoverageEightXPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_percentCoverageEightXPlot" , height="120vh")
	}
})

output$cumlativeReports_percentCoverageEightXPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentCoverageEightXFailThreshold

	sortby <- input$cumlativeReports_percentCoverageEightXGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Base.Coverage.At.8x)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="percent of target bases covered at 8x or higher")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
output$cumlativeReports_minCoverageForNinetyPercentOfTargetUI <- renderUI ({
	if (input$cumlativeReports_minCoverageForNinetyPercentOfTargetShowPanel) {
		sidebarLayout(
			sidebarPanel(
				sliderInput("cumlativeReports_minCoverageForNinetyPercentOfTargetFailThreshold",
					"fail threshold:",
					min = 0,
					max = 100.0,
					value = 0
				),
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
			),
			mainPanel(
				plotlyOutput("cumlativeReports_minCoverageForNinetyPercentOfTargetPlot" , height="120vh")
			)
		)
	} else {
		plotlyOutput("cumlativeReports_minCoverageForNinetyPercentOfTargetPlot" , height="120vh")
	}
})

output$cumlativeReports_minCoverageForNinetyPercentOfTargetPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_minCoverageForNinetyPercentOfTargetFailThreshold

	sortby <- input$cumlativeReports_minCoverageForNinetyPercentOfTargetGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("x","",df$X90..Covered.At..x.)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","record","value","threshold")
	d <- data.frame(d , df)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value))
	plot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold))
	plot <- plot + labs(x="all libraries", y="minimum level of coverage for 90% of target bases")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + scale_x_continuous(trans="reverse")
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + coord_flip()
	ggplotly(plot)
})
