library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/dxrx/core/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\t")

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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="aggregate reads (pass filter)") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="percent of aggregate reads excluded from alignment") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="average read length") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="percent of mapped reads on target") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="mean coverage") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="percent of target bases covered at 8x or higher") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
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
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "
Runs:Libraries:Lanes:" , Runs.Libraries.Lanes , "
Last Modified:", Last.Modified) ,fill=threshold) ) + labs(x="all libraries", y="minimum level of coverage for 90% of target bases") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(df[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- df[which(df[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			color <- (i %% 2) + 10
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
		plot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
