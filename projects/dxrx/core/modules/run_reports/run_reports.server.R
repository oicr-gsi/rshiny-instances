library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

output$runReports_totalReadsPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_totalReadsFailThreshold

	sortby <- input$runReports_totalReadsGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub(",","",rrdf$PF.Reads)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","threshold")
	d <- data.frame(d , rrdf)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="total reads (pass filter)") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
output$runReports_insertMeanPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_insertMeanFailThreshold

	sortby <- input$runReports_insertMeanGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library, rrdf$record, rrdf$Insert_Mean)
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","threshold")
	d <- data.frame(d , rrdf)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="mean insert size") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
output$runReports_percentMappedPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_percentMappedFailThreshold

	sortby <- input$runReports_percentMappedGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%","",rrdf$Map.Percent)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","threshold")
	d <- data.frame(d , rrdf)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="percent of reads mapped to hg19") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
output$runReports_percentOntPlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_percentOntFailThreshold

	sortby <- input$runReports_percentOntGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%","",rrdf$Percent.mapped.on.Target)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","threshold")
	d <- data.frame(d , rrdf)
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="percent of mapped reads on target") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
output$runReports_meanCoveragePlot <- renderPlotly({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_meanCoverageFailThreshold

	sortby <- input$runReports_meanCoverageGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("x","",rrdf$Coverage)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","threshold")
	d <- data.frame(d , rrdf)
	windowHeight <- max(d$value) * 1.10
	plot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="mean coverage") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	y1 <- c()
	y2 <- c()
    
	if (sortby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
		#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
	ggplotly(plot)
})
output$runReports_onTargetVSTotalReadsPlot <- renderPlot({

	d <- data.frame(rrdf$Library, as.numeric(gsub(",","",rrdf$PF.Reads)), as.numeric(gsub("%","",rrdf$Percent.mapped.on.Target)))
	colnames(d) <- c("library","my_x","my_y")
	windowWidth <- max(d$my_x) * 1.10
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=my_x, y=my_y)) + geom_point() + labs(x="total reads (pass filter)", y="percent of mapped reads on target") + scale_x_continuous(limits=c(0.0,windowWidth)) + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot
})
