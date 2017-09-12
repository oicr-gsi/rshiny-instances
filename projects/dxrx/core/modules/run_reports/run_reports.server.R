library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

output$ui <- renderUI({
    if (input$showpanel) {
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
                plotlyOutput("runReports_totalReadsPlot" , height="120vh")
                #plotOutput("runReports_totalReadsPlot" , height="120vh")
            )
        )
    } else {
        plotlyOutput("runReports_totalReadsPlot" , height="120vh")
    }
})


output$runReports_totalReadsPlot <- renderPlotly({
#output$runReports_totalReadsPlot <- renderPlot({
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
	plot <- ggplot(d, aes(x=record, y=value))
    plot <- plot + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) , fill=threshold) )
    plot <- plot + labs(x="all libraries", y="total reads (pass filter)")
    plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + scale_x_continuous(trans="reverse")
    
    
	#plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(rrdf) , ymin=t , ymax=t), color="red" , linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")

	unique.groupby <- unique(rrdf[[sortby]])

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
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
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
    #plot
})

