library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)

shinyServer(function(input,output) {
    
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    corr_eqn <- function(x,y, digits = 2) {
  		corr_coef <- round(cor(x, y, method="pearson"), digits = digits)
		return(corr_coef)
	}
    
    genP <- function(selection,selection_label,togsub,yvalue,ylabel,xmult,ymult) {
        d <- read.table("/home/ubuntu/data/other-apps/GBS-535-qscore/data/GECCO-qscore.runreport.cs_added.tsv",header=TRUE,sep="\t")
        d$value <- as.numeric(gsub(togsub,"",d[[yvalue]]))
        d$nPlate <- as.character(d$plate)
        
        ds <- d[which(grepl(selection,d$selectionGroup)),]
        coef <- corr_eqn(ds$qscore , ds$value)
        
        p <- ggplot(ds, aes(x=qscore, y=value))
        p <- p + geom_point(stat="identity", aes(text=paste("sample:",sample , "\nrun:",run , "\nlane:",lane , "\nplate:",plate, "\nBarcode:",Barcode , "\nselection:",selectionGroup) , color=nPlate))
        p <- p + labs(x=paste("qscore - ",selection_label," samples",sep=""), y=ylabel)
        p <- p + scale_x_continuous(limits=c(0.0,max(d$qscore)))
        p <- p + scale_y_continuous(limits=c(0.0,max(d$value)))
        
        p <- p + geom_smooth(colour = "gray", method = 'lm', se=FALSE)
        p <- p + geom_ribbon(stat = "smooth", fill="gray" , method = "lm", alpha = .15)
        
        p <- p + geom_text(aes(x=max(d$qscore)*xmult , y=max(d$value)*ymult , label=paste("max:",round(max(ds$value),digits=2) , "\nmin:",round(min(ds$value),digits=2) , "\nmedian:",round(median(ds$value),digits=2) , "\nmean:",round(mean(ds$value),digits=2) , "\ncor:",coef,sep="")))
        
        return(p)
    }
    
    output$pre.PF.Reads <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "," , "PF.Reads" , "PF.Reads" , 0.05 , 0.85))
    })
    output$post.PF.Reads <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "," , "PF.Reads" , "PF.Reads" , 0.05 , 0.85))
    })
    
    output$pre.Map.Percent <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "%" , "Map.Percent" , "Map.Percent" , 0.80 , 0.20))
    })
    output$post.Map.Percent <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "%" , "Map.Percent" , "Map.Percent" , 0.80 , 0.20))
    })
    
    output$pre.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target" , 0.80 , 0.20))
    })
    output$post.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target" , 0.80 , 0.20))
    })
    
    output$pre.Coverage <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "x" , "Coverage" , "Coverage" , 0.05 , 0.85))
    })
    output$post.Coverage <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "x" , "Coverage" , "Coverage" , 0.05 , 0.85))
    })
    
    output$pre.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "x" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher" , 0.80 , 0.20))
    })
    output$post.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "x" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher" , 0.80 , 0.20))
    })
    
    output$pre.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "x" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher" , 0.80 , 0.20))
    })
    output$post.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "x" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher" , 0.80 , 0.20))
    })
    
    output$pre.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "x" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher" , 0.80 , 0.20))
    })
    output$post.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "x" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher" , 0.80 , 0.20))
    })
    
    output$pre.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("preSelection", "preSelection" , "x" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher" , 0.80 , 0.20))
    })
    output$post.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("postSelection", "postSelection" , "x" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher" , 0.80 , 0.20))
    })
})
