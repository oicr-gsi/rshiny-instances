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
    
    genP <- function(dataset,pattern,type_label,ttype,togsub,yvalue,ylabel) {
        d <- read.table("/home/ubuntu/data/other-apps/GBS-534-target-amp/data/GBS-534-target-amp-run-report.sorted.rows_added.cs_added.tsv",header=TRUE,sep="\t")
        d$value <- as.numeric(gsub(togsub,"",d[[yvalue]]))
        d <- d[which(grepl(dataset,d$sample)),]
        d <- d[which(grepl(pattern,d$library_type)),]
        
        ds <- d[which(grepl(ttype,d$plate_type)),]
        p <- ggplot(ds, aes(x=sample, y=value))
        p <- p + geom_point(stat="identity", aes(text=paste("run:",run , "\nlane:",lane, "\nsample:",sample, "\nplate_type:",plate_type, "\nBarcode:",Barcode) , color=library_type))
        p <- p + labs(x=type_label, y=ylabel)
        p <- p + scale_y_continuous(limits=c(0.0,max(d$value)))
        return(p)
        
    }
    
    output$GECCO.A.T.PF.Reads <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GECCO.A.R.PF.Reads <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GECCO.B.T.PF.Reads <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GECCO.B.R.PF.Reads <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GCONT.A.T.PF.Reads <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GCONT.A.R.PF.Reads <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GCONT.B.T.PF.Reads <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "," , "PF.Reads" , "PF.Reads"))
    })
    output$GCONT.B.R.PF.Reads <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "," , "PF.Reads" , "PF.Reads"))
    })
    
    
    
    output$GECCO.A.T.Map.Percent <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GECCO.A.R.Map.Percent <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GECCO.B.T.Map.Percent <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GECCO.B.R.Map.Percent <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GCONT.A.T.Map.Percent <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GCONT.A.R.Map.Percent <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GCONT.B.T.Map.Percent <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "Map.Percent" , "Map.Percent"))
    })
    output$GCONT.B.R.Map.Percent <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "Map.Percent" , "Map.Percent"))
    })
    
    
    
    output$GECCO.A.T.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GECCO.A.R.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GECCO.B.T.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GECCO.B.R.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GCONT.A.T.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GCONT.A.R.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GCONT.B.T.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    output$GCONT.B.R.Percent.mapped.on.Target <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "Percent.mapped.on.Target" , "Percent.mapped.on.Target"))
    })
    
    
    
    output$GECCO.A.T.Coverage <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "x" , "Coverage" , "Coverage"))
    })
    output$GECCO.A.R.Coverage <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "x" , "Coverage" , "Coverage"))
    })
    output$GECCO.B.T.Coverage <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "x" , "Coverage" , "Coverage"))
    })
    output$GECCO.B.R.Coverage <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "x" , "Coverage" , "Coverage"))
    })
    output$GCONT.A.T.Coverage <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "x" , "Coverage" , "Coverage"))
    })
    output$GCONT.A.R.Coverage <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "x" , "Coverage" , "Coverage"))
    })
    output$GCONT.B.T.Coverage <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "x" , "Coverage" , "Coverage"))
    })
    output$GCONT.B.R.Coverage <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "x" , "Coverage" , "Coverage"))
    })
    
    
    
    output$GECCO.A.T.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GECCO.A.R.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GECCO.B.T.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GECCO.B.R.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GCONT.A.T.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GCONT.A.R.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GCONT.B.T.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    output$GCONT.B.R.pBasesCovered1xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered1xOrHigher" , "pBasesCovered1xOrHigher"))
    })
    
    
    
    output$GECCO.A.T.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GECCO.A.R.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GECCO.B.T.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GECCO.B.R.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GCONT.A.T.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GCONT.A.R.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GCONT.B.T.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    output$GCONT.B.R.pBasesCovered10xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered10xOrHigher" , "pBasesCovered10xOrHigher"))
    })
    
    
    
    output$GECCO.A.T.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GECCO.A.R.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GECCO.B.T.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GECCO.B.R.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GCONT.A.T.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GCONT.A.R.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GCONT.B.T.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    output$GCONT.B.R.pBasesCovered25xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered25xOrHigher" , "pBasesCovered25xOrHigher"))
    })
    
    
    
    output$GECCO.A.T.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "C|D" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GECCO.A.R.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "C|D" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GECCO.B.T.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO", "A|B|C" , "GECCO tumor libraries" , "tumor" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GECCO.B.R.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GECCO" , "A|B|C" , "GECCO normal libraries" , "normal" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GCONT.A.T.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "C|D" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GCONT.A.R.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "C|D" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GCONT.B.T.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT", "A|B|C" , "GCONT tumor plate libraries" , "tumor" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    output$GCONT.B.R.pBasesCovered50xOrHigher <- renderPlotly({
        ggplotly(genP("GCONT" , "A|B|C" , "GCONT normal plate libraries" , "normal" , "%" , "pBasesCovered50xOrHigher" , "pBasesCovered50xOrHigher"))
    })
    
    
    
    
})