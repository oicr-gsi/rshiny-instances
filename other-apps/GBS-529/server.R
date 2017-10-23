library(shiny)
library(ggplot2)
library(plotly)

shinyServer(function(input,output) {
    
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    general_render_plot <- function(input_table,label) {
        d <- read.table(input_table,header=TRUE,sep="\t")
        plot <- ggplot(d, aes(x=record, y=percent))
        plot <- plot + geom_point(stat="identity", color=gg_color_hue(2)[2] , aes(text=paste("interval:",interval , "\nchrom:",chrom , "\nstart:",start , "\nend:",end , "\nnSamples:",value)))
        plot <- plot + scale_y_continuous(limits=c(0.0,100.0))
        plot <- plot + labs(x="BRCA2 50bp interval", y=paste("percent of samples with mean coverage ", label , "x or higher" , sep=""))
        return(plot)
    }
    
    
    
    output$bart_render_plot_1x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-1x-or-higher.tsv","1")
        ggplotly(plot)
    })
    output$bart_render_plot_5x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-5x-or-higher.tsv","5")
        ggplotly(plot)
    })
    output$bart_render_plot_10x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-10x-or-higher.tsv","10")
        ggplotly(plot)
    })
    output$bart_render_plot_25x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-25x-or-higher.tsv","25")
        ggplotly(plot)
    })
    output$bart_render_plot_50x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-50x-or-higher.tsv","50")
        ggplotly(plot)
    })
    output$bart_render_plot_100x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-100x-or-higher.tsv","100")
        ggplotly(plot)
    })
    output$bart_render_plot_250x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-250x-or-higher.tsv","250")
        ggplotly(plot)
    })
    output$bart_render_plot_500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-500x-or-higher.tsv","500")
        ggplotly(plot)
    })
    output$bart_render_plot_1000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-1000x-or-higher.tsv","1000")
        ggplotly(plot)
    })
    output$bart_render_plot_2500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-2500x-or-higher.tsv","2500")
        ggplotly(plot)
    })
    output$bart_render_plot_5000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/BART_TS/number-of-samples-covering-intervals-at-5000x-or-higher.tsv","5000")
        ggplotly(plot)
    })
    
    
    
    
    output$ma5_render_plot_1x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-1x-or-higher.tsv","1")
        ggplotly(plot)
    })
    output$ma5_render_plot_5x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-5x-or-higher.tsv","5")
        ggplotly(plot)
    })
    output$ma5_render_plot_10x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-10x-or-higher.tsv","10")
        ggplotly(plot)
    })
    output$ma5_render_plot_25x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-25x-or-higher.tsv","25")
        ggplotly(plot)
    })
    output$ma5_render_plot_50x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-50x-or-higher.tsv","50")
        ggplotly(plot)
    })
    output$ma5_render_plot_100x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-100x-or-higher.tsv","100")
        ggplotly(plot)
    })
    output$ma5_render_plot_250x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-250x-or-higher.tsv","250")
        ggplotly(plot)
    })
    output$ma5_render_plot_500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-500x-or-higher.tsv","500")
        ggplotly(plot)
    })
    output$ma5_render_plot_1000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-1000x-or-higher.tsv","1000")
        ggplotly(plot)
    })
    output$ma5_render_plot_2500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-2500x-or-higher.tsv","2500")
        ggplotly(plot)
    })
    output$ma5_render_plot_5000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MA5_TS/number-of-samples-covering-intervals-at-5000x-or-higher.tsv","5000")
        ggplotly(plot)
    })
    
    
    
    
    output$mcnt_render_plot_1x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-1x-or-higher.tsv","1")
        ggplotly(plot)
    })
    output$mcnt_render_plot_5x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-5x-or-higher.tsv","5")
        ggplotly(plot)
    })
    output$mcnt_render_plot_10x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-10x-or-higher.tsv","10")
        ggplotly(plot)
    })
    output$mcnt_render_plot_25x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-25x-or-higher.tsv","25")
        ggplotly(plot)
    })
    output$mcnt_render_plot_50x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-50x-or-higher.tsv","50")
        ggplotly(plot)
    })
    output$mcnt_render_plot_100x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-100x-or-higher.tsv","100")
        ggplotly(plot)
    })
    output$mcnt_render_plot_250x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-250x-or-higher.tsv","250")
        ggplotly(plot)
    })
    output$mcnt_render_plot_500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-500x-or-higher.tsv","500")
        ggplotly(plot)
    })
    output$mcnt_render_plot_1000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-1000x-or-higher.tsv","1000")
        ggplotly(plot)
    })
    output$mcnt_render_plot_2500x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-2500x-or-higher.tsv","2500")
        ggplotly(plot)
    })
    output$mcnt_render_plot_5000x <- renderPlotly({
        plot <- general_render_plot("/home/ubuntu/data/other-apps/GBS-529/data/MCNT_TS/number-of-samples-covering-intervals-at-5000x-or-higher.tsv","5000")
        ggplotly(plot)
    })
    
})