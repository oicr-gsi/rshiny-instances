library(shiny)
library(ggplot2)
library(plotly)

library(pool)
library(DBI)

pool <- dbPool(
            drv = RMySQL::MySQL(),
            dbname = "DataReview",
            host = "gsi-bis.oicr.on.ca",
            username = "bisadmin",
            password = "oicr6wGSI"
)

shinyServer(function(input,output) {
    
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    
    output$table_single <- renderTable({
        sql <- "SELECT * FROM GECCO_FILEMAP_SINGLE;"
        query <- sqlInterpolate(pool,sql,id=input$ID)
        dbGetQuery(pool,query)
    })
    
    output$table_paired <- renderTable({
        sql <- "SELECT * FROM GECCO_FILEMAP_PAIRED;"
        query <- sqlInterpolate(pool,sql,id=input$ID)
        dbGetQuery(pool,query)
    })
    
})
