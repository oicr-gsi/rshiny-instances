library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
library(RODBC)

shinyServer(function(input,output) {
    
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    get_data <- function() {
        # Load RODBC package
        library(RODBC)

        # Create a connection to the database called "channel"
        # If you are using operating system authentication (the computer already knows who you
        # are because you are logged into it) you can leave out the uid="USERNAME", part.
        channel <- odbcConnect("gsi-bis.oicr.on.ca", uid="bisadmin", pwd="oicr6wGSI", believeNRows=FALSE)
        
        # Check that connection is working (Optional)
        odbcGetInfo(channel)
        
        # Find out what tables are available (Optional)
        Tables <- sqlTables(channel, schema="SCHEMA")
        
        # Query the database and put the results into the data frame "dataframe"
         dataframe <- sqlQuery(channel, "
         SELECT *
         FROM
         SCHEMA.DATATABLE")
    
        return(iris)
    }
    
    output$table <- renderTable(get_data())
    
})
