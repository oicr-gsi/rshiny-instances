library(shiny)
library(ggplot2)

shinyServer(function(input,output) {
    source("/home/ubuntu/git/rshiny-instances/projects/gecco/core/modules/run_reports/run_reports.server.R" , local=TRUE)
    source("/home/ubuntu/git/rshiny-instances/projects/gecco/core/modules/cumulative_reports/cumulative_reports.server.R" , local=TRUE)
})



