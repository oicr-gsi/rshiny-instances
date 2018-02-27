library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GBS-551 - DYS analysis of off-target pileup",
        tabPanel("About"
        ),
        navbarMenu("Hist Plots",
