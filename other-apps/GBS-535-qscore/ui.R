library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GBS-535 - GECCO analysis of qscore on sequencing metrics",
        tabPanel("About"
        ),
        navbarMenu("Plots",
            tabPanel("GECCO Analysis A - sequencing stats vs qscore",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("PF.Reads",
                            plotlyOutput("pre.PF.Reads",height="60vh",width="80vw"),
                            plotlyOutput("post.PF.Reads",height="60vh",width="80vw")),
                        tabPanel("Map.Percent",
                            plotlyOutput("pre.Map.Percent",height="60vh",width="80vw"),
                            plotlyOutput("post.Map.Percent",height="60vh",width="80vw")),
                        tabPanel("Percent.mapped.on.Target",
                            plotlyOutput("pre.Percent.mapped.on.Target",height="60vh",width="80vw"),
                            plotlyOutput("post.Percent.mapped.on.Target",height="60vh",width="80vw")),
                        tabPanel("Coverage",
                            plotlyOutput("pre.Coverage",height="60vh",width="80vw"),
                            plotlyOutput("post.Coverage",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered1xOrHigher",
                            plotlyOutput("pre.pBasesCovered1xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("post.pBasesCovered1xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered10xOrHigher",
                            plotlyOutput("pre.pBasesCovered10xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("post.pBasesCovered10xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered25xOrHigher",
                            plotlyOutput("pre.pBasesCovered25xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("post.pBasesCovered25xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered50xOrHigher",
                            plotlyOutput("pre.pBasesCovered50xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("post.pBasesCovered50xOrHigher",height="60vh",width="80vw"))
                    )
                )
            )
        )
    )
)
