library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GBS-534 - GECCO analysis of target amp data",
        tabPanel("About"
        ),
        navbarMenu("Plots",
            tabPanel("GECCO Analysis A - 8 minutes vs 16 minutes",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("PF.Reads",
                            plotlyOutput("GECCO.A.T.PF.Reads",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.PF.Reads",height="60vh",width="80vw")),
                        tabPanel("Map.Percent",
                            plotlyOutput("GECCO.A.T.Map.Percent",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.Map.Percent",height="60vh",width="80vw")),
                        tabPanel("Percent.mapped.on.Target",
                            plotlyOutput("GECCO.A.T.Percent.mapped.on.Target",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.Percent.mapped.on.Target",height="60vh",width="80vw")),
                        tabPanel("Coverage",
                            plotlyOutput("GECCO.A.T.Coverage",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.Coverage",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered1xOrHigher",
                            plotlyOutput("GECCO.A.T.pBasesCovered1xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.pBasesCovered1xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered10xOrHigher",
                            plotlyOutput("GECCO.A.T.pBasesCovered10xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.pBasesCovered10xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered25xOrHigher",
                            plotlyOutput("GECCO.A.T.pBasesCovered25xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.pBasesCovered25xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered50xOrHigher",
                            plotlyOutput("GECCO.A.T.pBasesCovered50xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.A.R.pBasesCovered50xOrHigher",height="60vh",width="80vw"))
                    )
                )
            ),
            
            tabPanel("GECCO Analysis B - original vs spike-in vs 8 minutes",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("PF.Reads",
                            plotlyOutput("GECCO.B.T.PF.Reads",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.PF.Reads",height="60vh",width="80vw")),
                        tabPanel("Map.Percent",
                            plotlyOutput("GECCO.B.T.Map.Percent",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.Map.Percent",height="60vh",width="80vw")),
                        tabPanel("Percent.mapped.on.Target",
                            plotlyOutput("GECCO.B.T.Percent.mapped.on.Target",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.Percent.mapped.on.Target",height="60vh",width="80vw")),
                        tabPanel("Coverage",
                            plotlyOutput("GECCO.B.T.Coverage",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.Coverage",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered1xOrHigher",
                            plotlyOutput("GECCO.B.T.pBasesCovered1xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.pBasesCovered1xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered10xOrHigher",
                            plotlyOutput("GECCO.B.T.pBasesCovered10xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.pBasesCovered10xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered25xOrHigher",
                            plotlyOutput("GECCO.B.T.pBasesCovered25xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.pBasesCovered25xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered50xOrHigher",
                            plotlyOutput("GECCO.B.T.pBasesCovered50xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GECCO.B.R.pBasesCovered50xOrHigher",height="60vh",width="80vw"))
                    )
                )
            ),
            
            tabPanel("GCONT Analysis A - 8 minutes vs 16 minutes",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("PF.Reads",
                            plotlyOutput("GCONT.A.T.PF.Reads",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.PF.Reads",height="60vh",width="80vw")),
                        tabPanel("Map.Percent",
                            plotlyOutput("GCONT.A.T.Map.Percent",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.Map.Percent",height="60vh",width="80vw")),
                        tabPanel("Percent.mapped.on.Target",
                            plotlyOutput("GCONT.A.T.Percent.mapped.on.Target",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.Percent.mapped.on.Target",height="60vh",width="80vw")),
                        tabPanel("Coverage",
                            plotlyOutput("GCONT.A.T.Coverage",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.Coverage",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered1xOrHigher",
                            plotlyOutput("GCONT.A.T.pBasesCovered1xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.pBasesCovered1xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered10xOrHigher",
                            plotlyOutput("GCONT.A.T.pBasesCovered10xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.pBasesCovered10xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered25xOrHigher",
                            plotlyOutput("GCONT.A.T.pBasesCovered25xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.pBasesCovered25xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered50xOrHigher",
                            plotlyOutput("GCONT.A.T.pBasesCovered50xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.A.R.pBasesCovered50xOrHigher",height="60vh",width="80vw"))
                        
                    )
                )
            ),
            
            tabPanel("GCONT Analysis B - spike-in vs 8 minutes",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("PF.Reads",
                            plotlyOutput("GCONT.B.T.PF.Reads",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.PF.Reads",height="60vh",width="80vw")),
                        tabPanel("Map.Percent",
                            plotlyOutput("GCONT.B.T.Map.Percent",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.Map.Percent",height="60vh",width="80vw")),
                        tabPanel("Percent.mapped.on.Target",
                            plotlyOutput("GCONT.B.T.Percent.mapped.on.Target",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.Percent.mapped.on.Target",height="60vh",width="80vw")),
                        tabPanel("Coverage",
                            plotlyOutput("GCONT.B.T.Coverage",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.Coverage",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered1xOrHigher",
                            plotlyOutput("GCONT.B.T.pBasesCovered1xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.pBasesCovered1xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered10xOrHigher",
                            plotlyOutput("GCONT.B.T.pBasesCovered10xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.pBasesCovered10xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered25xOrHigher",
                            plotlyOutput("GCONT.B.T.pBasesCovered25xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.pBasesCovered25xOrHigher",height="60vh",width="80vw")),
                        tabPanel("pBasesCovered50xOrHigher",
                            plotlyOutput("GCONT.B.T.pBasesCovered50xOrHigher",height="60vh",width="80vw"),
                            plotlyOutput("GCONT.B.R.pBasesCovered50xOrHigher",height="60vh",width="80vw"))
                        
                        
                    )
                )
            )
            
        )
    )
)
