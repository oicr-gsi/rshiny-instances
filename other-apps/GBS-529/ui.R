library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
    navbarPage(
        "GBS-529 - Assess BART coverage over target regions",
        tabPanel("About"
        ),
        navbarMenu("Projects Analyzed",
            tabPanel("BART TS",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("1x" , plotlyOutput("bart_render_plot_1x" , height="80vh" , width="100vw")),
                        tabPanel("5x" , plotlyOutput("bart_render_plot_5x" , height="80vh" , width="100vw")),
                        tabPanel("10x" , plotlyOutput("bart_render_plot_10x" , height="80vh" , width="100vw")),
                        tabPanel("25x" , plotlyOutput("bart_render_plot_25x" , height="80vh" , width="100vw")),
                        tabPanel("50x" , plotlyOutput("bart_render_plot_50x" , height="80vh" , width="100vw")),
                        tabPanel("100x" , plotlyOutput("bart_render_plot_100x" , height="80vh" , width="100vw")),
                        tabPanel("250x" , plotlyOutput("bart_render_plot_250x" , height="80vh" , width="100vw")),
                        tabPanel("500x" , plotlyOutput("bart_render_plot_500x" , height="80vh" , width="100vw")),
                        tabPanel("1000x" , plotlyOutput("bart_render_plot_1000x" , height="80vh" , width="100vw")),
                        tabPanel("2500x" , plotlyOutput("bart_render_plot_2500x" , height="80vh" , width="100vw")),
                        tabPanel("5000x" , plotlyOutput("bart_render_plot_5000x" , height="80vh" , width="100vw"))
                    )
                )
            ),
            
            tabPanel("MA5 TS",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("1x" , plotlyOutput("ma5_render_plot_1x" , height="80vh" , width="100vw")),
                        tabPanel("5x" , plotlyOutput("ma5_render_plot_5x" , height="80vh" , width="100vw")),
                        tabPanel("10x" , plotlyOutput("ma5_render_plot_10x" , height="80vh" , width="100vw")),
                        tabPanel("25x" , plotlyOutput("ma5_render_plot_25x" , height="80vh" , width="100vw")),
                        tabPanel("50x" , plotlyOutput("ma5_render_plot_50x" , height="80vh" , width="100vw")),
                        tabPanel("100x" , plotlyOutput("ma5_render_plot_100x" , height="80vh" , width="100vw")),
                        tabPanel("250x" , plotlyOutput("ma5_render_plot_250x" , height="80vh" , width="100vw")),
                        tabPanel("500x" , plotlyOutput("ma5_render_plot_500x" , height="80vh" , width="100vw")),
                        tabPanel("1000x" , plotlyOutput("ma5_render_plot_1000x" , height="80vh" , width="100vw")),
                        tabPanel("2500x" , plotlyOutput("ma5_render_plot_2500x" , height="80vh" , width="100vw")),
                        tabPanel("5000x" , plotlyOutput("ma5_render_plot_5000x" , height="80vh" , width="100vw"))
                    )
                )
            ),
            
            tabPanel("MCNT TS",
                mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("1x" , plotlyOutput("mcnt_render_plot_1x" , height="80vh" , width="100vw")),
                        tabPanel("5x" , plotlyOutput("mcnt_render_plot_5x" , height="80vh" , width="100vw")),
                        tabPanel("10x" , plotlyOutput("mcnt_render_plot_10x" , height="80vh" , width="100vw")),
                        tabPanel("25x" , plotlyOutput("mcnt_render_plot_25x" , height="80vh" , width="100vw")),
                        tabPanel("50x" , plotlyOutput("mcnt_render_plot_50x" , height="80vh" , width="100vw")),
                        tabPanel("100x" , plotlyOutput("mcnt_render_plot_100x" , height="80vh" , width="100vw")),
                        tabPanel("250x" , plotlyOutput("mcnt_render_plot_250x" , height="80vh" , width="100vw")),
                        tabPanel("500x" , plotlyOutput("mcnt_render_plot_500x" , height="80vh" , width="100vw")),
                        tabPanel("1000x" , plotlyOutput("mcnt_render_plot_1000x" , height="80vh" , width="100vw")),
                        tabPanel("2500x" , plotlyOutput("mcnt_render_plot_2500x" , height="80vh" , width="100vw")),
                        tabPanel("5000x" , plotlyOutput("mcnt_render_plot_5000x" , height="80vh" , width="100vw"))
                    )
                )
            )
            
        )
    )
)
        