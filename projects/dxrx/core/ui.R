library(shiny)

shinyUI(
    navbarPage(theme="/home/ubuntu/git/rshiny-instances/projects/dxrx/core/www/styles.css",
        "DxRx shiny server - core QC",
        tabPanel("About",
            helpText(
                h2("DxRx shiny server - core QC"),
                h3("Overview"),
                "This is the landing page for interactive visualization and reporting of QC stats for the DxRx project.",
                h3("Modules"),
                h4("Run Reports"),
                "library-level stats compiled from html run reports",
                h4("Cumulative Reports"),
                "sample-level stats compiled from project cumulative report" 
            )  
        ),
        navbarMenu("Modules",
            tabPanel("Run Reports",
                source("/home/ubuntu/git/rshiny-instances/projects/dxrx/core/modules/run_reports/run_reports.ui.R")
            ),
            tabPanel("Cumulative Reports",
                source("/home/ubuntu/git/rshiny-instances/projects/dxrx/core/modules/cumulative_reports/cumulative_reports.ui.R")
            )
        )
    )
)

