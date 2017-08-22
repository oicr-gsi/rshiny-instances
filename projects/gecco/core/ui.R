library(shiny)

shinyUI(
    navbarPage(theme="/home/ubuntu/git/rshiny-instances/projects/gecco/core/www/styles.css",
        "GECCO shiny server - core QC",
        tabPanel("About",
            helpText(
                h2("GECCO shiny server - core QC"),
                h3("Overview"),
                "This is the landing page for interactive visualization and reporting of QC stats for the GECCO project.",
                h3("Modules"),
                h4("Run Reports"),
                "library-level stats compiled from html run reports",
                h4("Cumulative Reports"),
                "sample-level stats compiled from project cumulative report" 
            )  
        ),
        navbarMenu("Modules",
            tabPanel("Run Reports",
                source("/home/ubuntu/git/rshiny-instances/projects/gecco/core/modules/run_reports/run_reports.ui.R")
            ),
            tabPanel("Cumulative Reports",
                source("/home/ubuntu/git/rshiny-instances/projects/gecco/core/modules/cumulative_reports/cumulative_reports.ui.R")
            )
        )
    )
)

