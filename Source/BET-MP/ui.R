library(shiny)

# Define UI ----
ui <- fluidPage(

  # App title 
  titlePanel("IOTC Big Eye Tuna Management Procedure"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      p(strong("MP:"), MP_Name),
      p(strong("Tuning Objective:"), TuningObj),
      fileInput("file", 
                h3("Catch, CPUE and TAC file"),
                accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")),
      textOutput("dummy"),
      tags$head(tags$script('
                            var PageWidth  = 0;
                            var PageHeight = 0;
                            $(document).on("shiny:connected", function(e) {
                                PageWidth  = window.innerWidth;
                                PageHeight = window.innerHeight;
                                Shiny.setInputValue("PageWidth", PageWidth);
                                Shiny.setInputValue("PageHeight", PageHeight);
                            });
                            $(window).resize(function(e) {
                                PageWidth  = window.innerWidth;
                                PageHeight = window.innerHeight;
                                Shiny.setInputValue("PageWidth", PageWidth);
                                Shiny.setInputValue("PageHeight", PageHeight);
                            });
                        ')),

      width=3                           
    ),

    # Main panel for outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Recommendation",
          plotOutput(outputId = "cobsPlot", inline=TRUE),
          plotOutput(outputId = "iobsPlot", inline=TRUE),
          htmlOutput("TAC")
        ),
        tabPanel("Pella-Tomlinson Model Diagnostics",
          fluidRow(
            column(6, 
              plotOutput(outputId = "biomassPlot", inline=TRUE)
            ),
            column(6, 
              plotOutput(outputId = "depletionPlot", inline=TRUE)
            )
          ),
          fluidRow(
            column(6, 
              plotOutput(outputId = "recDevPlot", inline=TRUE)
            ),
            column(6, 
              plotOutput(outputId = "prodPlot", inline=TRUE)
            )
          )
        )
      ),
      width=9
    )
  )
)
