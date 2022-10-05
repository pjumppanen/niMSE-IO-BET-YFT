library(shiny)

# MP name, tuning objective, tuning parameter
# "PT41F.t15.tmb","B2",3.718027
# "PT41F.t15.tmb","B3",3.405858
# "PTBoB0Targ.t15","B2",1.218814
# "PTBoB0Targ.t15","B3",1.295104

MP_SourcePath <- "./MPs/PTTMB/MPs_TMBMSY_tidied.R"
MP_Name       <- "PT41F.t15.tmb"
TuningObj     <- "B2"
MP_theta      <- 3.718027
MP_Interval   <- 1

# Define UI ----
ui <- fluidPage(

  # App title 
  titlePanel("IOTC Big Eye Tuna Management Procedure"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
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
          htmlOutput("TAC"),
          plotOutput(outputId = "cobsPlot", inline=TRUE)
        ),
        tabPanel("Model Diagnostics",
          plotOutput(outputId = "cpuePlot", inline=TRUE)
        )
      ),
      width=9
    )
  )
)
