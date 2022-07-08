library(keep)
library(shiny)
library(ggplot2)

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

# source the MP code
source("./Source/AssessMP.R")

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

# Define server logic ----
server <- function(input, output)
{
  check <- function(input)
  {
    if (is.null(input$file)          || 
        is.null(input$file$datapath) ||
        (nchar(input$file$datapath) <= 0))
    {
      return("please select a Catch and CPUE history .csv file")
    }

    CatchAndCPUE <- read.csv(input$file$datapath, header = TRUE)
    Names        <- names(CatchAndCPUE)

    # Check for Year column
    if (!any(Names == "Year"))
    {
      return("Year Column missing (year)")
    }

    # Check for Catch column
    if (!any(Names == "Catch"))
    {
      return("Catch Column missing (observed catch)")
    }

    # Check for CPUE column
    if (!any(Names == "CPUE"))
    {
      return("CPUE Column missing (observed CPUE)")
    }

    # Check for TAC column
    if (!any(Names == "TAC"))
    {
      return("TAC Column missing (historic TAC)")
    }

    # Check for ascending contiguous years
    MinYear <- min(CatchAndCPUE$Year)
    MaxYear <- max(CatchAndCPUE$Year)
    y       <- MinYear

    for (idx in 1:length(CatchAndCPUE$Year))
    {
      if (CatchAndCPUE$Year[idx] != y)
      {
        return("Year must be in contiguous ascending years")
      }

      y <- y + 1
    }

    return (NULL)
  }
  
  data <- reactive({
    validate(
      check(input)
    )

    CatchAndCPUE <- read.csv(input$file$datapath, header = TRUE)
    results      <- assessMP(MP_Name, MP_SourcePath, input$file$datapath, MP_Interval, MP_theta)

    return (list(CE=CatchAndCPUE, TAC=results$TAC, B=results$B, Depletion=results$Depletion, q=results$q, plots=results$plots))
  })

  graphWidth <- reactive({
    input$PageWidth * 0.5
  })

  graphWidth2 <- reactive({
    input$PageWidth * 0.25
  })

  graphHeight <- reactive({
    input$PageHeight * 0.35
  })

  output$dummy <- reactive({
    if (!is.null(input$file))
    {
      Data      <- data()
      TAC_point <- data.frame(Year=max(Data$CE$Year) + 1, Catch=Data$TAC, Label="TAC")
      colors    <- c("Catch"="darkblue", "CPUE"="blueviolet", "TAC"="darkcyan")

      output$cobsPlot <- renderPlot({
        ggplot(Data$CE, aes(x=Year, y=Catch)) +
          geom_line(mapping=aes(x=Year, y=Catch, color="Catch"), alpha=0.5, size=2) +
          geom_line(mapping=aes(x=Year, y=TAC, color="TAC"), alpha=0.5, size=2) +
          geom_point(data=TAC_point, color="black", shape=5, size=6, mapping=aes(x=Year, y=Catch)) + 
          geom_text(data=TAC_point, color="black", size=5, nudge_x=2.5, mapping=aes(x=Year, y=Catch, label=Label)) + 
          labs(x="Year", y="Catch", color="") +
          scale_color_manual(values=colors) + 
          theme_bw()
        },
        width=graphWidth,
        height=graphHeight)

      output$iobsPlot <- renderPlot({
          ggplot(Data$CE, aes(x=Year, y=CPUE)) +
          geom_line(mapping=aes(x=Year, y=CPUE, color="CPUE"), alpha=0.5, size=2) +
          labs(x="Year", y="CPUE", color="") +
          scale_color_manual(values=colors) + 
          theme_bw()
        },
        width=graphWidth,
        height=graphHeight)

      output$biomassPlot <- renderPlot({
          Data$plots[["biomass_plot"]]
        },
        width=graphWidth2,
        height=graphHeight)

      output$depletionPlot <- renderPlot({
          Data$plots[["depletion_plot"]]
        },
        width=graphWidth2,
        height=graphHeight)

      output$recDevPlot <- renderPlot({
          Data$plots[["recDev_plot"]]
        },
        width=graphWidth2,
        height=graphHeight)

      output$prodPlot <- renderPlot({
          Data$plots[["prod_plot"]]
        },
        width=graphWidth2,
        height=graphHeight)

      output$TAC <- renderUI({
        HTML(sprintf("<b>Recommended TAC:</b> %3g", Data$TAC))
      })
    }

    return ("")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

