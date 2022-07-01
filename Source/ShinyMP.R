library(keep)
library(shiny)
library(ggplot2)

# MP name, tuning objective, tuning parameter
# "PT41F.t15.tmb","B2",3.718027
# "PT41F.t15.tmb","B3",3.405858
# "PTBoB0Targ.t15","B2",1.218814
# "PTBoB0Targ.t15","B3",1.295104

MP_SourcePath <- "./MPs/PTTMB/MPs_TMBMSY_tidied.R"
MP_Name       <- "PTBoB0Targ.t15"
TuningObj     <- "B3"
MP_theta      <- 1.295104
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
      fileInput("file", 
                h3("Catch and CPUE file"),
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      numericInput("lastTAC",
                   "Last TAC",
                   value=NA,
                   min=10.0),
      actionButton("go", "Find TAC"),
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
      plotOutput(outputId = "cobsPlot", inline=TRUE),
      plotOutput(outputId = "iobsPlot", inline=TRUE),
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

    # Check for y column
    if (!any(Names == "y"))
    {
      return("y Column missing (year)")
    }

    # Check for Cobs column
    if (!any(Names == "Cobs"))
    {
      return("Cobs Column missing (observed catch)")
    }

    # Check for Cobs column
    if (!any(Names == "Iobs"))
    {
      return("Iobs Column missing (observed CPUE)")
    }

    # Check for ascending contiguous years
    MinYear <- min(CatchAndCPUE$y)
    MaxYear <- max(CatchAndCPUE$y)
    y       <- MinYear

    for (idx in 1:length(CatchAndCPUE$y))
    {
      if (CatchAndCPUE$y[idx] != y)
      {
        return("y must be in contiguous ascending years")
      }

      y <- y + 1
    }

    # Check for last TAC
    if (is.na(input$lastTAC))
    {
      return("Enter last assessments TAC")
    }

    return (NULL)
  }
  
  data <- reactive({
    validate(
      check(input)
    )

#    browser()
    CatchAndCPUE <- read.csv(input$file$datapath, header = TRUE)
    results      <- assessMP(MP_Name, MP_SourcePath, input$file$datapath, input$lastTAC, MP_Interval, MP_theta)

    return (list(CE=CatchAndCPUE, TAC=results$TAC, B=results$B, Dpeletion=results$Depletion, q=results$q))
  })

  graphWidth <- reactive({
    input$PageWidth * 0.5
    })

  graphHeight <- reactive({
    input$PageHeight * 0.4
    })

  observeEvent(input$go, 
    {
      Data <- data()

      output$cobsPlot <- renderPlot({
      ggplot(Data$CE, aes(x=y, y=Cobs)) +
        geom_line( color="darkblue", size=2, alpha=0.9) +
        theme_bw() +
        ggtitle("Catch")
      },
      width=graphWidth,
      height=graphHeight)

      output$iobsPlot <- renderPlot({
        ggplot(Data$CE, aes(x=y, y=Iobs)) +
        geom_line( color="darkred", size=2, alpha=0.9) +
        theme_bw() +
        ggtitle("CPUE")
      },
      width=graphWidth,
      height=graphHeight)
    })

}

# Run the app ----
shinyApp(ui = ui, server = server)

