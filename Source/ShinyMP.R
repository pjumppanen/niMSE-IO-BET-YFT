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
      p(strong("MP:"), MP_Name),
      p(strong("Tuning Objective:"), TuningObj),
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
      tabsetPanel(
          tabPanel("Recommendation",
            plotOutput(outputId = "cobsPlot", inline=TRUE),
            plotOutput(outputId = "iobsPlot", inline=TRUE),
            tableOutput('table'),
          ),
          tabPanel("Model Fit")
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

    return (list(CE=CatchAndCPUE, TAC=results$TAC, B=results$B, Depletion=results$Depletion, q=results$q))
  })

  graphWidth <- reactive({
    input$PageWidth * 0.5
    })

  graphHeight <- reactive({
    input$PageHeight * 0.35
    })

  observeEvent(input$go, 
    {
      Data <- data()
      TAC_point <- data.frame(y=max(Data$CE$y) + 1, Cobs=Data$TAC, Label="TAC")
      performance_data <- data.frame(TAC=sprintf("%3g", Data$TAC), Depletion=sprintf("%3g", Data$Depletion), B=sprintf("%3g", Data$B), q=sprintf("%3g", Data$q))

      output$cobsPlot <- renderPlot({
      ggplot(Data$CE, aes(x=y, y=Cobs)) +
        geom_line(color="darkblue", size=2, alpha=0.9) +
        geom_point(data=TAC_point, color="black", shape=18, size=6, mapping=aes(x=y, y=Cobs)) + 
        geom_text(data=TAC_point, color="black", size=5, nudge_x=2.5, mapping=aes(x=y, y=Cobs, label=Label)) + 
        theme_bw() +
        xlab("Year") + 
        ylab("Catch")
      },
      width=graphWidth,
      height=graphHeight)

      output$iobsPlot <- renderPlot({
        ggplot(Data$CE, aes(x=y, y=Iobs)) +
        geom_line( color="darkred", size=2, alpha=0.9) +
        theme_bw() +
        xlab("Year") + 
        ylab("CPUE")
      },
      width=graphWidth,
      height=graphHeight)

      output$table <- renderTable(performance_data)
    })

}

# Run the app ----
shinyApp(ui = ui, server = server)

