library(keep)
library(shiny)
library(ggplot2)
library(reshape2)
library(patchwork)

# Set the current working directory to where the 
# niMSE-IO-BET-YFT project code is located
setwd("/usr/niMSE-IO-BET-YFT/niMSE-IO-BET-YFT")

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
    results      <- assessMP(MP_Name, MP_SourcePath, input$file$datapath, MP_Interval, MP_theta, Build=FALSE)

    return (list(CE=CatchAndCPUE, TAC=results$TAC, B=results$B, Depletion=results$Depletion, q=results$q, plots=results$plots))
  })

  graphWidthCatch <- reactive({
    input$PageWidth * 0.43 + 58
  })

  graphWidthCPUE <- reactive({
    input$PageWidth * 0.43
  })

  graphHeight <- reactive({
    input$PageHeight * 0.66
  })

  graphWidthDiag <- reactive({
    input$PageWidth * 0.5
  })

  graphHeightDiag <- reactive({
    input$PageHeight * 0.70
  })

  output$dummy <- reactive({
    if (!is.null(input$file))
    {
      Data <- data()

      output$cobsPlot <- renderPlot({
          data <- Data$CE
          data <- rbind(data, list(Year=max(data$Year) + 1, CPUE=NA, Catch=NA, TAC=NA))

          RecommendedTAC                          <- rep(NA, nrow(data))
          RecommendedTAC[length(RecommendedTAC)]  <- Data$TAC
          data                                    <- cbind(data, "Recommended TAC"=RecommendedTAC)

          data_melt <- reshape2::melt(data[, c("Year", "Catch", "TAC", "Recommended TAC")], id.vars='Year', value.name='Catch')
          colors    <- c("Catch"="#00345D", "TAC"="#00A9CE", "Recommended TAC"="#000080")
          shapes    <- c("Catch"=NA,        "TAC"=NA,        "Recommended TAC"=1)
          types     <- c("Catch"=1,         "TAC"=1,         "Recommended TAC"=0)

          g1  <- ggplot(data_melt, aes(x=Year, y=Catch/1000, linetype=variable, color=variable, shape=variable)) +
            geom_line(size=2) +
            geom_point(size=6) + 
            scale_linetype_manual(values=types) + 
            scale_shape_manual(values=shapes) + 
            scale_color_manual(values=colors) + 
              scale_y_continuous(limits=c(0, 1.2*max(data_melt$Catch/1000)), expand=c(0, 0)) +
              xlab('') + 
              ylab('Catch (x 1000)\n ') + 
            theme_bw() + 
              theme(legend.title=element_blank())

          data_melt <- reshape2::melt(Data$CE[, c("Year", "CPUE")], id.vars='Year', value.name='CPUE')
          colors    <- c("CPUE"="#00345D")

          g2  <-  ggplot(data_melt, aes(x=Year, y=CPUE, color=variable)) +
            geom_line(size=2) +
            scale_color_manual(values=colors) + 
            scale_y_continuous(limits=c(0, NA)) + 
              theme_bw() +
              xlab(' \n Year') + 
            theme(legend.title=element_blank())          

          g1 + g2 + plot_layout(ncol=1)
        },
        width=graphWidthCatch,
        height=graphHeight)

      output$iobsPlot <- renderPlot({
          data_melt <- reshape2::melt(Data$CE[, c("Year", "CPUE")], id.vars='Year', value.name='CPUE')
          colors    <- c("CPUE"="#00345D")

          ggplot(data_melt, aes(x=Year, y=CPUE, color=variable)) +
            geom_line(size=2) +
            scale_color_manual(values=colors) + 
              scale_y_continuous(limits=c(0, NA)) +
              xlab(' \n Year') + 
              ylab('CPUE \n ') + 
            theme_bw() + 
            theme(legend.title=element_blank())
        },
        width=graphWidthCPUE,
        height=graphHeight)

      output$cpuePlot <- renderPlot({
          Data$plots[["cpue_plot"]]
        },
        width=graphWidthDiag,
        height=graphHeightDiag)

      output$TAC <- renderUI({
        HTML(sprintf("<H3 class='well' style='background-color:#001a76;border-color:#000f43;color:#fff;'>Recommended TAC: %3g</H3>", Data$TAC))
      })
    }

    return ("")
  })
}
