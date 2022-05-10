# -----------------------------------------------------------------------------
# Code to assess the MP behaviour outside of full MSE
# -----------------------------------------------------------------------------
# This code takes a user supplied Catch and CPUE series and uses it to run 
# the MP against that data to evaluate the MP decision making.
# -----------------------------------------------------------------------------

require(keep)

# -----------------------------------------------------------------------------
# assessMP
# -----------------------------------------------------------------------------
# this function applies the designated MP to the catch and cpue series data
# in the supplied csv file and measures performance statistics which are 
# returned in an environment. The reported statistics are:
#
#   TAC
#   B
#   Depletion
#   q
# 
# Typical example usage:
#
# source("./Source/AssessMP.R")
#
# MP_Interval     <- 1
# firstMPYr       <- 2020
# CatchAndCPUEcsv <- "./Source/test.csv"
# MP_SourcePath   <- "./MPs/PTTMB/MPs_TMBMSY_tidied.R"
# MP_Name         <- "PTBoB0Targ.t15.pr15"
# 
# results <- assessMP(MP_Name, MP_SourcePath, CatchAndCPUEcsv, firstMPYr, MP_Interval)
#
# print(results$TAC)
# print(results$B)
# print(results$Depletion)
# print(results$q)
#
# -----------------------------------------------------------------------------
assessMP <- function(MP_Name, MP_SourcePath, CatchAndCPUEcsv, firstMPYr, MP_Interval)
{
  # source the MP
  source(MP_SourcePath)
  source('./Source/pasteOperator.R')

  # make sure the MP is compiled if it is a c++ based one
  MP          <- get(MP_Name)
  BSysProject <- attr(MP, "BSysProject")

  if (!is.null(BSysProject))
  {
    BSysProject <- make(BSysProject)
    loadLibrary(BSysProject)
  }

  CatchAndCPUE <- read.csv(CatchAndCPUEcsv)
  Names        <- names(CatchAndCPUE)

  # Check for y column
  if (!any(Names == "y"))
  {
    stop("y Column missing (year)")
  }

  # Check for Cobs column
  if (!any(Names == "Cobs"))
  {
    stop("Cobs Column missing (observed catch)")
  }

  # Check for Cobs column
  if (!any(Names == "Iobs"))
  {
    stop("Iobs Column missing (observed CPUE)")
  }

  # Check for ascending contiguous years
  MinYear <- min(CatchAndCPUE$y)
  MaxYear <- max(CatchAndCPUE$y)
  y       <- MinYear

  for (idx in 1:length(CatchAndCPUE$y))
  {
    if (CatchAndCPUE$y[idx] != y)
    {
      stop("y must be in contiguous ascending years")
    }

    y <- y + 1
  }

  # Check MP_interval
  if (MP_Interval < 1)
  {
    stop("MP_Interval must be >= 1")
  }

  # Evaluate selected MP over all available MP years
  nfleets                   <- 1
  y                         <- firstMPYr
  MP_environment            <- rlang::new_environment()
  MP_environment$TAC        <- c()
  MP_environment$B          <- c()
  MP_environment$Depletion  <- c()
  MP_environment$q          <- c()
  TAC                       <- 0.0
  TAE                       <- karray(rep(0, nfleets), dim=c(nfleets))
  TACE                      <- list(TAEbyF=TAE, TAC=TAC)

  while (y <= MaxYear)
  {
    selection <- 1:which(y==CatchAndCPUE$y)

    pset <- list(y=y - MinYear + 1,
                Cobs=CatchAndCPUE$Cobs[selection],
                Iobs=CatchAndCPUE$Iobs[selection],
                tune=1.0,
                MSY=0,
                prevTACE = TACE,
                interval=MP_Interval,
                MP_environment=MP_environment)

    # Call MP with pset set of data
    TACE <- MP(pset)

    y <- y + MP_Interval
  }

  return (MP_environment)
}
