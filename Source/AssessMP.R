# -----------------------------------------------------------------------------
# Code to assess the MP behaviour outside of full MSE
# -----------------------------------------------------------------------------
# This code takes a user supplied Catch and CPUE series and uses it to run 
# the MP against that data to evaluate the MP decision making.
# -----------------------------------------------------------------------------

require(keep)

MP_Interval     <- 1
firstMPYr       <- 2020
CatchAndCPUEcsv <- "./Source/test.csv"
SourcePath      <- "./MPs/PTTMB/MPs_TMBMSY_tidied.R"
MP_Name         <- "PTBoB0Targ.t15.pr15"
nfleets         <- 1

# source the MP
source(SourcePath)
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
y               <- firstMPYr
MP_environment  <- rlang::new_environment()
TAC             <- 0.0
TAE             <- karray(rep(0, nfleets), dim=c(nfleets))
TACE            <- list(TAEbyF=TAE, TAC=TAC)

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

  print(pset)
  print("\n")

  # Call MP with pset set of data
  TACE <- MP(pset)

  print(TACE)
  print("\n")

  y <- y + MP_Interval
}

