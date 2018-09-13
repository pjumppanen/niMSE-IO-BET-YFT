cat("Installing and loading needed libraries")
cat("\n")
# Not actually used but C++ interface files require Oarray in the case of ADT arrays
# with starting indices other than 1
if(!require("Oarray"))install.packages("Oarray",repos="https://cloud.r-project.org")
if(!require("keep"))install.packages("keep",repos="https://cloud.r-project.org")
if(!require("stringr"))install.packages("stringr",repos="https://cloud.r-project.org")
if(!require("r4ss"))install.packages("r4ss",repos="https://cloud.r-project.org")
if(!require("ggplot2"))install.packages("ggplot2",repos="https://cloud.r-project.org")
if(!require("reshape2"))install.packages("reshape2",repos="https://cloud.r-project.org")
if(!require("parallel"))install.packages("parallel",repos="https://cloud.r-project.org")
if(!require("abind"))install.packages("abind",repos="https://cloud.r-project.org")

library(parallel)
library(abind)
library(stringr)
library(data.table)
library(mseviz)
library(ggstance)

# load niMseom module and R interface code
# Load the library
if (version$os == "mingw32")
{
  # Running in Windows
  LibName      <- "niMseom"
  LibExtension <- ".dll"

  if (version$arch == "i386")
  {
    LibFolder  <- "win32/"

  } else
  {
    LibFolder  <- "x64/"
  }

} else
{
  # Running in Linux
  LibName      <- "libniMseom"
  LibExtension <- ".so"
  LibFolder    <- "linux/"
}


# Only load library if not already loaded. Loading more than once results in
# R mis-behaving and crashing
if (is.na(match(LibName,  attr(getLoadedDLLs(), "names"))))
{
  LibPath <- paste("./lib/", LibFolder, LibName, LibExtension, sep="")
  dyn.load(LibPath)
}


source("./lib/OmB_R_interface.r")
source("./lib/Om_R_interface.r")


source("Source/seasAsYrToDecYr.f.R")
source("Source/pasteOperator.R")
source("Source/MPs.R")
source("Source/MseDefinition.R")
source("Source/StockSynthesisModelData.R")
source("Source/ReferenceVars.R")
source("Source/Projection.R")
source("Source/ManagementVars.R")
source("Source/MseFramework.R")
source("Source/StockSynthesisModel.R")
source("Source/mseviz2.R")
source("Source/utilities.R")
source("Source/clusterLogging.R")
