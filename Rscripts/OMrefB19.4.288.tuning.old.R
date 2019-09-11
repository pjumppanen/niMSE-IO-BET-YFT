# -----------------------------------------------------------------------------
# gridB19.4 tuned using MB18.1,MB18.2,MB18.3 model based tuning specifications
# -----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

source("Source/MseMain.R")
source("OMconditioning/RStuff/plotIndices.f.R")

# For comparison: Langley 2016 BET assessment ref case (provided by Dan Fu with nod from Adam) is a grid of 6
# So these figs are not strictly correct...
loadSSModel("TagLambda1", "Z:/MSE-IO-BET-YFT/OMconditioning/BET/AssessmentFiles2017/", force=FALSE)

# load model definition, OMrefB19.4.288
load("Objects/OMrefB19.4.288.RDA")

# set up some plot related variables
FLim   <- OMrefB19.4.288@MseDef@Flim
SBLim  <- OMrefB19.4.288@MseDef@SBlim

FTarg  <- 1.
SBTarg <- 1.

# Tuning objective 2018 TCMP BET1 - TCMP.B18.1 ...might need relaxed TAC constraint...
TCMP.B18.1 <- new("TuningParameters")
TCMP.B18.1@performanceMeasure       <- "GK"
TCMP.B18.1@performanceMeasureClass  <- "mean"
TCMP.B18.1@performanceMeasureYears  <- c(2030,2034)
TCMP.B18.1@tuningTarget             <- 0.5
TCMP.B18.1@tuningTolerance          <- 0.01
TCMP.B18.1@tuningLogDomain          <- c(-4,4)

# Tuning objective 2018 TCMP BET2 - TCMP.B18.2
TCMP.B18.2 <- new("TuningParameters")
TCMP.B18.2@performanceMeasure       <- "GK"
TCMP.B18.2@performanceMeasureClass  <- "mean"
TCMP.B18.2@performanceMeasureYears  <- c(2030,2034)
TCMP.B18.2@tuningTarget             <- 0.6
TCMP.B18.2@tuningTolerance          <- 0.01
TCMP.B18.2@tuningLogDomain          <- c(-4,4)  #(-2,2) not broad enough for some IT MPs

# Tuning objective 2018 TCMP BET3 - TCMP.B18.3
TCMP.B18.3 <- new("TuningParameters")
TCMP.B18.3@performanceMeasure       <- "GK"
TCMP.B18.3@performanceMeasureClass  <- "mean"
TCMP.B18.3@performanceMeasureYears  <- c(2030,2034)
TCMP.B18.3@tuningTarget             <- 0.7
TCMP.B18.3@tuningTolerance          <- 0.01
TCMP.B18.3@tuningLogDomain          <- c(-4,4)

MPL <- c("PT41.t15")

print(system.time(OMrefB19.4.288.tuned.1 <- runMse(OMrefB19.4.288, TuningPars=TCMP.B18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.4.288.tuned.1, file="Objects/OMrefB19.4.288.tuned.1.RDA")
load(file="Objects/OMrefB19.4.288.tuned.1.RDA")

MPs <- getMPs(OMrefB19.4.288.tuned.1)

MPs[[1]]@tuneError

tunedMP.MB18.1 <- MPs[[1]]
save(tunedMP.MB18.1, file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MB18.1.RDA")

betPlots.f(OMrefB19.4.288.tuned.1,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

rm(OMrefB19.4.288.tuned.1)
gc()

print(system.time(OMrefB19.4.288.tuned.2 <- runMse(OMrefB19.4.288, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.4.288.tuned.2, file="Objects/OMrefB19.4.288.tuned.2.RDA")
load(file="Objects/OMrefB19.4.288.tuned.2.RDA")

MPs <- getMPs(OMrefB19.4.288.tuned.2)

MPs[[1]]@tuneError

tunedMP.MB18.2 <- MPs[[1]]
save(tunedMP.MB18.2, file="Objects/tunedMP.MB18.2.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")

betPlots.f(OMrefB19.4.288.tuned.2,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

rm(OMrefB19.4.288.tuned.2)
gc()


print(system.time(OMrefB19.4.288.tuned.3 <- runMse(OMrefB19.4.288, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.4.288.tuned.3, file="Objects/OMrefB19.4.288.tuned.3.RDA")
load(file="Objects/OMrefB19.4.288.tuned.3.RDA")

MPs <- getMPs(OMrefB19.4.288.tuned.3)

MPs[[1]]@tuneError

tunedMP.MB18.3 <- MPs[[1]]
save(tunedMP.MB18.3, file="Objects/tunedMP.MB18.3.RDA")
load(file="Objects/tunedMP.MB18.3.RDA")

betPlots.f(OMrefB19.4.288.tuned.3,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

rm(OMrefB19.4.288.tuned.3)
gc()

# run tuned MPs on other grids.

# build MPs list
load(file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")
load(file="Objects/tunedMP.MB18.3.RDA")
MPL <- list(MB1=tunedMP.MB18.1, MB2=tunedMP.MB18.2, MB3=tunedMP.MB18.3)

# load model definition, OMrefB19.0
load("Objects/OMrefB19.0.RDA")

# run projections
print(system.time(OMrefB19.0.proj <- runMse(OMrefB19.0, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.0.proj, file="Objects/OMrefB19.0.proj.RDA")
load(file="Objects/OMrefB19.0.proj.RDA")

betPlots.f(OMrefB19.0.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

betPlots.f(OMrefB19.0.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87,
           outputPath="Report/", prefix="B19.0.")

createTable1(20, list(OMrefB19.0.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.0.")
createTable2(20, list(OMrefB19.0.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.0.")

#Models    <- unlist(OMrefB19.0.proj@MseDef@OMList)
#SSRootDir <- "Z:/MSE-IO-BET-YFT/OMconditioning/BET/gridB19.0/"
#
#plotIndices.f(modList = Models, SSRootDir=SSRootDir)

dt <- msevizPerformanceData(OMrefB19.0.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.0.", colourPalette=rainbow(10, s=0.6))

rm(OMrefB19.0.proj, OMrefB19.0)
gc()

#SSRootDir <- "Z:/MSE-IO-BET-YFT/OMconditioning/BET/gridB19.3/"

# load model definition, OMrefB19.2.288
load("Objects/OMrefB19.2.288.RDA")

# run projections
print(system.time(OMrefB19.2.288.proj <- runMse(OMrefB19.2.288, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.2.288.proj, file="Objects/OMrefB19.2.288.proj.RDA")
load(file="Objects/OMrefB19.2.288.proj.RDA")

betPlots.f(OMrefB19.2.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

betPlots.f(OMrefB19.2.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87,
           outputPath="Report/", prefix="B19.2.")

createTable1(20, list(OMrefB19.2.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.2.288.")
createTable2(20, list(OMrefB19.2.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.2.288.")

#Models <- unlist(OMrefB19.2.288.proj@MseDef@OMList)
#
#plotIndices.f(modList = Models, SSRootDir=SSRootDir)

dt <- msevizPerformanceData(OMrefB19.2.288.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.2.", colourPalette=rainbow(10, s=0.6))

rm(OMrefB19.2.288.proj, OMrefB19.2.288)

gc()

# load model definition, OMrefB19.3.288
load("Objects/OMrefB19.3.288.RDA")

# run projections
print(system.time(OMrefB19.3.288.proj <- runMse(OMrefB19.3.288, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.3.288.proj, file="Objects/OMrefB19.3.288.proj.RDA")
load(file="Objects/OMrefB19.3.288.proj.RDA")

betPlots.f(OMrefB19.3.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

betPlots.f(OMrefB19.3.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87,
           outputPath="Report/", prefix="B19.3.")

createTable1(20, list(OMrefB19.3.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.3.288.")
createTable2(20, list(OMrefB19.3.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.3.288.")

#Models <- unlist(OMrefB19.3.288.proj@MseDef@OMList)
#
#plotIndices.f(modList = Models, SSRootDir=SSRootDir)

dt <- msevizPerformanceData(OMrefB19.3.288.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.3.", colourPalette=rainbow(10, s=0.6))

rm(OMrefB19.3.288.proj, OMrefB19.3.288)
gc()

# load model definition, OMrefB19.4.288
load("Objects/OMrefB19.4.288.RDA")

# run projections
print(system.time(OMrefB19.4.288.proj <- runMse(OMrefB19.4.288, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.4.288.proj, file="Objects/OMrefB19.4.288.proj.RDA")
load(file="Objects/OMrefB19.4.288.proj.RDA")

betPlots.f(OMrefB19.4.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

betPlots.f(OMrefB19.4.288.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87,
           outputPath="Report/", prefix="B19.4.")

createTable1(20, list(OMrefB19.4.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.4.288.")
createTable2(20, list(OMrefB19.4.288.proj), c("MB1", "MB2", "MB3"), c("MB1", "MB2", "MB3"), prefix="OMrefB19.4.288.")

dt <- msevizPerformanceData(OMrefB19.4.288.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.4.", colourPalette=rainbow(10, s=0.6))

#Models <- unlist(OMrefB19.4.288.proj@MseDef@OMList)
#
#plotIndices.f(modList = Models, SSRootDir=SSRootDir)

rm(OMrefB19.4.288.proj, OMrefB19.4.288)
gc()


# -----------------------------------------
# Create robustnest case for OMrefB19.4.288
# -----------------------------------------
# load model definition, OMrefB19.4.288
load("Objects/OMrefB19.4.288.RDA")

# set robustness parameters


#save(SSModelCache, file="Objects/ss.model.cache.RDA")
#load(file="Objects/ss.model.cache.RDA")
#
#source("OMconditioning/RStuff/importGrid2.f.R")
#load("Objects/OMgridB19.0.List.RDA")
#
#importGrid.f(gridList=names(OMgridB19.0.List), gridDir="Z:/MSE-IO-BET-YFT/OMconditioning/BET/gidB19.0/", convergedNum=3, covar=F, stdOnly=F, doLoad=F)
#
#source("Rscripts/Build OM Model-OMrefB19.0.288.R")
#
# Create an MseFramework object for the MseDefinition
#print(system.time(OMrefB19.0 <- createMseFramework(MseDef, UseCluster=1)))
#save(OMrefB19.0, file="Objects/OMrefB19.0.RDA")


source("Rscripts/Build OM Model-OMrefB19.5.288.R")
print(system.time(OMrefB19.5 <- createMseFramework(MseDef, UseCluster=1)))
save(OMrefB19.5, file="Objects/OMrefB19.5.RDA")
