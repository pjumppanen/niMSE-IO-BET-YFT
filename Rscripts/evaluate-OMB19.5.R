# -----------------------------------------------------------------------------
# gridB19.5 tuned using MB18.1,MB18.2,MB18.3 model based tuning specifications
# -----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

source("Source/MseMain.R")

# Create gridB19.5 model definition
source("Rscripts/Build OM Model-OMrefB19.5.288.R")
print(system.time(OMrefB19.5 <- createMseFramework(MseDef, UseCluster=1)))
save(OMrefB19.5, file="Objects/OMrefB19.5.RDA")



source("OMconditioning/RStuff/plotIndices.f.R")

# For comparison: Langley 2016 BET assessment ref case (provided by Dan Fu with nod from Adam) is a grid of 6
# So these figs are not strictly correct...
loadSSModel("TagLambda1", "Z:/MSE-IO-BET-YFT/OMconditioning/BET/AssessmentFiles2017/", force=FALSE)


# load model definition, OMrefB19.5.288
load("Objects/OMrefB19.5.RDA")

# set up some plot related variables
FLim   <- OMrefB19.5@MseDef@Flim
SBLim  <- OMrefB19.5@MseDef@SBlim

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

# Model based MP
MPL <- c("PT41.t15")

print(system.time(OMrefB19.5.tuned.1 <- runMse(OMrefB19.5, TuningPars=TCMP.B18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.tuned.1, file="Objects/OMrefB19.5.tuned.1.RDA")
load(file="Objects/OMrefB19.5.tuned.1.RDA")

MPs <- getMPs(OMrefB19.5.tuned.1)

MPs[[1]]@tuneError

tunedMP.MB18.1 <- MPs[[1]]
save(tunedMP.MB18.1, file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MB18.1.RDA")

betPlots.f(OMrefB19.5.tuned.1,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.1)
gc()


print(system.time(OMrefB19.5.IT.PI <- runMse(OMrefB19.5, TuningPars=TCMP.B18.2, MPs="IT.PI.tune.15", CppMethod=1, interval=3, Report=F, UseCluster=1)))

MPs <- getMPs(OMrefB19.5.tuned.2)

MPs[[1]]@tuneError

tunedMP.MB18.2 <- MPs[[1]]
save(tunedMP.MB18.2, file="Objects/tunedMP.MB18.2.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")

betPlots.f(OMrefB19.5.tuned.2,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.2)
gc()


print(system.time(OMrefB19.5.tuned.3 <- runMse(OMrefB19.5, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.tuned.3, file="Objects/OMrefB19.5.tuned.3.RDA")
load(file="Objects/OMrefB19.5.tuned.3.RDA")

MPs <- getMPs(OMrefB19.5.tuned.3)

MPs[[1]]@tuneError

tunedMP.MB18.3 <- MPs[[1]]
save(tunedMP.MB18.3, file="Objects/tunedMP.MB18.3.RDA")
load(file="Objects/tunedMP.MB18.3.RDA")

betPlots.f(OMrefB19.5.tuned.3,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.3)
gc()

# Alternate MP
MPL <- c("IT5.t15")

print(system.time(OMrefB19.5.tuned.4 <- runMse(OMrefB19.5, TuningPars=TCMP.B18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.tuned.4, file="Objects/OMrefB19.5.tuned.4.RDA")
load(file="Objects/OMrefB19.5.tuned.4.RDA")

MPs <- getMPs(OMrefB19.5.tuned.4)

MPs[[1]]@tuneError

tunedMP.DB18.1 <- MPs[[1]]
save(tunedMP.DB18.1, file="Objects/tunedMP.DB18.1.RDA")
load(file="Objects/tunedMP.DB18.1.RDA")

betPlots.f(OMrefB19.5.tuned.4,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.4)
gc()


print(system.time(OMrefB19.5.tuned.5 <- runMse(OMrefB19.5, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.tuned.5, file="Objects/OMrefB19.5.tuned.5.RDA")
load(file="Objects/OMrefB19.5.tuned.5.RDA")

MPs <- getMPs(OMrefB19.5.tuned.5)

MPs[[1]]@tuneError

tunedMP.DB18.2 <- MPs[[1]]
save(tunedMP.DB18.2, file="Objects/tunedMP.DB18.2.RDA")
load(file="Objects/tunedMP.DB18.2.RDA")

betPlots.f(OMrefB19.5.tuned.5,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.5)
gc()


print(system.time(OMrefB19.5.tuned.6 <- runMse(OMrefB19.5, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.tuned.6, file="Objects/OMrefB19.5.tuned.6.RDA")
load(file="Objects/OMrefB19.5.tuned.6.RDA")

MPs <- getMPs(OMrefB19.5.tuned.6)

MPs[[1]]@tuneError

tunedMP.DB18.3 <- MPs[[1]]
save(tunedMP.DB18.3, file="Objects/tunedMP.DB18.3.RDA")
load(file="Objects/tunedMP.DB18.3.RDA")

betPlots.f(OMrefB19.5.tuned.6,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=87)

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.tuned.6)
gc()


# build MPs list
load(file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")
load(file="Objects/tunedMP.MB18.3.RDA")
load(file="Objects/tunedMP.DB18.1.RDA")
load(file="Objects/tunedMP.DB18.2.RDA")
load(file="Objects/tunedMP.DB18.3.RDA")
MPL <- list(MB1=tunedMP.MB18.1, MB2=tunedMP.MB18.2, MB3=tunedMP.MB18.3, DB1=tunedMP.DB18.1, DB2=tunedMP.DB18.2, DB3=tunedMP.DB18.3)

# load model definition, OMrefB19.5
load("Objects/OMrefB19.5.RDA")

# run projections
print(system.time(OMrefB19.5.proj <- runMse(OMrefB19.5, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.proj, file="Objects/OMrefB19.5.proj.RDA")
load(file="Objects/OMrefB19.5.proj.RDA")

rename       <- list(MB1="PT41.t15.Gk.mean-0.5",
                     MB2="PT41.t15.Gk.mean-0.6",
                     MB3="PT41.t15.Gk.mean-0.7",
                     DB1="IT5.t15.Gk.mean-0.5",
                     DB2="IT5.t15.Gk.mean-0.6",
                     DB3="IT5.t15.Gk.mean-0.7")
MP_names     <- c("MB1", "MB2", "MB3", "DB1", "DB2", "DB3")
MP_new_names <- c("PT41.t15.Gk.mean-0.5", "PT41.t15.Gk.mean-0.6", "PT41.t15.Gk.mean-0.7", "IT5.t15.Gk.mean-0.5", "IT5.t15.Gk.mean-0.6", "IT5.t15.Gk.mean-0.7")

betPlots.f(OMrefB19.5.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87)

betPlots.f(OMrefB19.5.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87,
           outputPath="Report/", prefix="B19.5.")

createTable1(20, list(OMrefB19.5.proj), MP_names, MP_new_names, prefix="OMrefB19.5.")
createTable2(20, list(OMrefB19.5.proj), MP_names, MP_new_names, prefix="OMrefB19.5.")

dt <- msevizPerformanceData(OMrefB19.5.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.5.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.proj, OMrefB19.5)
gc()


# -----------------------------------------
# Create robustnest cases for OMrefB19.5
# -----------------------------------------
# Case 1:
#   CPUE CV = 0.3, auto-correlation = 0.5
# -----------------------------------------
# load model definition, OMrefB19.5
load("Objects/OMrefB19.5.RDA")

# set robustness parameters CPUE CV = 0.3, auto-correlation = 0.5
getParameters(OMrefB19.5)$IACin
getParameters(OMrefB19.5)$Icv
OMrefB19.5 <- setParameters(OMrefB19.5, parameters=list(IACin=0.5, Icv=c(0.30000,0.30001)))

# run projections
print(system.time(OMrefB19.5.proj.CPUE.rt <- runMse(OMrefB19.5, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.proj.CPUE.rt, file="Objects/OMrefB19.5.proj.CPUE.rt.RDA")
load(file="Objects/OMrefB19.5.proj.CPUE.rt.RDA")

betPlots.f(OMrefB19.5.proj.CPUE.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87)

betPlots.f(OMrefB19.5.proj.CPUE.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87,
           outputPath="Report/", prefix="B19.5.CPUE.rt.")

createTable1(20, list(OMrefB19.5.proj.CPUE.rt), MP_names, MP_new_names, prefix="OMrefB19.5.CPUE.rt.")
createTable2(20, list(OMrefB19.5.proj.CPUE.rt), MP_names, MP_new_names, prefix="OMrefB19.5.CPUE.rt.")

dt <- msevizPerformanceData(OMrefB19.5.proj.CPUE.rt, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.5.CPUE.rt.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.proj.CPUE.rt, OMrefB19.5)
gc()


# -----------------------------------------
# Case 2:
#   10% reported over catch
# -----------------------------------------
# load model definition, OMrefB19.5
load("Objects/OMrefB19.5.RDA")

proyears <- OMrefB19.5@MseDef@proyears

# set 10% reported over catch implemented as 1.1 times TAC
OMrefB19.5 <- setParameters(OMrefB19.5, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears))))

# run projections
print(system.time(OMrefB19.5.proj.10over.rt <- runMse(OMrefB19.5, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.proj.10over.rt, file="Objects/OMrefB19.5.proj.10over.rt.RDA")
load(file="Objects/OMrefB19.5.proj.10over.rt.RDA")

betPlots.f(OMrefB19.5.proj.10over.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87)

betPlots.f(OMrefB19.5.proj.10over.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87,
           outputPath="Report/", prefix="B19.5.10over.rt.")

createTable1(20, list(OMrefB19.5.proj.10over.rt), MP_names, MP_new_names, prefix="OMrefB19.5.10over.rt.")
createTable2(20, list(OMrefB19.5.proj.10over.rt), MP_names, MP_new_names, prefix="OMrefB19.5.10over.rt.")

dt <- msevizPerformanceData(OMrefB19.5.proj.10over.rt, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.5.10over.rt.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.proj.10over.rt, OMrefB19.5)
gc()


# -----------------------------------------
# Case 3:
#   10% unreported over catch
# -----------------------------------------
# load model definition, OMrefB19.5
load("Objects/OMrefB19.5.RDA")

proyears <- OMrefB19.5@MseDef@proyears

# set 10% unreported under catch implemented as 1.1 times TAC and catch bias 1/1.1
OMrefB19.5 <- setParameters(OMrefB19.5, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears)), Cbmean=1.0 / 1.1))

# run projections
print(system.time(OMrefB19.5.proj.10under.rt <- runMse(OMrefB19.5, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.proj.10under.rt, file="Objects/OMrefB19.5.proj.10under.rt.RDA")
load(file="Objects/OMrefB19.5.proj.10under.rt.RDA")

betPlots.f(OMrefB19.5.proj.10under.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87)

betPlots.f(OMrefB19.5.proj.10under.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87,
           outputPath="Report/", prefix="B19.5.10under.rt.")

createTable1(20, list(OMrefB19.5.proj.10under.rt), MP_names, MP_new_names, prefix="OMrefB19.5.10under.rt.")
createTable2(20, list(OMrefB19.5.proj.10under.rt), MP_names, MP_new_names, prefix="OMrefB19.5.10under.rt.")

dt <- msevizPerformanceData(OMrefB19.5.proj.10under.rt, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.5.10under.rt.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.proj.10under.rt, OMrefB19.5)
gc()


# -----------------------------------------
# Case 4:
#   CPUE catchability trend of 3% per annum
# -----------------------------------------
# load model definition, OMrefB19.5
load("Objects/OMrefB19.5.RDA")

# set CPUE catchability trend of 3% per annum
OMrefB19.5 <- setParameters(OMrefB19.5, parameters=list(ITrendin=3.0))

print(system.time(OMrefB19.5.proj.CPUEtrend.rt <- runMse(OMrefB19.5, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.5.proj.CPUEtrend.rt, file="Objects/OMrefB19.5.proj.CPUEtrend.rt.RDA")
load(file="Objects/OMrefB19.5.proj.CPUEtrend.rt.RDA")

betPlots.f(OMrefB19.5.proj.CPUEtrend.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87)

betPlots.f(OMrefB19.5.proj.CPUEtrend.rt,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=87,
           outputPath="Report/", prefix="B19.5.CPUEtrend.rt.")

createTable1(20, list(OMrefB19.5.proj.CPUEtrend.rt), MP_names, MP_new_names, prefix="OMrefB19.5.CPUEtrend.rt.")
createTable2(20, list(OMrefB19.5.proj.CPUEtrend.rt), MP_names, MP_new_names, prefix="OMrefB19.5.CPUEtrend.rt.")

dt <- msevizPerformanceData(OMrefB19.5.proj.CPUEtrend.rt, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.5.CPUEtrend.rt.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.5.proj.CPUEtrend.rt, OMrefB19.5)
gc()
