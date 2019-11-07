# -----------------------------------------------------------------------------
# gridB19.6 tuned using MB18.1,MB18.2,MB18.3 model based tuning specifications
# -----------------------------------------------------------------------------

setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")

rm(list=ls(all=TRUE))

source("Source/MseMain.R")

# Create gridB19.6 model definition
source("Rscripts/Build OM Model-OMrefB19.6.500.R")
print(system.time(OMrefB19.6.500 <- createMseFramework(MseDef, UseCluster=0)))
#load("Objects/OMrefB19.6.500.mixedCPUE.RDA")
#OMrefB19.6.500 <- initCPUE_SeriesFrom(OMrefB19.6.500, "h70_M10_t10_q0_iH_iR1_CLRW_SL")  
save(OMrefB19.6.500, file="Objects/OMrefB19.6.500.RDA")

source("OMconditioning/RStuff/plotIndices.f.R")

# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

# For comparison: Langley 2016 BET assessment ref case (provided by Dan Fu with nod from Adam) is a grid of 6
# So these figs are not strictly correct...
#loadSSModel("TagLambda1", "Z:/MSE-IO-BET-YFT/OMconditioning/BET/AssessmentFiles2017/", force=FALSE)

# set up some plot related variables
Cref2017 <- 91 # 91K t caught in 2017

FLim   <- OMrefB19.6.500@MseDef@Flim
SBLim  <- OMrefB19.6.500@MseDef@SBlim

FTarg  <- 1.
SBTarg <- 1.

Cref <- Cref2017 
BETTargs <- c(SBTarg, FTarg)
BETLims  <- c(SBLim, FLim)

names(BETTargs) <- c("S3", "S4")
names(BETLims) <- c("S3", "S4")
names(Cref) <- "S10"


# Constant Catch OM demonstration test
print(system.time(OMrefB19.6.500.CC <- runMse(OMrefB19.6.500, MPs=c("CC001", "CC091"), CppMethod=1, interval=3, Report=F, UseCluster=1)))

betPlots.f(OMrefB19.6.500.CC,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)




# Tuning objective 2018 TCMP BET1 - TCMP.B18.1 ...might need relaxed TAC constraint...
# TCMP 2019 recommended dropping this for the next iteration 
#TCMP.B18.1 <- new("TuningParameters")
#TCMP.B18.1@performanceMeasure       <- "GK"
#TCMP.B18.1@performanceMeasureClass  <- "mean"
#TCMP.B18.1@performanceMeasureYears  <- c(2030,2034)
#TCMP.B18.1@tuningTarget             <- 0.5
#TCMP.B18.1@tuningTolerance          <- 0.01
#TCMP.B18.1@tuningLogDomain          <- c(-4,4)

# Tuning objective 2018 TCMP BET2 - TCMP.B18.2
TCMP.B18.2 <- new("TuningParameters")
TCMP.B18.2@performanceMeasure       <- "GK"
TCMP.B18.2@performanceMeasureClass  <- "mean"
TCMP.B18.2@performanceMeasureYears  <- c(2030,2034)
TCMP.B18.2@tuningTarget             <- 0.6
TCMP.B18.2@tuningTolerance          <- 0.01
TCMP.B18.2@tuningLogDomain          <- c(-4,4)  #(-2,2) not broad enough for some IT MPs
#TCMP.B18.2@tuningLogDomain          <- c(0,2)  #(-2,2) not broad enough for some IT MPs

# Tuning objective 2018 TCMP BET3 - TCMP.B18.3
TCMP.B18.3 <- new("TuningParameters")
TCMP.B18.3@performanceMeasure       <- "GK"
TCMP.B18.3@performanceMeasureClass  <- "mean"
TCMP.B18.3@performanceMeasureYears  <- c(2030,2034)
TCMP.B18.3@tuningTarget             <- 0.7
TCMP.B18.3@tuningTolerance          <- 0.01
TCMP.B18.3@tuningLogDomain          <- c(-4,4)
#TCMP.B18.3@tuningLogDomain          <- c(0,2)


# test Tuning objective 
testTune <- new("TuningParameters")
testTune@performanceMeasure       <- "GK"
testTune@performanceMeasureClass  <- "mean"
testTune@performanceMeasureYears  <- c(2030,2034)
testTune@tuningTarget             <- 0.6
testTune@tuningTolerance          <- 0.8
testTune@tuningLogDomain          <- c(4)  #(-2,2) not broad enough for some IT MPs


#test if we can simply put in a tuned MP after the fact and rerun MSE ... not sure its possible...
#MPL <- list("PT41.t15")
#print(system.time(OMrefB19.6.tuned.2 <- runMse(OMrefB19.6, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#print(system.time(OMrefB19.6.tuned.2 <- runMse(OMrefB19.6,  pset$tune = tunedMP.MB18.3@tune,        MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))



# Model based MP
MPL <- list("PT41.t15")

print(system.time(OMrefB19.6.500.tuned.1 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=F)))
save(OMrefB19.6.500.tuned.1, file="Objects/OMrefB19.6.500.tuned.1.RDA")
load(file="Objects/OMrefB19.6.500.tuned.1.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.1)

MPs[[1]]@tuneError

tunedMP.MB18.2 <- MPs[[1]]
save(tunedMP.MB18.2, file="Objects/tunedMP.MB18.2.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.1,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.1)
gc()


print(system.time(OMrefB19.6.500.tuned.2 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.2, file="Objects/OMrefB19.6.500.tuned.2.RDA")
load(file="Objects/OMrefB19.6.500.tuned.2.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.2)

MPs[[1]]@tuneError

tunedMP.MB18.3 <- MPs[[1]]
save(tunedMP.MB18.3, file="Objects/tunedMP.MB18.3.RDA")
load(file="Objects/tunedMP.MB18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.2,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.2)
gc()


# PJ's YFT rebuilding model-based MP -abbreviation Mr  
MPL <- list("PTproj.1.35bmsy.25")

print(system.time(OMrefB19.6.500.tuned.3 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.3, file="Objects/OMrefB19.6.500.tuned.3.RDA")
load(file="Objects/OMrefB19.6.500.tuned.3.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.3)

MPs[[1]]@tuneError

tunedMP.MrB18.2 <- MPs[[1]]
save(tunedMP.MrB18.2, file="Objects/tunedMP.MrB18.2.RDA")
load(file="Objects/tunedMP.MrB18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.3,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.3)
gc()


print(system.time(OMrefB19.6.500.tuned.4 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.4, file="Objects/OMrefB19.6.500.tuned.4.RDA")
load(file="Objects/OMrefB19.6.500.tuned.4.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.4)

MPs[[1]]@tuneError

tunedMP.MrB18.3 <- MPs[[1]]
save(tunedMP.MrB18.3, file="Objects/tunedMP.MrB18.3.RDA")
load(file="Objects/tunedMP.MrB18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.4,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.4)
gc()


# PJ's YFT rebuilding model-based MP with 15% change constrint-abbreviation Mrc  
MPL <- list("PTproj.1.35bmsy.25.tc15")

print(system.time(OMrefB19.6.500.tuned.5 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.5, file="Objects/OMrefB19.6.500.tuned.5.RDA")
load(file="Objects/OMrefB19.6.500.tuned.5.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.5)

MPs[[1]]@tuneError

tunedMP.MrcB18.2 <- MPs[[1]]
save(tunedMP.MrcB18.2, file="Objects/tunedMP.MrcB18.2.RDA")
load(file="Objects/tunedMP.MrcB18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.5,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.5)
gc()


print(system.time(OMrefB19.6.500.tuned.6 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.6, file="Objects/OMrefB19.6.500.tuned.6.RDA")
load(file="Objects/OMrefB19.6.500.tuned.6.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.6)

MPs[[1]]@tuneError

tunedMP.MrcB18.3 <- MPs[[1]]
save(tunedMP.MrcB18.3, file="Objects/tunedMP.MrcB18.3.RDA")
load(file="Objects/tunedMP.MrcB18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.6,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.6)
gc()



# Empirical MP
MPL <- list("IT5.t15")


print(system.time(OMrefB19.6.500.tuned.7 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.7, file="Objects/OMrefB19.6.500.tuned.7.RDA")
load(file="Objects/OMrefB19.6.500.tuned.7.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.7)

MPs[[1]]@tuneError

tunedMP.DB18.2 <- MPs[[1]]
save(tunedMP.DB18.2, file="Objects/tunedMP.DB18.2.RDA")
load(file="Objects/tunedMP.DB18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.7,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.7)
gc()


print(system.time(OMrefB19.6.500.tuned.8 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.8, file="Objects/OMrefB19.6.500.tuned.8.RDA")
load(file="Objects/OMrefB19.6.500.tuned.8.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.8)

MPs[[1]]@tuneError

tunedMP.DB18.3 <- MPs[[1]]
save(tunedMP.DB18.3, file="Objects/tunedMP.DB18.3.RDA")
load(file="Objects/tunedMP.DB18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.8,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.8)
gc()


# PT Model F-based 4010 MP
MPL <- list("PT41F.t15")
#print(system.time(OMrefB19.6.500.tuned.9 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=F)))
print(system.time(OMrefB19.6.500.tuned.9 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.9, file="Objects/OMrefB19.6.500.tuned.9.RDA")
load(file="Objects/OMrefB19.6.500.tuned.9.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.9)

MPs[[1]]@tuneError

tunedMP.MfB18.2 <- MPs[[1]]
save(tunedMP.MfB18.2, file="Objects/tunedMP.MfB18.2.RDA")
load(file="Objects/tunedMP.MfB18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.9,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.9)
gc()


print(system.time(OMrefB19.6.500.tuned.10 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.10, file="Objects/OMrefB19.6.500.tuned.10.RDA")
load(file="Objects/OMrefB19.6.500.tuned.10.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.10)

MPs[[1]]@tuneError

tunedMP.MfB18.3 <- MPs[[1]]
save(tunedMP.MfB18.3, file="Objects/tunedMP.MB18.3.RDA")
load(file="Objects/tunedMP.MfB18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.10,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.10)
gc()




# Model based MP with delayed start
MPL <- c("PT41.td15")

print(system.time(OMrefB19.6.500.tuned.11 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.11, file="Objects/OMrefB19.6.500.tuned.11.RDA")
load(file="Objects/OMrefB19.6.500.tuned.11.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.11)

MPs[[1]]@tuneError

tunedMP.M2B18.1 <- MPs[[1]]
save(tunedMP.M2B18.1, file="Objects/tunedMP.M2B18.1.RDA")
load(file="Objects/tunedMP.M2B18.1.RDA")

betPlots.f(OMrefB19.6.500.tuned.11,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.11)
gc()


print(system.time(OMrefB19.6.500.tuned.12 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.2, file="Objects/OMrefB19.6.500.tuned.12.RDA")
load(file="Objects/OMrefB19.6.500.tuned.12.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.2)

MPs[[1]]@tuneError

tunedMP.MB18.2 <- MPs[[1]]
save(tunedMP.M2B18.2, file="Objects/tunedMP.M2B18.2.RDA")
load(file="Objects/tunedMP.M2B18.2.RDA")

betPlots.f(OMrefB19.6.500.tuned.12,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.7)
gc()


print(system.time(OMrefB19.6.500.tuned.12 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.12, file="Objects/OMrefB19.6.500.tuned.12.RDA")
load(file="Objects/OMrefB19.6.500.tuned.12.RDA")

MPs <- getMPs(OMrefB19.6.500.tuned.12)

MPs[[1]]@tuneError

tunedMP.M2B18.3 <- MPs[[1]]
save(tunedMP.M2B18.3, file="Objects/tunedMP.M2B18.3.RDA")
load(file="Objects/tunedMP.M2B18.3.RDA")

betPlots.f(OMrefB19.6.500.tuned.12,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.tuned.12)
gc()




################################################################################################################################################
# Select reference set MP results for presentation
load(file="Objects/OMrefB19.6.500.tuned.1.RDA")
load(file="Objects/OMrefB19.6.500.tuned.2.RDA")
load(file="Objects/OMrefB19.6.500.tuned.5.RDA")
load(file="Objects/OMrefB19.6.500.tuned.6.RDA")
load(file="Objects/OMrefB19.6.500.tuned.7.RDA")
load(file="Objects/OMrefB19.6.500.tuned.8.RDA")
load(file="Objects/OMrefB19.6.500.tuned.9.RDA")
load(file="Objects/OMrefB19.6.500.tuned.10.RDA")






#MPL <- list(MB2=tunedMP.MB18.2, MrB2=tunedMP.MrB18.2, DB2=tunedMP.DB18.2)
#tunedMP.MB18.2 <- MPs[[1]]

# time-aggregated plots

YearsAveraged <- 20
#rename MPs (should assign unique names on creation instead)
perfd <- msevizPerformanceData(OMrefB19.6.500.tuned.1, YearsAveraged)
perfd$mp <- as.character(perfd$mp)
perfd$mp[perfd$mp=="PT41.t15" ] <- "PT41.t15.B2" 

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.5, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="PTproj.1.35bmsy.25.tc15" ] <- "PTproj.1.35bmsy.25.tc15.B2" 
perfd <- rbind(perfd, perfdtmp)

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.7, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="IT5.t15" ] <- "IT5.t15.B2" 
perfd <- rbind(perfd, perfdtmp)

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.9, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="PT41F.t15" ] <- "PT41F.t15.B2" 
perfd <- rbind(perfd, perfdtmp)



perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.2, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="PT41.t15" ] <- "PT41.t15.B3" 
perfd <- rbind(perfd, perfdtmp)

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.6, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="PTproj.1.35bmsy.25.tc15" ] <- "PTproj.1.35bmsy.25.tc15.B3" 
perfd <- rbind(perfd, perfdtmp)

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.8, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="IT5.t15" ] <- "IT5.t15.B3" 
perfd <- rbind(perfd, perfdtmp)

perfdtmp <- msevizPerformanceData(OMrefB19.6.500.tuned.10, YearsAveraged)
perfdtmp$mp <- as.character(perfdtmp$mp)
perfdtmp$mp[perfdtmp$mp=="PT41F.t15" ] <- "PT41F.t15.B3" 
perfd <- rbind(perfd, perfdtmp)

print(plotTOs2(perfd))
plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


########################################################################################################
#time series plots

histd <- msevizHistoricTimeSeriesData(OMrefB19.6.500.tuned.1)

projd <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.1)
projd$mp <- as.character(projd$mp)
projd$mp[projd$mp=="PT41.t15" ] <- "PT41.t15.B2" 

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.5)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="PTproj.1.35bmsy.25.tc15" ] <- "PTproj.1.35bmsy.25.tc15.B2" 
projd <- merge(projd, projdtmp, all=T)

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.7)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="IT5.t15" ] <- "IT5.t15.B2" 
projd <- merge(projd, projdtmp, all=T)

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.9)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="PT41F.t15" ] <- "PT41F.t15.B2" 
projd <- merge(projd, projdtmp, all=T)


projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.2)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="PT41.t15" ] <- "PT41.t15.B3" 
projd <- merge(projd, projdtmp, all=T)

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.6)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="PTproj.1.35bmsy.25.tc15" ] <- "PTproj.1.35bmsy.25.tc15.B3" 
projd <- merge(projd, projdtmp, all=T)

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.8)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="IT5.t15" ] <- "IT5.t15.B3" 
projd <- merge(projd, projdtmp, all=T)

projdtmp <- msevizProjectedTimeSeriesData(OMrefB19.6.500.tuned.10)
projdtmp$mp <- as.character(projdtmp$mp)
projdtmp$mp[projdtmp$mp=="PT41F.t15" ] <- "PT41F.t15.B3" 
projd <- merge(projd, projdtmp, all=T)

projd <- projd[projd$year <= 2040,]

plotKobeCols(om=histd, runs=projd, lastHistYr = 2016, firstMPYr = 2021)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", Cref=91000)


################################################################################################
# create table1

# build tuned MPs list
load(file="Objects/tunedMP.MB18.2.RDA")
tunedMP.MB18.2@MP_Name <- "MB18.2"
load(file="Objects/tunedMP.MB18.3.RDA") 
tunedMP.MB18.3@MP_Name <- "MB18.3"
load(file="Objects/tunedMP.MrcB18.2.RDA")
tunedMP.MrcB18.2@MP_Name <- "MrcB18.2"
load(file="Objects/tunedMP.MrcB18.3.RDA")
tunedMP.MrcB18.3@MP_Name <- "MrcB18.3"
load(file="Objects/tunedMP.DB18.2.RDA")
tunedMP.DB18.2@MP_Name <- "DB18.2"
load(file="Objects/tunedMP.DB18.3.RDA") 
tunedMP.DB18.3@MP_Name <- "DB18.3"
load(file="Objects/tunedMP.MfB18.2.RDA")
tunedMP.MfB18.2@MP_Name <- "MfB18.2"
load(file="Objects/tunedMP.MfB18.3.RDA") 
tunedMP.MfB18.3@MP_Name <- "MfB18.3"

MPL <- list(MB2=tunedMP.MB18.2, MB3=tunedMP.MB18.3, 
            MrcB2=tunedMP.MrcB18.2, MrcB3=tunedMP.MrcB18.3,
            DB2=tunedMP.DB18.2, DB3=tunedMP.DB18.3,
            MfB2=tunedMP.MfB18.2, MfB3=tunedMP.MfB18.3)
MPshortNames <- list("MB2",   "MB3", 
                     "MrcB2", "MrcB3",
                     "DB2",   "DB3",
                     "MfB2",  "MfB3")

#does not seem to currently exist a shortcut for merging objects that share MPs but multiple tuning objectives...rerun without tuning
print(system.time(OMrefB19.6.500.tuned.full <- runMse(OMrefB19.6.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.tuned.full, file="Objects/OMrefB19.6.500.tuned.full.RDA")

YearsAveraged <- 20
#rename MPs (should assign unique names on creation instead)
perfd <- msevizPerformanceData(OMrefB19.6.500.tuned.full, YearsAveraged)
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4))



MPVector  <- c("MB18.2",   "MB18.3", 
                     "MrcB18.2", "MrcB18.3",
                     "DB18.2",   "DB18.3",
                     "MfB18.2",  "MfB18.3")
MPshortNames <- c("MB2",   "MB3", 
                              "MrcB2", "MrcB3",
                              "DB2",   "DB3",
                              "MfB2",  "MfB3")

createTable1(20, list(OMrefB19.6.500.tuned.full), MPs=MPVector, MPs_short=MPshortNames,  prefix="OMrefB19.6.500.")

createTable2(1, list(OMrefB19.6.500.tuned.full), MPs=MPVector, MPs_short=MPshortNames,  prefix="OMrefB19.6.500.")
createTable2(5, list(OMrefB19.6.500.tuned.full), MPs=MPVector, MPs_short=MPshortNames,  prefix="OMrefB19.6.500.")
createTable2(10, list(OMrefB19.6.500.tuned.full), MPs=MPVector, MPs_short=MPshortNames,  prefix="OMrefB19.6.500.")
createTable2(20, list(OMrefB19.6.500.tuned.full), MPs=MPVector, MPs_short=MPshortNames,  prefix="OMrefB19.6.500.")




#Subset of models for robustness tests
MProbL <- list(MB2=tunedMP.MB18.2, MB3=tunedMP.MB18.3, 
            DB2=tunedMP.DB18.2, DB3=tunedMP.DB18.3)
MProbshortNames <- list("MB2",   "MB3", 
                     "DB2",   "DB3")


# -----------------------------------------
# Create robustness OMs for OMrefB19.6.500
# -----------------------------------------
# Case 1:   OMrobB19.6.ICV30 
#   CPUE CV = 0.3, auto-correlation = 0.5
# -----------------------------------------
# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

# set robustness parameters CPUE CV = 0.3, auto-correlation = 0.5
getParameters(OMrefB19.6.500)$IACin
getParameters(OMrefB19.6.500)$Icv
OMrefB19.6.500 <- setParameters(OMrefB19.6.500, parameters=list(IACin=0.5, Icv=c(0.30000,0.30001)))

# run projections
print(system.time(OMrobB19.6.ICV3.proj  <- runMse(OMrefB19.6.500, MPs=MProbL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrobB19.6.ICV3.proj, file="Objects/OMrobB19.6.ICV3.proj.RDA")
load(file="Objects/OMrobB19.6.ICV3.proj.RDA")

betPlots.f(OMrobB19.6.ICV3.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=Cref2017)

betPlots.f(OMrobB19.6.ICV3.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=Cref2017,
           outputPath="Report/", prefix="OMrobB19.6.ICV3.proj")

createTable1(20, list(OMrobB19.6.ICV3.proj), MP_names, MP_new_names, prefix="OMrobB19.6.ICV3.proj")
createTable2(20, list(OMrobB19.6.ICV3.proj), MP_names, MP_new_names, prefix="OMrobB19.6.ICV3.proj")

dt <- msevizPerformanceData(OMrobB19.6.ICV3.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="OMrobB19.6.ICV3.proj", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.proj.CPUE.rt, OMrefB19.6.500, OMrobB19.6.ICV3.proj)
gc()


# -----------------------------------------
# Case 2:  OMrobB19.6.10overRep 
#   10% reported over catch
# -----------------------------------------
# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

proyears <- OMrefB19.6.500@MseDef@proyears

# set 10% reported over catch implemented as 1.1 times TAC
OMrefB19.6.500 <- setParameters(OMrefB19.6.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears))))

# run projections
print(system.time(OMrobB19.6.10overRep.proj  <- runMse(OMrefB19.6.500, MPs=MProbL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrobB19.6.10overRep.proj, file="Objects/OMrobB19.6.10overRep.proj.RDA")
load(file="Objects/OMrobB19.6.10overRep.proj.RDA")

betPlots.f(OMrobB19.6.10overRep.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=Cref2017)

betPlots.f(OMrobB19.6.10overRep.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=Cref2017,
           outputPath="Report/", prefix="B19.6.10over.rt.")

createTable1(20, list(OMrobB19.6.10overRep.proj), MP_names, MP_new_names, prefix="OMrobB19.6.10overRep.proj")
createTable2(20, list(OMrobB19.6.10overRep.proj), MP_names, MP_new_names, prefix="OMrobB19.6.10overRep.proj")

dt <- msevizPerformanceData(OMrobB19.6.10overRep.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="OMrobB19.6.10overRep.proj", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.proj.10over.rt, OMrefB19.6.500, OMrobB19.6.10overRep.proj)
gc()


# -----------------------------------------
# Case 3:  OMrobB19.6.10overIUU
#   10% unreported over catch
# -----------------------------------------
# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

proyears <- OMrefB19.6.500@MseDef@proyears

# set 10% unreported under catch implemented as 1.1 times TAC and catch bias 1/1.1
OMrefB19.6.500 <- setParameters(OMrefB19.6.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears)), Cbmean=1.0 / 1.1))

 
# run projections
print(system.time(OMrobB19.6.10overIUU.proj <- runMse(OMrefB19.6.500, MPs=MProbL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrobB19.6.10overIUU.proj, file="Objects/OMrobB19.6.10overIUU.proj.RDA")
load(file="Objects/OMrobB19.6.10overIUU.proj.RDA")

betPlots.f(OMrobB19.6.10overIUU.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=Cref2017)

betPlots.f(OMrobB19.6.10overIUU.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=Cref2017,
           outputPath="Report/", prefix="B19.6.10under.rt.")

createTable1(20, list(OMrobB19.6.10overIUU.proj), MP_names, MP_new_names, prefix="OMrobB19.6.10overIUU.proj")
createTable2(20, list(OMrobB19.6.10overIUU.proj), MP_names, MP_new_names, prefix="OMrobB19.6.10overIUU.proj")

dt <- msevizPerformanceData(OMrefB19.6.500.proj.10under.rt, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="OMrobB19.6.10overIUU.proj", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.proj.10under.rt, OMrefB19.6.500, OMrobB19.6.10overIUU.proj)
gc()


# -----------------------------------------
# Case 4: OMrobB19.6.qTrend3 
#   CPUE catchability trend of 3% per annum
# -----------------------------------------
# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

# set CPUE catchability trend of 3% per annum
OMrefB19.6.500 <- setParameters(OMrefB19.6.500, parameters=list(ITrendin=3.0))

print(system.time(OMrobB19.6.qTrend3.proj <- runMse(OMrefB19.6.500, MPs=MProbL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrobB19.6.qTrend3.proj, file="Objects/OMrobB19.6.qTrend3.proj")
load(file="Objects/OMrobB19.6.qTrend3.proj.RDA")

betPlots.f(OMrobB19.6.qTrend3.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=Cref2017)

betPlots.f(OMrobB19.6.qTrend3.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           rename=rename,
           Cref=Cref2017,
           outputPath="Report/", prefix="B19.6.CPUEtrend.rt.")

createTable1(20, list(OMrobB19.6.qTrend3.proj), MP_names, MP_new_names, prefix="OMrobB19.6.qTrend3.proj")
createTable2(20, list(OMrobB19.6.qTrend3.proj), MP_names, MP_new_names, prefix="OMrobB19.6.qTrend3.proj")

dt <- msevizPerformanceData(OMrobB19.6.qTrend3.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="OMrobB19.6.qTrend3.proj", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrobB19.6.qTrend3.proj, OMrefB19.6.500)
gc()















zzz old below here

################################################################################
# Create Table 1

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0","PrSBgtSBlim","Y","relCPUE","YoMSY","APCY","YcvPct","PrYlt0.1MSY", "AAVY")

YearsAveraged <- 20
stats1 <- performanceStatistics(OMrefB19.6.500.tuned.1,  AllPerformanceMeasures, YearsAveraged)
stats2 <- performanceStatistics(OMrefB19.6.500.tuned.5,   AllPerformanceMeasures, YearsAveraged)
stats3 <- performanceStatistics(OMrefB19.6.500.tuned.7,   AllPerformanceMeasures, YearsAveraged)
stats4 <- performanceStatistics(OMrefB19.6.500.tuned.9,  AllPerformanceMeasures, YearsAveraged)
stats5 <- performanceStatistics(OMrefB19.6.500.tuned.2,  AllPerformanceMeasures, YearsAveraged)
stats6 <- performanceStatistics(OMrefB19.6.500.tuned.6,  AllPerformanceMeasures, YearsAveraged)
stats7 <- performanceStatistics(OMrefB19.6.500.tuned.8,  AllPerformanceMeasures, YearsAveraged)
stats8 <- performanceStatistics(OMrefB19.6.500.tuned.10,  AllPerformanceMeasures, YearsAveraged)
stats <- rbind(stats1, stats2, stats3, stats4, stats5, stats6, stats7, stats8, stats9, stats10)
rownames(stats) <- c(rownames(stats1), rownames(stats2), rownames(stats3), rownames(stats4), 
                     rownames(stats5), rownames(stats6), rownames(stats7), rownames(stats8))

finalMPList <- c("PT41.t15.B2", "PTproj.1.35bmsy.25.tc15.B2", "IT5.t15.B2", "PT41F.t15.B2")

tab1 <- NULL
for (mp in finalMPList){
  tab1 <- rbind(tab1, stats[rownames(stats)==mp, c("SBoSBMSY0.5",
                                                   "GK0.5","PrSBgtSBlim0.5",
                                                   "Y0.5",
                                                   "AAVY0.5",
                                                   "SBoSBMSY0.25", "SBoSBMSY0.75",
                                                   "Y0.25", "Y0.75")])
}
tab1 <- as.data.frame(cbind(finalMPList, tab1))








# build MPs list
#load(file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MB18.2.RDA")
#load(file="Objects/tunedMP.MB18.3.RDA")
#load(file="Objects/tunedMP.DB18.1.RDA")
load(file="Objects/tunedMP.DB18.2.RDA")
#load(file="Objects/tunedMP.DB18.3.RDA")
#load(file="Objects/tunedMP.MB18.1.RDA")
load(file="Objects/tunedMP.MrB18.2.RDA")
#load(file="Objects/tunedMP.MrB18.3.RDA")
#load(file="Objects/tunedMP.MfB18.1.RDA")
load(file="Objects/tunedMP.MfB18.2.RDA")
#load(file="Objects/tunedMP.MfB18.3.RDA")
load(file="Objects/tunedMP.MrcB18.2.RDA")



#MPL <- list(MB2=tunedMP.MB18.2, MrB2=tunedMP.MrB18.2, DB2=tunedMP.DB18.2, MB3=tunedMP.MB18.3, MrB3=tunedMP.MrB18.3, DB3=tunedMP.DB18.3)
MPL <- list(MB2=tunedMP.MB18.2, MrB2=tunedMP.MrB18.2, DB2=tunedMP.DB18.2, MfB2=tunedMP.MfB18.2, MrcB2=tunedMP.MrcB18.2)
#MPL <- list(DB2=tunedMP.DB18.2, MrcB2=tunedMP.MrcB18.2)
MPL <- list(MfB2=tunedMP.MfB18.2)

# load model definition, OMrefB19.6.500
load("Objects/OMrefB19.6.500.RDA")

# run projections
print(system.time(OMrefB19.6.500.proj <- runMse(OMrefB19.6.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.proj, file="Objects/OMrefB19.6.500.proj.RDA")
load(file="Objects/OMrefB19.6.500.proj.RDA")

rename       <- list(MB1="PT41.t15.Gk.mean-0.5",
                     MB2="PT41.t15.Gk.mean-0.6",
                     MB3="PT41.t15.Gk.mean-0.7",
                     DB1="IT5.t15.Gk.mean-0.5",
                     DB2="IT5.t15.Gk.mean-0.6",
                     DB3="IT5.t15.Gk.mean-0.7")
MP_names     <- c("MB1", "MB2", "MB3", "DB1", "DB2", "DB3")
MP_new_names <- c("PT41.t15.Gk.mean-0.5", "PT41.t15.Gk.mean-0.6", "PT41.t15.Gk.mean-0.7", "IT5.t15.Gk.mean-0.5", "IT5.t15.Gk.mean-0.6", "IT5.t15.Gk.mean-0.7")

betPlots.f(OMrefB19.6.500.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=91) #91=2017 catch


betPlots.f(OMrefB19.6.500.proj,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           #rename=rename,
           Cref=91, #91=2017 catch
           outFileType="emf", #"png" is rough 
           outputPath="Report/BET_Figs_Tables/", prefix="B19.6.")


#createTable1(20, list(OMrefB19.6.500.proj), MP_names, MP_new_names, prefix="OMrefB19.6.500.")
#createTable2(20, list(OMrefB19.6.500.proj), MP_names, MP_new_names, prefix="OMrefB19.6.500.")
createTable1(20, list(OMrefB19.6.500.proj), MP_names, MPs_short=MP_names, prefix="OMrefB19.6.500.")
createTable2(1, list(OMrefB19.6.500.proj), MP_names, MPs_short=MP_names, prefix="OMrefB19.6.500.")
createTable2(5, list(OMrefB19.6.500.proj), MP_names, MPs_short=MP_names, prefix="OMrefB19.6.500.")
createTable2(10, list(OMrefB19.6.500.proj), MP_names, MPs_short=MP_names, prefix="OMrefB19.6.500.")
createTable2(20, list(OMrefB19.6.500.proj), MP_names, MPs_short=MP_names, prefix="OMrefB19.6.500.")




dt <- msevizPerformanceData(OMrefB19.6.500.proj, 20)

indicatorSensitivityBPs(dt[mp=='MB2'], colourPalette=rainbow(10, s=0.6))
indicatorSensitivityBPs(dt[mp=='MB2'], outputPath="Report/", prefix="B19.6.", colourPalette=rainbow(10, s=0.6))

dev.off(dev.list())
dev.new()

rm(OMrefB19.6.500.proj, OMrefB19.6.500)
gc()




#################################################################################################################
# try a range of BET MPs for tuning level B18.2 for contrast

MPL <- list(MB2a ="PT41.t15", MB2b ="PT41.td15", MB2c ="PT41.t10",
            DB2a ="IT5.t15",  DB2b ="IT5.t10",   DB2c ="IT5.t15g3131")


print(system.time(OMrefB19.6.500.B2range <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefB19.6.500.B2range, file="OMrefB19.6.500.B2range.RDA")
load(file="Objects/OMrefB19.6.500.B2range.RDA")

MPL2 <- list(DB2d ="IT5.td15")
print(system.time(OMrefB19.6.500.B2range1 <- runMse(OMrefB19.6.500, TuningPars=TCMP.B18.2, MPs=MPL2, CppMethod=1, interval=3, Report=F, UseCluster=1)))



MPs <- getMPs(OMrefB19.6.500.B2range1)
for(i in 1:length(MPL)){
  print(unlist(c(MPL[i],MPs[[i]]@tuneError)))
  print(" ")
}

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefB19.6.500.B2range, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(OMrefB19.6.500.B2range1, YearsAveraged), all=TRUE)

perfd <- perfd[perfd$mp != "DB2c",]

plotBPs2(perfd, limit=YFTLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


#time series plots
histd <- msevizHistoricTimeSeriesData(OMrefB19.6.500.B2range)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefB19.6.500.B2range1), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefB19.6.500.B2range)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefB19.6.500.B2range1), all=TRUE)

projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
projd <- projd[projd$mp != "DB2c",]

lastHistYr <- 2017
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)





betPlots.f(OMrefB19.6.500.B2range1,
           SBLim,
           SBTarg,
           FLim,
           FTarg,
           Cref=Cref2017)

dev.off(dev.list())
dev.new()
gc()










