# R Script for conducting IO-BET MSE for TCMP 2018
#

#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master")  # Bowen cloud working directory
#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror")  # Set the working directory
#setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror")  # Set the working directory
setwd("M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master")  # Set the working directory
#setwd("C:\\tmp")  # Set the working directory
#sourceDir <- "M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master"  # Set the working directory
rm(list=ls(all=TRUE))

#for Tinn-R users only: (may no longer be required)
.trPaths <- file.path(Sys.getenv("TEMP"), "Tinn-R", c("", "search.txt", "objects.txt", "file.r", "selection.r", "block.r", "lines.r", "reformat-input.r", "reformat-output.r"), fsep="\\")
source("Source\\MseMain.R")
#source("Source\\mseviz2.R")
#source("Source\\utilities.R")
library(ggstance)

MPListC <- c("CC087")
MPList0 <- c("PT41.t25","IT5.t25", "CCt")
MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")


################################################################################
# extract the BET reference points for plotting
load(file="Objects/mseOMrefB18.2.304.TB3.RDA")

FLim  <- mseOMrefB18.2.304.TB3@MseDef@Flim
SBLim  <- mseOMrefB18.2.304.TB3@MseDef@SBlim
FTarg <- 1.
SBTarg <- 1.

Cref <- 87. #should calculate within object when BET off by 1 yr fixed
BETTargs <- c(SBTarg, FTarg)
BETLims  <- c(SBLim, FLim)

names(BETTargs) <- c("S3", "S4")
names(BETLims) <- c("S3", "S4")
names(Cref) <- "S10"




#########################################################################################################
# Create OMrefB18.2.20 - 20 reps for quick testing graphics and stuff

source('Rscripts\\Build OM Model-OMrefB18.2.20.R')

# Create or load an OM object
#print(system.time(OMrefB18.2.20 <- createMseFramework(MseDef)))
#save(sourceDir %&% OMrefB18.2.20,file=paste(getwd(),"/Objects/OMrefB18.2.20.RDA",sep=""))
load(file="Objects/OMrefB18.2.20.RDA")

# Run the MSE without any tuning (fishing moratorium and current catch)
# Some numerical options
print(system.time(mseOMrefB18.2.20.0    <- runMse(OMrefB18.2.20,MPs <- c("CC001","CC087"),interval=3, Report=F,UseCluster=0)))
print(system.time(mseOMrefB18.2.20.0    <- runMse(OMrefB18.2.20,MPs <- c("CC001","CC087"),interval=3, Report=F,UseCluster=1)))
print(system.time(mseOMrefB18.2.20.0    <- runMse(OMrefB18.2.20,MPs <- c("CC001","CC087"),interval=3, CppMethod=1, Report=F,UseCluster=0)))
print(system.time(mseOMrefB18.2.20.0    <- runMse(OMrefB18.2.20,MPs <- c("CC001","CC087"),interval=3, CppMethod=1, Report=F,UseCluster=1)))

#Time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefB18.2.20.0)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.2.20.0)

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")

#Time-integrated plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.2.20.0, YearsAveraged)

plotTOs2(perfd)
plotBPs2(perfd)
kobeMPs2(perfd)


# 2018 BET Tuning objective test
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain          <- c(-2,2)
print(system.time(mseOMrefB18.2.20.TB3.1  <- runMse(OMrefB18.2.20, TuningPars=TuningPars, MPs=MPList1[1:3], CppMethod=1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.20.TB3.1@StockSynthesisModels)){
  names(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB3.1." %&% names(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB3.1." %&% mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.20.TB3.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.20.TB3.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.20.TB3.1.RDA")

#one year off fudge for plots - need to fix in BET OM specification
#mseOMrefB18.2.20.TB3.1@MseDef@firstCalendarYr <- as.integer(1952)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.2.20.TB3.1)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.2.20.TB3.1)

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")


YearsAveraged <- 20 # cannot use c(2019,2038)
perfd <- msevizPerformanceData(mseOMrefB18.2.20.TB3.1, YearsAveraged)
plotBPs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))
plotTOs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14"))
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, ymax=3)




# Try with RecCV = 0.
OMrefB18.2.20.Rcv0 <- OMrefB18.2.20
for(iom in 1:length(OMrefB18.2.20.Rcv0@StockSynthesisModels)){
  OMrefB18.2.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT <-  OMrefB18.2.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT * 0. + 1.
  OMrefB18.2.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs <-  OMrefB18.2.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs * 0. + 1.
}
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain          <- c(-2,2)
print(system.time(mseOMrefB18.2.20.Rcv0.TB3.1  <- runMse(OMrefB18.2.20.Rcv0, TuningPars=TuningPars, MPs=MPList1[1:3], CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.2.20.Rcv0.TB3.1)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.2.20.Rcv0.TB3.1)
plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

YearsAveraged <- 20 # cannot use c(2019,2038)
perfd <- msevizPerformanceData(mseOMrefB18.2.20.Rcv0.TB3.1, YearsAveraged)
plotBPs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))
plotTOs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14"))
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, ymax=3)

#compare both
perfd <- msevizPerformanceData(mseOMrefB18.2.20.Rcv0.TB3.1, YearsAveraged)
perfd <- rbind(perfd, msevizPerformanceData(mseOMrefB18.2.20.TB3.1, YearsAveraged))
plotBPs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))
plotTOs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14"))
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, ymax=3)



#########################################################################################################
# Create OMrefB18.2.304 - standard OM for TCMP 2018, with 304 realizations (2000+ sim version defined below)
#########################################################################################################

#########################################################################################################
# Create or load an OM object
#source('Rscripts\\Build OM Model-OMrefB18.2.304.R')
#print(system.time(OMrefB18.2.304 <- createMseFramework(MseDef)))
#save(OMrefB18.2.304,file=paste(getwd(),"/Objects/OMrefB18.2.304.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMrefB18.2.304.RDA",sep=""))

#########################################################################################################
# Run the MSE without any tuning (fishing moratorium and current catch)
print(system.time(mseOMrefB18.2.304.0          <- runMse(OMrefB18.2.304,MPs <- c("CC087"),interval=3, Report=F,UseCluster=1)))
print(system.time(mseOMrefB18.2.304.0          <- runMse(OMrefB18.2.304,MPs <- c("CC001","CC087"),interval=3, Report=F,UseCluster=1)))
print(system.time(mseOMrefB18.2.304.0.cluster  <- runMse(OMrefB18.2.304,MPs <- c("CC087"),interval=3, Report=F,UseCluster=1)))



# Plot some key time series
histd <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.0)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.0)
plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

YearsAveraged          <- 20
perfd <- msevizPerformanceData(mseOMrefB18.2.304.0, YearsAveraged)
print(plotTOs(perfd))
print(plotBPs(perfd))
print(kobeMPs(perfd, y="S5"))

print(system.time(mseOMrefB18.2.304.C  <- new("MSE",OMrefB18.2.304, TuningPars=TuningPars, MPs <- "CC087", interval=3, Report=F,UseCluster=0)))
save(mseOMrefB18.2.304.C,file=paste(getwd(),"/Objects/phase2/mseOMrefB18.2.304.C.RDA",sep=""))
#load(file=paste(getwd(),"/Objects/phase2/mseOMrefB18.2.304.C.RDA",sep=""))
#plotTS.f(mseOMrefB18.2.304.C, plotByRF=F, doWorms=T)  #Time series (worm) plots
#tableMSE.f(mseOMrefB18.2.304.C)


# BET Tuning objective 1 - not attainable with some or all 15% change constraint
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- 1001
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)

# OMrefB18.2.304@tunePM     <- "SBoSBMSY0.5"
#OMrefB18.2.304@tunePM     <- "SBoSBMSYmean"
#OMrefB18.2.304@tunePMProjPeriod <-1001
#OMrefB18.2.304@tunePMTarget <- 1.0
#OMrefB18.2.304@tuneLogDomain <- c(-2,2)
print(system.time(mseOMrefB18.2.304.TB1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList0[3], interval=3, Report=F, UseCluster=1)))

mseOMrefB18.2.304.TB1@MPs <-"TB1." %&% mseOMrefB18.2.304.TB1@MPs
save(mseOMrefB18.2.304.TB1,file=paste(getwd(),"/Objects/phase2/mseOMrefB18.2.304.TB1.RDA",sep=""))
#load(file=paste(getwd(),"/Objects/phase2/mseOMrefB18.2.304.TB1.RDA",sep=""))
#plotTS.f(mseOMrefB18.2.304.TB1, plotByRF=F, doWorms=T)  #Time series (worm) plots
#tableMSE.f(mseOMrefB18.2.304.TB1)





# BET Tuning objective TB1.1 - revised
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB1.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList0, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB1.1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB1.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB1.1." %&% names(mseOMrefB18.2.304.TB1.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB1.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB1.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB1.1." %&% mseOMrefB18.2.304.TB1.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB1.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB1.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB1.1.RDA")

#confirm tuning achieved
mseOMrefB18.2.304.TB1.1@tune

# BET Tuning objective TB1.2 - redo failed tuning...too many fnEvals?
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(0,0.5)
print(system.time(mseOMrefB18.2.304.TB1.2  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList0[2], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB1.2@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars) <- "TB1.2." %&% names(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB1.2." %&% mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB1.2,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB1.2.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB1.2.RDA")



# BET Tuning objective TB1.2 - revised
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(0,0.5)
print(system.time(mseOMrefB18.2.304.TB1.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList0[2], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB1.2@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars) <- "TB1.2." %&% names(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB1.2." %&% mseOMrefB18.2.304.TB1.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB1.2,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB1.2.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB1.2.RDA")

#confirm tuning achieved
mseOMrefB18.2.304.TB1.2@tune





# BET Tuning objective TB6.1 - revised
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.3
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB6.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[1:3], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB6.1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB6.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB6.1." %&% names(mseOMrefB18.2.304.TB6.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB6.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB6.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB6.1." %&% mseOMrefB18.2.304.TB6.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB6.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB6.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB6.1.RDA")


# BET Tuning objective TB7.1 - revised
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB7.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[1:3], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB7.1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB7.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB7.1." %&% names(mseOMrefB18.2.304.TB7.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB7.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB7.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB7.1." %&% mseOMrefB18.2.304.TB7.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB7.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB7.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB7.1.RDA")



# BET Tuning objective TB8.1
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.8
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB8.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[1:3], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB8.1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB8.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB8.1." %&% names(mseOMrefB18.2.304.TB8.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB8.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB8.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB8.1." %&% mseOMrefB18.2.304.TB8.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB8.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB8.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB8.1.RDA")


# BET Tuning objective TB9.1
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.9
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB9.1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[1:3], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB9.1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB9.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB9.1." %&% names(mseOMrefB18.2.304.TB9.1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB9.1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB9.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB9.1." %&% mseOMrefB18.2.304.TB9.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB9.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB9.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB9.1.RDA")

# BET Tuning objective TB9.2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.9
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB9.2  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=c(MPList1[4:8],MPList2), interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB9.2@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB9.2@StockSynthesisModels[[i]]@ProjectedVars) <- "TB9.2." %&% names(mseOMrefB18.2.304.TB9.2@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.TB9.2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB9.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB9.2." %&% mseOMrefB18.2.304.TB9.2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefB18.2.304.TB9.2,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB9.2.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB9.2.RDA")







# BET Tuning objective TB2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB2  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB2." %&% mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars) <- "TB2." %&% names(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB2,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB2.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB2.RDA")

# BET Tuning objective TB3
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB3  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB3@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB3@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB3." %&% mseOMrefB18.2.304.TB3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB3@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB3@StockSynthesisModels[[i]]@ProjectedVars) <- "TB3." %&% names(mseOMrefB18.2.304.TB3@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB3,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB3.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB3.RDA")

# BET Tuning objective TB4
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB4  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB4@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB4@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB4." %&% mseOMrefB18.2.304.TB4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB4@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB4@StockSynthesisModels[[i]]@ProjectedVars) <- "TB4." %&% names(mseOMrefB18.2.304.TB4@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB4,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB4.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB4.RDA")





#repeat with MPList2
# BET Tuning objective TB1
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.2TB1  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.2TB1@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.2TB1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.2TB1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB1." %&% mseOMrefB18.2.304.2TB1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.2TB1@StockSynthesisModels)){
  names(mseOMrefB18.2.304.2TB1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB1." %&% names(mseOMrefB18.2.304.2TB1@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.2TB1,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.2TB1.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.2TB1.RDA")


# BET Tuning objective TB2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.2TB2  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.2TB2@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.2TB2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.2TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB2." %&% mseOMrefB18.2.304.2TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.2TB4@StockSynthesisModels)){
  names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars) <- "TB2." %&% names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.2TB2,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.2TB2.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.2TB2.RDA")

# BET Tuning objective TB3
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.2TB3  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.2TB3@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.2TB3@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.2TB3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB3." %&% mseOMrefB18.2.304.2TB3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.2TB3@StockSynthesisModels)){
  names(mseOMrefB18.2.304.2TB3@StockSynthesisModels[[i]]@ProjectedVars) <- "TB3." %&% names(mseOMrefB18.2.304.2TB3@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.2TB3,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.2TB3.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.2TB3.RDA")

# BET Tuning objective TB4
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.2TB4  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.2TB4@StockSynthesisModels)){
  names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars) <- "TB4." %&% names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB4." %&% mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.2TB4@StockSynthesisModels)){
  names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars) <- "TB4." %&% names(mseOMrefB18.2.304.2TB4@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.2TB4,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.2TB4.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.2TB4.RDA")





################################################################################
# make a full set of plots for one tuning value
load(file="Objects/mseOMrefB18.2.304.TB3.RDA")


FLim  <- mseOMrefB18.2.304.TB3@MseDef@Flim
SBLim  <- mseOMrefB18.2.304.TB3@MseDef@SBlim
FTarg <- 1.
SBTarg <- 1.

Cref <- 87. #should calculate within object when BET off by 1 yr fixed
BETTargs <- c(SBTarg, FTarg)
BETLims  <- c(SBLim, FLim)

names(BETTargs) <- c("S3", "S4")
names(BETLims) <- c("S3", "S4")
names(Cref) <- "S10"




histd.TB3 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB3)
projd.TB3 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB3)

plotKobeCols(om=histd.TB3, runs=projd.TB3)
plotOMruns2(histd.TB3, projd.TB3, "SSB/SSBMSY", limit=SBLim, target=SBTarg)
plotOMruns2(histd.TB3, projd.TB3, "F/FMSY", limit=FLim, target=FTarg)
plotOMruns2(histd.TB3, projd.TB3, "C")


YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.2.304.TB3, YearsAveraged)
plotBPs2(perfd, limit=BETLims, target=BETTargs)
plotTOs2(perfd, limit=BETLims, target=BETTargs)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim)


################################################################################
#make a combined Kobe plot for all (most) MPs and all tuning levels
#load(file="Objects/mseOMrefB18.2.304.TB1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB1.1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB1.2.RDA")
#load(file="Objects/mseOMrefB18.2.304.0TB1.RDA")
load(file="Objects/mseOMrefB18.2.304.2TB1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB2.RDA")
load(file="Objects/mseOMrefB18.2.304.2TB2.RDA")
load(file="Objects/mseOMrefB18.2.304.TB3.RDA")
load(file="Objects/mseOMrefB18.2.304.2TB3.RDA")
load(file="Objects/mseOMrefB18.2.304.TB4.RDA")
load(file="Objects/mseOMrefB18.2.304.2TB4.RDA")
load(file="Objects/mseOMrefB18.2.304.TB6.1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB7.1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB8.1.RDA")
load(file="Objects/mseOMrefB18.2.304.TB9.1.RDA")

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.2.304.TB1.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB1.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.2TB2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.2TB3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.2TB4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB6.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB7.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB8.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB9.1, YearsAveraged))

kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)
plotBPs2(perfd, limit=BETLims, target=BETTargs)



################################################################################
# make a series of tuning level-specific plots

#Tuning level 1
histd.TB1 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB1.1)
histd.TB1 <- rbind(histd.TB1, msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB1.2))
projd.TB1 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB1.1)
projd.TB1 <- rbind(projd.TB1, msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB1.2))


plotOMruns2(histd.TB1, projd.TB1, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB1, projd.TB1, "C", ylab= "Catch (1000t)")


#Tuning level TB2
histd.TB2 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB2)
histd.TB2 <- rbind(histd.TB2, msevizHistoricTimeSeriesData(mseOMrefB18.2.304.2TB2))
projd.TB2 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB2)
projd.TB2 <- rbind(projd.TB2, msevizProjectedTimeSeriesData(mseOMrefB18.2.304.2TB2))

plotOMruns2(histd.TB2, projd.TB2, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB2, projd.TB2, "C", ylab= "Catch (1000t)")


#Tuning level TB3
histd.TB3 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB3)
histd.TB3 <- rbind(histd.TB3, msevizHistoricTimeSeriesData(mseOMrefB18.2.304.2TB3))
projd.TB3 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB3)
projd.TB3 <- rbind(projd.TB3, msevizProjectedTimeSeriesData(mseOMrefB18.2.304.2TB3))

plotOMruns2(histd.TB3, projd.TB3, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB3, projd.TB3, "C", ylab= "Catch (1000t)")


#Tuning level TB4
histd.TB4 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB4)
histd.TB4 <- rbind(histd.TB4, msevizHistoricTimeSeriesData(mseOMrefB18.2.304.2TB4))
projd.TB4 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB4)
projd.TB4 <- rbind(projd.TB4, msevizProjectedTimeSeriesData(mseOMrefB18.2.304.2TB4))

plotOMruns2(histd.TB4, projd.TB4, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB4, projd.TB4, "C", ylab= "Catch (1000t)")

#Tuning level TB6
histd.TB6 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB6.1)
projd.TB6 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB6.1)

plotOMruns2(histd.TB6, projd.TB6, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB6, projd.TB6, "C", ylab= "Catch (1000t)")

#Tuning level TB7
histd.TB7 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB7.1)
projd.TB7 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB7.1)

plotOMruns2(histd.TB7, projd.TB7, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB7, projd.TB7, "C", ylab= "Catch (1000t)")

#Tuning level TB8
histd.TB8 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB8.1)
projd.TB8 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB8.1)

plotOMruns2(histd.TB8, projd.TB8, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB8, projd.TB8, "C", ylab= "Catch (1000t)")

#Tuning level TB9
histd.TB9 <- msevizHistoricTimeSeriesData(mseOMrefB18.2.304.TB9.1)
projd.TB9 <- msevizProjectedTimeSeriesData(mseOMrefB18.2.304.TB9.1)

plotOMruns2(histd.TB9, projd.TB9, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TB9, projd.TB9, "C", ylab= "Catch (1000t)")







################################################################################
# select and rename MPs for final cross-MP plots - MP does not really matter, its the tuning that counts

projd.all  <- rbind(projd.TB1, projd.TB2, projd.TB3, projd.TB4, projd.TB6, projd.TB7, projd.TB8, projd.TB9)
histd.all  <- rbind(histd.TB1, histd.TB2, histd.TB3, histd.TB4, histd.TB6, histd.TB7, histd.TB8, histd.TB9)

finalMPList <- c('TB1.1.PT41.t25', 'TB1.2.IT5.t25',
                 'TB2.PT41.t15', 'TB2.IT5.t15',
                 'TB3.PT41.t15', 'TB3.IT5.t15',
                 'TB4.PT41.t15', 'TB4.IT5.t15',
                 'TB9.1.PT41.t15', 'TB9.1.IT5.t15')

projd.sub <- projd.all[mp %in% finalMPList]
# drop post-tuning part of projection period
projd.sub <- projd.sub[year <2039]


# compact names
projd.sub[projd.sub$mp == 'TB1.1.PT41.t25','mp'] <- "TB1.M"
projd.sub[projd.sub$mp == 'TB1.2.IT5.t25','mp']  <- "TB1.D"
projd.sub[projd.sub$mp == 'TB2.PT41.t15','mp'] <- "TB2.M"
projd.sub[projd.sub$mp == 'TB2.IT5.t15','mp']  <- "TB2.D"
projd.sub[projd.sub$mp == 'TB3.PT41.t15','mp'] <- "TB3.M"
projd.sub[projd.sub$mp == 'TB3.IT5.t15','mp']  <- "TB3.D"
projd.sub[projd.sub$mp == 'TB4.PT41.t15','mp'] <- "TB4.M"
projd.sub[projd.sub$mp == 'TB4.IT5.t15','mp']  <- "TB4.D"
projd.sub[projd.sub$mp == 'TB9.1.PT41.t15','mp'] <- "TB9.M"
projd.sub[projd.sub$mp == 'TB9.1.IT5.t15','mp']  <- "TB9.D"

# Time Series plots
plotKobeCols(om=histd.all, runs=projd.sub)
plotOMruns2(histd.all, projd.sub, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.all, projd.sub, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd.all, projd.sub, "C", ylab= "Catch (1000 t)", Cref=87000)


#different subset for discussion purposes
demoMPList <- c('TB1.1PT41.t25', 'TB1.2IT5.t25',
                           'TB6.1.PT41.t15', 'TB6.1.IT5.t15',
                           'TB7.1.PT41.t15', 'TB7.1.IT5.t15',
                           'TB9.1.PT41.t15', 'TB9.1.IT5.t15')

projd.sub <- projd.all[mp %in% demoMPList]
# drop post-tuning part of projection period
projd.sub <- projd.sub[year <2039]
plotKobeCols(om=histd.all, runs=projd.sub)
plotOMruns2(histd.all, projd.sub, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.all, projd.sub, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd.all, projd.sub, "C", ylab= "Catch (1000 t)", Cref=87000)




# temporally-averaged plots
#YearsAveraged <- 10
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.2.304.TB1.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB1.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB6.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB7.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB8.1, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.2.304.TB9.1, YearsAveraged))

perfd.sub <- perfd[mp %in% finalMPList]


# compact names
perfd.sub[perfd.sub$mp == 'TB1.1.PT41.t25','mp'] <- "TB1.M"
perfd.sub[perfd.sub$mp == 'TB1.2.IT5.t25','mp']  <- "TB1.D"
perfd.sub[perfd.sub$mp == 'TB2.PT41.t15','mp'] <- "TB2.M"
perfd.sub[perfd.sub$mp == 'TB2.IT5.t15','mp']  <- "TB2.D"
perfd.sub[perfd.sub$mp == 'TB3.PT41.t15','mp'] <- "TB3.M"
perfd.sub[perfd.sub$mp == 'TB3.IT5.t15','mp']  <- "TB3.D"
perfd.sub[perfd.sub$mp == 'TB4.PT41.t15','mp'] <- "TB4.M"
perfd.sub[perfd.sub$mp == 'TB4.IT5.t15','mp']  <- "TB4.D"
perfd.sub[perfd.sub$mp == 'TB9.1.PT41.t15','mp'] <- "TB9.M"
perfd.sub[perfd.sub$mp == 'TB9.1.IT5.t15','mp']  <- "TB9.D"

#perfd.first10 <- perfd.sub
#perfd.20 <- perfd.sub
#perfd.last10 <- perfd.first10
#perfd.last10$data <- 2*perfd.20$data - perfd.first10$data
#kobeMPs2(perfd.first10, xlim=SBLim, ylim=FLim, xmax=3, ymax=5)
#kobeMPs2(perfd.last10, xlim=SBLim, ylim=FLim, xmax=3, ymax=5)



plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd.sub, xlim=SBLim, ylim=FLim, xmax=3, ymax=5)



perfd.sub <- perfd[mp %in% c(    'TB2.PT41.t15', 'TB2.IT5.t15',
                                 'TB3.PT41.t15', 'TB3.IT5.t15',
                                 'TB4.PT41.t15', 'TB4.IT5.t15')]
plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))


#different subset for discussion purposes
perfd.sub <- perfd[mp %in% c('TB1.1.PT41.t25', 'TB1.2.IT5.t25',
                           'TB6.1.PT41.t15', 'TB6.1.IT5.t15',
                           'TB7.1.PT41.t15', 'TB7.1.IT5.t15',
                           'TB9.1.PT41.t15', 'TB9.1.IT5.t15')]
plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)


################################################################################
# Create Table 1

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0","PrSBgtSBlim","Y","relCPUE","YoMSY","APCY","YcvPct","PrYlt0.1MSY", "AAVY")

YearsAveraged <- 20
stats0 <- performanceStatistics(mseOMrefB18.2.304.TB1.1, AllPerformanceMeasures, YearsAveraged)
stats1 <- performanceStatistics(mseOMrefB18.2.304.TB1.2, AllPerformanceMeasures, YearsAveraged)
stats2 <- performanceStatistics(mseOMrefB18.2.304.TB2,   AllPerformanceMeasures, YearsAveraged)
stats3 <- performanceStatistics(mseOMrefB18.2.304.TB3,   AllPerformanceMeasures, YearsAveraged)
stats4 <- performanceStatistics(mseOMrefB18.2.304.TB4,   AllPerformanceMeasures, YearsAveraged)
stats <- rbind(stats0, stats1, stats2, stats3, stats4)
rownames(stats) <- c(rownames(stats0), rownames(stats1), rownames(stats2), rownames(stats3), rownames(stats4))

#stats <- stats[rownames(stats) %in% finalMPList,]

# max of 8 MPs at the moment

 finalMPList_short <- c('TB1.M', 'TB1.D',
                        'TB2.M', 'TB2.D',
                        'TB3.M', 'TB3.D',
                        'TB4.M', 'TB4.D')

 results <- list(mseOMrefB18.2.304.TB1.1, mseOMrefB18.2.304.TB1.2,
                 mseOMrefB18.2.304.TB2,
                 mseOMrefB18.2.304.TB3,
                 mseOMrefB18.2.304.TB4,
                 mseOMrefB18.2.304.TB9.1)
# createTable1(20, results, MPs=finalMPList, MPs_short=finalMPList_short)
 createTable1(20, results, MPs=finalMPList, MPs_short=finalMPList_short)

# ADD TB9
 tmpMPList <- c('TB1.1PT41.t25', 'TB1.2IT5.t25',
                 #'TB2.PT41.t15', 'TB2.IT5.t15',
                 #'TB3.PT41.t15', 'TB3.IT5.t15',
                 #'TB4.PT41.t15', 'TB4.IT5.t15',
                 'TB9.1.PT41.t15', 'TB9.1.IT5.t15')


 tmpMPList_short <- c('TB1.M', 'TB1.D',
                        #'TB2.M', 'TB2.D',
                        #'TB3.M', 'TB3.D',
                        #'TB4.M', 'TB4.D',
                        'TB9.M', 'TB9.D')

 results <- list(mseOMrefB18.2.304.TB1.1, mseOMrefB18.2.304.TB1.2,
                 #mseOMrefB18.2.304.TB2,
                 #mseOMrefB18.2.304.TB3,
                 #mseOMrefB18.2.304.TB4,
                 mseOMrefB18.2.304.TB9.1)


 createTable1(20, results, MPs=tmpMPList, MPs_short=tmpMPList)






#old clunky versions...
tab1 <- NULL
for (i in 1:length(finalMPList)){
  mp <- finalMPList[i]
  print(c(i,mp))
  print(stats[rownames(stats)==mp, c("SBoSBMSY0.5")])
   tab1 <- rbind(tab1, stats[rownames(stats)==mp, c("SBoSBMSY0.5",
                   "GK0.5","PrSBgtSBlim0.5",
                   "Y0.5",
                   "AAVY0.5",
                   "SBoSBMSY0.25", "SBoSBMSY0.75",
                   "Y0.25", "Y0.75")])
}
tab1 <- as.data.frame(cbind(finalMPList, tab1))



################################################################################
# Create Table 2

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK",
     "PrSBgt0.2SB0","PrSBgtSBlim","Y",
     "YoMSY","relCPUE","AAVY",
     "YcvPct","PrYlt0.1MSY")
meanAllPerf <- AllPerformanceMeasures %&% "mean"


tab2List <- as.list(NULL)
yList <- c(1,5,10,20)
for(iy in 1:length(yList)){
  yearsAveraged <- yList[iy]

  stats0 <- performanceStatistics(mseOMrefB18.2.304.TB1.1, AllPerformanceMeasures, YearsAveraged)
  stats1 <- performanceStatistics(mseOMrefB18.2.304.TB1.2, AllPerformanceMeasures, YearsAveraged)
  stats2 <- performanceStatistics(mseOMrefB18.2.304.TB2,   AllPerformanceMeasures, YearsAveraged)
  stats3 <- performanceStatistics(mseOMrefB18.2.304.TB3,   AllPerformanceMeasures, YearsAveraged)
  stats4 <- performanceStatistics(mseOMrefB18.2.304.TB4,   AllPerformanceMeasures, YearsAveraged)
  stats <- rbind(stats0, stats1, stats2, stats3, stats4)
  rownames(stats) <- c(rownames(stats0), rownames(stats1), rownames(stats2), rownames(stats3), rownames(stats4))



  tab <- NULL
  for (mp in finalMPList){
   #can't seem to use meanAllPerf here ?!
   tab <- rbind(tab, stats[rownames(stats)==mp, c("SBoSB0mean","minSBoSB0mean","SBoSBMSYmean","FoFMSYmean",
     "FoFtargmean","GKmean","RKmean",
     "PrSBgt0.2SB0mean","PrSBgtSBlimmean","Ymean",
     "YoMSYmean","relCPUEmean","AAVYmean",
     "YcvPctmean","PrYlt0.1MSYmean")])
  }
  tab2List[[iy]] <- tab
}

gc() #memory check and clean up


delete this
# BET Tuning objective TB2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB2  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[2], interval=3, Report=F, UseCluster=0)))
for(i in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB2." %&% mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB2@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars) <- "TB2." %&% names(mseOMrefB18.2.304.TB2@StockSynthesisModels[[i]]@ProjectedVars)
}
delete this



########################################################################################################
# try out the TCMP-02 BET tunings to ensure that the system works for arbitarty tuning aggregation dates

# BET Tuning objective TB12
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB12  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB12@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB12@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB12@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB12." %&% mseOMrefB18.2.304.TB12@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB12@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB12@StockSynthesisModels[[i]]@ProjectedVars) <- "TB12." %&% names(mseOMrefB18.2.304.TB12@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB12,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB12.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB12.RDA")

# BET Tuning objective TB13
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB13  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1[1:2], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB13@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB13@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB13@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB13." %&% mseOMrefB18.2.304.TB13@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB13@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB13@StockSynthesisModels[[i]]@ProjectedVars) <- "TB13." %&% names(mseOMrefB18.2.304.TB13@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB13,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB13.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB13.RDA")

# BET Tuning objective TB14
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefB18.2.304.TB14  <- runMse(OMrefB18.2.304, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefB18.2.304.TB14@StockSynthesisModels)){
  for(j in 1:length(mseOMrefB18.2.304.TB14@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefB18.2.304.TB14@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB14." %&% mseOMrefB18.2.304.TB14@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefB18.2.304.TB14@StockSynthesisModels)){
  names(mseOMrefB18.2.304.TB14@StockSynthesisModels[[i]]@ProjectedVars) <- "TB14." %&% names(mseOMrefB18.2.304.TB14@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefB18.2.304.TB14,file=paste(getwd(),"/Objects/mseOMrefB18.2.304.TB14.RDA",sep=""))
load(file="Objects/mseOMrefB18.2.304.TB14.RDA")




