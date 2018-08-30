#------------------------------------------------------------------------------
# Example script demonstrating MSE usage
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

source("Source/MseMain.R")


#------------------------------------------------------------------------------
#YFT Demos
#------------------------------------------------------------------------------
# A minimalist demonstration OM test
# Define an MseDefinition object
# OMyftNEr: R-based demo MseDef with minimal process or observation error,
# MSE run with fishery shut down, Only 2 SS specifications, and one replicate
# for each.
source("RScripts/Build OMyftNEr.R")

# Create an MseFramework object for the MseDefinition
print(system.time(OMyftNEr <- createMseFramework(MseDef, UseCluster=0)))

# Save the MseFramework
save(OMyftNEr, file=paste(getwd(),"/Objects/OMyftNEr.RDA",sep=""))

# Load the previously created MseFramework
load(file=paste(getwd(),"/Objects/OMyftNEr.RDA",sep=""))

# Run an MSE; "CC001" has fisheries shut off to test unfished equilibrium recovery dynamics
print(system.time(OMyftNEr <- runMse(OMyftNEr, MPs="CC001", interval=3, Report=F, UseCluster=0)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyftNEr)
projd <- msevizProjectedTimeSeriesData(OMyftNEr)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

# Re-Run the MSE with the C++ projection code this time
print(system.time(OMyftNErCpp <- runMse(OMyftNEr, MPs="CC001", interval=3, Report=F, CppMethod=1, UseCluster=0)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyftNErCpp)
projd <- msevizProjectedTimeSeriesData(OMyftNErCpp)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

# add 100 ton catch bridge
OMyftNEr@MseDef@catchBridge <- as.karray(c(100000))

# Run an MSE; "CC100" for higher effort to test Implementation error bias
print(system.time(OMyftNEr <- runMse(OMyftNEr, MPs="CC100", interval=3, Report=F)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyftNEr)
projd <- msevizProjectedTimeSeriesData(OMyftNEr)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

# Re-Run the MSE with the C++ projection code this time
print(system.time(OMyftNErCpp <- runMse(OMyftNEr, MPs="CC100", interval=3, Report=F, CppMethod=1)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyftNErCpp)
projd <- msevizProjectedTimeSeriesData(OMyftNErCpp)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])


#------------------------------------------------------------------------------
# YFT - a more substantive demonstration MSE; 2 equally weighted SS
# specifications; 108 stochastic replicates
#------------------------------------------------------------------------------
source("Rscripts/Build OMyft2r108.R")

# Create an MseFramework object for the MseDefinition
print(system.time(OMyft2r108 <- createMseFramework(MseDef)))
save(OMyft2r108, file=paste(getwd(), "/Objects/OMyft2r108.RDA", sep=""))
load(file=paste(getwd(), "/Objects/OMyft2r108.RDA", sep=""))

# MSE on a list of candidate MPs
MPL1 <- c("CC200", "CC400","IT1.50","IT3.50","PT41.100.2","PT41.100.9")

# forcing C++ method because it runs faster
print(system.time(OMyft2r108 <- runMse(OMyft2r108, MPs=MPL1, interval=3, Report=F, UseCluster=1, CppMethod=1)))
save(OMyft2r108, file=paste(getwd(), "/Objects/mseOMyft2r108.MPL1.RDA", sep=""))
load(file=paste(getwd(), "/Objects/mseOMyft2r108.MPL1.RDA", sep=""))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyft2r108)
projd <- msevizProjectedTimeSeriesData(OMyft2r108)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0","PrSBgtSBlim","Y","relCPUE","YoMSY","APCY","YcvPct","PrYlt0.1MSY")
YearsAveraged          <- 20

# Table all Performance Measures
performanceStatistics(OMyft2r108, AllPerformanceMeasures, YearsAveraged)

# Table all Performance Measures for a given MP
performanceStatistics(OMyft2r108, AllPerformanceMeasures, YearsAveraged, thisMP="PT41.100.9")

# Table some Performance Measures
performanceStatistics(OMyft2r108, c("SBoSB0","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0"), YearsAveraged)

# Do various performance plots
perfd <- msevizPerformanceData(OMyft2r108, YearsAveraged)


print(plotTOs(perfd, x="S10", y=c("T1","S3","S6","S9")))
print(plotBPs(perfd, indicators=c("T1","S3","S6","S9","S10")))
print(kobeMPs(perfd))


# Do one of the MSE MPs with tuning
load(file=paste(getwd(), "/Objects/OMyft2r108.RDA", sep=""))

TuningPars <- new("TuningParameters")

TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- 20
TuningPars@tuningTarget             <- 1.7
TuningPars@tuningTolerance          <- 0.01

MPL2 <- c("PT41.100.2", "PT41.100.9", "PT41.tune.9")

# forcing C++ method because it runs faster
print(system.time(OMyft2r108 <- runMse(OMyft2r108, TuningPars=TuningPars, MPs=MPL2, interval=3, Report=F, UseCluster=1, CppMethod=1)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyft2r108)
projd <- msevizProjectedTimeSeriesData(OMyft2r108)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

# Do various performance plots
perfd <- msevizPerformanceData(OMyft2r108, YearsAveraged)

print(plotTOs(perfd, x="S10", y=c("T1","S3","S6","S9")))
print(plotBPs(perfd, indicators=c("T1","S3","S6","S9","S10")))
print(kobeMPs(perfd))

# Illustration of using tuned MP along side other MPs and renaming MPs
LastMPs <- getMPs(OMyft2r108)
MPL3    <- list(MP1="CC200", MP2="CC400",MP3="IT1.50",MP4="IT3.50",MP5="PT41.100.2",MP6=LastMPs[[3]])

# forcing C++ method because it runs faster
print(system.time(OMyft2r108 <- runMse(OMyft2r108, MPs=MPL3, interval=3, Report=F, UseCluster=1, CppMethod=1)))

# Plot some key time series
histd <- msevizHistoricTimeSeriesData(OMyft2r108)
projd <- msevizProjectedTimeSeriesData(OMyft2r108)

plotOMruns(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])

# Do various performance plots
perfd <- msevizPerformanceData(OMyft2r108, YearsAveraged)

print(plotTOs(perfd, x="S10", y=c("T1","S3","S6","S9")))
print(plotBPs(perfd, indicators=c("T1","S3","S6","S9","S10")))
print(kobeMPs(perfd))

print("Remaining demos yet to be ported from MSE-IO-BET-YFT")
stop()


#------------------------------------------------------------------------------
# YFT - compare the R, C++ and SS MSY calculations

source("Rscripts/Build OMyft2c108.R")
print(system.time(OMyft2c108<-new("OMss",OMd, Report=F)))
save(OMyft2c108,file=paste(getwd(),"/Objects/OMyft2c108.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMyft2c108.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMyft2r108.RDA",sep=""))

#YFT: compare R and C++ based reference point calculations with the corresponding SS values
print(c(mean(OMyft2r108@SSB0),range(OMyft2r108@SSB0),mean(OMyft2c108@SSB0),range(OMyft2c108@SSB0), mean(OMyft2r108@SSB0ss),range(OMyft2r108@SSB0ss)), digits=3)
print(c(mean(OMyft2r108@SSBMSY),range(OMyft2r108@SSBMSY),mean(OMyft2c108@SSBMSY),range(OMyft2c108@SSBMSY), mean(OMyft2r108@SSBMSYss),range(OMyft2r108@SSBMSYss)), digits=3)
print(c(mean(OMyft2r108@MSY),range(OMyft2r108@MSY),mean(OMyft2c108@MSY),range(OMyft2c108@MSY), mean(OMyft2r108@MSYss),range(OMyft2r108@MSYss)), digits=3)




#------------------------------------------------------------------------------
# YFT - compare the R and C++ projection sub-routines across a range of harvest rates
MPL2 <- c("CC050","CC100","CC200","CC300","CC400")

# R-based projections (with the R-based MSY reference points) - TAC extraction midway through the continuous F and M
print(system.time(mseOMyft2r108.MPL2<-new("MSE",OMyft2r108,MPs=MPL2, interval=3, Report=F,UseCluster=0)))
save(mseOMyft2r108.MPL2,file=paste(getwd(),"/Objects/mseOMyft2r108.MPL2.RDA",sep=""))

# C++ projections (with the R-based MSY reference points, i.e. same OM as above)
print(system.time(mseOMyft2r108.c.MPL2<-new("MSE",OMyft2r108,MPs=MPL2, interval=3, Report=F,UseCluster=0, CppMethod=1)))
save(mseOMyft2r108.c.MPL2,file=paste(getwd(),"/Objects/mseOMyft2r108.c.MPL2.RDA",sep=""))

# R-based projections (with the R-based MSY reference points) and TAC extraction before continuous M and F
print(system.time(mseOMyft2r108.r01.MPL2<-new("MSE",OMyft2r108,MPs=MPL2, interval=3, Report=F,UseCluster=0, TACTime=0.01)))
save(mseOMyft2r108.r01.MPL2,file=paste(getwd(),"/Objects/mseOMyft2r108.r01.MPL2.RDA",sep=""))

# trade-off plot comparing results from 3 different OMs (i.e. R-based vs Cpp projections)
mseOMyft2r108.MPL2@Label <- "R.50"
mseOMyft2r108.c.MPL2@Label <- "C++"
mseOMyft2r108.r01.MPL2@Label <- "R.01"
plotTO.f(mseObj=list(mseOMyft2r108.c.MPL2,mseOMyft2r108.MPL2,mseOMyft2r108.r01.MPL2), MPs=MPL2, ylims <- c(2.5,1,1,1))





#------------------------------------------------------------------------------
# YFT - a full-scale MSE with 2160 stochastic replicates
source("Rscripts/Build OMyft2r.R")
print(system.time(OMyft2r<-new("OMss",OMd, Report=F)))
save(OMyft2r,file=paste(getwd(),"/Objects/OMyft2r.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMyft2r.RDA",sep=""))
print(system.time(mseOMyft2r.MPL1<-new("MSE",OMyft2r,MPs=MPL1, interval=3, Report=F,UseCluster=0)))
save(mseOMyft2r.MPL1,file=paste(getwd(),"/Objects/mseOMyft2r.MPL1.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMyft2r.MPL1.RDA",sep=""))

# standard plots
plotTO.f(mseObj=list(mseOMyft2r.MPL1), MPs=MPL1, ylims <- c(2.5,1,1,1))
plotUS.f(mseObj=mseOMyft2r.MPL1,MPs=MPL1, plotStats=c("SBoSB0","RK","Y"))
plotTS.f(mseOMyft2r.MPL1, plotByRF=F, mwgPlots=T)

# trade-off plot comparing results from 2 different OMs (i.e. 108 vs 2160 reps)
plotTO.f(mseObj=list(mseOMyft2r.MPL1,mseOMyft2r108.MPL1), MPs=MPL1, ylims <- c(2.5,1,1,1))





#------------------------------------------------------------------------------
# YFT - Many MP comparisons with 108 reps
MPL3 <- c("CC050","CC100","CC200","CC300","CC400", "IT1.50", "IT1.50.9", "IT2.50","IT2.50.9","IT3.50","PT41.100.1", "PT41.100.2","PT41.100.9","PT42.125.2","PT82.150.2")
load(file=paste(getwd(),"/Objects/OMyft2r108.RDA",sep=""))
print(system.time(mseOMyft2r108.MPL3<-new("MSE",OMyft2r108,MPs=MPL3, interval=3, Report=F,UseCluster=0)))
save(mseOMyft2r108.MPL3,file=paste(getwd(),"/Objects/mseOMyft2r108.MPL3.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMyft2r108.MPL3.RDA",sep=""))

plotTO.f(mseObj=list(mseOMyft2r108.MPL3), MPs=MPL3, ylims <- c(2.5,1,1,1))
plotUS.f(mseObj=mseOMyft2r108.MPL1,MPs=MPL3, plotStats=c("SBoSB0","RK","Y"))
plotTS.f(mseOMyft2r108.MPL3, plotByRF=F, mwgPlots=T)




#########################################################################################################
#========================================================================================================
#BET Demos
#========================================================================================================

# A minimalist demonstration OM test
# Define an OMd (Operating Model definition object)
# OMbetNEr: R-based demo OM with minimal process or observation error, MSE run with fishery shut down,
# Only 2 SS specifications, and one replicate for each
source("Rscripts/Build OMbetNEr.R")

# Create an OM object for the OMd
print(system.time(OMbetNEr<-new("OMss",OMd, Report=F)))

# Save the OM
save(OMbetNEr,file=paste(getwd(),"/Objects/OMbetNEr.RDA",sep=""))

# Load a previously created OM
load(file=paste(getwd(),"/Objects/OMbetNEr.RDA",sep=""))

# Run an MSE; "CC001" has fisheries shut off to test unfished equilibrium recovery dynamics
print(system.time(mseOMbetNEr<-new("MSE",OMbetNEr,MPs<-"CC001",interval=3, Report=F,UseCluster=0)))

# Plot some key time series, including recruitment and total biomass, which are not part of the MWG standard
plotTS.f(mseOMbetNEr, plotByRF=F, doWorms=F)  #Time series (worm) plots

# Re-Run the MSE with the C++ projection code this time
print(system.time(mseOMbetNEr.c<-new("MSE",OMbetNEr,MPs<-"CC001",interval=3, Report=F,CppMethod=1, UseCluster=0)))
plotTS.f(mseOMbetNEr.c, plotByRF=F, doWorms=F)  #Time series (worm) plots




#========================================================================================================
# BET - a more substantive demonstration MSE; 54 equally weighted SS specifications; 108 stochastic replicates
source("Rscripts/Build OMbet1r108.R")
print(system.time(OMbet1r108<-new("OMss",OMd, Report=F)))
save(OMbet1r108,file=paste(getwd(),"/Objects/OMbet1r108.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMbet1r108.RDA",sep=""))

# MSE on a list of candidate MPs
MPLb1 <- c("CC050", "CC100","IT1.50","IT3.50","PT41.100.2","PT41.100.9")
print(system.time(mseOMbet1r108.MPLb1<-new("MSE",OMbet1r108,MPs=MPLb1, interval=3, Report=F,UseCluster=0)))
save(mseOMbet1r108.MPLb1,file=paste(getwd(),"/Objects/mseOMbet1r108.MPLb1.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMbet1r108.MPLb1.RDA",sep=""))

# Tables of Performance Measures
tableMSE.f(mseOMbet1r108.MPLb1)      # aggregated over fisheries and regions
tableMSEbyRF.f(mseOMbet1r108.MPLb1)  # disaggregated by fisheries and regions (not all PMs)

# Compare some specific stats for MPs "CC200" and "PT41.100.2"
# "y10" indicates first 10 year projection period
# 0,5, 0.1, 0.9 =percentiles
c(tableMSE.f(mseOMbet1r108.MPLb1)["CC100y10",c("SBoSBMSY0.5","SBoSBMSY0.1","SBoSBMSY0.9")])
c(tableMSE.f(mseOMbet1r108.MPLb1)["PT41.100.2y10",c("SBoSBMSY0.5","SBoSBMSY0.1","SBoSBMSY0.9")])

# Bi-variate trade-off plots
plotTO.f(mseObj=list(mseOMbet1r108.MPLb1), MPs=MPLb1, ylims <- c(2.5,1,1,1))

# Confidence interval plots (Udon-Soba plots)
dev.new(width=6,height=8)
plotUS.f(mseObj=mseOMbet1r108.MPLb1,MPs=MPLb1, plotStats=c("SBoSB0","RK","Y"))

# Time series plots
dev.new(width=6,height=4)
plotTS.f(mseOMbet1r108.MPLb1, plotByRF=F, mwgPlots=T)




#========================================================================================================
# BET - compare the R, C++ and SS MSY calculations

source("Rscripts/Build OMbet1c108.R")
print(system.time(OMbet1c108<-new("OMss",OMd, Report=F)))
save(OMbet1c108,file=paste(getwd(),"/Objects/OMbet1c108.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMbet1c108.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMbet1r108.RDA",sep=""))

#BET: compare R and C++ based reference point calculations with the corresponding SS values
print(c(mean(OMbet1r108@SSB0),range(OMbet1r108@SSB0),mean(OMbet1c108@SSB0),range(OMbet1c108@SSB0), mean(OMbet1r108@SSB0ss),range(OMbet1r108@SSB0ss)), digits=3)
print(c(mean(OMbet1r108@SSBMSY),range(OMbet1r108@SSBMSY),mean(OMbet1c108@SSBMSY),range(OMbet1c108@SSBMSY), mean(OMbet1r108@SSBMSYss),range(OMbet1r108@SSBMSYss)), digits=3)
print(c(mean(OMbet1r108@MSY),range(OMbet1r108@MSY),mean(OMbet1c108@MSY),range(OMbet1c108@MSY), mean(OMbet1r108@MSYss),range(OMbet1r108@MSYss)), digits=3)




#========================================================================================================
# BET - compare the R and C++ projection sub-routines across a range of harvest rates
MPLb2 <- c("CC050","CC100","CC150","CC200")

# R-based projections (with the R-based MSY reference points) - TAC extraction midway through the continuous F and M
print(system.time(mseOMbet1r108.MPLb2<-new("MSE",OMbet1r108,MPs=MPLb2, interval=3, Report=F,UseCluster=0)))
save(mseOMbet1r108.MPLb2,file=paste(getwd(),"/Objects/mseOMbet1r108.MPLb2.RDA",sep=""))

# C++ projections (with the R-based MSY reference points, i.e. same OM as above)
print(system.time(mseOMbet1r108.c.MPLb2<-new("MSE",OMbet1r108,MPs=MPLb2, interval=3, Report=F,UseCluster=0, CppMethod=1)))
save(mseOMbet1r108.c.MPLb2,file=paste(getwd(),"/Objects/mseOMbet1r108.c.MPLb2.RDA",sep=""))

# R-based projections (with the R-based MSY reference points) and TAC extraction before continuous M and F
print(system.time(mseOMbet1r108.r01.MPLb2<-new("MSE",OMbet1r108,MPs=MPLb2, interval=3, Report=F,UseCluster=0, TACTime=0.01)))
save(mseOMbet1r108.r01.MPLb2,file=paste(getwd(),"/Objects/mseOMbet1r108.r01.MPLb2.RDA",sep=""))

# trade-off plot comparing results from 3 different OMs (i.e. R-based vs Cpp projections)
mseOMbet1r108.MPLb2@Label <- "R.50"
mseOMbet1r108.c.MPLb2@Label <- "C++"
mseOMbet1r108.r01.MPLb2@Label <- "R.01"
plotTO.f(mseObj=list(mseOMbet1r108.c.MPLb2,mseOMbet1r108.MPLb2,mseOMbet1r108.r01.MPLb2), MPs=MPLb2, ylims <- c(2.5,1,1,1))





#========================================================================================================
# BET - a full-scale MSE with 2160 stochastic replicates
source("Rscripts/Build OMbet1r.R")
print(system.time(OMbet1r<-new("OMss",OMd, Report=F)))
save(OMbet1r,file=paste(getwd(),"/Objects/OMbet1r.RDA",sep=""))
load(file=paste(getwd(),"/Objects/OMbet1r.RDA",sep=""))
print(system.time(mseOMbet1r.MPLb1<-new("MSE",OMbet1r,MPs=MPLb1, interval=3, Report=F,UseCluster=0)))
save(mseOMbet1r.MPLb1,file=paste(getwd(),"/Objects/mseOMbet1r.MPLb1.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMbet1r.MPLb1.RDA",sep=""))

# standard plots
plotTO.f(mseObj=list(mseOMbet1r.MPLb1), MPs=MPLb1, ylims <- c(2.5,1,1,1))
plotUS.f(mseObj=mseOMbet1r.MPLb1,MPs=MPLb1, plotStats=c("SBoSB0","RK","Y"))
plotTS.f(mseOMbet1r.MPLb1, plotByRF=F, mwgPlots=T)

# trade-off plot comparing results from 2 different OMs (i.e. 108 vs 2160 reps)
plotTO.f(mseObj=list(mseOMbet1r.MPLb1,mseOMbet1r108.MPLb1), MPs=MPLb1, ylims <- c(2.5,1,1,1))





#========================================================================================================
# BET - Many MP comparisons with 108 reps
MPLb3 <- c("CC050","CC100","CC150","CC200", "IT1.50", "IT1.50.9", "IT2.50","IT2.50.9","IT3.50","PT41.100.1", "PT41.100.2","PT41.100.9","PT42.125.2","PT82.150.2")
load(file=paste(getwd(),"/Objects/OMbet1r108.RDA",sep=""))
print(system.time(mseOMbet1r108.MPLb3<-new("MSE",OMbet1r108,MPs=MPLb3, interval=3, Report=F,UseCluster=0)))
save(mseOMbet1r108.MPLb3,file=paste(getwd(),"/Objects/mseOMbet1r108.MPLb3.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMbet1r108.MPLb3.RDA",sep=""))

plotTO.f(mseObj=list(mseOMbet1r108.MPLb3), MPs=MPLb3, ylims <- c(2.5,1,1,1))
plotUS.f(mseObj=mseOMbet1r108.MPLb1,MPs=MPLb3, plotStats=c("SBoSB0","RK","Y"))
plotTS.f(mseOMbet1r108.MPLb3, plotByRF=F, mwgPlots=T)









#########################################################################################################
# miscellaneous stuff...

#inspect the SS diagnositcs from an individual SS model using r4ss
yft <- SS_output(dir="H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY3\\R4MvEst_h70_M06_t00_q1", covar=F, ncols=213,forecast=F) # ssoutput.f no longer req"d, r4ss fixed
SS_plots(yft,uncertainty=F)

gc() # R garbage collection function can free up useful memory
closeAllConnections()  # useful if something crashes while files are open
