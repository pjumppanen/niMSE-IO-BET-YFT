# R Script for conducting IO-YFT MSE for MSE task force 2019


#
#library(devtools)
#install_github("iagomosqueira/mseviz") # if installing on new machine for first time
#library(mseviz)

#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master")  # Bowen cloud working directory
#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror")  # Set the working directory
setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")  # Set the working directory
#setwd("M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")  # Set the working directory

#setwd("C:\\tmp")  # Set the working directory local - test indicated a 10% speed improvement from synced back-up dir
#sourceDir <- "M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master"  # Set the working directory
rm(list=ls(all=TRUE))

#for Tinn-R only:
.trPaths <- file.path(Sys.getenv("TEMP"), "Tinn-R", c("", "search.txt", "objects.txt", "file.r", "selection.r", "block.r", "lines.r", "reformat-input.r", "reformat-output.r"), fsep="\\")

source("Source\\MseMain.R")
#source("Source\\mseviz2.R")
#library(ggstance)


# standard MP lists
MPListC <- c("CC001","CC413")
MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t50","IT5.t50")
MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")





################################################################################
# Set the plotting reference points
# load(file="Objects/mseOMrefY19.2SL.250.RDA")

FLim  <- mseOMrefY19.2SL.250.TY18.3@MseDef@Flim
SBLim  <- mseOMrefY19.2SL.250.TY18.3@MseDef@SBlim
FTarg <- 1.
SBTarg <- 1.

Cref <- 409. # 409=2017 YFT catch
YFTTargs <- c(SBTarg, FTarg)
YFTLims  <- c(SBLim, FLim)

names(YFTTargs) <- c("S3", "S4")
names(YFTLims) <- c("S3", "S4")
names(Cref) <- "S10"



#########################################################################################################
# OMrefY19.2SL - a 69 model fractional, converged grid for the 2019 MSE task force
# Create or load an OM object
source('Rscripts\\Build OM Model-OMrefY19.2SL.250.R')
#print(system.time(OMrefY19.2SL.250 <- createMseFramework(MseDef)))
#save(OMrefY19.2SL.250,file=paste(getwd(),"/Objects/OMrefY19.2SL.250.RDA",sep=""))
load(file="Objects/OMrefY19.2SL.250.RDA")


#########################################################################################################
# OMrobY19.2opt - a 20-30 model subset of OMrefY19.2SL in which all models have Bcurrent>=BMSY to illustrate performance
# in case the YFT assessment and reference OM are unduly pessimistic

# Create or load an OM object
source('Rscripts\\Build OM Model-OMrobY19.2opt.250.R')
#print(system.time(OMrobY19.2opt.250 <- createMseFramework(MseDef)))
#save(OMrobY19.2opt.250,file=paste(getwd(),"/Objects/OMrobY19.2opt.250.RDA",sep=""))
load(file="Objects/OMrobY19.2opt.250.RDA")




# Run the MSE without tuning (fishing moratorium and current catch)
print(system.time(mseOMrefY19.2SL.250.CC  <- runMse(OMrefY19.2SL.250,MPs <- c("CC001","CC413"),interval=3, Report=F,UseCluster=1)))

# Time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.2SL.250.CC)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.2SL.250.CC)
plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)")

# Time averaged plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY19.2SL.250.CC, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)



###################################################################################
# set the tuning objectives
# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.1 <- new("TuningParameters")
TCMP.Y18.1@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.1@performanceMeasureClass  <- "0.5"
TCMP.Y18.1@performanceMeasureYears  <- c(2024,2024)
TCMP.Y18.1@tuningTarget             <- 1.0
TCMP.Y18.1@tuningTolerance          <- 0.01
TCMP.Y18.1@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)



###################################################################################
# tune 3 MPs X 3 tuning objectives

# fails with TAC limit 0f 25%, okay with 50%
MPL <- list(M.Y18.1 = MPList0[4], D.Y18.1 = MPList0[5], C.Y18.1 = MPList0[3])
print(system.time(mseOMrefY19.2SL.250.TY18.1  <- runMse(OMrefY19.2SL.250, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(mseOMrefY19.2SL.250.TY18.1)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(mseOMrefY19.2SL.250.TY18.1,file=paste(getwd(),"/Objects/mseOMrefY19.2SL.250.TY18.1.RDA",sep=""))
load(file="Objects/mseOMrefY19.2SL.250.TY18.1.RDA")

# fails with TAC limit 0f 15%, ok with 25%
MPL <- list(M.Y18.2 = MPList0[1], D.Y18.2 = MPList0[2], C.Y18.2 = MPList0[3])
print(system.time(mseOMrefY19.2SL.250.TY18.2  <- runMse(OMrefY19.2SL.250, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2.MPs <- getMPs(mseOMrefY19.2SL.250.TY18.2)
TY18.2.MPs[[1]]@tuneError
TY18.2.MPs[[2]]@tuneError
TY18.2.MPs[[3]]@tuneError
save(mseOMrefY19.2SL.250.TY18.2,file=paste(getwd(),"/Objects/mseOMrefY19.2SL.250.TY18.2.RDA",sep=""))
load(file="Objects/mseOMrefY19.2SL.250.TY18.2.RDA")

# fails with TAC limit 0f 15%, ok with 25%
MPL <- list(M.Y18.3 = MPList0[1], D.Y18.3 = MPList0[2], C.Y18.3 = MPList0[3])
print(system.time(mseOMrefY19.2SL.250.TY18.3  <- runMse(OMrefY19.2SL.250, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.3.MPs <- getMPs(mseOMrefY19.2SL.250.TY18.3)
TY18.3.MPs[[1]]@tuneError
TY18.3.MPs[[2]]@tuneError
TY18.3.MPs[[3]]@tuneError
save(mseOMrefY19.2SL.250.TY18.3,file=paste(getwd(),"/Objects/mseOMrefY19.2SL.250.TY18.3.RDA",sep=""))
load(file="Objects/mseOMrefY19.2SL.250.TY18.3.RDA")



###################################################################################
# plot the 3 MPs X 3 tuning objectives aggregate plots

#MPL <-list(MP1="PT41.100.2", MP2="PT41.100.9", MP3="PT41.tune.9")
#MPL <- list(M.Y18.1 = MPList1[1] , )

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY19.2SL.250.TY18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY19.2SL.250.TY18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY19.2SL.250.TY18.3, YearsAveraged))
print(plotTOs2(perfd))
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.2SL.250.TY18.1)
histd <- merge(histd,msevizHistoricTimeSeriesData(mseOMrefY19.2SL.250.TY18.2), all=T)
histd <- merge(histd,msevizHistoricTimeSeriesData(mseOMrefY19.2SL.250.TY18.3), all=T)

projd <- msevizProjectedTimeSeriesData(mseOMrefY19.2SL.250.TY18.1)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.2SL.250.TY18.2), all=T)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.2SL.250.TY18.3), all=T)

projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)")




###################################################################################
# Run the optimistic robustness scenario OMrob19.2opt with the same 9 tuned MPs as the reference case

MPL <- getMPs(mseOMrefY19.2SL.250.TY18.1)
MPL <- c(unlist(MPL),unlist(getMPs(mseOMrefY19.2SL.250.TY18.2)),unlist(getMPs(mseOMrefY19.2SL.250.TY18.3)))

print(system.time(OMrobB19.2.opt.250.proj <- runMse(OMrobY19.2opt.250, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

#MPs[[1:3]]@tuneError

save(OMrobB19.2.opt.250.proj, file="Objects/OMrobB19.2.opt.250.proj.RDA")
load(file="Objects/OMrobB19.2.opt.250.proj.RDA")

YearsAveraged <- 20
perfd <- msevizPerformanceData(OMrobB19.2.opt.250.proj, YearsAveraged)
#print(plotTOs2(perfd))
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(OMrobB19.2.opt.250.proj)

projd <- msevizProjectedTimeSeriesData(OMrobB19.2.opt.250.proj)
projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = 2017, firstMPYr=2021)






###################################################################################
# run the 2 feedback-based MPs X central tuning objective for the "optimistic" robustness OM
# i.e.



###################################################################################
# run the 2 feedback-based MPs X central tuning objective for the SD robustness OM





# -----------------------------------------
# Case 1:
#   30% CPUE CV
# -----------------------------------------
# load model definition, OMrefB19.5

getParameters(OMrefY19.2SL.250)$IACin
getParameters(OMrefY19.2SL.250)$Icv
OMrefY19.2SLcv3.250 <- setParameters(OMrefY19.2SL.250, parameters=list(IACin=0.5, Icv=c(0.30000,0.30001)))
getParameters(OMrefY19.2SLcv3.250)$Icv

print(system.time(mseOMrefY19.2SLcv3.250.proj <- runMse(OMrefY19.2SLcv3.250, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.2SLcv3.250.proj, file="Objects/mseOMrefY19.2SLcv3.250.proj.RDA")

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY19.2SLcv3.250.proj, YearsAveraged)
#print(plotTOs2(perfd))
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.2SLcv3.250.proj)

projd <- msevizProjectedTimeSeriesData(mseOMrefY19.2SLcv3.250.proj)
projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = 2017, firstMPYr=2021)




# -----------------------------------------
# Case 2:
#   10% reported over catch
# -----------------------------------------
# load model definition, OMrefB19.5

proyears <- OMrefY19.2SL.250@MseDef@proyears

# set 10% reported over catch implemented as 1.1 times TAC
OMrefY19.2SLoc10.250 <- setParameters(OMrefY19.2SL.250, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears))))

# run projections
print(system.time(mseOMrefY19.2SLoc10.250.proj <- runMse(OMrefY19.2SLoc10.250, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.2SLoc10.250.proj, file="Objects/mseOMrefY19.2SLoc10.250.proj.RDA")

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY19.2SLoc10.250.proj, YearsAveraged)
#print(plotTOs2(perfd))
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.2SLoc10.250.proj)

projd <- msevizProjectedTimeSeriesData(mseOMrefY19.2SLoc10.250.proj)
projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = 2017, firstMPYr=2021)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = 2017, firstMPYr=2021)





# -----------------------------------------
# Case 3:
#   10% unreported over catch ... need to resync with git before this will work
# -----------------------------------------
proyears <- OMrefY19.2SL.250@MseDef@proyears

# set 10% unreported under catch implemented as 1.1 times TAC and catch bias 1/1.1
OMrefY19.2SLiuu10.250 <- setParameters(OMrefY19.2SL.250, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears)), Cbmean=1.0 / 1.1))

# run projections
print(system.time(mseOMrefY19.2SLiuu10.250.proj <- runMse(OMrefY19.2SLiuu10.250, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.2SLiuu10.250.proj, file="Objects/mseOMrefY19.2SLiuu10.250.proj.RDA")





 ...old below here...










# Run the MSE with tuning to rebuilding target
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2024,2024)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
#print(system.time(mseOMrefY18.1.20.1           <- runMse(OMrefY18.1.20,MPs = MPList1[1], TuningPars = TuningPars, interval=3, Report=F,UseCluster=0)))
print(system.time(mseOMrefY18.1.20.1c          <- runMse(OMrefY18.1.20,MPs = MPList1[1:2], TuningPars = TuningPars, interval=3, Report=F,UseCluster=1)))

# Time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY18.1.20.1c)
projd <- msevizProjectedTimeSeriesData(mseOMrefY18.1.20.1c)
plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)")

# Time averaged plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.20.1c, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)



# Run the MSE with tuning to forward dated summation period (new BET tuning)
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain          <- c(-2,2)
print(system.time(mseOMrefY18.1.20.2          <- runMse(OMrefY18.1.20,MPs = MPList1[1:2], TuningPars = TuningPars, interval=3, Report=F,UseCluster=1)))

# Time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefY18.1.20.2)
projd <- msevizProjectedTimeSeriesData(mseOMrefY18.1.20.2)
plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)")

# Time averaged plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.20.2, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)







plotOMruns2(histd[histd$qname=="PrGreen",], projd[projd$qname=="PrGreen",])

plotOMruns2(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
plotOMruns2(histd[histd$qname=="C",], projd[projd$qname=="C",])
plotOMruns2(histd[histd$qname=="CPUE(aggregate)",], projd[projd$qname=="CPUE(aggregate)",])
plotOMruns2(histd[histd$qname=="F/FMSY",], projd[projd$qname=="F/FMSY",])



TuningPars@performanceMeasureYears  <- c(2019,2038)
print(system.time(mseOMrefY18.1.20.2c          <- runMse(OMrefY18.1.20,MPs = MPList1[1], TuningPars = TuningPars, interval=3, Report=F,UseCluster=1)))

TuningPars@performanceMeasureYears  <- 20
print(system.time(mseOMrefY18.1.20.3c          <- runMse(OMrefY18.1.20,MPs = MPList1[1:2], TuningPars = TuningPars, interval=3, Report=F,UseCluster=1)))


# Other plots
AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0","PrSBgtSBlim","Y","relCPUE","YoMSY","APCY","YcvPct","PrYlt0.1MSY")
YearsAveraged          <- 20


# Do various performance plots
perfd <- msevizPerformanceData(mseOMrefY18.1.20.3c, YearsAveraged,MPsSub="PT41.t15")

print(plotTOs2(perfd))
print(plotBPs2(perfd))
print(kobeMPs2(perfd))



#Output Tables
# Table all Performance Measures
# performanceStatistics(OMyft2r108, AllPerformanceMeasures, YearsAveraged)

# Table all Performance Measures for a given MP
# performanceStatistics(OMyft2r108, AllPerformanceMeasures, YearsAveraged, thisMP="PT41.100.9")

# Table subset of Performance Measures, for subset of MPs
performanceStatistics(mseOMrefY18.1.20.3c, c("SBoSB0","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0"), YearsAveraged, thisMP="PT41.t15")


#Try resetting RecCV to 0
OMrefY18.1.20.Rcv0 <- OMrefY18.1.20
for(iom in 1:length(OMrefY18.1.20.Rcv0@StockSynthesisModels)){
  OMrefY18.1.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT <-  OMrefY18.1.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT * 1. + 0.
  OMrefY18.1.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs <-  OMrefY18.1.20.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs * 1. + 0.
}
print(system.time(mseOMrefY18.1.20.Rcv0.CC          <- runMse(OMrefY18.1.20.Rcv0,MPs <- c("CC001","CC413"),interval=3, Report=F,UseCluster=1)))
histd.tmp <- msevizHistoricTimeSeriesData(mseOMrefY18.1.20.Rcv0.CC)
projd.tmp <- msevizProjectedTimeSeriesData(mseOMrefY18.1.20.Rcv0.CC)
plotOMruns2(histd.tmp, projd.tmp, "Recruitment", ylab= "Recruitment")

# YFT Tuning objective 5
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2024,2024)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.20.Rcv0.TY5  <- runMse(OMrefY18.1.20.Rcv0, TuningPars=TuningPars, MPs=MPList1[1:2], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels)){
  names(mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars) <- "TY5." %&% names(mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY5." %&% mseOMrefY18.1.20.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
histd.tmp <- msevizHistoricTimeSeriesData(mseOMrefY18.1.20.Rcv0.TY5)
projd.tmp <- msevizProjectedTimeSeriesData(mseOMrefY18.1.20.Rcv0.TY5)
plotOMruns2(histd.tmp, projd.tmp, "Recruitment", ylab= "Recruitment")





#########################################################################################################
# Create OMrefY18.1.250 - full model for WPTT/WPM 2018 -
#########################################################################################################


# Create or load an OM object
source('Rscripts\\Build OM Model-OMrefY18.1.250.R')
print(system.time(OMrefY18.1.250 <- createMseFramework(MseDef)))

# error in weight-at-age file: R4MvEst_h70_sr6_M08_t0001_q0_x3_i10C_SS_CLRW...network failure?

save(OMrefY18.1.250,file=paste(getwd(),"/Objects/OMrefY18.1.250.RDA",sep=""))
#load(file=paste(getwd(),"/Objects/OMrefY18.1.250.RDA",sep=""))
print(system.time(mseOMrefY18.1.250.CC          <- runMse(OMrefY18.1.250,MPs <- c("CC001","CC413"),interval=3, Report=F,UseCluster=1)))
save(mseOMrefY18.1.250.CC,file=paste(getwd(),"/Objects/mseOMrefY18.1.250.CC.RDA",sep=""))

# Plot some key time series
histd.CC <- msevizHistoricTimeSeriesData(mseOMrefY18.1.250.CC)
projd.CC <- msevizProjectedTimeSeriesData(mseOMrefY18.1.250.CC)
plotOMruns2(histd.CC, projd.CC, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd.CC, projd.CC, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.CC, projd.CC, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd.CC, projd.CC, "C", ylab= "Catch (1000t)")




# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.1 <- new("TuningParameters")
TCMP.Y18.1@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.1@performanceMeasureClass  <- "0.5"
TCMP.Y18.1@performanceMeasureYears  <- c(2024,2024)
TCMP.Y18.1@tuningTarget             <- 1.0
TCMP.Y18.1@tuningTolerance          <- 0.01
TCMP.Y18.1@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)


MPL <- list(M.Y18.1 = MPList1[1], D.Y18.1 = MPList1[2], C.Y18.1 = MPList1[3])
print(system.time(mseOMrefY18.1.250.TY18.1  <- runMse(OMrefY18.1.250, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(mseOMrefY18.1.250.TY18.1)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(mseOMrefY18.1.250.TY18.1,file=paste(getwd(),"/Objects/mseOMrefY18.1.250.TY18.1.RDA",sep=""))
load(file="Objects/mseOMrefY18.1.250.TY18.1.RDA")

MPL <- list(M.Y18.2 = MPList1[1], D.Y18.2 = MPList1[2], C.Y18.2 = MPList1[3])
print(system.time(mseOMrefY18.1.250.TY18.2  <- runMse(OMrefY18.1.250, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2.MPs <- getMPs(mseOMrefY18.2.250.TY18.2)
TY18.2.MPs[[1]]@tuneError
TY18.2.MPs[[2]]@tuneError
TY18.2.MPs[[3]]@tuneError
save(mseOMrefY18.1.250.TY18.2,file=paste(getwd(),"/Objects/mseOMrefY18.1.250.TY18.2.RDA",sep=""))
load(file="Objects/mseOMrefY18.1.250.TY18.2.RDA")

MPL <- list(M.Y18.3 = MPList1[1], D.Y18.3 = MPList1[2], C.Y18.3 = MPList1[3])
print(system.time(mseOMrefY18.1.250.TY18.3  <- runMse(OMrefY18.1.250, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.3.MPs <- getMPs(mseOMrefY18.1.250.TY18.3)
TY18.3.MPs[[1]]@tuneError
TY18.3.MPs[[2]]@tuneError
TY18.3.MPs[[3]]@tuneError
save(mseOMrefY18.1.250.TY18.3,file=paste(getwd(),"/Objects/mseOMrefY18.1.250.TY18.3.RDA",sep=""))
load(file="Objects/mseOMrefY18.1.250.TY18.3.RDA")


################################################################################
# broader suite of MPs for tuning Y18.2 only
MPLfull <- list(PT41.t15.Y18.2   = "PT41.t15",
                 IT5.t15.Y18.2   = "IT5.t15",
                 PT41.t10.Y18.2  = "PT41.t10",
                 PT80.t15.Y18.2  = "PT80.t15",
                 IT5.t10.Y18.2   = "IT5.t10",
                 IT10.t15.Y18.2     = "IT10.t15",
                 IT5.t15.l1.Y18.2   = "IT5.t15.l1",
                 PT80.t10.Y18.2     = "PT80.t10")

print(system.time(mseOMrefY18.1.250.TY18.2.all  <- runMse(OMrefY18.1.250, TuningPars=TCMP.Y18.2, MPs=MPLfull, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2.all.MPs <- getMPs(mseOMrefY18.1.250.TY18.2.all)
TY18.2.all.MPs[[1]]@tuneError
TY18.2.all.MPs[[2]]@tuneError
TY18.2.all.MPs[[3]]@tuneError
save(mseOMrefY18.1.250.TY18.2.all,file=paste(getwd(),"/Objects/mseOMrefY18.1.250.TY18.2.all.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.1.250.TY18.2.all, YearsAveraged)
plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.1.250.TY18.2.all)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

#
################################################################################



################################################################################
# repeat the Y18.1 results with the R code and cpp tuning (to test the high F implications)
print(system.time(mseOMrefY18.5.250.TY18.1.R  <- runMse(OMrefY18.5.250, MPs=getMPs(mseOMrefY18.5.250.TY18.1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
# repeat the Y18.1 results with the cpp effort ceiling = 1.61 (80% removal of highest exploitation rate)
print(system.time(mseOMrefY18.5.250.TY18.1.C80  <- runMse(OMrefY18.5.250, MPs=getMPs(mseOMrefY18.5.250.TY18.1), EffortCeiling=1.61, CppMethod=1, interval=3, Report=F, UseCluster=1)))

# plots comparing the R and cpp code to show the high F implementation issue
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.5.250.TY18.1, YearsAveraged)
perfd$mp <- as.character(perfd$mp) %&% ".cpp"
perfdR <- msevizPerformanceData(mseOMrefY18.5.250.TY18.1.R, YearsAveraged)
perfdR$mp <- as.character(perfdR$mp) %&% ".R"
perfdC80 <- msevizPerformanceData(mseOMrefY18.5.250.TY18.1.C80, YearsAveraged)
perfdC80$mp <- as.character(perfdC80$mp) %&% ".C80"
perfd <- rbind(perfd, perfdR, perfdC80)

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
#
################################################################################



#MPL <-list(MP1="PT41.100.2", MP2="PT41.100.9", MP3="PT41.tune.9")
MPL <- list(M.Y18.1 = MPList1[1] , )

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.5.250.TY18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.5.250.TY18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.5.250.TY18.3, YearsAveraged))
print(plotTOs2(perfd, MPs= MPList1[1:2]))




################################################################################
#make a combined Kobe plot for all (most) MPs and all tuning levels
load(file="Objects/mseOMrefY18.5.250.TY18.1.RDA")
load(file="Objects/mseOMrefY18.5.250.TY18.2.RDA")
load(file="Objects/mseOMrefY18.5.250.TY18.3.RDA")

yearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.250.TY18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.250.TY18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.250.TY18.3, YearsAveraged))

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.Y18.1', 'D.Y18.1', 'C.Y18.1',     'M.Y18.2', 'D.Y18.2', 'C.Y18.2',   'M.Y18.3', 'D.Y18.3', 'C.Y18.3')
createTable1(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.Y18.1', 'D.Y18.1',    'M.Y18.2', 'D.Y18.2',   'M.Y18.3', 'D.Y18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.Y18.1', 'D.Y18.1',    'M.Y18.2', 'D.Y18.2',   'M.Y18.3', 'D.Y18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")


################################################################################
#make a combined Kobe plot for several MPs for the middle tuning level
load(file="Objects/mseOMrefY18.5.250.TY18.2.all.RDA")

yearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.5.250.TY18.2.all, YearsAveraged)

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.Y18.1', 'D.Y18.1', 'C.Y18.1',     'M.Y18.2', 'D.Y18.2', 'C.Y18.2',   'M.Y18.3', 'D.Y18.3', 'C.Y18.3')
createTable1(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.Y18.1', 'D.Y18.1',    'M.Y18.2', 'D.Y18.2',   'M.Y18.3', 'D.Y18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefY18.5.250.TY18.1, mseOMrefY18.5.250.TY18.2, mseOMrefY18.5.250.TY18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefY18.1.250.TY18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefY18.1.250.TY18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.Y18.1', 'D.Y18.1',    'M.Y18.2', 'D.Y18.2',   'M.Y18.3', 'D.Y18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")















xxx old below here




# YFT Tuning objective TY1
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY1  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY1@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY1@StockSynthesisModels[[i]]@ProjectedVars) <- "TY1." %&% names(mseOMrefY18.1.200.TY1@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.200.TY1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY1." %&% mseOMrefY18.1.200.TY1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefY18.1.200.TY1,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY1.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY1.RDA",sep=""))

# YFT Tuning objective 5
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2024,2024)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY5  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY5@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.TY5@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY5." %&% mseOMrefY18.1.200.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.TY5@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY5@StockSynthesisModels[[i]]@ProjectedVars) <- "TY5." %&% names(mseOMrefY18.1.200.TY5@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.TY5,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY5.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY5.RDA",sep=""))






# YFT Tuning objective 2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY2  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY2@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.TY2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY2." %&% mseOMrefY18.1.200.TY2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.TY2@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY2@StockSynthesisModels[[i]]@ProjectedVars) <- "TY2." %&% names(mseOMrefY18.1.200.TY2@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.TY2,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY2.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY2.RDA",sep=""))

# YFT Tuning objective 3
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY3  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY3@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.TY3@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY3." %&% mseOMrefY18.1.200.TY3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.TY3@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY3@StockSynthesisModels[[i]]@ProjectedVars) <- "TY3." %&% names(mseOMrefY18.1.200.TY3@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.TY3,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY3.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY3.RDA",sep=""))




# YFT Tuning objective 4
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY4  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY4@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.TY4@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY4." %&% mseOMrefY18.1.200.TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.TY4@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY4@StockSynthesisModels[[i]]@ProjectedVars) <- "TY4." %&% names(mseOMrefY18.1.200.TY4@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.TY4,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY4.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY4.RDA",sep=""))


# YFT Tuning objective 4, TAC change constraint relaxed
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-1,1)
print(system.time(mseOMrefY18.1.200.0TY4  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList0, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.0TY4@StockSynthesisModels)){
  names(mseOMrefY18.1.200.0TY4@StockSynthesisModels[[i]]@ProjectedVars) <- "TY4." %&% names(mseOMrefY18.1.200.0TY4@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.200.0TY4@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.0TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY4." %&% mseOMrefY18.1.200.0TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefY18.1.200.0TY4,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.0TY4.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.0TY4.RDA",sep=""))


# YFT Tuning objective TY6
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 0.8
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY6  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList0, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY6@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY6@StockSynthesisModels[[i]]@ProjectedVars) <- "TY6." %&% names(mseOMrefY18.1.200.TY6@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.200.TY6@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY6@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY6." %&% mseOMrefY18.1.200.TY6@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefY18.1.200.TY6,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY6.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY6.RDA",sep=""))

# YFT Tuning objective TY7
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.2
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.TY7  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList0, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.TY7@StockSynthesisModels)){
  names(mseOMrefY18.1.200.TY7@StockSynthesisModels[[i]]@ProjectedVars) <- "TY7." %&% names(mseOMrefY18.1.200.TY7@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.200.TY7@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.TY7@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY7." %&% mseOMrefY18.1.200.TY7@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefY18.1.200.TY7,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY7.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.TY7.RDA",sep=""))








# Repeat tunings with MPLIst2


# YFT Tuning objective TY1
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2019,2038) # =20 in this case
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.2TY1  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.2TY1@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.2TY1@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.2TY1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY1." %&% mseOMrefY18.1.200.2TY1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.2TY1@StockSynthesisModels)){
  names(mseOMrefY18.1.200.2TY1@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY1." %&% names(mseOMrefY18.1.200.2TY1@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.2TY1,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY1.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY1.RDA",sep=""))


# YFT Tuning objective 2
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.5
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.2TY2  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.2TY2@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.2TY2@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.2TY2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY2." %&% mseOMrefY18.1.200.2TY2@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.2TY2@StockSynthesisModels)){
  names(mseOMrefY18.1.200.2TY2@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY2." %&% names(mseOMrefY18.1.200.2TY2@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.2TY2,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY2.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY2.RDA",sep=""))

# YFT Tuning objective 3
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.2TY3  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.2TY3@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.2TY3@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.2TY3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY3." %&% mseOMrefY18.1.200.2TY3@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.2TY3@StockSynthesisModels)){
  names(mseOMrefY18.1.200.2TY3@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY3." %&% names(mseOMrefY18.1.200.2TY3@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.2TY3,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY3.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY3.RDA",sep=""))

# YFT Tuning objective 4
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "mean"
TuningPars@performanceMeasureYears  <- c(2019,2038)
TuningPars@tuningTarget             <- 0.7
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.2TY4  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.2TY4@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.2TY4@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.2TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY4." %&% mseOMrefY18.1.200.2TY4@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.2TY4@StockSynthesisModels)){
  names(mseOMrefY18.1.200.2TY4@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY4." %&% names(mseOMrefY18.1.200.2TY4@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.2TY4,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY4.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY4.RDA",sep=""))

# YFT Tuning objective 5
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2024,2024)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.2TY5  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList2, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.2TY5@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.2TY5@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.2TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY5." %&% mseOMrefY18.1.200.2TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.2TY5@StockSynthesisModels)){
  names(mseOMrefY18.1.200.2TY5@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY5." %&% names(mseOMrefY18.1.200.2TY5@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.2TY5,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY5.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.2TY5.RDA",sep=""))



# YFT Tuning objective TY10  (new tuning objective from TCMP-02)
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2029,2029)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.1TY10  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY10." %&% mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels)){
  names(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY10." %&% names(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.1TY10,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY10.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY10.RDA",sep=""))

# YFT Tuning objective TY11  (new tuning objective from TCMP-02)
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2034,2034)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.1TY11  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY11." %&% mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels)){
  names(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY11." %&% names(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.1TY11,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY11.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY11.RDA",sep=""))







################################################################################
#make a combined Kobe plot for all (most) MPs and all tuning levels
load(file="Objects/mseOMrefY18.1.200.TY1.RDA")
load(file="Objects/mseOMrefY18.1.200.TY2.RDA")
load(file="Objects/mseOMrefY18.1.200.TY3.RDA")
load(file="Objects/mseOMrefY18.1.200.TY4.RDA")
load(file="Objects/mseOMrefY18.1.200.0TY4.RDA")
load(file="Objects/mseOMrefY18.1.200.TY5.RDA")
#load(file="Objects/mseOMrefY18.1.200.Rcv0.TY5.RDA")
load(file="Objects/mseOMrefY18.1.200.2TY1.RDA")
load(file="Objects/mseOMrefY18.1.200.2TY2.RDA")
load(file="Objects/mseOMrefY18.1.200.2TY3.RDA")
load(file="Objects/mseOMrefY18.1.200.2TY4.RDA")
load(file="Objects/mseOMrefY18.1.200.2TY5.RDA")
load(file="Objects/mseOMrefY18.1.200.TY6.RDA")
load(file="Objects/mseOMrefY18.1.200.TY7.RDA")
load(file="Objects/mseOMrefY18.1.200.2.TY7.RDA")

load(file="Objects/mseOMrefY18.1.200.1TY10.RDA")
load(file="Objects/mseOMrefY18.1.200.1TY11.RDA")



YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.200.TY1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.2TY2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.2TY3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.0TY4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.2TY4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY5, YearsAveraged))


#only TY7 for illustration purposes
perfd <- msevizPerformanceData(mseOMrefY18.1.200.2.TY7, YearsAveraged)

kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=3)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs)



################################################################################
# make a series of tuning level-secific plots

#Tuning level 1
histd.TY1 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY1)
projd.TY1 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY1)

plotOMruns2(histd.TY1, projd.TY1, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY1, projd.TY1, "C", ylab= "Catch (1000t)")


#Tuning level TY2
histd.TY2 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY2)
histd.TY2 <- rbind(histd.TY2, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2TY2))
projd.TY2 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY2)
projd.TY2 <- rbind(projd.TY2, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2TY2))

plotOMruns2(histd.TY2, projd.TY2, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY2, projd.TY2, "C", ylab= "Catch (1000t)", Cref=87000)


#Tuning level TY3
histd.TY3 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY3)
histd.TY3 <- rbind(histd.TY3, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2TY3))
projd.TY3 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY3)
projd.TY3 <- rbind(projd.TY3, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2TY3))

plotOMruns2(histd.TY3, projd.TY3, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY3, projd.TY3, "C", ylab= "Catch (1000t)")


#Tuning level TY4
histd.TY4 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY4)
histd.TY4 <- rbind(histd.TY4, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.0TY4))
histd.TY4 <- rbind(histd.TY4, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2TY4))
projd.TY4 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY4)
projd.TY4 <- rbind(projd.TY4, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.0TY4))
projd.TY4 <- rbind(projd.TY4, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2TY4))

plotOMruns2(histd.TY4, projd.TY4, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY4, projd.TY4, "C", ylab= "Catch (1000t)")


#Tuning level TY5
histd.TY5 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY5)
histd.TY5 <- rbind(histd.TY5, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2TY5))
projd.TY5 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY5)
projd.TY5 <- rbind(projd.TY5, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2TY5))
plotKobeCols(om=histd.TY5, runs=projd.TY5)

#plotOMruns(histd.TY5, projd.TY5, "S3")
plotOMruns2(histd.TY5, projd.TY5, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY5, projd.TY5, "C", ylab= "Catch (1000t)")



histd.2.TY7 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2.TY7)
projd.2.TY7 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2.TY7)

plotOMruns2(histd.2.TY7, projd.2.TY7, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.2.TY7, projd.2.TY7, "C", ylab= "Catch (1000t)")






################################################################################
# select and rename MPs for final cross-MP plots - MP does not really matter, its the tuning that counts

projd.all  <- rbind(projd.TY1, projd.TY2, projd.TY3, projd.TY4, projd.TY5)
histd.all  <- rbind(histd.TY1, histd.TY2, histd.TY3, histd.TY4, histd.TY5)

finalMPList <- c('TY1.PT41.t15', 'TY1.IT5.t15',
                 'TY2.PT41.t15', 'TY2.IT5.t15',
                 'TY3.PT41.t15', 'TY3.IT5.t15',
                 'TY4.PT41.t25', 'TY4.IT5.t25',
                 'TY5.PT41.t15', 'TY5.IT5.t15')

projd.sub <- projd.all[mp %in% finalMPList]
projd.sub <- projd.sub[year <2039]


# compact names
projd.sub[projd.sub$mp == 'TY1.PT41.t15','mp'] <- "TY1.M"
projd.sub[projd.sub$mp == 'TY1.IT5.t15','mp']  <- "TY1.D"
projd.sub[projd.sub$mp == 'TY2.PT41.t15','mp'] <- "TY2.M"
projd.sub[projd.sub$mp == 'TY2.IT5.t15','mp']  <- "TY2.D"
projd.sub[projd.sub$mp == 'TY3.PT41.t15','mp'] <- "TY3.M"
projd.sub[projd.sub$mp == 'TY3.IT5.t15','mp']  <- "TY3.D"
projd.sub[projd.sub$mp == 'TY4.PT41.t25','mp'] <- "TY4.M"
projd.sub[projd.sub$mp == 'TY4.IT5.t25','mp']  <- "TY4.D"
projd.sub[projd.sub$mp == 'TY5.PT41.t15','mp'] <- "TY5.M"
projd.sub[projd.sub$mp == 'TY5.IT5.t15','mp']  <- "TY5.D"

# Time Series plots
plotKobeCols(om=histd.all, runs=projd.sub)
plotOMruns2(histd.all, projd.sub, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.all, projd.sub, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd.all, projd.sub, "C", ylab= "Catch (1000 t)", Cref=413000)


# temporally-averaged plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.200.TY1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY3, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.0TY4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY4, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.TY5, YearsAveraged))

perfd.sub <- perfd[mp %in% finalMPList]


# compact names
perfd.sub[perfd.sub$mp == 'TY1.PT41.t15','mp'] <- "TY1.M"
perfd.sub[perfd.sub$mp == 'TY1.IT5.t15','mp']  <- "TY1.D"
perfd.sub[perfd.sub$mp == 'TY2.PT41.t15','mp'] <- "TY2.M"
perfd.sub[perfd.sub$mp == 'TY2.IT5.t15','mp']  <- "TY2.D"
perfd.sub[perfd.sub$mp == 'TY3.PT41.t15','mp'] <- "TY3.M"
perfd.sub[perfd.sub$mp == 'TY3.IT5.t15','mp']  <- "TY3.D"
perfd.sub[perfd.sub$mp == 'TY4.PT41.t25','mp'] <- "TY4.M"
perfd.sub[perfd.sub$mp == 'TY4.IT5.t25','mp']  <- "TY4.D"
perfd.sub[perfd.sub$mp == 'TY5.PT41.t15','mp'] <- "TY5.M"
perfd.sub[perfd.sub$mp == 'TY5.IT5.t15','mp']  <- "TY5.D"

plotBPs2(perfd.sub, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd.sub, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd.sub, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)


# cannot make tuning objective 1 with 15% change cosntraints
#perfd.sub <- perfd[mp %in% c(    'TY2.PT41.t15', 'TY2.IT5.t15',
#                                 'TY3.PT41.t15', 'TY3.IT5.t15',
#                                 'TY4.PT41.t15', 'TY4.IT5.t15')]
#plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))


# Final plot
perfd.sub <- perfd[mp %in% c('TY1.1.PT41.t25', 'TY1.1.IT5.t25',
                           'TY6.1.PT41.t15', 'TY6.1.IT5.t15',
                           'TY7.1.PT41.t15', 'TY7.1.IT5.t15')]
plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))


# demo plot for single tuning  TY7
perfd.sub <- perfd[mp %in% c()]
plotBPs2(perfd.sub, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))





################################################################################
# Create Table 1

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg","GK","RK","PrSBgtSBMSY","PrSBgt0.2SB0","PrSBgtSBlim","Y","relCPUE","YoMSY","APCY","YcvPct","PrYlt0.1MSY", "AAVY")

YearsAveraged <- 20
stats1 <- performanceStatistics(mseOMrefY18.1.200.TY1,  AllPerformanceMeasures, YearsAveraged)
stats2 <- performanceStatistics(mseOMrefY18.1.200.TY2,   AllPerformanceMeasures, YearsAveraged)
stats3 <- performanceStatistics(mseOMrefY18.1.200.TY3,   AllPerformanceMeasures, YearsAveraged)
stats4 <- performanceStatistics(mseOMrefY18.1.200.0TY4,  AllPerformanceMeasures, YearsAveraged)
stats5 <- performanceStatistics(mseOMrefY18.1.200.TY5,   AllPerformanceMeasures, YearsAveraged)
stats <- rbind(stats1, stats2, stats3, stats4, stats5)
rownames(stats) <- c(rownames(stats1), rownames(stats2), rownames(stats3), rownames(stats4), rownames(stats5))

#stats <- stats[rownames(stats) %in% finalMPList,]

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



################################################################################
# Create Table 2

AllPerformanceMeasures <- c("SBoSB0","minSBoSB0","SBoSBMSY","FoFMSY","FoFtarg",
     "GK","RK", "PrSBgt0.2SB0","PrSBgtSBlim","Y",
     "YoMSY","relCPUE","AAVY",
     "YcvPct","PrYlt0.1MSY")

# mix of medians and means
TabAllPerfMeasures <- c("SBoSB00.5","minSBoSB00.5","SBoSBMSY0.5","FoFMSY0.5","FoFtarg0.5",
     "GKmean","RKmean", "PrSBgt0.2SB0mean","PrSBgtSBlimmean",
     "Y0.5", "YoMSY0.5","relCPUE0.5","AAVY0.5",
     "YcvPct0.5","PrYlt0.1MSY0.5")

tab2List <- as.list(NULL)
yList <- c(1,5,10,20)
for(iy in 1:length(yList)){
  yearsAveraged <- yList[iy]

  stats1 <- performanceStatistics(mseOMrefY18.1.200.TY1, AllPerformanceMeasures, YearsAveraged)
  stats2 <- performanceStatistics(mseOMrefY18.1.200.TY2,   AllPerformanceMeasures, YearsAveraged)
  stats3 <- performanceStatistics(mseOMrefY18.1.200.TY3,   AllPerformanceMeasures, YearsAveraged)
  stats4 <- performanceStatistics(mseOMrefY18.1.200.0TY4,   AllPerformanceMeasures, YearsAveraged)
  stats5 <- performanceStatistics(mseOMrefY18.1.200.TY5,   AllPerformanceMeasures, YearsAveraged)
  stats <- rbind(stats1, stats2, stats3, stats4, stats5)
rownames(stats) <- c(rownames(stats1), rownames(stats2), rownames(stats3), rownames(stats4), rownames(stats5))

  tab <- NULL
  for (mp in finalMPList){
   #can't seem to use TabAllPerfMeasures here
   tab <- rbind(tab, stats[rownames(stats)==mp, c("SBoSB00.5",
     "minSBoSB00.5","SBoSBMSY0.5","FoFMSY0.5","FoFtarg0.5",
     "GKmean","RKmean", "PrSBgt0.2SB0mean","PrSBgtSBlimmean",
     "Y0.5", "YoMSY0.5","relCPUE0.5","AAVY0.5",
     "YcvPct0.5","PrYlt0.1MSY0.5")])
  }
  tab2List[[iy]] <- tab
}


gc() #memory check and clean up


################################################################################
# new since TCMP-02 (2018)


# YFT Tuning objective TY10  (new tuning objective from TCMP-02)
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2029,2029)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.1TY10  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY10." %&% mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.1TY10@StockSynthesisModels)){
  names(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY10." %&% names(mseOMrefY18.1.200.1TY10@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.1TY10,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY10.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY10.RDA",sep=""))

# YFT Tuning objective TY11  (new tuning objective from TCMP-02)
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2034,2034)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.1TY11  <- runMse(OMrefY18.1.200, TuningPars=TuningPars, MPs=MPList1, interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels)){
  for(j in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY11." %&% mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
for(i in 1:length(mseOMrefY18.1.200.1TY11@StockSynthesisModels)){
  names(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars) <- "2TY11." %&% names(mseOMrefY18.1.200.1TY11@StockSynthesisModels[[i]]@ProjectedVars)
}
save(mseOMrefY18.1.200.1TY11,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY11.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.1TY11.RDA",sep=""))


#Tuning level TY5
histd.TY5 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY5)
histd.TY5 <- rbind(histd.TY5, msevizHistoricTimeSeriesData(mseOMrefY18.1.200.2TY5))
projd.TY5 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY5)
projd.TY5 <- rbind(projd.TY5, msevizProjectedTimeSeriesData(mseOMrefY18.1.200.2TY5))

plotOMruns2(histd.TY5, projd.TY5, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY5, projd.TY5, "C", ylab= "Catch (1000t)")


#Tuning level TY10
histd.TY10 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.1TY10)
projd.TY10 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.1TY10)

plotOMruns2(histd.TY10, projd.TY10, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY10, projd.TY10, "C", ylab= "Catch (1000t)")

#Tuning level TY11
histd.TY11 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.1TY11)
projd.TY11 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.1TY11)

plotOMruns2(histd.TY11, projd.TY11, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.TY11, projd.TY11, "C", ylab= "Catch (1000t)")


projd.all  <- rbind(projd.TY5, projd.TY10, projd.TY11)
histd.all  <- rbind(projd.TY5, projd.TY10, projd.TY11)

#finalMPList <- c('TY5.CCt',
#                 'TY10.CCt',
#                 'TY11.CCt')

finalMPList <- c('TY5.PT41.t15', 'TY5.IT5.t15',
                 'TY10.PT41.t15', 'TY10.IT5.t15',
                 'TY11.PT41.t15', 'TY11.IT5.t15')

projd.sub <- projd.all[mp %in% finalMPList]
projd.sub <- projd.sub[year <2039]

plotKobeCols(om=histd.all, runs=projd.sub)
plotOMruns2(histd.all, projd.sub, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.all, projd.sub, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd.all, projd.sub, "C", ylab= "Catch (1000 t)", Cref=413000)

# temporally-averaged plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefY18.1.200.TY5, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.1TY10, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefY18.1.200.1TY11, YearsAveraged))

perfd.sub <- perfd[mp %in% finalMPList]

plotBPs2(perfd.sub, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd.sub, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd.sub, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)



####################################################################################
#check the impact of rec variability on OM outcomes by setting recCV = 0.001

#reset the rec CV...actually need to reset the devs if the OM is already created ... seems to be even more complicated than that
OMrefY18.1.200.Rcv0 <- OMrefY18.1.200
for(iom in 1:length(OMrefY18.1.200.Rcv0@StockSynthesisModels)){
  OMrefY18.1.200.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT <-  OMrefY18.1.200.Rcv0@StockSynthesisModels[[iom]]@ModelData@ReccvT * 0. + 1.
  OMrefY18.1.200.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs <-  OMrefY18.1.200.Rcv0@StockSynthesisModels[[iom]]@ModelData@Recdevs * 0. + 1.
}

print(system.time(mseOMrefY18.1.200.Rcv0.CC          <- runMse(OMrefY18.1.200.Rcv0,MPs <- c("CC001","CC413"),interval=3, Report=F,UseCluster=1)))
histd.tmp <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.Rcv0.CC)
projd.tmp <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.Rcv0.CC)

plotOMruns2(histd.tmp, projd.tmp, "Recruitment", ylab= "Recruitment")

# YFT Tuning objective 5
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "SBoSBMSY"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2024,2024)
TuningPars@tuningTarget             <- 1.0
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain         = c(-2,2)
print(system.time(mseOMrefY18.1.200.Rcv0.TY5  <- runMse(OMrefY18.1.200.Rcv0, TuningPars=TuningPars, MPs=MPList1[1:2], interval=3, Report=F, UseCluster=1)))
for(i in 1:length(mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels)){
  names(mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars) <- "TY5." %&% names(mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars)
  for(j in 1:length(mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars)){
    mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TY5." %&% mseOMrefY18.1.200.Rcv0.TY5@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
  }
}
save(mseOMrefY18.1.200.Rcv0.TY5,file=paste(getwd(),"/Objects/mseOMrefY18.1.200.Rcv0.TY5.RDA",sep=""))
load(file=paste(getwd(),"/Objects/mseOMrefY18.1.200.Rcv0.TY5.RDA",sep=""))


#Compare Tuning level TY5 with Rec CV 0.6 and 0.0
histd.6 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.TY5)
projd.6 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.TY5)

histd.0 <- msevizHistoricTimeSeriesData(mseOMrefY18.1.200.Rcv0.TY5)
projd.0 <- msevizProjectedTimeSeriesData(mseOMrefY18.1.200.Rcv0.TY5)

plotKobeCols(om=histd.6, runs=projd.6)
plotKobeCols(om=histd.0, runs=projd.0)
plotOMruns2(histd.6, projd.6, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd.0, projd.0, "Recruitment", ylab= "Recruitment")
plotOMruns2(histd.6, projd.6, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.0, projd.0, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd.6, projd.6, "C", ylab= "Catch (1000t)")
plotOMruns2(histd.0, projd.0, "C", ylab= "Catch (1000t)")


YearsAveraged <- 20
perfd.6 <- msevizPerformanceData(mseOMrefY18.1.200.TY5, YearsAveraged)
plotBPs2(perfd.6, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd.6, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd.6, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)

YearsAveraged <- 20
perfd.0 <- msevizPerformanceData(mseOMrefY18.1.200.Rcv0.TY5, YearsAveraged)
plotBPs2(perfd.0, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)
plotTOs2(perfd.0, limit=YFTLims, target=YFTTargs, indicators = c("S9", "S3", "S6", "S14"), blackRef=Cref)
kobeMPs2(perfd.0, xlim=SBLim, ylim=FLim, xmax=2.5, ymax=2)


perfd.all <- rbind(perfd.6, perfd.0)
plotBPs2(perfd.all, limit=YFTLims, target=YFTTargs, indicators = c("S3", "S9", "S6", "S10", "S14"), blackRef=Cref)



