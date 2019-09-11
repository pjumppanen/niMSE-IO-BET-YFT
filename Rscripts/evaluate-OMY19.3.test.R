# -----------------------------------------------------------------------------
# gridy19.3 tuned using ?,?,? model based tuning specifications
# -----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

source("Source/MseMain.R")
# source("OMconditioning/RStuff/importGrid2.f.R")

load("objects/OMrefY19.3List.RDA")       #the weighted list of models

# grid <- importGrid.f(gridList=names(OMrefY19.3List), gridDir="Z:\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\", convergedNum=3)

# Create gridY19.3.500 model definition
# source("Rscripts/Build OM Model-OMrefY19.3.500.R")
# print(system.time(OMrefY19.3.500 <- createMseFramework(MseDef, UseCluster=1)))
# save(OMrefY19.3.500, file="Objects/OMrefY19.3.500.RDA")
load(file="Objects/OMrefY19.3.500.RDA")

# set up some plot related variables
FLim   <- OMrefY19.3.500@MseDef@Flim
SBLim  <- OMrefY19.3.500@MseDef@SBlim

FTarg  <- 1.
SBTarg <- 1.

Cref <- 409. # 409=2017 YFT catch
YFTTargs <- c(SBTarg, FTarg)
YFTLims  <- c(SBLim, FLim)

names(YFTTargs) <- c("S3", "S4")
names(YFTLims) <- c("S3", "S4")
names(Cref) <- "S10"

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

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.2 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.3 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)

# standard MP lists
MPListC <- c("CC001","CC413")
#MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t50","IT5.t50")
MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t90", "IT5.t90", "PT41.t50","IT5.t50")
MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")

# Testing PT41
TCMP.Y18 <- new("TuningParameters")
TCMP.Y18@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18@performanceMeasureClass  <- "0.5"
TCMP.Y18@performanceMeasureYears  <- c(2024,2040)
TCMP.Y18@tuningTarget             <- 1.0
TCMP.Y18@tuningTolerance          <- 0.01
TCMP.Y18@tuningLogDomain         = c(-4,4)

MPL <- list(D.Y18.1 = "PT41.t50")
OMrefY19.3.500.TY18.1.test2 <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1.test)
TY18.1.MPs[[1]]@tuneError

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.1.test2, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.1.test2)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.1.test2)
projd <- projd[projd$year <= 2040,]

lastHistYr <- 2018
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

###################################################################################
# tune 3 MPs X 3 tuning objectives

# fails with TAC limit 0f 25%, okay with 50%
MPL <- list(M.Y18.1 = MPList0[4], D.Y18.1 = MPList0[5], C.Y18.1 = MPList0[3])
#OMrefY19.3.500.TY18.1.test <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL[1], CppMethod=1, interval=3, Report=F, UseCluster=0)
print(system.time(OMrefY19.3.500.TY18.1  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.1,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.1.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.1.RDA")

# fails with TAC limit 0f 15%, ok with 25%
MPL <- list(M.Y18.2 = MPList0[6], D.Y18.2 = MPList0[7], C.Y18.2 = MPList0[3])
print(system.time(OMrefY19.3.500.TY18.2  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2.MPs <- getMPs(OMrefY19.3.500.TY18.2)
TY18.2.MPs[[1]]@tuneError
TY18.2.MPs[[2]]@tuneError
TY18.2.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.2,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.2.RDA")

# fails with TAC limit 0f 15%, ok with 25%
MPL <- list(M.Y18.3 = MPList0[1], D.Y18.3 = MPList0[2], C.Y18.3 = MPList0[3])
print(system.time(OMrefY19.3.500.TY18.3  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.3.MPs <- getMPs(OMrefY19.3.500.TY18.3)
TY18.3.MPs[[1]]@tuneError
TY18.3.MPs[[2]]@tuneError
TY18.3.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.3,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.3.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.3.RDA")

# This fails to converge
# # try 15% limits
# MPL <- list(M.Y18.3 = MPList1[1], D.Y18.3 = MPList1[2], C.Y18.3 = MPList1[3])
# print(system.time(OMrefY19.3.500.TY18.3t  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
# TY18.3.MPs <- getMPs(OMrefY19.3.500.TY18.3t)
# TY18.3.MPs[[1]]@tuneError
# TY18.3.MPs[[2]]@tuneError
# TY18.3.MPs[[3]]@tuneError
# save(OMrefY19.3.500.TY18.3t,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.3t.RDA",sep=""))
# load(file="Objects/OMrefY19.3.500.TY18.3t.RDA")


###################################################################################
# plot the 3 MPs X 3 tuning objectives aggregate plots

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.1, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.3, YearsAveraged), all=TRUE)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

require(devEMF)


#as emf files
emf(file="Report/OMrefY19.3.500.TY18.BPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.TOs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.KobeMPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()


#as png files
png(file="Report/OMrefY19.3.500.TY18.BPs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

png(file="Report/OMrefY19.3.500.TY18.TOs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

png(file="Report/OMrefY19.3.500.TY18.KobeMPs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.1)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.3), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.1)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.3), all=TRUE)

projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]
projd <- projd[projd$year <= 2040,]

lastHistYr <- 2018
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

# as metafiles
emf(file="Report/OMrefY19.3.500.TY18.KobeCols.emf", width=7, height=7)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.Recruitment.emf", width=7, height=7)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.SSB_SSBMSY.emf", width=7, height=7)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.F_FMSY.emf", width=7, height=7)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.C.emf", width=7, height=7)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

# as png files
png(file="Report/OMrefY19.3.500.TY18.KobeCols.png", width=588, height=588)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.Recruitment.png", width=588, height=588)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.SSB_SSBMSY.png", width=588, height=588)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.F_FMSY.png", width=588, height=588)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.C.png", width=588, height=588)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

MP_names <- c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3")

results <- list(OMrefY19.3.500.TY18.1, OMrefY19.3.500.TY18.2, OMrefY19.3.500.TY18.3)
createTable1(20, results, MP_names, MP_names, prefix="OMrefY19.3.500.TY18.")
createTable2(1, results, MP_names, MP_names, prefix="OMrefY19.3.500.TY18.")
createTable2(5, results, MP_names, MP_names, prefix="OMrefY19.3.500.TY18.")
createTable2(10, results, MP_names, MP_names, prefix="OMrefY19.3.500.TY18.")
createTable2(20, results, MP_names, MP_names, prefix="OMrefY19.3.500.TY18.")






MPL <- list(D.Y18.4 = "ISP_0.5_0.3_0.3_0.05")

print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=0)))
print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=0)))

print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs="IT.PI.tune.15", CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, MPs="IT.PI.5", CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.t.PI  <- runMse(OMrefY19.3.500, MPs="IT.PI.5", CppMethod=1, interval=3, Report=F, UseCluster=0)))

MPs <- getMPs(OMrefY19.3.500.t.PI)

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.t.PI, YearsAveraged)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.t.PI)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.t.PI)
projd <- projd[projd$year <= 2040,]

lastHistYr <- 2018
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "TAC", ylab= "TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE(aggregate)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)


# Create gridY19.3.500.test model definition
source("Rscripts/Build OM Model-OMrefY19.3.500.test.R")
print(system.time(OMrefY19.3.500.test <- createMseFramework(MseDef, UseCluster=0)))
save(OMrefY19.3.500.test, file="Objects/OMrefY19.3.500.test.RDA")
load(file="Objects/OMrefY19.3.500.test.RDA")
load(file="Objects/OMrefY19.3.500.RDA")


print(system.time(OMrefY19.3.500.2 <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.1 <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500 <- runMse(OMrefY19.3.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=0)))
perfd <- msevizPerformanceData(OMrefY19.3.500, YearsAveraged)
histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500)

###################################################################################
# set the tuning objectives
# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
TCMP.Y18.1b <- new("TuningParameters")
TCMP.Y18.1b@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.1b@performanceMeasureClass  <- "0.5"
TCMP.Y18.1b@performanceMeasureYears  <- c(2024,2040)
TCMP.Y18.1b@tuningTarget             <- 1.0
TCMP.Y18.1b@tuningTolerance          <- 0.01
TCMP.Y18.1b@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.2 (TY5 retained from TCMP01)
TCMP.Y18.2b <- new("TuningParameters")
TCMP.Y18.2b@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2b@performanceMeasureClass  <- "0.5"
TCMP.Y18.2b@performanceMeasureYears  <- c(2029,2040)
TCMP.Y18.2b@tuningTarget             <- 1.0
TCMP.Y18.2b@tuningTolerance          <- 0.01
TCMP.Y18.2b@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.3 (TY5 retained from TCMP01)
TCMP.Y18.3b <- new("TuningParameters")
TCMP.Y18.3b@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3b@performanceMeasureClass  <- "0.5"
TCMP.Y18.3b@performanceMeasureYears  <- c(2034,2040)
TCMP.Y18.3b@tuningTarget             <- 1.0
TCMP.Y18.3b@tuningTolerance          <- 0.01
TCMP.Y18.3b@tuningLogDomain         = c(-4,4)

MPL <- list("PT41.t90", "PT41A.t90", "PT41AL.t90")

print(system.time(OMrefY19.3.500.T18.1b <- runMse(OMrefY19.3.500, MPs=MPL, TuningPars=TCMP.Y18.1b, CppMethod=1, interval=3, Report=F, UseCluster=1)))

save(OMrefY19.3.500.T18.1b, file="Objects/OMrefY19.3.500.T18.1b.RDA")

MPL <- list("PT41.t50", "PT41A.t50", "PT41AL.t50")

print(system.time(OMrefY19.3.500.T18.2b <- runMse(OMrefY19.3.500, MPs=MPL, TuningPars=TCMP.Y18.2b, CppMethod=1, interval=3, Report=F, UseCluster=1)))

save(OMrefY19.3.500.T18.2b, file="Objects/OMrefY19.3.500.T18.2b.RDA")

MPL <- list("PT41.t25", "PT41A.t25", "PT41AL.t25")

print(system.time(OMrefY19.3.500.T18.3b <- runMse(OMrefY19.3.500, MPs=MPL, TuningPars=TCMP.Y18.3b, CppMethod=1, interval=3, Report=F, UseCluster=1)))

save(OMrefY19.3.500.T18.3b, file="Objects/OMrefY19.3.500.T18.3b.RDA")

MPL <- list(D.Y18.4 = "ISP_0.4_0.3_0.3_0.05")

print(system.time(OMrefY19.3.500.T18.1c <- runMse(OMrefY19.3.500, MPs=MPL, TuningPars=TCMP.Y18.1b, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.T18.1b, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.T18.1c, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.T18.2b, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.T18.3b, YearsAveraged), all=TRUE)

perfd <- perfd[perfd$mp %in% c("PT41.t90", "ISP_0.4_0.3_0.3_0.05"),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.T18.1b)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.T18.1c), all=TRUE)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.T18.2b), all=TRUE)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.T18.3b), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.T18.1b)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.T18.1c), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.T18.2b), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.T18.3b), all=TRUE)

projd <- projd[projd$mp %in% c("PT41.t90", "ISP_0.4_0.3_0.3_0.05"),]

MPL <- list(D.Y18.4 = "ISP_0.4_0.3_0.3_0.05")
MPL <- list(PT41.t25 = "PT41AL.t25")
MPL <- list(PT41.t50 = "PT41AL.t50")
MPL <- list(PT41.t50 = "PT41A.t50")
MPL <- list(PT41.t50 = "PT41.t50")
MPL <- list(PT41.t50 = new("MP_Spec", "PT41A.t50", tune=0.5))
MPL <- list(D.Y18.4 = new("MP_Spec", "ISP_0.4_0.3_0.3_0.05", tune=1.0))
MPL <- list(PTproj = new("MP_Spec", "PTproj.25", tune=0.7))
bMPL <- list(PTproj = new("MP_Spec", "PTproj.15", tune=0.7))
MPL <- list(PTproj = new("MP_Spec", "PTproj.15", tune=1.4))
MPL <- list(PTproj = new("MP_Spec", "PTproj.15", tune=0.83))
MPL <- list(PTproj = new("MP_Spec", "PTproj.25", tune=0.83))
MPL <- list(PTproj = new("MP_Spec", "PTproj.25", tune=1.0))

load(file="Objects/OMrefY19.3.500.test.RDA")
OMrefY19.3.500.test <- setProjectionYears(OMrefY19.3.500.test, 100)
print(system.time(OMrefY19.3.500.test <- runMse(OMrefY19.3.500.test, MPs=MPL, TuningPars=TCMP.Y18.3b, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.test <- runMse(OMrefY19.3.500.test, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=0)))


MPL <- list("PTproj.80.25")
MPL <- list("PTproj.1.35bmsy.25")
print(system.time(OMrefY19.3.500.TY18.2.PTproj <- runMse(OMrefY19.3.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(OMrefY19.3.500.TY18.2.PTproj <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefY19.3.500.TY18.2.PTproj, file="Objects/OMrefY19.3.500.TY18.2.PTproj.RDA")
TY18.2.MPs <- getMPs(OMrefY19.3.500.TY18.2.PTproj)
#load(file="Objects/OMrefY19.3.500.TY18.2.PTproj.RDA")

histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2.PTproj)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2.PTproj)
projd <- projd[projd$year <= 2040,]


MPL <- list("PTproj.25")
MPL <- list("PTproj.25", "PTproj.15")
print(system.time(OMrefY19.3.500.TY18.1.PTproj <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(OMrefY19.3.500.TY18.1.PTproj, file="Objects/OMrefY19.3.500.TY18.1.PTproj.RDA")
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1.PTproj)
TY18.1.MPs[[1]]@tuneError
load(file="Objects/OMrefY19.3.500.TY18.1.PTproj.RDA")

histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.1.PTproj)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.1.PTproj)
projd <- projd[projd$year <= 2040,]


OMrefY19.3.500.test@StockSynthesisModels[[1]]@RefVars

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.test, YearsAveraged)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)


histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.test)
projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.test)
projd <- projd[projd$year <= 2040,]

lastHistYr <- 2018
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "C/TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "TAC", ylab= "TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE(aggregate)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
