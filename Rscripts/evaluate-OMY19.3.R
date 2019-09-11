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
# print(system.time(OMrefY19.3.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason
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

###################################################################################
# tune 3 MPs X 3 tuning objectives

# fails with TAC limit 0f 50%, okay with 90%
MPL <- list(M.Y18.1 = MPList0[4], D.Y18.1 = MPList0[5], C.Y18.1 = MPList0[3])
#OMrefY19.3.500.TY18.1.test <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL[1], CppMethod=1, interval=3, Report=F, UseCluster=0)
print(system.time(OMrefY19.3.500.TY18.1  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.1,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.1.RDA",sep=""))
save(OMrefY19.3.500.TY18.1,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.1n.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.1.RDA")

# --- delete when finished ---

# Temporary testing of alternate MP implementations
MPL <- list(M.Y18.1b = "PT41AL.t25", M.Y18.1c = "PT41AL.t50", M.Y18.1d = "PT41A.t90")
#OMrefY19.3.500.TY18.1.test <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL[1], CppMethod=1, interval=3, Report=F, UseCluster=0)
print(system.time(OMrefY19.3.500.TY18.1b  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1b)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.1b,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.1b.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.1b.RDA")

perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.1b, YearsAveraged), all=TRUE)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.1b), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.1b), all=TRUE)

# --- delete when finished end ---


# fails with TAC limit 0f 25%, ok with 50%
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

#select and rename perfd MPs
perfd <- perfd[perfd$mp %in% c("M.Y18.1", "D.Y18.1", "M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

rename <- list(M.Y18.1 ="MY1", D.Y18.1 ="DY1",
               M.Y18.2 ="MY2", D.Y18.2 ="DY2",
               M.Y18.3 ="MY3", D.Y18.3 ="DY3")

if (!is.na(rename))
{
  substitute <- function(names)
  {
    return (sapply(as.vector(names), FUN=function(name){rename[[name]]}))
  }

  perfd[,"mp"] <- factor(perfd[,substitute(mp)], levels=rename)
}
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

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

projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

rename <- list(M.Y18.1 ="MY1", D.Y18.1 ="DY1",
               M.Y18.2 ="MY2", D.Y18.2 ="DY2",
               M.Y18.3 ="MY3", D.Y18.3 ="DY3")

if (!is.na(rename))
{
  substitute <- function(names)
  {
    return (sapply(as.vector(names), FUN=function(name){rename[[name]]}))
  }

  projd[,"mp"] <- factor(projd[,substitute(mp)], levels=rename)
}


lastHistYr <- 2017
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
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", Cref=Cref*1000, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
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

MP_names    <- c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3")
MP_newnames <- c("MY1",    "DY1",    "MY2",    "DY2"    ,"MY3",   "DY3")


results <- list(OMrefY19.3.500.TY18.1, OMrefY19.3.500.TY18.2, OMrefY19.3.500.TY18.3)
createTable1(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(1,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(5,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(10, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")



##################################################################################################
# Contrasting MPs for tuning level 2 only

#MPListC <- c("CC001","CC413")
#MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t50","IT5.t50")
#MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t90", "IT5.t90", "PT41.t50","IT5.t50")
#MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
#MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")


MPL <- list(MY2a ="PT41.t50",  MY2b = "PT41.x60t25", MY2c = "PT41.t90",  MY2d = "PT80.t50",
            DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )

#MPL <- list(MY2b = MPList0[6])

print(system.time(OMrefY19.3.500.TY18.2xC  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2xC.MPs <- getMPs(OMrefY19.3.500.TY18.2xC)
for(i in 1:length(MPL)){
  print(c(i,MPL[[i]],TY18.2xC.MPs[[1]]@tuneError))
}
save(OMrefY19.3.500.TY18.2xC,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2xC.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.2xC.RDA")


histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2xC)
#histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2xC)
#projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- projd[projd$year <= 2040,]

#6 MPs with highest contrast
projd <- projd[projd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]


lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.2xC, YearsAveraged)
#perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- perfd[perfd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)




