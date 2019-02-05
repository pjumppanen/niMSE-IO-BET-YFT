# R Script for conducting IO-BET MSE for March 2019 MSE task force meeting
#

#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master")  # Bowen cloud working directory
#setwd("C:\\Users\\kol018\\MSE-IO-BET-YFT\\gitMirror")  # Set the working directory
setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")  # Set the working directory
#setwd("M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")  # Set the working directory
#setwd("C:\\tmp")  # Set the working directory
#sourceDir <- "M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT-master"  # Set the working directory
rm(list=ls(all=TRUE))

#for Tinn-R users only: (may no longer be required)
.trPaths <- file.path(Sys.getenv("TEMP"), "Tinn-R", c("", "search.txt", "objects.txt", "file.r", "selection.r", "block.r", "lines.r", "reformat-input.r", "reformat-output.r"), fsep="\\")
source("Source\\MseMain.R")
#source("Source\\mseviz2.R")
#source("Source\\utilities.R")
library(ggstance)

MPListC <- c("CC001", "CC087")
MPList0 <- c("PT41.t25","IT5.t25", "CCt")
MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")


#########################################################################################################
# Create or load the candidate reference case OM OMrefB19.1.288 - for Ispra 2019 MSE task force

# Create and save
source('Rscripts\\Build OM Model-OMrefB19.1.288.R')
print(system.time(OMrefB19.1.288 <- createMseFramework(MseDef)))
#save(OMrefB19.1.288,file=paste(getwd(),"/Objects/OMrefB19.1.288.RDA",sep=""))
#Load pre-existing
load(file="Objects/OMrefB19.1.288.RDA")

error on:
h70_M06_t10_q1_iC_i1_iR2_gr1_CLRW


#########################################################################################################
# Create or load the candidate reference case OM OMrefB19.2.288 - for Ispra 2019 MSE task force
# Create and save
source('Rscripts\\Build OM Model-OMrefB19.2.288.R')
print(system.time(OMrefB19.2.288 <- createMseFramework(MseDef)))
save(OMrefB19.2.288,file=paste(getwd(),"/Objects/OMrefB19.2.288.RDA",sep=""))
#Load pre-existing
load(file="Objects/OMrefB19.2.288.RDA")


#########################################################################################################
# Create or load the candidate reference case OM OMrefB19.4.288 - for Ispra 2019 MSE task force
# Create and save
source('Rscripts\\Build OM Model-OMrefB19.4.288.R')
print(system.time(OMrefB19.4.288 <- createMseFramework(MseDef)))
save(OMrefB19.4.288,file=paste(getwd(),"/Objects/OMrefB19.4.288.RDA",sep=""))
#Load pre-existing
#load(file="Objects/OMrefB19.4.288.RDA")


#########################################################################################################
# Create or load the candidate reference case OM OMrefB19.3.288 - for Ispra 2019 MSE task force
# Create and save
source('Rscripts\\Build OM Model-OMrefB19.3.288.R')
print(system.time(OMrefB19.3.288 <- createMseFramework(MseDef)))
save(OMrefB19.3.288,file=paste(getwd(),"/Objects/OMrefB19.3.288.RDA",sep=""))
#Load pre-existing
#load(file="Objects/OMrefB19.3.288.RDA")








not updated below here



################################################################################
# extract the BET reference points for plotting
#load(file="Objects/mseOMrefB18.2.304.TB3.RDA")
#FLim  <- mseOMrefB18.2.304.TB3@MseDef@Flim
#SBLim  <- mseOMrefB18.2.304.TB3@MseDef@SBlim

FLim  <- OMrefB18.5.252@MseDef@Flim
SBLim  <- OMrefB18.5.252@MseDef@SBlim

FTarg <- 1.
SBTarg <- 1.

Cref <- 87. #should calculate from within object (originally not done because of BET off by 1 yr error)
BETTargs <- c(SBTarg, FTarg)
BETLims  <- c(SBLim, FLim)

names(BETTargs) <- c("S3", "S4")
names(BETLims) <- c("S3", "S4")
names(Cref) <- "S10"


# wrapper for some standard plots - intially hard-wired with BET reference points and summary specifications
# should set up so window history works properly with ggplots ... not sure this is possible
betPlots.f <- function(mseObj)
{
  histd <- msevizHistoricTimeSeriesData(mseObj)
  projd <- msevizProjectedTimeSeriesData(mseObj)

  #windows()
  plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")

  #windows()
  plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
  #windows()
  plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
  #windows()
  plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
  #windows()
  plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")
  windows()
  plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])


  YearsAveraged <- 20 # cannot use c(2019,2038)
  perfd <- msevizPerformanceData(mseObj, YearsAveraged)
  windows()
  plotBPs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14"))
  windows()
  plotTOs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14"))
  windows()
  kobeMPs2(perfd, xlim=SBLim, ylim=FLim, ymax=3)
}


#########################################################################################################
# Create or load OMrefB18.5.20 - 20 reps for quick testing graphics and stuff
# source('Rscripts\\Build OM Model-OMrefB18.5.20.R')
# print(system.time(OMrefB18.5.20 <- createMseFramework(MseDef)))
# save(OMrefB18.5.20,file=paste(getwd(),"/Objects/OMrefB18.5.20.RDA",sep=""))
# Load pre-existing
load(file="Objects/OMrefB18.5.20.RDA")

getParameters(OMrefB18.5.20)
# Robustness set projections representing simple 1 step modification from ref set
OMrefB18.5.20.impErr2 <- OMrefB18.5.20.recShock2  <- OMrefB18.5.20
OMrefB18.5.20.impErr2   <- setParameters(OMrefB18.5.20.impErr2, list(ImplErrBias = as.karray(c(rep(-1,times=10),rep(1,times=20)))))
OMrefB18.5.20.recShock2 <- setParameters(OMrefB18.5.20.recShock2, list(RecScale <-as.karray(c(rep(1,times=4),rep(0.55,times=2),rep(1,times=24)))))



#########################################################################################################
# This can now be achieved with getMethod() and setMethod() above
#
# Create or load OMrobB18.5.20.impErr - simulate effect of no incentive to raise catch for first 10 years of MP, then TAC active
# Create and save
# source('Rscripts\\Build OM Model-OMrefB18.5.20.impErr.R')
# print(system.time(OMrefB18.5.20.impErr <- createMseFramework(MseDef)))
# save(OMrefB18.5.20.impErr,file=paste(getwd(),"/Objects/OMrefB18.5.20.impErr.RDA",sep=""))
#load(file="Objects/OMrefB18.5.20.impErr.RDA")


#########################################################################################################
# This can now be achieved with getMethod() and setMethod() above

# Create or load OMrobB18.5.20.recShock - simulate effect of no incentive to raise catch for first 10 years of MP, then TAC active
# Create and save
# source('Rscripts\\Build OM Model-OMrefB18.5.20.recShock.R')
# print(system.time(OMrefB18.5.20.recShock <- createMseFramework(MseDef)))
# save(OMrefB18.5.20.recShock,file=paste(getwd(),"/Objects/OMrefB18.5.20.recShock.RDA",sep=""))
#load(file="Objects/OMrefB18.5.20.recShock.RDA")


# Run the MSE without any tuning (fishing moratorium and current catch)
# Some numerical options

print(system.time(mseOMrefB18.5.20.0    <- runMse(OMrefB18.5.20,MPs <- c("CC001","CC087"),interval=3, Report=F,UseCluster=0)))
print(system.time(mseOMrefB18.5.20.0    <- runMse(OMrefB18.5.20,MPs <- c("CC001","CC087"),interval=3, Report=F,UseCluster=1)))
print(system.time(mseOMrefB18.5.20.0    <- runMse(OMrefB18.5.20,MPs <- c("CC001","CC087"),interval=3, CppMethod=1, Report=F,UseCluster=0)))
print(system.time(mseOMrefB18.5.20.0    <- runMse(OMrefB18.5.20,MPs <- c("CC001","CC087"),interval=3, CppMethod=1, Report=F,UseCluster=1)))

#Time series plots
histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.20.0)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.20.0)

plotKobeCols(om=histd, runs=projd)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")


#Time-integrated plots
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.20.0, YearsAveraged)

plotTOs2(perfd)
plotBPs2(perfd)
kobeMPs2(perfd)


# 2018 BET Tuning objective test ...might not be an actual value
TuningPars <- new("TuningParameters")
TuningPars@performanceMeasure       <- "GK"
TuningPars@performanceMeasureClass  <- "0.5"
TuningPars@performanceMeasureYears  <- c(2030,2034)
TuningPars@tuningTarget             <- 0.66 # checking if tuning failure or a courseness issue #0.6
TuningPars@tuningTolerance          <- 0.01
TuningPars@tuningLogDomain          <- c(-2,2)
print(system.time(mseOMrefB18.5.20.TB3.1  <- runMse(OMrefB18.5.20, TuningPars=TuningPars, MPs=MPList1[1:3], CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.20.B18.1  <- runMse(OMrefB18.5.20, TuningPars=TCMP.B18.1, MPs=MPList1[1:2], CppMethod=1, interval=3, Report=F, UseCluster=1)))
#save(mseOMrefB18.2.20.TB3.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.20.TB3.1.RDA",sep=""))
#load(file="Objects/mseOMrefB18.2.20.TB3.1.RDA")

#confirm tuning achieved
TB3.1.MPs <- getMPs(mseOMrefB18.5.20.B18.1)
TB3.1.MPs[[1]]@tuneError
TB3.1.MPs[[2]]@tuneError
#TB3.1.MPs[[3]]@tuneError


# Run a new MSE using the tuned MP from an earlier tuning ...
print(system.time(mseOMrefB18.5.20.TB3.1.R  <- runMse(OMrefB18.5.20, MPs=TB3.1.MPs, CppMethod=0, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.20.TB3.1.C2  <- runMse(OMrefB18.5.20, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling = as.double(10.0))))
print(system.time(mseOMrefB18.5.20.TB3.1.C3  <- runMse(OMrefB18.5.20, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling = as.double(2.0))))
print(system.time(mseOMrefB18.5.20.TB3.1.C4  <- runMse(OMrefB18.5.20, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling = as.double(0.4))))
print(system.time(mseOMrefB18.5.20.TB3.1.impErr2 <- runMse(OMrefB18.5.20.impErr2, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.20.TB3.1.recShock2 <- runMse(OMrefB18.5.20.recShock2, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#print(system.time(mseOMrefB18.5.254.TB3.1  <- runMse(OMrefB18.5.254, MPs=TB3.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))


#make some plots
betPlots.f(mseOMrefB18.5.20.TB3.1)
betPlots.f(mseOMrefB18.5.20.TB3.1.R)
betPlots.f(mseOMrefB18.5.20.TB3.1.C2)
betPlots.f(mseOMrefB18.5.20.TB3.1.C3)
betPlots.f(mseOMrefB18.5.20.TB3.1.C4)
betPlots.f(mseOMrefB18.5.20.TB3.1.impErr2)
betPlots.f(mseOMrefB18.5.20.TB3.1.recShock2)
betPlots.f(mseOMrefB18.5.254.TB3.1)





###################################################################################################################
# OMrefB18.5.252.RDA - actual results

# Constant catch
print(system.time(mseOMrefB18.5.252.cc     <- runMse(OMrefB18.5.252,MPs <- c("CC001","CC087"), EffortCeiling=20, interval=3, CppMethod=1, Report=F,UseCluster=1)))
betPlots.f(mseOMrefB18.5.252.cc)

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


MPL <- list(M.B18.1 = MPList1[1], D.B18.1 = MPList1[2], C.B18.1 = MPList1[3])
print(system.time(mseOMrefB18.5.252.TB18.1  <- runMse(OMrefB18.5.252, TuningPars=TCMP.B18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.TB18.1,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.TB18.1.RDA",sep=""))
load(file="Objects/mseOMrefB18.5.252.TB18.1.RDA")
TB18.1.MPs <- getMPs(mseOMrefB18.5.252.TB18.1)
TB18.1.MPs[[1]]@tuneError
TB18.1.MPs[[2]]@tuneError
TB18.1.MPs[[3]]@tuneError

MPL <- list(M.B18.2 = MPList1[1], D.B18.2 = MPList1[2], C.B18.2 = MPList1[3])
print(system.time(mseOMrefB18.5.252.TB18.2  <- runMse(OMrefB18.5.252, TuningPars=TCMP.B18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.TB18.2,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.TB18.2.RDA",sep=""))
load(file="Objects/mseOMrefB18.5.252.TB18.2.RDA")
TB18.2.MPs <- getMPs(mseOMrefB18.5.252.TB18.2)
TB18.2.MPs[[1]]@tuneError
TB18.2.MPs[[2]]@tuneError
TB18.2.MPs[[3]]@tuneError

MPL <- list(M.B18.3 = MPList1[1], D.B18.3 = MPList1[2], C.B18.3 = MPList1[3])
print(system.time(mseOMrefB18.5.252.TB18.3  <- runMse(OMrefB18.5.252, TuningPars=TCMP.B18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.TB18.3,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.TB18.3.RDA",sep=""))
load(file="Objects/mseOMrefB18.5.252.TB18.3.RDA")
TB18.3.MPs <- getMPs(mseOMrefB18.5.252.TB18.3)
TB18.3.MPs[[1]]@tuneError
TB18.3.MPs[[2]]@tuneError
TB18.3.MPs[[3]]@tuneError

betPlots.f(mseOMrefB18.5.252.TB18.1)



################################################################################
# broader suite of MPs for tuning B18.2 only
MPLfull <- list(PT41.t15.B18.2   = "PT41.t15",
                 IT5.t15.B18.2   = "IT5.t15",
                 PT41.t10.B18.2  = "PT41.t10",
                 PT80.t15.B18.2  = "PT80.t15",
                 IT5.t10.B18.2   = "IT5.t10",
                 IT10.t15.B18.2     = "IT10.t15",
                 IT5.t15.l1.B18.2   = "IT5.t15.l1",
                 PT80.t10.B18.2     = "PT80.t10")

print(system.time(mseOMrefB18.5.252.TB18.2.all  <- runMse(OMrefB18.5.252, TuningPars=TCMP.B18.2, MPs=MPLfull, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TB18.2.all.MPs <- getMPs(mseOMrefB18.5.252.TB18.2.all)
TB18.2.all.MPs[[1]]@tuneError
TB18.2.all.MPs[[2]]@tuneError
TB18.2.all.MPs[[3]]@tuneError
#save(mseOMrefB18.5.252.TB18.2.all,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.TB18.2.all.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.TB18.2.all, YearsAveraged)
plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.2.all)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

#
################################################################################


################################################################################
# repeat the B18.1 results with the R code and cpp tuning (to test the high F implications)
print(system.time(mseOMrefB18.5.252.TB18.1.R  <- runMse(OMrefB18.5.252, MPs=getMPs(mseOMrefB18.5.252.TB18.1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
# repeat the B18.1 results with the cpp effort ceiling = 1.61 (80% removal of highest exploitation rate)
print(system.time(mseOMrefB18.5.252.TB18.1.C80  <- runMse(OMrefB18.5.252, MPs=getMPs(mseOMrefB18.5.252.TB18.1), EffortCeiling=1.61, CppMethod=1, interval=3, Report=F, UseCluster=1)))

# plots comparing the R and cpp code to show the high F implementation issue
YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.TB18.1, YearsAveraged)
perfd$mp <- as.character(perfd$mp) %&% ".cpp"
perfdR <- msevizPerformanceData(mseOMrefB18.5.252.TB18.1.R, YearsAveraged)
perfdR$mp <- as.character(perfdR$mp) %&% ".R"
perfdC80 <- msevizPerformanceData(mseOMrefB18.5.252.TB18.1.C80, YearsAveraged)
perfdC80$mp <- as.character(perfdC80$mp) %&% ".C80"
perfd <- rbind(perfd, perfdR, perfdC80)

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
#
################################################################################




#MPL <-list(MP1="PT41.100.2", MP2="PT41.100.9", MP3="PT41.tune.9")
MPL <- list(M.B18.1 = MPList1[1] , )

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.TB18.3, YearsAveraged))
print(plotTOs2(perfd, MPs= MPList1[1:2]))




################################################################################
#make a combined Kobe plot for all (most) MPs and all tuning levels
load(file="Objects/mseOMrefB18.5.252.TB18.1.RDA")
load(file="Objects/mseOMrefB18.5.252.TB18.2.RDA")
load(file="Objects/mseOMrefB18.5.252.TB18.3.RDA")

yearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")


################################################################################
#make a combined Kobe plot for several MPs for the middle tuning level
load(file="Objects/mseOMrefB18.5.252.TB18.2.all.RDA")

yearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.TB18.2.all, YearsAveraged)

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.TB18.1, mseOMrefB18.5.252.TB18.2, mseOMrefB18.5.252.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")








################################################################################
# Robustness set projections representing simple 1 step modification from ref set
OMrefB18.5.252.impErrUnder <- OMrefB18.5.252.impErrOver  <- OMrefB18.5.252.impErrCV10 <- OMrefB18.5.252.recShock <- OMrefB18.5.252.qTrend3 <- OMrefB18.5.252
getParameters(OMrefB18.5.252)
OMrefB18.5.252.impErrUnder   <- setParameters(OMrefB18.5.252.impErrUnder, list(ImplErrBias = as.karray(c(rep(-1,times=10),rep(1,times=20)))))
OMrefB18.5.252.impErrOver    <- setParameters(OMrefB18.5.252.impErrOver,  list(ImplErrBias = as.karray(c(rep(1.1,times=30)))))
OMrefB18.5.252.impErrCV10    <- setParameters(OMrefB18.5.252.impErrCV10,  list(TACEcv = as.karray(c(rep(0.4, 15))))) # mean of 15  samples with CV 0.4 restulst on total CV of 0.1
OMrefB18.5.252.recShock      <- setParameters(OMrefB18.5.252.recShock,    list(RecScale = as.karray(c(rep(1,times=4),rep(0.55,times=2),rep(1,times=24)))))
OMrefB18.5.252.qTrend3       <- setParameters(OMrefB18.5.252.qTrend3,     list(ITrendin = 3))





################################################################################
# Run the under-catch implementation error robustness OMs for the three tunings
print(system.time(mseOMrefB18.5.252.impErrUnder.TB18.1   <- runMse(OMrefB18.5.252.impErrUnder, MPs=TB18.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrUnder.TB18.2   <- runMse(OMrefB18.5.252.impErrUnder, MPs=TB18.2.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrUnder.TB18.3   <- runMse(OMrefB18.5.252.impErrUnder, MPs=TB18.3.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.impErrUnder.TB18.2,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.impErrUnder.TB18.2.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.impErrUnder.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrUnder.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrUnder.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#Table all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.impErrUnder.TB18.1, mseOMrefB18.5.252.impErrUnder.TB18.2, mseOMrefB18.5.252.impErrUnder.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrUnder.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")







################################################################################
# Run the over-catch implementation error robustness OMs for the three tunings
print(system.time(mseOMrefB18.5.252.impErrOver.TB18.1   <- runMse(OMrefB18.5.252.impErrOver, MPs=TB18.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrOver.TB18.2   <- runMse(OMrefB18.5.252.impErrOver, MPs=TB18.2.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrOver.TB18.3   <- runMse(OMrefB18.5.252.impErrOver, MPs=TB18.3.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.impErrOver.TB18.2,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.impErrOver.TB18.2.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.impErrOver.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrOver.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrOver.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#Table all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.impErrOver.TB18.1, mseOMrefB18.5.252.impErrOver.TB18.2, mseOMrefB18.5.252.impErrOver.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrOver.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")







################################################################################
# Run the 10%CV implementation error robustness OMs for the three tunings
print(system.time(mseOMrefB18.5.252.impErrCV10.TB18.1   <- runMse(OMrefB18.5.252.impErrCV10, MPs=TB18.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrCV10.TB18.2   <- runMse(OMrefB18.5.252.impErrCV10, MPs=TB18.2.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.impErrCV10.TB18.3   <- runMse(OMrefB18.5.252.impErrCV10, MPs=TB18.3.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.impErrCV10.TB18.2,file=paste(getwd(),"/Objects/mseOMrefB18.5.252.impErrCV10.TB18.2.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.impErrCV10.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrCV10.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.impErrCV10.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#Table all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.impErrCV10.TB18.1, mseOMrefB18.5.252.impErrCV10.TB18.2, mseOMrefB18.5.252.impErrCV10.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.impErrCV10.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")





##################################################################################
# Run the recruitment shock robustness OMs

#illustrate the recruitment shock
print(system.time(mseOMrefB18.5.252.recShock.cc     <- runMse(OMrefB18.5.252.recShock,MPs <- c("CC001","CC087"), EffortCeiling=20, interval=3, CppMethod=1, Report=F,UseCluster=1)))
histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.recShock.cc)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.recShock.cc)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")


print(system.time(mseOMrefB18.5.252.recShock.TB18.1 <- runMse(OMrefB18.5.252.recShock, MPs=TB18.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.recShock.TB18.2 <- runMse(OMrefB18.5.252.recShock, MPs=TB18.2.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.recShock.TB18.3 <- runMse(OMrefB18.5.252.recShock, MPs=TB18.3.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefB18.5.252.recShock.TB18.2,file=paste(getwd(),"/mseOMrefB18.5.252.recShock.TB18.2.RDA",sep=""))

yearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.recShock.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.recShock.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.recShock.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.recShock.TB18.1, mseOMrefB18.5.252.recShock.TB18.2, mseOMrefB18.5.252.recShock.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.3))

projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.recShock.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")





##################################################################################
# Run the qTrend 3% catchability trend robustness OM

print(system.time(mseOMrefB18.5.252.qTrend3.TB18.1 <- runMse(OMrefB18.5.252.qTrend3, MPs=TB18.1.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.qTrend3.TB18.2 <- runMse(OMrefB18.5.252.qTrend3, MPs=TB18.2.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
print(system.time(mseOMrefB18.5.252.qTrend3.TB18.3 <- runMse(OMrefB18.5.252.qTrend3, MPs=TB18.3.MPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#save(mseOMrefB18.5.252.qTrend3.TB18.2,file=paste(getwd(),"/mseOMrefB18.5.252.qTrend3.TB18.2.RDA",sep=""))

YearsAveraged <- 20
perfd <- msevizPerformanceData(mseOMrefB18.5.252.qTrend3.TB18.1, YearsAveraged)
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.qTrend3.TB18.2, YearsAveraged))
perfd <- rbind(perfd,msevizPerformanceData(mseOMrefB18.5.252.qTrend3.TB18.3, YearsAveraged))

plotBPs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
plotTOs2(perfd, limit=BETLims, target=BETTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#TAble all MPs with original names
MPL <- c('M.B18.1', 'D.B18.1', 'C.B18.1',     'M.B18.2', 'D.B18.2', 'C.B18.2',   'M.B18.3', 'D.B18.3', 'C.B18.3')
createTable1(20, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(1, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(5, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(10, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)
createTable2(20, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPL)

#Table subset of MPs with abbreviated names
#MPL <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
#MPLShort <- 'MP' %&% 1:6
#createTable1(20, list(mseOMrefB18.5.252.qTrend3.TB18.1, mseOMrefB18.5.252.qTrend3.TB18.2, mseOMrefB18.5.252.qTrend3.TB18.3), prefix="BET", MPs = MPL, MPs_short = MPLShort)

histd <-              msevizHistoricTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.1)
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.2))
histd <- rbind(histd, msevizHistoricTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.3))

projd <-              msevizProjectedTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.1)
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.2))
projd <- rbind(projd, msevizProjectedTimeSeriesData(mseOMrefB18.5.252.qTrend3.TB18.3))

plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])

#remove the constant catch MPs for clarity
MPL6 <- c('M.B18.1', 'D.B18.1',    'M.B18.2', 'D.B18.2',   'M.B18.3', 'D.B18.3' )
projd <- projd[projd$mp %in% MPL6]
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
plotOMruns2(histd, projd, "C", Cref = 87, ylab= "Catch (1000t)")
plotOMruns2(histd, projd, "CPUE(aggregate)", Cref = 87, ylab= "CPUE(aggregate)")

plotOMruns2(histd, tmp, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")














... xxx zzz
old stuff below here not functional

# trade-off plot comparing results from 3 different OMs (i.e. R-based vs Cpp projections)
mseOMbet1r108.MPLb2@Label <- "R.50"
mseOMbet1r108.c.MPLb2@Label <- "C++"
mseOMbet1r108.r01.MPLb2@Label <- "R.01"


mseOMrefB18.5.252.TB18.1@Label <- "TB18.1"
mseOMrefB18.5.252.TB18.2@Label <- "TB18.2"
mseOMrefB18.5.252.TB18.3@Label <- "TB18.3"





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










#print(system.time(mseOMrefB18.5.254TuneTest <- runMse(OMrefB18.5.254, MPs=NewMPs, CppMethod=1, interval=3, Report=F, UseCluster=1)))





# 2018 BET Tuning objective test
#TuningPars <- new("TuningParameters")
#TuningPars@performanceMeasure       <- "GK"
#TuningPars@performanceMeasureClass  <- "0.5"
#TuningPars@performanceMeasureYears  <- c(2030,2034)
#TuningPars@tuningTarget             <- 0.6
#TuningPars@tuningTolerance          <- 0.01
#TuningPars@tuningLogDomain          <- c(-2,2)
#print(system.time(mseOMrefB18.2.20.TB3.1  <- runMse(OMrefB18.2.20, TuningPars=TuningPars, MPs=MPList1[1:3], CppMethod=1, interval=3, Report=F, UseCluster=1)))
#for(i in 1:length(mseOMrefB18.2.20.TB3.1@StockSynthesisModels)){
#  names(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars) <- "TB3.1." %&% names(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars)
#  for(j in 1:length(mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars)){
#    mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP  <- "TB3.1." %&% mseOMrefB18.2.20.TB3.1@StockSynthesisModels[[i]]@ProjectedVars[[j]]@MP
#  }
#}
#save(mseOMrefB18.2.20.TB3.1,file=paste(getwd(),"/Objects/mseOMrefB18.2.20.TB3.1.RDA",sep=""))
#load(file="Objects/mseOMrefB18.2.20.TB3.1.RDA")

#one year off fudge for plots - need to fix in BET OM specification
#mseOMrefB18.2.20.TB3.1@MseDef@firstCalendarYr <- as.integer(1952)

histd <- msevizHistoricTimeSeriesData(mseOMrefB18.5.20.TB3.1)
projd <- msevizProjectedTimeSeriesData(mseOMrefB18.5.20.TB3.1)

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




