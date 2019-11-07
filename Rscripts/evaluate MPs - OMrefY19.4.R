# -----------------------------------------------------------------------------
# gridy19.3 tuned using ?,?,? model based tuning specifications
# -----------------------------------------------------------------------------

setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase2\\niMSE-IO-BET-YFT")


rm(list=ls(all=TRUE))

source("Source/MseMain.R")
# source("OMconditioning/RStuff/importGrid2.f.R")

load("objects/OMrefY19.4List.RDA")       #the weighted list of models

# grid <- importGrid.f(gridList=names(OMrefY19.3List), gridDir="Z:\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\", convergedNum=3)

# Create gridY19.4.500 model definition - the new ref case
# load("objects/OMrefY19.4List.RDA")       #the weighted list of models
# table(unlist(strsplit(names(OMrefY19.4List), split="_")))/length(OMrefY19.4List)
# source("Rscripts/Build OM Model-OMrefY19.4.500.R")
# print(system.time(OMrefY19.4.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4.500 <- initCPUE_SeriesFrom(OMrefY19.4.500, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4.500, file="Objects/OMrefY19.4.500.RDA")

# Create gridY19.4.420 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4.420.R")
# print(system.time(OMrefY19.4.420 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4.420 <- initCPUE_SeriesFrom(OMrefY19.4.420, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4.420, file="Objects/OMrefY19.4.420.RDA")

# Create gridY19.4BF.368 model definition - uniform sample of 1 per model with the worst fit of the repeated minimization results
# load("objects/OMrefY19.4BFList.RDA")       #the weighted list of models
# source("Rscripts/Build OM Model-OMrefY19.4BF.368.R")
# print(system.time(OMrefY19.4BF.368 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4BF.368 <- initCPUE_SeriesFrom(OMrefY19.4BF.368, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4BF.368, file="Objects/OMrefY19.4BF.368.RDA")


# Create OMrefY19.3b.500 model - fixes minor errors in original presentedt to TCMP2019 and adds one model with preferred CPUE series for MP
load("Objects/OMrefY19.3List.RDA")
newNameList <- c(names(OMrefY19.3List), "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
OMrefY19.3bList <- rep(1/length(newNameList), length(newNameList))
names(OMrefY19.3bList) <- newNameList
save(OMrefY19.3bList, file="Objects/OMrefY19.3bList.RDA")
source("Rscripts/Build OM Model-OMrefY19.3b.500.R")
print(system.time(OMrefY19.3b.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
OMrefY19.3b.500 <- initCPUE_SeriesFrom(OMrefY19.3b.500, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
save(OMrefY19.3b.500, file="Objects/OMrefY19.3b.500.RDA")

# the old and erroneous OMrefY19.3
# grid <- importGrid.f(gridList=names(OMrefY19.3List), gridDir="Z:\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\", convergedNum=3)
# note that due to confounded design and model failure perhaps, CPUE combination q0, iH, iR1 is not available for base observed CPUE  
#load("Objects/OMrefY19.3List.RDA")
#load("Objects/OMrefY19.3.500.error.RDA")
#OMrefY19.3.500 <- initCPUE_SeriesFrom(OMrefY19.3.500, "h70_M06_t10_q0_iH_i3_iR2_gr2_ess5_SD_x8")
#getParameters(OMrefY19.3.500)$IACin
#getParameters(OMrefY19.3.500)$Icv
#OMrefY19.3.500 <- setParameters(OMrefY19.3.500, parameters=list(IACin=0.5, Icv=c(0.20000,0.20001)))
#save(OMrefY19.3.500, file="Objects/OMrefY19.3.500.RDA")






load(file="Objects/OMrefY19.4.500.RDA")

# set up some plot related variables
FLim   <- OMrefY19.4.500@MseDef@Flim
SBLim  <- OMrefY19.4.500@MseDef@SBlim

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
#TCMP.Y18.1 <- new("TuningParameters")
#TCMP.Y18.1@performanceMeasure       <- "SBoSBMSY"
#TCMP.Y18.1@performanceMeasureClass  <- "0.5"
#TCMP.Y18.1@performanceMeasureYears  <- c(2024,2024)
#TCMP.Y18.1@tuningTarget             <- 1.0
#TCMP.Y18.1@tuningTolerance          <- 0.01
#TCMP.Y18.1@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT2 - TCMP.Y18.2 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT3 - TCMP.Y18.3 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)

# standard constant catch demo plots
MPListC <- c("CC001","CC413")
print(system.time(mseOMrefY19.4.500.CC  <- runMse(OMrefY19.4.500, MPs=MPListC, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.CC)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.CC)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)


# suite of models that tune well for 18.2 with default settings
#MPL18.2 <- list("PT41F.t50", "PT41F.t75", "IT5.t50", "IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20") 
#print(system.time(mseOMrefY19.4.500.TY18.2.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

#rerun with corrected OM these for presentations
#MPL18.2 <- list("IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20") 
#print(system.time(mseOMrefY19.4.500.TY18.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL18.2, CppMethod=1, interval=3, Report=F, UseCluster=1)))

TCMP.Y18.2@tuningLogDomain         = c(-4,4)
MPL <- list("IT5.t75")
print(system.time(mseOMrefY19.4.500.TY18.2.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.1, file="Objects/mseOMrefY19.4.500.TY18.2.1.RDA")


TCMP.Y18.2@tuningLogDomain         = c(0.,1.)
MPL <- list("PTproj.1.35bmsy.25")
print(system.time(mseOMrefY19.4.500.TY18.2.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.2, file="Objects/mseOMrefY19.4.500.TY18.2.2.RDA")


TCMP.Y18.2@tuningLogDomain         = c(0.,1.5)
MPL <- list("PT30F.t75.s20")
print(system.time(mseOMrefY19.4.500.TY18.2.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.3, file="Objects/mseOMrefY19.4.500.TY18.2.3.RDA")






# need more nuanced search for models to tune for 18.3 because multiple solutions may be possible, and/or tuning might fail, due to oscillation frequency

MPL <- list("IT5.t50")  #
#TCMP.Y18.3@tuningTolerance          <- 0.05
TCMP.Y18.3@tuningLogDomain         = c(0,3)
print(system.time(mseOMrefY19.4.500.TY18.3.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.1, file="Objects/mseOMrefY19.4.500.TY18.3.1.RDA")
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01


# PJ's projection MP okay
TCMP.Y18.3@tuningLogDomain         = c(0.,1)
MPL <- list("PTproj.1.35bmsy.25")
print(system.time(mseOMrefY19.4.500.TY18.3.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.2, file="Objects/mseOMrefY19.4.500.TY18.3.2.RDA")

getMPs(mseOMrefY19.4.500.TY18.3.2)

#MPL <- list("PT41F.t75")  #tunes on 18.3 but not stable
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT41F.t50")  
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT41F.t50.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.4  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01



#MPL <- list("PT80F.t75.s30")
#MPL <- list("PT30F.t75.s20")
#MPL <- list("PTproj.1.35bmsy.25") #tunes on 18.3 fine
#MPL <- list("PT30F.t75.s20")      #18.3 tuning problems - small oscillations can prevent successful tuning even though it should be possible
#MPL <- list("PT41F.t50.s30")      # should be possible

MPL <- list("PT80F.t50.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
print(system.time(mseOMrefY19.4.500.TY18.3.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.3, file="Objects/mseOMrefY19.4.500.TY18.3.3.RDA")
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT80F.t75.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.7  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01




# plots -  only need one histd
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.1)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.2), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.3), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.1), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.2), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.3), all=TRUE)

# select Y18.2 tunings to plot and rename
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.1)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.2), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.3), all=TRUE)
#projd <- projd[as.character(projd$mp) %in% c("IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20"),]  
projd$mp <- "Y2." %&%  as.character(projd$mp) 

# select Y18.3 tunings to plot and rename
projd.3 <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.1)
projd.3 <- merge(projd.3, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.2), all=TRUE)
projd.3 <- merge(projd.3, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.3), all=TRUE)
projd.3$mp <- "Y3." %&%  as.character(projd.3$mp) 

projd <- merge(projd, projd.3, all=TRUE)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)


YearsAveraged <- 20  #9, 14, 20
perfd <- msevizPerformanceData(mseOMrefY19.4.500.TY18.2.1, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(mseOMrefY19.4.500.TY18.2.2, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(mseOMrefY19.4.500.TY18.2.3, YearsAveraged), all=TRUE)
perfd$mp <- "Y2." %&% as.character(perfd$mp)  

perfd.3 <- msevizPerformanceData(mseOMrefY19.4.500.TY18.3.1, YearsAveraged)
perfd.3 <- merge(perfd.3, msevizPerformanceData(mseOMrefY19.4.500.TY18.3.2, YearsAveraged), all=TRUE)
perfd.3 <- merge(perfd.3, msevizPerformanceData(mseOMrefY19.4.500.TY18.3.3, YearsAveraged), all=TRUE)
perfd.3$mp <- "Y3." %&% as.character(perfd.3$mp)  

# not sure why this fails and rbind works in this case...?
#perfd <- merge(perfd, perfd.3, all=TRUE)
perfd <- rbind(perfd, perfd.3)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



resultsY2.1 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.1, list("IT5.t75" =           "D.Y2"))
resultsY2.2 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.2, list("PTproj.1.35bmsy.25"= "Mr.Y2"))
resultsY2.3 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.3, list("PT30F.t75.s20"=      "M.Y2"))
resultsY3.1 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.1, list("IT5.t50" =           "D.Y3"))
resultsY3.2 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.2, list("PTproj.1.35bmsy.25"= "Mr.Y3"))
resultsY3.3 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.3, list("PT80F.t50.s30" =     "M.Y3"))  

getMPs(mseOMrefY19.4.500.TY18.3.1)

results <- list(resultsY2.1, resultsY2.2, resultsY2.3, resultsY3.1, resultsY3.2, resultsY3.3)
MP_names <-    c("D.Y2", "Mr.Y2", "M.Y2", "D.Y3", "Mr.Y3", "M.Y3")
MP_newnames <- MP_names

createTable1(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.20")
createTable1(9, results, MPs=MP_names, MPs_short=MP_newnames,  prefix="OMrefY19.4.500.9")
createTable1(14, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.14")


# not updated
createTable2(1,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(5,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(10, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")



############################################################################################################################
# evaluate alternate OMs with same MPs - first set is alternate sampling of reference grid
# in terms of multinomial, uniform, best and worst convergence, main effeccts vs all 2 way interactions

# Create gridY19.4.420 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4.420.R")
# print(system.time(OMrefY19.4.420 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
load("objects/OMrefY19.4.420.RDA")       #the weighted list of models

D.Y2 <- getMPs(mseOMrefY19.4.500.TY18.2.1)
M.Y3  <- getMPs(mseOMrefY19.4.500.TY18.3.3)
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrefY19.4.420  <- runMse(OMrefY19.4.420, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.420)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.420)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)





# load("objects/OMrefY19.4BFList.RDA")       #the weighted list of models
# Create gridY19.4BF.368 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4BF.368.R")
# print(system.time(OMrefY19.4BF.368 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# save(OMrefY19.4BF.368, file="Objects/OMrefY19.4BF.368.RDA")

load("objects/OMrefY19.4BF.368.RDA")       #the weighted list of models
print(system.time(mseOMrefY19.4BF.368<- runMse(OMrefY19.4BF.368, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4BF.368)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4BF.368)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)




load("objects/OMrefY19.3b.500.RDA")       #the weighted list of models
print(system.time(mseOMrefY19.3b.500  <- runMse(OMrefY19.3b.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.3b.500)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.3b.500)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)




# rename the selected MPs and plot together in the standard plots 

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrefY19.4.500.TY18.3.3, YearsAveraged)
perfd$mp   <- "Y3.M.4.500" 
perfdtmp    <- msevizPerformanceData(mseOMrefY19.4.500.TY18.2.1, YearsAveraged)
perfdtmp$mp <- "Y2.D.4.500"   

perfd <- rbind(perfd, perfdtmp)

perfd4.420 <- msevizPerformanceData(mseOMrefY19.4.420, YearsAveraged)
perfd4.420$mp <- as.character(perfd4.420$mp)
perfd4.420$mp[perfd4.420$mp == "IT5.t75"] <- "Y2.D.4.420"   
perfd4.420$mp[perfd4.420$mp == "PT80F.t50.s30"] <- "Y3.M.4.420"   
plotBPs2(perfd4.420, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd4.420)

perfd4BF.368 <- msevizPerformanceData(mseOMrefY19.4BF.368, YearsAveraged)
perfd4BF.368$mp <- as.character(perfd4BF.368$mp)
perfd4BF.368$mp[perfd4BF.368$mp == "IT5.t75"] <- "Y2.D.4BF.368"   
perfd4BF.368$mp[perfd4BF.368$mp == "PT80F.t50.s30"] <- "Y3.M.4BF.368"   
plotBPs2(perfd4BF.368, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd4BF.368)

perfd3b.500 <- msevizPerformanceData(mseOMrefY19.3b.500, YearsAveraged)
perfd3b.500$mp <- as.character(perfd3b.500$mp)
perfd3b.500$mp[perfd3b.500$mp == "IT5.t75"] <- "Y2.D.3b.500"   
perfd3b.500$mp[perfd3b.500$mp == "PT80F.t50.s30"] <- "Y3.M.3b.500"   
plotBPs2(perfd3b.500, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd3b.500)



plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



################################################################################################################
# find minimum rebuilding times

#MseDef@proyears      <- as.integer(26)  

#OMrefY19.4.500.longTime <- setParameters(OMrefY19.4.500, parameters=list(proyears=46)) #not required
MPL <- c("CC001.t15", "CC001.t25", "CC001.t35", "CC001.t45",  "CC001.t65", "CC001")
print(system.time(mseOMrefY19.4.500.CCtest  <- runMse(OMrefY19.4.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))


# plots -  only need one histd
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.CCtest)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.CCtest)

#projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)


####################################################################################################################
# robustness tests
####################################################################################################################


# Case 1:   OMrobY19.4.ICV30 
#   CPUE CV = 0.3, auto-correlation = 0.5
# -----------------------------------------
load(file="Objects/OMrefY19.4.500.RDA")

getParameters(OMrefY19.4.500)$IACin
getParameters(OMrefY19.4.500)$Icv
OMrobY19.4.ICV30 <- setParameters(OMrefY19.4.500, parameters=list(IACin=0.5, Icv=c(0.30000,0.30001)))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.ICV30  <- runMse(OMrobY19.4.ICV30, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))


YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.ICV30, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.ICV30)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.ICV30)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 2:  OMrobY19.4.10overRep 
#   10% reported over catch
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears

# set 10% reported over catch implemented as 1.1 times TAC
OMrobY19.4.10overRep <- setParameters(OMrefY19.4.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears))))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.10overRep  <- runMse(OMrobY19.4.10overRep, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.10overRep, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(OMrobY19.4.10overRep)
projd <- msevizProjectedTimeSeriesData(OMrobY19.4.10overRep)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 3:  OMrobY19.4.10overIUU
#   10% unreported over catch
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears

# set 10% unreported under catch implemented as 1.1 times TAC and catch bias 1/1.1
OMrobY19.4.10overIUU <- setParameters(OMrefY19.4.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears)), Cbmean=1.0 / 1.1))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.10overIUU  <- runMse(OMrobY19.4.10overIUU, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.10overIUU, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.10overIUU)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.10overIUU)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 4: OMrobY19.4.qTrend2 
#   CPUE catchability trend of 2% per annum
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears
# set CPUE catchability trend of 3% per annum
OMrobY19.4.qTrend2 <- setParameters(OMrefY19.4.500, parameters=list(ITrendin=2.0))

print(system.time(mseOMrobY19.4.qTrend2 <- runMse(OMrobY19.4.qTrend2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.qTrend2, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.qTrend2)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.qTrend2)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)



# -----------------------------------------
# Case 5: OMrobY19.4.recShock 
#   recruitment failure of 50% for 8 quarters starting near beginning of projections
# -----------------------------------------
proyears <- OMrefY19.4.500@MseDef@proyears

OMrobY19.4.recShock <- setParameters(OMrefY19.4.500, parameters=list(RecScale = as.karray(c(rep(1,times=4),rep(0.55,times=2),rep(1,times=proyears-6)))))

MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.recShock  <- runMse(OMrobY19.4.recShock, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.recShock, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)


histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.recShock)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.recShock)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)



# -----------------------------------------
# Case 0: just rerun the refernce set to make comparable plots with the robustness sets 
# -----------------------------------------

MPL <- c(M.Y3,D.Y2) 
print(system.time(mseTmp  <- runMse(OMrefY19.4.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseTmp, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseTmp)
projd <- msevizProjectedTimeSeriesData(mseTmp)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)






gc()











zzz old below here



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







MPL <- list(MY2a ="PT41.t50",  MY2b = "PT41.x60t25", MY2c = "PT41.t90",  MY2d = "PT80.t50",
            DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )




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
load(file="Objects/OMrefY19.3.500.TY18.1.RDA")

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
#projd <- projd[projd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]
projd <- projd[projd$mp %in% c("DY2a","DY2b","DY2c","DY2d","DY2e","DY2f","DY2g" ),]


lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CoTAC", ylab= "C / TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.2xC, YearsAveraged)
#perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- perfd[perfd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)




##################################################################################################
# additional Contrasting MPs for tuning level 2 only

MPL2 <- list(MY2d = "PT80.t50", MY2e = "PT120.t50", MY2f = "PT80.t90", MY2g = "PT80.t35", MY2h = "PT80.t39")
MPL3 <- list(MY2i = "PT120.t50.25", MY2j = "PT120.t75.15", MY2k = "PT120.t50.15")

# DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )
# MPL4 <- list(DY2b = "IT5.t50g20", DY2c = "IT5.t50g60", DY2d = "IT5.t50g20L2", DY2e = "IT5.t50g60L2")
#MPL4 <- list(DY2g = "IT5.t50g20x.5") #probsably best IT series so far , DY2f = "IT5.t50g20x2")
#MPL4 <- list(IT5.t50g60x.1 = "IT5.t50g60x.1", IT5.t50g60x.5 = "IT5.t50g60x.5", IT5.t50g60x2 = "IT5.t50g60x2") first two fail to plot, third okay
MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2", IT5.t50g60x.1 = "IT5.t50g60x.1", IT5.t50g60x.5 = "IT5.t50g60x.5", IT5.t50g60x2 = "IT5.t50g60x2")
MPL4 <- list(IT5.t50g60x.1 = "IT5.t50g60x.1") #tune failure
MPL4 <- list(IT5.t50g60x2 = "IT5.t50g60x2") 
MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2", IT5.t50g20x.5 = "IT5.t50g20x.5", IT5.t50g60x2 = "IT5.t50g60x2", IT5.t50g20 = "IT5.t50g20") 
MPL4 <- list(IT5.t60.25g20 = "IT5.t60.25g20", IT5.t60.25g20x2 = "IT5.t60.25g20x2") #nothing special here
MPL4 <- list(IT5.t50.25g20 = "IT5.t50.25g20x.5", IT5.t60.25g20x2 = "IT5.t50.25g20x2") 


MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2")
MPL4 <- list(IT5.t50.CAT = "IT5.t50.CAT", IT5.t50 = "IT5.t50")
MPL4 <- list(IT5.t50.g51.CAT = "IT5.t50.g51.CAT",IT5.t50.g15.CAT = "IT5.t50.g15.CAT", IT5.t50.g1m5.CAT = "IT5.t50.g1m5.CAT",
             IT5.t50.g51 = "IT5.t50.g51",IT5.t50.g15 = "IT5.t50.g15", IT5.t50.g1m5 = "IT5.t50.g1m5")
MPL4 <- list(IT5.t50.g55 = "IT5.t50.g55", IT5.t50.g55L10 = "IT5.t50.g55L10",  IT5.t50.g5m5 = "IT5.t50.g5m5",   IT5.t50.g5m5L10 = "IT5.t50.g5m5L10")
MPL4 <- list(IT5.t50.g5m1 = "IT5.t50.g5m1",   IT5.t50.g5m1L10 = "IT5.t50.g5m1L10")
MPL4 <- list(IT5.t50.g5m2 = "IT5.t50.g5m2", IT5.t50.g5m1 = "IT5.t50.g5m1")
MPL4 <- list(IT5.t50.g52 = "IT5.t50.g55", IT5.t50.g51 = "IT5.t50.g51")
MPL4 <- list(IT5.t50b = "IT5.t50d", IT5.t50c = "IT5.t50e")


TCMP.Y18.2@tuningLogDomain         =  c(-4,4)      # c(.3,1.1) 
TCMP.Y18.2@tuningTolerance          <- 0.05 # 0.05

print(system.time(OMrefY19.3.500.TY18.2xD  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL4, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2xD.MPs <- getMPs(OMrefY19.3.500.TY18.2xD)
for(i in 1:length(MPL4)){
  print(c(i,MPL4[[i]],TY18.2xD.MPs[[1]]@tune))
  print(c(i,MPL4[[i]],TY18.2xD.MPs[[1]]@tuneError))
}
#save(OMrefY19.3.500.TY18.2xD,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2xD.RDA",sep=""))
#load(file="Objects/OMrefY19.3.500.TY18.2xD.RDA")

#Use tuned value
print(system.time(testMSE  <- runMse(OMrefY19.3.500, MPs=TY18.2xD.MPs, CppMethod=1, interval=3, Report=F, UseCluster=0)))







histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2xD)
#histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2xD)
#projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- projd[projd$year <= 2040,]

#6 MPs with highest contrast
#projd <- projd[projd$mp %in% c('IT5.t50g60x.1', 'IT5.t50g60x.5', 'IT5.t50g60x2')]
#projd <- projd[projd$mp %in% c('IT5.t50g60x.1', 'IT5.t50g60x.5')]
#projd <- projd[projd$mp %in% c('IT5.t50g60x2')]


lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch / TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
#plotOMruns2(histd, projd, "TAC", ylab= "TAC (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)







plotCbyTAC(projd, firstMPYr = 2021)

totalCAT   <- as.numeric(unlist(projd[projd$qname=="C","data"]))
TAC  <- as.numeric(unlist(projd[projd$qname=="TAC","data"]))
plot(x=TAC,y=totalCAT)
lines(TAC,TAC, col=2)

Ctmp     <- projd[projd$qname=="C",]
TACtmp   <- projd[projd$qname=="TAC",]
Ctmp2    <- tmp[tmp$data>600000,]
TACtmp2  <- tmp[tmp$data>600000,]
plot(as.numeric(TACtmp2$data),as.numeric(Ctmp2$data))

TACtmp <- projd[projd$model=="h90_M08_t0001_q1_iH_i1_iR2_gr2_CL75_SD_x4",]

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.2xD, YearsAveraged)
#perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- perfd[perfd$mp %in% c("MY2e"),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



