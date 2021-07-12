#=============================================================================================================================
#  R script for making and inspecting the conditioned BET Operating Model OMrefB20.1 and associated robustness scenarios
#  OMgridB20.1 uses a fractional factorial design which only calculates a grid for main effects estimation (no interactions)
#  OM fitting was repeated from jittered starting condiitons with a target of 3 independent convergences (or 10 failures)
#  Results are based on the lowest OBJfn value among the converged models
#
#  changes from 2019 include (among others):
#     reference case updated to the 2019 assessment configuration from Fu et al
#     new uncertainty dimensions - CPUE standardization method reduced to 1 dimension 
#       i.e. because recommended alternative from 2018 CPUE group was not reproduced in 2019 
#     new approach for specifying CPUE CV and auto-correlation
#     new projection dates
#
#=============================================================================================================================
#online
mainRootDir  <- "H:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path
OMRootDir   <- "E:\\KOL018\\MSE-IO-BET-YFT\\"
OMRootDir   <- mainRootDir

#offline
#rootDir <- "M:\\C-offline\\MSE-IO-BET-YFT\\"
#rootDir <- "C:\\MSE-IO-BET-YFT\\"

#library(TinnRcom)                             # if using TinnR IDE
library(data.table)
library (r4ss)                                # R package supporting Stock Synthesis - if something's broken it might be becuase this changed
library(planor)                               # fractional factorial design
library(PerformanceAnalytics)                 # for chart.Correlation
library(MASS)
library(ggplot2)

source(paste(mainRootDir,"OMconditioning\\RStuff\\phase2\\pasteOperator.R",sep=""))

#source(paste(rootDir,"Source\\pasteOperator.R",sep=""))
source(mainRootDir %&% "OMconditioning\\RStuff\\seasAsYrToDecYr.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\makeGridB20.1.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\makeGridB20.2.f.R")

source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\SS_output2.R") #temporarily bypasses file inconsistency error caused by tangled batch files
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\importGrid2.f.R") #temporarily bypasses file inconsistency error caused by tangled batch files

#source(mainRootDir %&% "OMconditioning\\RStuff\\plotIndices.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\plotIndices3.f.DK.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\plotIndices3.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\plotIndices.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\timeSeriesPlots.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\gridSamplerBivar.f.R")


#extract some reference case assessment values from 2019 assessment

#Fu 2019 "reference" case (part of an equally balanced grid, so has no special staus on its own)
SArefB2019 <- SS_output(dir = mainRootDir %&% "OMconditioning\\Bigeye\\ref2019\\cSci_sL_TagLambda1_h80", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(SArefB2019, uncertainty=T)
ref <- SArefB2019

#jitter, almost identical LLH - no Hess seems to render some values accessible that are otherwise reported as 0?!
SArefB2019b <- SS_output(dir = mainRootDir %&% "OMconditioning\\Bigeye\\ref2019\\cSci_sL_TagLambda1_h80\\jitter1", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(SArefB2019b, uncertainty=F)

#SAref with increased Fmax and number Fhybrid iterations
SArefB2019Fmax6 <- SS_output(dir = mainRootDir %&% "OMconditioning\\Bigeye\\ref2019\\cSci_sL_TagLambda1_h80_FN7Fmax6\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(SArefB2019Fmax6, uncertainty=T)

#SAref with increased Fmax and number Fhybrid iterations + estimated young M - initial M slope is opposite of expected
SArefB2019Fmax6Mest <- SS_output(dir = mainRootDir %&% "OMconditioning\\Bigeye\\ref2019\\cSci_sL_TagLambda1_h80_FN7Fmax6Mest\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(SArefB2019Fmax6Mest, uncertainty=F)

#SAref with increased Fmax and number Fhybrid iterations + estimated old M 
SArefB2019Fmax6Mest2 <- SS_output(dir = mainRootDir %&% "OMconditioning\\Bigeye\\ref2019\\cSci_sL_TagLambda1_h80_FN7Fmax6Mest2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(SArefB2019Fmax6Mest2, uncertainty=F)



#  (merged seasnal temperate CPUE series)temp CV = 0.2
OMrefB2019 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(OMrefB2019, uncertainty=F)

#  (downweighted merged seasonal temperate CPUE = 0.4),
OMrefB2019b <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(OMrefB2019b, uncertainty=F)

# OMref (downweighted merged seasonal temperate CPUE in seasons 1-3 (cv=0.4), full weight on season 4 (CV = 0.2), most similar to SA of those tested),
OMrefB2019c <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(OMrefB2019c, uncertainty=F)

# series of models to test implications of F-related specifications on SA dynamics and catch LLH - 
ctest1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.1q40.2Ccv0.01FN4\\converged3\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest1, uncertainty=F)

ctest2 <-  OMref <- OMrefB2019c

# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest3 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Ccv0.01FN9\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest3, uncertainty=F)

# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest4 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Ccv0.5FN9\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest4, uncertainty=F)

# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest5 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Fmax1.4\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest5, uncertainty=F)

# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest6 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Fmax6\\converged3\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest6, uncertainty=F)

# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest7 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Ccv0.01FN9Fmax6\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest7, uncertainty=F)


# series of models to test implications of F-related specifications on SA dynamics and catch LLH
ctest8 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.tempIcv0.4q40.2Ccv0.5FN9Fmax6\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ctest8, uncertainty=F)

#add spatial rec devs
OMrefB2019.spatialRec <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.spatialRec\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(OMrefB2019.spatialRec, uncertainty=F)

#use fake environmental link to migration
OMrefB2019.ENV1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB20.1Fmax2.9\\OMref-h80_M06_t10_q0_iR1_ess10_SL.env1\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(OMrefB2019.ENV1, uncertainty=F)



m <- SArefB2019Fmax6Mest2

#Catch LLH
m$likelihoods_used[rownames(m$likelihoods_used)=="Catch", "values"]

#SBcurrent  
m[['derived_quants']][m[['derived_quants']]$"LABEL"=="SPB_368","Value"] 

#B/BMSY    
m[['derived_quants']][m[['derived_quants']]$"LABEL"=="SPB_368","Value"] / m[['derived_quants']][m[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #

#MSY       
m$derived_quants[m$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000

# take a look at disaggregated Fs -these should correspond to effective effort by fleet
F <- m$timeseries[m$timeseries$Yr<=368,c(1,19+(0:14)*8)]

plot(1-exp(-F[F[,1]==4,13+1]), type='l', col=4, main="LL Harvest Rate  (highest selected age)", ylab="1-exp(-F)", ylim=c(0,1), yaxs='i') # NW-north LL (Area 4, F13)
lines(1-exp(-F[F[,1]==1,2+1]), type='l', col=1) # NW-south LL (Area 1, F2)
lines(1-exp(-F[F[,1]==2,3+1]), type='l', col=2) # NE LL       (Area 2, F3)
lines(1-exp(-F[F[,1]==3,4+1]), type='l', col=3) # southern LL (Area 3, F4)

plot(1-exp(-F[F[,1]==4,15+1]), type='l', col=4, main="PSLS Harvest Rate (highest selected age)", ylab="1-exp(-F)", ylim=c(0,1), yaxs='i') # NW-north PSLS (Area 4, F15)
lines(1-exp(-F[F[,1]==1,9+1]), type='l', col=1)                      # NW-south PSLS (Area 1, F9)
lines(1-exp(-F[F[,1]==2,10+1]), type='l', col=2)                     # NE PSLS       (Area 2, F10)




#SBcurrent  % dev
SArefB2019b[['derived_quants']][SArefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"] 
OMrefB2019[['derived_quants']][OMrefB2019[['derived_quants']]$"LABEL"=="SPB_368","Value"] 
OMrefB2019b[['derived_quants']][OMrefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"] 
OMrefB2019c[['derived_quants']][OMrefB2019c[['derived_quants']]$"LABEL"=="SPB_368","Value"] 
SArefB2019b[['derived_quants']][SArefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"]/OMrefB2019b[['derived_quants']][OMrefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"] 

#B/BMSY    % dev
SArefB2019b[['derived_quants']][SArefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"] / SArefB2019b[['derived_quants']][SArefB2019b[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
OMrefB2019[['derived_quants']][OMrefB2019[['derived_quants']]$"LABEL"=="SPB_368","Value"] / OMrefB2019[['derived_quants']][OMrefB2019[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
OMrefB2019b[['derived_quants']][OMrefB2019b[['derived_quants']]$"LABEL"=="SPB_368","Value"] / OMrefB2019b[['derived_quants']][OMrefB2019b[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
OMrefB2019c[['derived_quants']][OMrefB2019c[['derived_quants']]$"LABEL"=="SPB_368","Value"] / OMrefB2019c[['derived_quants']][OMrefB2019c[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #

#MSY       %  dev
SArefB2019b$derived_quants[SArefB2019b$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
OMrefB2019$derived_quants[OMrefB2019$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
OMrefB2019b$derived_quants[OMrefB2019b$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
OMrefB2019c$derived_quants[OMrefB2019b$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000

#Catch LLH
SArefB2019b$likelihoods_used[rownames(SArefB2019b$likelihoods_used)=="Catch", "values"]
OMrefB2019$likelihoods_used[rownames(OMrefB2019$likelihoods_used)=="Catch", "values"]
OMrefB2019b$likelihoods_used[rownames(OMrefB2019b$likelihoods_used)=="Catch", "values"]
OMrefB2019c$likelihoods_used[rownames(OMrefB2019b$likelihoods_used)=="Catch", "values"]




#bounds check
r<- SArefB2019b
r$parameters$Label[r$parameters$Status=="LO" ][!is.na(r$parameters$Label[r$parameters$Status=="LO"])]
r$parameters$Label[r$parameters$Status=="HI"][!is.na(r$parameters$Label[r$parameters$Status=="HI"])]

r<- OMrefB2019
r$parameters$Label[r$parameters$Status=="LO" ][!is.na(r$parameters$Label[r$parameters$Status=="LO"])]
r$parameters$Label[r$parameters$Status=="HI"][!is.na(r$parameters$Label[r$parameters$Status=="HI"])]


# these values are based on the aggregate of the assessment grid, however they were produced for the WPTT report
#refMSY   <- 104 # from WPTT (derived from Langley 2016 assessment)
#refMSYcv <- 0.127  #STD TotYield_MSY 3.67914e+03 (times 4 = 14716.56 )... CV = 3.5% ?!
#refB_B.MSY   <- 1.29 # from Langley report
#refB_B0 <- 0.38
# refBYoBMSYcv <- 0.136  # assuming 0 correlation yields cv = 0.075

#ref2017SSBCurrent    <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SPB_356","Value"]  #
#ref2017SSBCurSD      <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SPB_355","StdDev"]  #
#ref2017SSBMSY        <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
#ref2017SSBMSYSD      <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SSB_MSY","StdDev"]  #
# SPB_MSY is not included in covar
#ref2017SSBCurMSYcorr <- ref2017[["CoVar"]][ref2017[['CoVar']]$"label.i"=="SPB_276" & ref2017[['CoVar']]$"label.j"=="SPB_MSY","corr"]
# Use correlation from MSY filtered grid (SPB_276, SPB_MSY) = 0.59
# then using cavar calcns (at bottom) of BY/BMSY

#ref$derived_quants[ref$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#max(ref$equil_yield$Catch)



##########################################################################################################################################
# OMgridB20.1 = fractional factorial design grid with WPM/WPTT 2019 uncertainty requests 

##########################################################################################################################################
path=OMRootDir %&% 'OMconditioning\\Bigeye\\'


#OMgridB20.1 has the non-seasonal southern CPUE
#check fractional grid without creating batch hierarchy
OMgridB20.1.List <- makeGridB20.1.f(path=path, gridName= 'gridB20.1', makeGrid=F, makeBatFiles=F, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#create batch hierarchy
OMgridB20.1.List <- makeGridB20.1.f(path=path, gridName= 'gridB20.1', makeGrid=T, makeBatFiles=T, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")   

save(OMgridB20.1.List,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB20.1.List.RDA")
# Exit R and run the batch files on cluster

load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB20.1.List.RDA")


# Decided not to modify omGrid this iteration
OMrefB20.1.List <- OMgridB20.1.List   
save(OMrefB20.1.List,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB20.1.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB20.1.List.RDA")


#make the list into a uniformaly weighted list with OM specs as names
names(OMgridB20.1.List) <- OMgridB20.1.List
OMgridB20.1.List[] <- 1/length(OMgridB20.1.List)
print(OMgridB20.1.List)

gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB20.1\\'
#just check convergence, don't import
importGrid.f(gridList=names(OMgridB20.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
#import
out <- importGrid.f(gridList=names(OMgridB20.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

gridB20.1.convergedList <- out$loadList
names(gridB20.1.convergedList) <- gridB20.1.convergedList



#OMgridB20.2 has the seasonal southern CPUE
#check fractional grid without creating batch hierarchy
OMgridB20.2.List <- makeGridB20.2.f(path=path, gridName= 'gridB20.2', makeGrid=F, makeBatFiles=F, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#create batch hierarchy
OMgridB20.2.List <- makeGridB20.2.f(path=path, gridName= 'gridB20.2', makeGrid=T, makeBatFiles=T, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")   

save(OMgridB20.2.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB20.2.List.RDA")
# Exit R and run the batch files on cluster

load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB20.2.List.RDA")

#make the list into a uniformaly weighted list with OM specs as names
names(OMgridB20.2.List) <- OMgridB20.2.List
OMgridB20.2.List[] <- 1/length(OMgridB20.2.List)
print(OMgridB20.2.List)

gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB20.2\\'
#just check convergence, don't import
importGrid.f(gridList=names(OMgridB20.2.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
#import
out <- importGrid.f(gridList=names(OMgridB20.2.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

gridB20.2.convergedList <- out$loadList
names(gridB20.2.convergedList) <- gridB20.2.convergedList





################################################################################
#examine convergence characteristics
#conv <- as.data.frame(type.convert(outSL$conStats[,2:6]))
#conv$modID <- outSL$conStats[,'modID']

#import the MP cpue 
MPcpue <- read.table("H:/C-offline/MSE-IO-BET-YFT/OMconditioning/Bigeye/AssessmentFiles2019/CPUEfromIOTC/BETMPcpue2019.dat")
MPcpueSeas <- read.table("H:/C-offline/MSE-IO-BET-YFT/OMconditioning/Bigeye/AssessmentFiles2019/CPUEfromIOTC/BETMPcpue2019Seas.dat")




#plot some summary stuff
#old approach
#B20.1.sumStats <- #cbind(omList,
#   plotIndices3.f(doPlots=T, modList=names(gridB20.1.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_368",
#   inputRefLines=F, #override list below which are extracted from reports etc
#   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
#   refModel=ref, MPcpue=MPcpue)

#new approach original OM with FMax and ecDev Issues
#gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB20.1\\'
#chooseBestFitModels(modelList=names(OMgridB20.1.List), SSRootDir = gridDir)

#B20.1.sumStats <- #cbind(omList,
#  plotIndices.f(doPlots=T, modList=names(OMgridB20.1.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_368",
#                 inputRefLines=F, #override list below which are extracted from reports etc
#                 refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
#                 refModel=ref, SSRootDir = gridDir ) #, MPcpue=MPcpue)


#new approach
gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB20.1\\'

# only do this once - copies best fit of each jitter to root dir 
# chooseBestFitModels(modelList=names(OMgridB20.1.List), SSRootDir = gridDir) 

#test why MP CPUE RMSE seems to differ between plotIndices and PJ approach




B20.1.sumStats <- #cbind(omList,
  plotIndices.f(doPlots=T, modList=names(OMgridB20.1.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_368",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(503,370,748), refSSBYoSSBMSY=c(1.22,0.82,1.81), refSSBYoSSB0=c(0.31, 0.21, 0.34),refMSY=c(75, 87,104),
                refModel=ref, SSRootDir = gridDir,
                cpueMP_File="H:/C-offline/MSE-IO-BET-YFT/OMconditioning/Bigeye/AssessmentFiles2019/CPUEfromIOTC/BETMPcpue2019.dat",
                cpueNormYrs=  c(1980:2018), # c(2014:2018), # c(1972,2018),
                lastCalendarYr = 2018, 
                #firstCalendarYr = 1952, 
                firstYrQtr = 101) #, MPcpue=MPcpue)


#new approach
gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB20.2\\'

# only do this once - copies best fit of each jitter to root dir 
# chooseBestFitModels(modelList=names(OMgridB20.2.List), SSRootDir = gridDir) 

B20.2.sumStats <- #cbind(omList,
  plotIndices.f(doPlots=T, modList=names(OMgridB20.2.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_368",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(503,370,748), refSSBYoSSBMSY=c(1.22,0.82,1.81), refSSBYoSSB0=c(0.31, 0.21, 0.34),refMSY=c(75, 87,104),
                refModel=ref, SSRootDir = gridDir,
                cpueMP_File="H:/C-offline/MSE-IO-BET-YFT/OMconditioning/Bigeye/AssessmentFiles2019/CPUEfromIOTC/BETMPcpue2019Seas.dat",
                cpueNormYrs= c(1972:2018),   #c(2014:2018), # c(1972,2018),
                firstCalendarYr = 1952, 
                firstYrQtr = 101) #, MPcpue=MPcpue)





#B20.1converged.sumStats <- #cbind(omList,
#  plotIndices.f(SSRootDir=gridDir, doPlots=T, modList=names(OMgridB19.5SL.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
#                 inputRefLines=F, #override list below which are extracted from reports etc
#                 refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
#                 refModel="ref", repeatedMin=3)


# check bounds
print("loBounds warnings")
table(unlist(B20.1.sumStats[[2]]))
sum(table(unlist(B20.1.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(B20.1.sumStats[[3]]))
print(B20.1.sumStats[[4]]) #identify specific problem models
#print(B19.5.sumStats[[4]][c(85,104)]) # bait boat bound problems relaxed and rerun "AgeSel_11P_1_BB1"
# init_F hi bound tst - h90_M06_t0001_q1_iH_i1_iR2_gr2_CLRW
#  tmp <- SS_output(dir = "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.5\\h90_M06_t0001_q1_iH_i1_iR2_gr2_CLRW" , repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
#SS_plots(tmp, uncertainty=F)



#subset with a catch penalty cutoff of 0.00001
#gridB19.5CP1convergedList <- gridB19.5convergedList[as.numeric(B19.5.sumStats[[1]][,'catchLLH'])<0.1]
#gridB19.5SLCP5convergedList <- gridB19.5SLconvergedList[as.numeric(B19.5SL.sumStats[[1]][,'catchLLH'])<0.00001]
#B19.5CP5.sumStats <- #cbind(omList,
#   plotIndices.f(doPlots=T, modList=names(gridB19.5SLCP5convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
#   inputRefLines=F, #override list below which are extracted from reports etc
#   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
#   refModel="ref")



##########################################################################################################################################
# grid 19.5SD = fractional factorial design grid with WPM/WPTT 2018 uncertainty requests (fixes regional scaling error) and omits robustness test dimensions
# second half of BET request for TCMP - uses dome-shaped sel instead of logistic
# 19.5 and 19.5SD are 72 model grids, which may be too few and cause polymodal resutls, particulalry if additional filtering is used

path=OMRootDir %&% 'OMconditioning\\BET\\'
#OMgridB19.5SD.List <- makeGridB19.5SD.f(path=path, gridName= 'gridB19.5SD', makeGrid=F, makeBatFiles=F, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.5SD.List <- makeGridB19.5SD.f(path=path, gridName= 'gridB19.5SD', makeGrid=F, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.5SD.List <- makeGridB19.5SD.f(path=path, gridName= 'gridB19.5SD', makeGrid=T, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#for (i in 1:length(OMgridB19.5SD.List)){
#  OMgridB19.5SD.List[i] <- OMgridB19.5SD.List[i] %&% "_SD" 
#}

#make the list into a uniformaly weighted list with OM specs as names
#names(OMgridB19.5SD.List) <- OMgridB19.5SD.List
#OMgridB19.5SD.List[] <- 1/length(OMgridB19.5SD.List)
#print(OMgridB19.5SD.List)

#save(OMgridB19.5SD.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.5SD.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.5SD.List.RDA")


# import summary results & diagnostics from the gridB19.5SDList of models
gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB19.5SD\\'
#importGrid.f(gridList=names(OMgridB19.5SD.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
#outSD <- importGrid.f(gridList=names(OMgridB19.5SD.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)
#out <- importGrid.f(gridList=names(OMgridB19.5.List[names(OMgridB19.5.List) %in% c("h80_M10_t0001_q0_iH_i3_iR2_gr1_CLRW", "h80_M08_t0001_q0_iC_i1_iR2_gr1_ess10")]), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

#gridB19.5SDconvergedList <- outSD$loadList
#names(gridB19.5SDconvergedList) <- gridB19.5SDconvergedList

################################################################################
#examine convergence characteristics
#conv <- as.data.frame(type.convert(outSD$conStats[,2:6]))
#conv$modID <- outSD$conStats[,'modID']

#plot some summary stuff
B19.5SDconverged.sumStats <- #cbind(omList,
  plotIndices.f(SSRootDir=gridDir,doPlots=T, modList=names(OMgridB19.5SD.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
                refModel="ref", repeatedMin=3)



# check bounds
print("loBounds warnings")
table(unlist(B19.5SDconverged.sumStats[[2]]))
sum(table(unlist(B19.5SDconverged.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(B19.5SDconverged.sumStats[[3]]))
print(B19.5SDconverged.sumStats[[4]]) #identify specific problem models

#subset with a catch penalty cutoff of 0.00001
#gridB19.5SDCP5convergedList <- gridB19.5SDconvergedList[as.numeric(B19.5SD.sumStats[[1]][,'catchLLH'])<0.00001]


#Merge the 19.5SD and 19.5SL grids into OMrefB19.6
tmp1SL <- as.data.frame(B19.5SLconverged.sumStats[1])
tmp2SL <- tmp1SL[as.numeric(as.character(tmp1SL$"catchLLH")) < 1E-5,]

tmp1SD <- as.data.frame(B19.5SDconverged.sumStats[1])
tmp2SD <- tmp1SD[as.numeric(as.character(tmp1SD$"catchLLH")) < 1E-5,]

tmp <- c(as.character(tmp2SL$modList), as.character(tmp2SD$modList))
tmp <- tmp[!is.na(tmp)]
OMrefB19.6convergedList <- rep(1/length(tmp), length(tmp))
names(OMrefB19.6convergedList) <- tmp

#plot the merged list summary stuff...note that this OM is 2 pooled grids from different DIRs, so need to be pre-loaded
refB19.6.sumStats <- #cbind(omList,
  plotIndices.f(SSRootDir="should be loaded already", doPlots=T, modList=names(OMrefB19.6convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
                refModel="ref", repeatedMin=3)

OMrefB19.6.List <- OMrefB19.6convergedList

save(OMrefB19.6.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB19.6.List.RDA")
load(mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB19.6.List.RDA")



#inspect some of the extreme individual models in full detail
# greatest rec dev trend
tmp <- as.data.frame(refB19.6.sumStats[[1]], stringsAsFactors = F)
modID <- tmp[min(as.numeric(tmp$rec.Trend))==as.numeric(tmp$rec.Trend),"modList"]
minrecTrend <- SS_output(dir = gridDir %&% modID %&% "\\converged1", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(minrecTrend, uncertainty=F)



#inspect some of the extreme individual models in full detail
#highest MSY
tmp <- as.data.frame(refB19.6.sumStats[[1]], stringsAsFactors = F)
modID <- tmp[max(as.numeric(tmp$MSY))==as.numeric(tmp$MSY),"modList"]
SS_plots(get(modID), uncertainty=F)

#lowest MSY
tmp <- as.data.frame(refB19.6.sumStats[[1]], stringsAsFactors = F)
modID <- tmp[min(as.numeric(tmp$MSY))==as.numeric(tmp$MSY),"modList"]
SS_plots(get(modID), uncertainty=F)


#highest B/BMSY
tmp <- as.data.frame(refB19.6.sumStats[[1]], stringsAsFactors = F)
modID <- tmp[max(as.numeric(tmp$B_B.MSY))==as.numeric(tmp$B_B.MSY),"modList"]
SS_plots(get(modID), uncertainty=F)

#lowest B/BMSY
tmp <- as.data.frame(refB19.6.sumStats[[1]], stringsAsFactors = F)
modID <- tmp[min(as.numeric(tmp$B_B.MSY))==as.numeric(tmp$B_B.MSY),"modList"]
SS_plots(get(modID), uncertainty=F)

#look at some double-normal sel plots...are they domed or not?

# M06
tmp <- SS_output(dir = "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.6\\h70_M06_t0001_q0_iC_iR1_ess10_SD\\converged0", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(tmp, uncertainty=F)

tmp <- SS_output(dir = "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.6\\h70_M10_t0001_q0_iC_iR1_ess10_SD\\converged0", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(tmp, uncertainty=F)


