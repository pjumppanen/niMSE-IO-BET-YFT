#=============================================================================================================================
# Basic R script for making and inspecting the conditioned YFT Operating Model OMrefY20.1 and associated robustness scenarios
# Mar 2020
# Some dimensions changed from omgridY19.x - 
# env link not intended as uncertainty dimension - its a test to be retained if it matters and MSE code can be updated in time
#
#=============================================================================================================================
#online
mainRootDir <- "H:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path
gitMirrorDir <- "H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase3\\niMSE-IO-BET-YFT\\" 
OMRootDir   <- "E:\\KOL018\\MSE-IO-BET-YFT\\"  #modify for local path
OMRootDir <- mainRootDir
#OMRootDir <- "H:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path


#offline
#rootDir <- "C:\\MSE-IO-BET-YFT\\"  #modify for local path

library(data.table)
library(ggplot2)

library(mvtnorm)
library(MASS)
#library(TinnRcom)                            #if using TinnR IDE
library (r4ss)                                #R package supporting Stock Synthesis - if something's broken it might be becuase this changed
library(PerformanceAnalytics)                 # for chart.Correlation
library(planor)                               # fractional factorial design
source(paste(mainRootDir,"OMconditioning\\RStuff\\phase2\\pasteOperator.R",sep=""))


#source(paste(rootDir,"Source\\pasteOperator.R",sep=""))
source(mainRootDir %&% "OMconditioning\\RStuff\\seasAsYrToDecYr.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\makeGridY20.1.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.3fullCross.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\gridSamplerBivar.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\SS_output2.R") #temporarily bypasses file inconsistency error caused by tangled batch files
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\importGrid2.f.R") #bypasses file a inconsistency identifier caused by tangled batch files and partial reruns
#source(mainRootDir %&% "OMconditioning\\RStuff\\importGrid.f.R")
source(gitMirrorDir %&% "OMconditioning\\RStuff\\plotIndices.f.R")
#source(gitMirrorDir %&% "OMconditioning\\RStuff\\plotIndices.f.DK.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\plotIndices.DK.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\timeSeriesPlots.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\timeSeriesPlots2.f.R")


#extract some reference case assessment values
#For comparison: Langley 2016 YFT assessment ref case (provided by Dan Fu with nod from Adam)
#

ref2018 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
#SS_plots(ref2018, uncertainty=F)
ref2018NoEnv <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\gridY19.1\\refNoEnv", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
#SS_plots(ref2018NoEnv, uncertainty=F)

#ref MSY = 351...seems low? convergence sensitivity?
ref2018$derived_quants[ref2018$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#ref MSY = 306...repeating with jitter.
ref2018NoEnv$derived_quants[ref2018NoEnv$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000

#max(ref2018$equil_yield$Catch)
#max(ref2018NoEnv$equil_yield$Catch)



# xxx update from 2018 assessment...
#ref2017 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2017\\refYFT2016", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
ref2018 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
ref <- ref2018

#values from 2018 WPTT report (which envelop a small grid of MPDs)
refMSY   <- 403 #(339-436)
refMSYcv <- 0.035  #backcalculated from 2018 report assuming plausibility interval = 95% CI
refB_B.MSY   <- 0.83 # (0.74-0.97)
refB_B0 <- 0.30
ref2017SSBCurrent    <- ref[['derived_quants']][ref[['derived_quants']]$"LABEL"=="SPB_284","Value"]  #
ref2017SSBCurSD      <- ref[['derived_quants']][ref[['derived_quants']]$"LABEL"=="SPB_284","StdDev"]  #
ref2017SSBMSY        <- ref[['derived_quants']][ref[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
ref2017SSBMSYSD      <- ref[['derived_quants']][ref[['derived_quants']]$"LABEL"=="SSB_MSY","StdDev"]  #
# SPB_MSY is not included in covar
#ref2017SSBCurMSYcorr <- ref[["CoVar"]][ref[['CoVar']]$"label.i"=="SPB_284" & ref[['CoVar']]$"label.j"=="SPB_MSY","corr"]
# Use correlation from MSY filtered grid (SPB_276, SPB_MSY) = 0.59
# then using cavar calcns (at bottom) of BY/BMSY
#refBYoBMSYcv <- 0.052  # assuming 0 correlation yields cv = 0.075

#SS_plots(ref2017, uncertainty=F)
ref$derived_quants[ref$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
max(ref$equil_yield$Catch)




##################################################################################
# gridY19.1test - minimal fractional factorial new template test
#path=OMRootDir %&% 'OMconditioning\\YFT\\'
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.1test.f.R")
#OMgridY19.1test.List <- makeGridY19.1test.f(path=path, gridName= 'gridY19.1', makeGrid=F, makeBatFiles=F, splitMasterBatch=F, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY19.1test.List <- makeGridY19.1test.f(path=path, gridName= 'gridY19.1', makeGrid=T, makeBatFiles=T, splitMasterBatch=4, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#print(OMgridY19.1test.List)
#
#
#make the list into a uniformaly weighted list with OM specs as names
#names(OMgridY19.1test.List) <- OMgridY19.1test.List
#OMgridY19.1test.List[] <- 1/length(OMgridY19.1test.List)
#print(OMgridY19.1test.List)
#
#save(OMgridY19.1test.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.1test.List.RDA")
#load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.1test.List.RDA")
#
# import summary results & diagnostics from the gridB19.0List of models
#gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY19.1\\'
#importGrid.f(gridList=names(OMgridY19.1test.List), gridDir=gridDir, convergedNum=1, covar=F, stdOnly=F, doLoad=F)
#out <- importGrid.f(gridList=names(OMgridY19.1test.List), gridDir=gridDir, convergedNum=1, covar=F, stdOnly=F, doLoad=T)
#
#plotIndices2.f(modList=names(OMgridY19.1test.List), mfrowLayout = c(4,2), MSYyLim=c(0,700))





##################################################################################
# gridY19.0 - minimal fractional factorial new template test
#path=OMRootDir %&% 'OMconditioning\\YFT\\'
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.0.f.R")
#OMgridY19.0.List <- makeGridY19.0.f(path=path, gridName= 'gridY19.0', makeGrid=F, makeBatFiles=F, splitMasterBatch=F, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY19.0.List <- makeGridY19.0.f(path=path, gridName= 'gridY19.0', makeGrid=T, makeBatFiles=T, splitMasterBatch=4, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#print(OMgridY19.0.List)
#
#
##make the list into a uniformaly weighted list with OM specs as names
#names(OMgridY19.0.List) <- OMgridY19.0.List
#OMgridY19.0.List[] <- 1/length(OMgridY19.0.List)
#print(OMgridY19.0.List)
#
#save(OMgridY19.0.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.0.List.RDA")
#load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.0.List.RDA")
#
## import summary results & diagnostics from the gridB19.0List of models
#gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY19.1\\'
#importGrid.f(gridList=names(OMgridY19.0.List), gridDir=gridDir, convergedNum=1, covar=F, stdOnly=F, doLoad=F)
#out <- importGrid.f(gridList=names(OMgridY19.0.List), gridDir=gridDir, convergedNum=1, covar=F, stdOnly=F, doLoad=T)
#
#plotIndices2.f(modList=names(OMgridY19.0.List), mfrowLayout = c(4,2), MSYyLim=c(0,700))
#
#





##################################################################################
# gridY20.1 - main effects fractional factorial design - no environmental movement
path=OMRootDir %&% 'OMconditioning\\Yellowfin\\'
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\makeGridY20.1.f.R")

OMgridY20.1.List <- makeGridY20.1.f(path=path, gridName= 'gridY20.1', makeGrid=F, makeBatFiles=F, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY20.1.List <- makeGridY20.1.f(path=path, gridName= 'gridY20.1', makeGrid=T, makeBatFiles=T, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
print(OMgridY20.1.List)

# gridY20.2 - main effects fractional factorial design - all environmental movement
path=OMRootDir %&% 'OMconditioning\\Yellowfin\\'
source(mainRootDir %&% "OMconditioning\\RStuff\\phase3\\makeGridY20.2.f.R")

OMgridY20.2.List <- makeGridY20.2.f(path=path, gridName= 'gridY20.2', makeGrid=F, makeBatFiles=F, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY20.2.List <- makeGridY20.2.f(path=path, gridName= 'gridY20.2', makeGrid=T, makeBatFiles=T, splitMasterBatch=30, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
print(OMgridY20.2.List)




#make the list into a uniformaly weighted list with OM specs as names
names(OMgridY20.1.List) <- OMgridY20.1.List
OMgridY20.1.List[] <- 1/length(OMgridY20.1.List)
print(OMgridY20.1.List)

#make the list into a uniformaly weighted list with OM specs as names
names(OMgridY20.2.List) <- OMgridY20.2.List
OMgridY20.2.List[] <- 1/length(OMgridY20.2.List)
print(OMgridY20.2.List)

save(OMgridY20.1.List,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.1.List.RDA")
save(OMgridY20.2.List,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.2.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.1.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.2.List.RDA")

# use plotIndices instead of importGrid...
gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY20.1\\'

# only do this once - copies best fit of each jitter to root dir 
#chooseBestFitModels(modelList=names(OMgridY20.1.List), SSRootDir = gridDir) 
OMgridY20.1.List.Converged <- chooseBestFitModels(modelList=names(OMgridY20.1.List), SSRootDir = gridDir)$GoodModels 

names(OMgridY20.1.List.Converged) <- OMgridY20.1.List.Converged
OMgridY20.1.List.Converged[] <- 1/length(OMgridY20.1.List.Converged)
save(OMgridY20.1.List.Converged,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.1.List.Converged.RDA")
load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.1.List.Converged.RDA")

#SSModelCache <- new.env()

Y20.1.sumStats <- #cbind(omList,
  plotIndices.f(doPlots=T, modList=names(OMgridY20.1.List.Converged), mfrowLayout = c(4,2), MSYyLim=c(0,600), mFile = F,  SPB_Yr="SPB_284",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(403, 339, 436), refSSBYoSSBMSY=c(0.83, 0.74, 0.97), refSSBYoSSB0=c(0.30, 0.27, 0.33),refMSY=c(403, 339, 436),
                refModel=ref, SSRootDir = gridDir,
                cpueMP_File="H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\Yellowfin\\AssessmentFiles2018\\LL CPUE/yftMPcpue1962-2018.dat",
                cpueNormYrs=  c(1980:2017), # c(2014:2018), # c(1972:2018),
                #firstCalendarYr = 1950, 
                lastCalendarYr = 2017,
                firstYrQtr = 13) #, MPcpue=MPcpue)


OMrefY20.1.List <- Y20.1.sumStats[[1]][as.numeric(Y20.1.sumStats[[1]][,'catchLLH']) < 0.00001,'modList']
length(OMrefY20.1.List)
names(OMrefY20.1.List) <- OMrefY20.1.List
OMrefY20.1.List[] <- 1/length(OMrefY20.1.List)
save(OMrefY20.1.List,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY20.1.List.RDA")


tmpList <- 1
names(tmpList) <- "h90_M10_t01_q0_iC_i3_iR1_gr1_ess5_SD_x4"     

plotIndices.f(doPlots=T, modList=names(tmpList), mfrowLayout = c(4,2), MSYyLim=c(0,600), mFile = F,  SPB_Yr="SPB_284",
              inputRefLines=F, #override list below which are extracted from reports etc
              refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(403, 339, 436), refSSBYoSSBMSY=c(0.83, 0.74, 0.97), refSSBYoSSB0=c(0.30, 0.27, 0.33),refMSY=c(403, 339, 436),
              refModel=ref, SSRootDir = gridDir,
              cpueMP_File="H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\Yellowfin\\AssessmentFiles2018\\LL CPUE/yftMPcpue1962-2018.dat",
              cpueNormYrs=  c(1980:2017), # c(2014:2018), # c(1972:2018),
              #firstCalendarYr = 1950, 
              lastCalendarYr = 2017,
              firstYrQtr = 13) #, MPcpue=MPcpue)



OMrefY20.1.sumStats <- #cbind(omList,
  plotIndices.f(doPlots=T, modList=names(OMrefY20.1.List), mfrowLayout = c(4,2), MSYyLim=c(0,600), mFile = F,  SPB_Yr="SPB_284",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(403, 339, 436), refSSBYoSSBMSY=c(0.83, 0.74, 0.97), refSSBYoSSB0=c(0.30, 0.27, 0.33),refMSY=c(403, 339, 436),
                refModel=ref, SSRootDir = gridDir,
                cpueMP_File="H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\Yellowfin\\AssessmentFiles2018\\LL CPUE/yftMPcpue2018.dat",
                cpueNormYrs=  c(1972:2018), # c(2014:2018), # c(1972:2018),
                firstCalendarYr = 1950, 
                firstYrQtr = 13) #, MPcpue=MPcpue)







# clear cache since OMgrid20.1 and OMgrid20.2 have same model names, despite difference in environmental variable usage
source('H:/C-offline/MSE-IO-BET-YFT/OMconditioning/RStuff/phase3/plotIndices.f.R', echo=TRUE)
gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY20.2\\'
OMgridY20.2.List.Converged <- chooseBestFitModels(modelList=names(OMgridY20.2.List), SSRootDir = gridDir)$GoodModels 
names(OMgridY20.2.List.Converged) <- OMgridY20.2.List.Converged
OMgridY20.2.List.Converged[] <- 1/length(OMgridY20.2.List.Converged)
save(OMgridY20.2.List.Converged,file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.2.List.Converged.RDA")

# copies best fit of each jitter to root dir and creates list of models that meet certain criteria (e.g. converged) 
# chooseBestFitModels(modelList=names(OMgridY20.1.List), SSRootDir = gridDir) 
OMgridY20.2.List.Converged <- chooseBestFitModels(modelList=names(OMgridY20.2.List), SSRootDir = gridDir)$GoodModels 

#SSModelCache <- new.env()
Y20.2.sumStats <- #cbind(omList,
  plotIndices.f(doPlots=T, modList=names(OMgridY20.2.List.Converged), mfrowLayout = c(4,2), MSYyLim=c(0,600), mFile = F,  SPB_Yr="SPB_284",
                inputRefLines=F, #override list below which are extracted from reports etc
                refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(403, 339, 436), refSSBYoSSBMSY=c(0.83, 0.74, 0.97), refSSBYoSSB0=c(0.30, 0.27, 0.33),refMSY=c(403, 339, 436),
                refModel=ref, SSRootDir = gridDir,
                cpueMP_File="H:/C-offline/MSE-IO-BET-YFT/OMconditioning/Bigeye/AssessmentFiles2019/CPUEfromIOTC/BETMPcpue2019.dat",
                cpueNormYrs=  c(1972,2018), # c(2014:2018), # c(1972,2018),
                firstCalendarYr = 1950, 
                firstYrQtr = 13) #, MPcpue=MPcpue)

# gridY20.1 and Y20.2 look really almost identical - check if this is an error + jitter noise

noENV <- SS_output(dir = OMRootDir %&% 'OMconditioning\\YFT\\gridY20.1\\' %&% names(OMgridY20.1.List.Converged[1]), repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(noENV, uncertainty=F)

ENV <- SS_output(dir = OMRootDir %&% 'OMconditioning\\YFT\\gridY20.2\\' %&% names(OMgridY20.1.List.Converged[1]), repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ENV, uncertainty=F)

noENV2 <- SS_output(dir = OMRootDir %&% 'OMconditioning\\YFT\\gridY20.1\\' %&% names(OMgridY20.1.List.Converged[4]), repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(noENV2, uncertainty=F)
ENV2 <- SS_output(dir = OMRootDir %&% 'OMconditioning\\YFT\\gridY20.2\\' %&% names(OMgridY20.2.List.Converged[4]), repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(ENV2, uncertainty=F)








# import summary results & diagnostics from the gridB19.0List of models
gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY19.1\\'
importGrid.f(gridList=names(OMgridY19.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
out <- importGrid.f(gridList=names(OMgridY19.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

# 13 of 144 models failed to converged 3 times; ~9 failed to converge once
OMgridY19.1.convergedList <- rep(1/length(out$loadList), length(out$loadList))
names(OMgridY19.1.convergedList) <- out$loadList

plotIndices2.f(modList=names(OMgridY19.1.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
    SPB_Yr = "SPB_284", inputRefLines = F,
    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
        0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
    doPlots = T)







##################################################################################
# gridY19.2 - Y19.1 with additional CLRW option (min(100, ESS^0.75)
path=OMRootDir %&% 'OMconditioning\\YFT\\'
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.2.f.R")

OMgridY19.2.List <- makeGridY19.2.f(path=path, gridName= 'gridY19.2', makeGrid=F, makeBatFiles=F, splitMasterBatch=60, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#sum(names(OMgridY19.1.List) %in% OMgridY19.2.List5)
#sum(OMgridY19.2.List5 %in% OMgridY19.2.List2)
#OMgridY19.2.List <- makeGridY19.2.f(path=path, gridName= 'gridY19.2', makeGrid=T, makeBatFiles=T, splitMasterBatch=40, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
print(OMgridY19.2.List)


#make the list into a uniformaly weighted list with OM specs as names
names(OMgridY19.2.List) <- OMgridY19.2.List
OMgridY19.2.List[] <- 1/length(OMgridY19.2.List)
print(OMgridY19.2.List)

save(OMgridY19.2.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.2.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.2.List.RDA")

# import summary results & diagnostics from the gridB19.0List of models
gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY19.2\\'
importGrid.f(gridList=names(OMgridY19.2.List), gridDir=gridDir, convergedNum=2, covar=F, stdOnly=F, doLoad=F)
out <- importGrid.f(gridList=names(OMgridY19.2.List), gridDir=gridDir, convergedNum=2, covar=F, stdOnly=F, doLoad=T)

OMgridY19.2.convergedList <- rep(1/length(out$loadList), length(out$loadList))
names(OMgridY19.2.convergedList) <- out$loadList

Y19.2.sumStats <- plotIndices2.f(modList=names(OMgridY19.2.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
    SPB_Yr = "SPB_284", inputRefLines = F,
    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
        0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
    doPlots = T)

# check bounds
print("loBounds warnings")
table(unlist(Y19.2.sumStats[[2]]))
sum(table(unlist(Y19.2.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(Y19.2.sumStats[[3]]))
print(Y19.2.sumStats[[4]]) #identify specific problem models



#check productivity outlier
outlier <- names(OMgridY19.2.convergedList[as.numeric(Y19.2.sumStats[[1]][,'MSY'])> 800])

outlier1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.2\\" %&% outlier %&% "\\converged1", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
outlier2 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.2\\" %&% outlier %&% "\\converged2", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
testList <- rep(1,2)
names(testList) <- c('outlier1', 'outlier2')

test.sumStats <- plotIndices2.f(modList=names(testList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
    SPB_Yr = "SPB_284", inputRefLines = F,
    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
        0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
    doPlots = F)
test.sumStats[[1]]




#subset with a catch penalty cutoff of 0.1 and 0.00001
#OMgridY19.2CP1convergedList <- OMgridY19.2convergedList[as.numeric(Y19.2.sumStats[[1]][,'catchLLH'])<0.1]
OMgridY19.2CP5.convergedList <- OMgridY19.2.convergedList[as.numeric(Y19.2.sumStats[[1]][,'catchLLH'])<0.00001]
#plot some summary stuff
#Y19.2CP1.sumStats <- #cbind(omList,
#   plotIndices2.f(doPlots=T, modList=names(gridY19.2CPconvergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
#   inputRefLines=F, #override list below which are extracted from reports etc
#   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

Y19.2CP5.sumStats <- plotIndices2.f(modList=names(OMgridY19.2CP5.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
    SPB_Yr = "SPB_284", inputRefLines = F,
    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
        0.74, 0.97), refSSBYoSSB0 = c(0., 0, 0), refMSY = c(403),
    doPlots = T)


# drop the dome-shaped selectivity
OMgridY19.2SL.convergedList <- OMgridY19.2.convergedList[grepl("SD", names(OMgridY19.2.convergedList))]
OMrefY19.2SL.List <- OMgridY19.2SL.convergedList
save(OMrefY19.2SL.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.2SL.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.2SL.List.RDA")



Y19.2SL.sumStats <- plotIndices2.f(modList=names(OMgridY19.2SL.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
    SPB_Yr = "SPB_284", inputRefLines = F,
    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
        0.74, 0.97), refSSBYoSSB0 = c(0., 0, 0), refMSY = c(403),
    doPlots = T)


# drop the pessimistic half as a robustness case to demonstrate that feedback works??

OMrobY19.2opt.List  <- OMgridY19.2.convergedList[as.numeric(Y19.2.sumStats[[1]][,"B_B.MSY"])>1.]
save(OMrobY19.2opt.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrobY19.2opt.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrobY19.2opt.List.RDA")








##################################################################################
# gridY19.3 - grid agreed in TCMP 2019 - replaces unadulterated re-weighting option CLRW with CL75 
path=OMRootDir %&% 'OMconditioning\\YFT\\'
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.3.f.R")

##################################################################################
# gridY19.3fullCross - gridY19.3 full model cross for testing purposes (gridY19.3 is a subset) 

#path=OMRootDir %&% 'OMconditioning\\YFT\\'
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.3fullCross.f.R")





OMgridY19.3.List <- makeGridY19.3.f(path=path, gridName= 'gridY19.3', makeGrid=F, makeBatFiles=F, splitMasterBatch=60, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#sum(names(OMgridY19.1.List) %in% OMgridY19.3.List5)
#sum(OMgridY19.3.List5 %in% OMgridY19.3.List2)
#OMgridY19.3.List <- makeGridY19.3.f(path=path, gridName= 'gridY19.3', makeGrid=T, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
print(OMgridY19.3.List)


#make the list into a uniformaly weighted list with OM specs as names
names(OMgridY19.3.List) <- OMgridY19.3.List
OMgridY19.3.List[] <- 1/length(OMgridY19.3.List)
print(OMgridY19.3.List)

save(OMgridY19.3.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.3.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.3.List.RDA")


# import summary results & diagnostics from the gridB19.0List of models
gridDir <- OMRootDir %&% 'OMconditioning\\YFT\\gridY19.3\\'
importGrid.f(gridList=names(OMgridY19.3.List), gridDir=gridDir, convergedNum=1, covar=F, stdOnly=F, doLoad=F)
importGrid.f(gridList=names(OMgridY19.3.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
out3 <- importGrid.f(gridList=names(OMgridY19.3.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)



OMgridY19.3.convergedList <- rep(1/length(out3$loadList), length(out3$loadList))
names(OMgridY19.3.convergedList) <- out3$loadList

Y19.3.sumStats <- plotIndices2.f(modList=names(OMgridY19.3.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                 SPB_Yr = "SPB_284", inputRefLines = F,
                                 refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                 refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                 0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403))
                                 #, doPlots = T)


#subset with a catch penalty cutoff of 
#OMgridY19.2CP1convergedList <- OMgridY19.2convergedList[as.numeric(Y19.2.sumStats[[1]][,'catchLLH'])<0.1]
OMgridY19.3CP5.convergedList <- OMgridY19.3.convergedList[as.numeric(Y19.3.sumStats[[1]][,'catchLLH'])<0.00001]
Y19.3CP5.sumStats <- plotIndices2.f(modList=names(OMgridY19.3CP5.convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                 SPB_Yr = "SPB_284", inputRefLines = F,
                                 refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                 refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                          0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                 doPlots = T)




#Remove mean annualized CPUE RMSE > 0.3, as this is a poor fit to critical data and corresonds to a handful of high MSY outliers
tmp <- as.data.frame(Y19.3CP5.sumStats[[1]], stringsAsFactors = F)
OMrefY19.3List <- tmp$modList[tmp$CPUE.fit<0.3]

OMrefY19.3.sumStats <- plotIndices2.f(modList=OMrefY19.3List, mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                    SPB_Yr = "SPB_284", inputRefLines = F,
                                    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                             0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                    doPlots = T)


# check bounds
print("loBounds warnings")
table(unlist(OMrefY19.3.sumStats[[2]]))
sum(table(unlist(OMrefY19.3.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(OMrefY19.3.sumStats[[3]]))
print(OMrefY19.3.sumStats[[4]]) #identify specific problem models


#The reference case OM for the 2019 TCMP 
names(OMrefY19.3List) <- OMrefY19.3List
OMrefY19.3List[1:length(OMrefY19.3List)] <- 1/length(OMrefY19.3List) 

save(OMrefY19.3List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.3List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.3List.RDA")




#inspect some of the extreme individual models in full detail
#highest MSY -  "h90_M08_t0001_q0_iC_i3_iR1_gr2_CL75_SD_x4"
tmp <- as.data.frame(OMrefY19.3.sumStats[[1]], stringsAsFactors = F)
modID1 <- tmp[max(as.numeric(tmp$MSY))==as.numeric(tmp$MSY),"modList"]
SS_plots(get(modID1), uncertainty=F)

#lowest MSY.,, "h80_M06_t0001_q0_iC_i3_iR1_gr2_CL75_SL_x8"
modID2 <- tmp[min(as.numeric(tmp$MSY))==as.numeric(tmp$MSY),"modList"]
SS_plots2(get(modID2), uncertainty=F)

#highest B/BMSY - "h90_M08_t0001_q0_iC_i3_iR1_gr2_CL75_SD_x4"
modID3 <- tmp[max(as.numeric(tmp$B_B.MSY))==as.numeric(tmp$B_B.MSY),"modList"]
SS_plots(get(modID3), uncertainty=F)

#lowest B/BMSY - "h70_M06_t0001_q1_iH_i3_iR1_gr1_ess5_SD_x4"
modID4 <- tmp[min(as.numeric(tmp$B_B.MSY))==as.numeric(tmp$B_B.MSY),"modList"]
SS_plots(get(modID4), uncertainty=F)








##################################################################################
# gridY19.3fullCross - gridY19.3 full model cross for testing purposes (gridY19.3 is a subset) 
# gridY19.3.2way is all 2 way interactions

#path=OMRootDir %&% 'OMconditioning\\YFT\\'
path=mainRootDir %&% 'OMconditioning\\YFT\\'
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridY19.3fullCross.f.R")

OMgridY19.3.2way.List <- makeGridY19.3fullCross.f(path=path, gridName= 'gridY19.3.2way', makeGrid=F, makeBatFiles=F, splitMasterBatch=60, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY19.3.2way.List <- makeGridY19.3fullCross.f(path=path, gridName= 'gridY19.3.2way', makeGrid=F, makeBatFiles=T, splitMasterBatch=60, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridY19.3.2way.List <- makeGridY19.3fullCross.f(path=path, gridName= 'gridY19.3.2way', makeGrid=T, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")   #create the grid DIR structure
print(OMgridY19.3.2way.List)

names(OMgridY19.3.2way.List) <- OMgridY19.3.2way.List
OMgridY19.3.2way.List[1:length(OMgridY19.3.2way.List)] <- 1/length(OMgridY19.3.2way.List) 


save(OMgridY19.3.2way.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.3.2way.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY19.3.2way.List.RDA")

OMgridY19.3.2way.List.sumStats <- plotIndices.f(SSRootDir="E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3.2way\\", modList=names(OMgridY19.3.2way.List), mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                      SPB_Yr = "SPB_284", inputRefLines = F,
                                      refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                      refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                               0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                      doPlots = T)

dim(OMgridY19.3.2way.List.sumStats[[1]])
hist(log10(as.numeric(as.numeric(OMgridY19.3.2way.List.sumStats[[1]][,'catchLLH']))), breaks=100, xlab="Catch LLH", main="OMgridY19.4")
#subset with a catch penalty cutoff  
#OMgridY19.2CP1convergedList <- OMgridY19.2convergedList[as.numeric(Y19.2.sumStats[[1]][,'catchLLH'])<0.1]
OMgridY19.3.2way.List.CP5 <- OMgridY19.3.2way.List.sumStats[[1]][as.numeric(OMgridY19.3.2way.List.sumStats[[1]][,'catchLLH'])<0.00001,'modList']
length(OMgridY19.3.2way.List.CP5)

OMgridY19.3.2way.List.CP5.sumStats <- plotIndices.f(SSRootDir="E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3.2way\\", modList=OMgridY19.3.2way.List.CP5, mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                    SPB_Yr = "SPB_284", inputRefLines = F,
                                    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                             0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                    doPlots = T)


OMgridY19.3.2way.List.CP5.CV3 <- OMgridY19.3.2way.List.CP5.sumStats[[1]][as.numeric(OMgridY19.3.2way.List.CP5.sumStats[[1]][,'CPUE.fit'])<0.3,'modList']
length(OMgridY19.3.2way.List.CP5.CV3)

OMgridY19.3.2way.List.CP5.CV3.sumStats <- plotIndices.f(SSRootDir="E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3.2way\\", modList=OMgridY19.3.2way.List.CP5.CV3, mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                                    SPB_Yr = "SPB_284", inputRefLines = F,
                                                    refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                                    refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                                             0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                                    doPlots = T)


OMrefY19.4List <- OMgridY19.3.2way.List.CP5.CV3.sumStats[[1]][,'modList']
names(OMrefY19.4List) <- OMrefY19.4List
OMrefY19.4List[] <-  1/length(OMrefY19.4List)

save(OMrefY19.4List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.4List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.4List.RDA")


# test case with the worst fit of the converged models...(based on best fit rejection of catch penalty problems which would affect some of these)
OMgridY19.4.BadFit.sumStats <- plotIndices.f(SSRootDir="E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3.2way\\", modList=names(OMrefY19.4List), mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                                        SPB_Yr = "SPB_284", inputRefLines = F,
                                                        refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                                        refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                        0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                                        doPlots = T, 
                                                        worstLLH=T)

# best fit of the converged models...
OMgridY19.4.BestFits.sumStats <- plotIndices.f(SSRootDir="E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3.2way\\", modList=names(OMrefY19.4List), mfrowLayout = c(4,2), MSYyLim=c(0,700),
                                                        SPB_Yr = "SPB_284", inputRefLines = F,
                                                        refSSB0 = c(0,0,0), refSSBY = c(0,0,0),
                                                        refSSBMSY = c(0,0,0), refSSBYoSSBMSY = c(0.83,
                                                                                                 0.74, 0.97), refSSBYoSSB0 = c(0.3, 0, 0), refMSY = c(403),
                                                        doPlots = T)


# error calculations on basis of best fit models
deltaMSY     <- as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY']) -  as.numeric(OMgridY19.4.BadFit.sumStats[[1]][,'MSY'])
deltaB_B.MSY <- as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY']) -  as.numeric(OMgridY19.4.BadFit.sumStats[[1]][,'B_B.MSY'])

hist(100*deltaMSY/as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY']), breaks=50, main="MSY Minimization error", xlab=" % Error")
hist(100*deltaB_B.MSY/as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY']), breaks=50, main="B/BMSY Minimization error", xlab=" % Error")
plot(deltaMSY,deltaB_B.MSY)

sd(deltaMSY/as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY']))
range(deltaMSY/as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY']))
sd(as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY'])) / mean(as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'MSY'])) 

sd(deltaB_B.MSY / as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY']))
range(deltaB_B.MSY / as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY']))
sd(as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY'])) / mean(as.numeric(OMgridY19.4.BestFits.sumStats[[1]][,'B_B.MSY'])) 

# catch penalty filtering on poor fit models results in 368 models
WorstFitCP5 <-  OMgridY19.4.BadFit.sumStats[[1]][as.numeric(OMgridY19.4.BadFit.sumStats[[1]][,'catchLLH'])<0.00001,]
BestFit.WorstFitCP5 <- OMgridY19.4.BestFits.sumStats[[1]][as.numeric(OMgridY19.4.BadFit.sumStats[[1]][,'catchLLH'])<0.00001,]

# error calculations
deltaMSY     <- as.numeric(BestFit.WorstFitCP5[,'MSY']) -  as.numeric(WorstFitCP5[,'MSY'])
deltaB_B.MSY <- as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']) -  as.numeric(WorstFitCP5[,'B_B.MSY'])

hist(100*deltaMSY/as.numeric(BestFit.WorstFitCP5[,'MSY']), breaks=50, main="MSY Minimization error", xlab=" % Error")
hist(100*deltaB_B.MSY/as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']), breaks=50, main="B/BMSY Minimization error", xlab=" % Error")
plot(deltaMSY,deltaB_B.MSY)

sd(deltaMSY/as.numeric(BestFit.WorstFitCP5[,'MSY']))
range(deltaMSY/as.numeric(BestFit.WorstFitCP5[,'MSY']))
sd(as.numeric(BestFit.WorstFitCP5[,'MSY']) / mean(as.numeric(BestFit.WorstFitCP5[,'MSY']))) 

sd(deltaB_B.MSY / as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']))
range(deltaB_B.MSY / as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']))
sd(as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']) / mean(as.numeric(BestFit.WorstFitCP5[,'B_B.MSY']))) 



OMrefY19.4BFList <- WorstFitCP5[,'modList']
names(OMrefY19.4BFList) <- OMrefY19.4BFList
OMrefY19.4BFList[] <-  1/length(OMrefY19.4BFList)

save(OMrefY19.4BFList,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.4BFList.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefY19.4BFList.RDA")






reject on the basis of catch penalty

check bounds issues

marginal summary plots 

create OMrefY19.4.500

repeat above with worst set of converged models
  
create OMrefY19.4.BLLH.500

create OMrefY19.3 with fixed projection assumptions
  
compare OM Performance






  
  
)









# explore some other OM model configurations
#SS3.24y.tpl extract:
#  vul_bio(f,j)=value(vbio);
#  if(Q_setup(f,1)>0) vbio=pow(vbio,1.0+Q_parm(Q_setup_parms(f,1)));
# note non-linear Q parm is Q_parm 1, Q base is Q_parm 2

#confirm alternate Q paramterization (with H=0) equal to original
refYFT2018H0.1972 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018H0.1972", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018H0.1972, uncertainty=F)

# estimate H = 0.0170
# minimization failure in phase 6: really slow after restart, might need better initial H? 
refYFT2018Hest.1972 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018Hest.1972", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018Hest.1972, uncertainty=F)
refYFT2018Hest.1972$derived_quants[refYFT2018Hest.1972$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 353

#estimate H with pre-1972 - results in poor fit, presumably popn simply cannnot decline much in early years (without rec trend)  
refYFT2018Hest.1954 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018Hest.1954", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018Hest.1954, uncertainty=F)

# short time series CPUE estimate H = -0.014
refYFT2018Hest.1987 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018Hest.1987", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018Hest.1987, uncertainty=F)
refYFT2018Hest.1987$derived_quants[refYFT2018Hest.1987$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 363

# force H=0.1
refYFT2018H0.1.1972 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018H0.1.1972", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018H0.1.1972, uncertainty=F)
refYFT2018H0.1.1972$derived_quants[refYFT2018H0.1.1972$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 366

# force H=0.2
refYFT2018H0.2.1972 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018H0.2.1972", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018H0.2.1972, uncertainty=F)


# high mixing
#seems to work as expected, though a bit of differential depletion and tropical vs: temperate difference due to rec and/or source-sink dynamics
refYFT2018R1 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018R1", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018R1, uncertainty=F)
refYFT2018R1$derived_quants[refYFT2018R1$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MYS 447

# high even mixing with even rec dist - result as  previous almost, except all areas about equal
refYFT2018R1b <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018R1b", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018R1b, uncertainty=F)
refYFT2018R1b$derived_quants[refYFT2018R1b$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 434

# PS free-school CPUE overlap with NW LL CPUE (latter removed in 2007-2017)
# internal inconsistency with non-stationary selectivity 
# warning: must create base Q parm to use Q_envlink for fleet: 30
# All CPUE fit curiously well considering the envlink was mis-speified
refYFT2018PSItrend0 <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018PSItrend0", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018PSItrend0, uncertainty=F)
refYFT2018PSItrend0$derived_quants[refYFT2018PSItrend0$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 330; 


#as above - except with base Q to enable ENV link
refYFT2018PSItrendEst <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018PSItrendEst", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018PSItrendEst, uncertainty=F)
refYFT2018PSItrendEst$derived_quants[refYFT2018PSItrendEst$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY 327; 

#PS with H estimated - SS crashed, but seemingly after at least some final files were produced 
refYFT2018PSHEst <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018PSHEst", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018PSHEst, uncertainty=F)
refYFT2018PSHEst$derived_quants[refYFT2018PSHEst$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY ; 

#as ref2018 except with Ricker SR h=0.8
refYFT2018Ricker <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018Ricker", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018Ricker, uncertainty=F)
refYFT2018Ricker$derived_quants[refYFT2018Ricker$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY ; 244


#as ref2018 except with Ricker SR h estiamted
refYFT2018RickerhEst <- SS_output(dir = mainRootDir %&% "OMconditioning\\YFT\\AssessmentFiles2018\\refYFT2018RickerhEst", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(refYFT2018RickerhEst, uncertainty=F)
refYFT2018RickerhEst$derived_quants[refYFT2018RickerhEst$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#MSY ; 244
refYFT2018RickerhEst$maximum_gradient_component


print(cbind(yearLab, SPBoSPBMSYMatSeries)[(142+127):(142+130),1:2])
mean(SPBoSPBMSYMatSeries[(142+127):(142+130),2])

mList1=c("ref2018",
        "refYFT2018H0.1972",
        "refYFT2018Hest.1972",
        "refYFT2018H0.1.1972",
        "refYFT2018H0.2.1972",
        "refYFT2018Hest.1954",
        "refYFT2018Hest.1987")
mList1Labels=c("ref2018",
        "H0.1972",
        "Hest.1972",
        "H0.1.1972",
        "H0.2.1972",
        "Hest.1954",
        "Hest.1987")


mList2=c("ref2018",
         "refYFT2018R1",
         "refYFT2018R1b")
mList2Labels=c("ref2018",
               "R1",
               "R1b")


mList3=c("ref2018",
         "refYFT2018PSItrend0",
         "refYFT2018PSItrendEst",
         "refYFT2018PSHEst")
mList3Labels=c("ref2018",
               "PSq0",
               "PSqEst",
               "PSHEst")


timeSeriesPlots2.f(mList=mList1, mListLabels = mList1Labels)
timeSeriesPlots2.f(mList=mList2, mListLabels = mList2Labels)
timeSeriesPlots2.f(mList=mList3, mListLabels = mList3Labels)
timeSeriesPlots2.f(mList="refYFT2018Ricker", mListLabels = "Ricker SR")




...old below here...






##################################################################################
# gridY17.2Tag - suite of models with tags for OM-refY17.2
path=OMRootDir %&% 'OMconditioning\\YFT\\gridY17.2Tag'
gridY17.2TagList <- makeGridY17.2Tag.f(path=path, doHess=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure
print(gridY17.2TagList)
#gridY17.2TagList <- makeGridY17.2Tag.f(path=path, doHess=F, makeGrid=T)    #create the grid structure for running a batch file
# exit this R script and run the SS batch file(s)

#partial grid to fix errs
#   path=rootDir %&% 'OMconditioning\\YFT\\gridY17.2noTag'
#   gridY17.2noTagList <- makeGridY17.2noTag.f(path=path, doHess=F, makeGrid=F, ess.val  = c("CLRW"))    #check the list of grid elements, but don't create the grid DIR structure
#   print(gridY17.2noTagList)
#   gridY17.2noTagList <- makeGridY17.2noTag.f(path=path, doHess=F, makeGrid=T, ess.val  = c("CLRW"))    #create the grid structure for running a batch file
#partial grid to fix errs
#   path=rootDir %&% 'OMconditioning\\YFT\\gridY17.2Tag'
#   gridY17.2TagList <- makeGridY17.2Tag.f(path=path, doHess=F, makeGrid=F, ess.val  = c("CLRW"))    #check the list of grid elements, but don't create the grid DIR structure
#   print(gridY17.2TagList)
#   gridY17.2TagList <- makeGridY17.2Tag.f(path=path, doHess=F, makeGrid=T, ess.val  = c("CLRW"))    #create the grid structure for running a batch file


gridDir <- OMRootDir %&% 'OMconditioning//YFT//gridY17.2Tag//'
importGrid.f(gridList=gridY17.2TagList, gridDir=gridDir, covar=F)
save(list=gridY17.2TagList, file=rootDir %&% 'OMconditioning//YFT//gridY17.2Tag//gridY17.2Tag.RDA')


plotIndices2.f(modList=gridY17.2TagList, mfrowLayout = c(4,2), MSYyLim=c(0,700))



##################################################################################
# Merge and plot the two grids to make one unbalanced aggregate...

#gridDir <- rootDir %&% 'OMconditioning//YFT//gridY17.2noTag//'
#importGrid.f(gridList=gridY17.2noTagList, gridDir=gridDir, covar=F)
#gridDir <- rootDir %&% 'OMconditioning//YFT//gridY17.2Tag//'
#importGrid.f(gridList=gridY17.2TagList, gridDir=gridDir, covar=F)
OMref17.2grid <- c(gridY17.2TagList, gridY17.2noTagList)
#gridList <- OMref17.2grid

#ccDat <- cbind(OMref17.2grid,plotIndices2.f(modList=OMref17.2grid, mfrowLayout = c(4,2), MSYyLim=c(0,700)))
#colnames(ccDat)[1] <- 'modID'

#use boxplots and pairwise correlations to plot some key model results and diagnostics, marginalized by grid Options for the whole ensemble
#identify potential convergence problems, parameters on bounds etc
#skipPlots=T might be useful if there are stran
ccDat <- #cbind(omList,
   plotIndices2.f(doPlots=F, modList=OMref17.2grid, mfrowLayout = c(4,2), MSYyLim=c(0,700), #mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F) #override list below which are extracted from reports etc
   #refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))
   #)


# drop failed convergence
cDat <- ccDat[as.numeric(ccDat[,'max.Grad']) < -1, ]

cor(as.numeric(cDat[,'BY']),as.numeric(cDat[,'MSY']))

samp2 <- cDat

titleText <- "\nOMref17.2grid"

N <- nrow(samp2)

#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
layout(matrix(c(1,4,2,4,3,5,6,6), 4, 2, byrow = TRUE))
#par(mar = c(3,4,2,1))
par(mar = c(4,4,4,4))

pDat <- as.numeric(samp2[,'MSY'])
pDat[pDat>799] <- 799
pDat[pDat<201] <- 201

#h <- hist(pDat, breaks = c(200:800),
#h <- hist(pDat, breaks = c(20:80)*10,
h <- hist(pDat, breaks = c(20:80)*10,
  main="MSY Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="MSY", probability=T)
#lines(200:800, dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)/sum(dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)), col=3)
points(c(refMSY), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'MSY']))

#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:300)/100,
#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:30)/10,
h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:50)/20,
  main="SSB(2016)/SSB(MSY) Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="B(2016)/B(MSY)", probability=T)
points(c(refB_B.MSY), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'B_B.MSY']))

h <- hist(as.numeric(samp2[,'B_B0']), breaks = c(0:40)/40,
  main="SSB(2016)/SSB0 Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="B(2016)/B0", probability=T)
points(c(refB_B0), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'B_B0']))

X <-cbind(as.numeric(samp2[,'MSY']),as.numeric(samp2[,'B_B.MSY']))
print(summary(X))
print(cor(X[,1],X[,2]))
plot(X, xlab="MSY", ylab="SSB/SSBMSY", pch=19, cex=0.5, main="Bivariate Sample Plot")
points(jitter(X[,1], amount=mean(X[,1])/50),jitter(X[,2], amount=mean(X[,2])/50), pch=19, cex=0.01, col='grey')

#plot(table(samp2[,'modID']), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')
plot(sort(table(samp2[,'modID']))/sum(table(samp2[,'modID'])), main="Proportion of each model in OM ensemble", xlab="", ylab='Proportion', xaxt='n')
#plot(cumsum(sort(table(samp2[,'modID']))), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')

gridOptions <- unlist(c(strsplit(gridList, "_")))
sampledOptions <- unlist(c(strsplit(samp2[,'modID'], "_")))
plot(table(sampledOptions)/sum(table(sampledOptions)),type='p', col=3, pch=15, main = "Grid option frequency before and after filtering", ylab="Proportion")
lines(table(gridOptions)/sum(table(gridOptions)))

title(titleText, outer=TRUE)

plotIndices2.f(modList=as.character(samp2[,1]), mfrowLayout = c(4,2), MSYyLim=c(0,700))



###############################################################################################################################
# OM-ref17.2
# Bivariate sample of OMref17.2grid on basis of MSY and B/BMSY consistency with assessment and assumed variance

filtered <- gridSamplerBivar(
  gridListPlus = cDat, # model definitions and factors of interest for sampling
  sampleQuant = c('MSY','B_B.MSY'),        # quantities upon which to base the sample
  corr =0., # correlation among characteristics; works but appropriate value not determined

  FSList = list(             # subsample models to achieve the following proportions (specifying too many interactions will have an adverse effect)
  #  FSOpt1 <- c('t0001', 0.5),
  #  FSOpt2 <- c('t10', 0.5)),

  #  FSOpt1 <- c('t0001', 1.)),
  #  FSOpt2 <- c('t10', 0.5)),

  #  FSOpt1 <- c('q0', 0.5),
  #  FSOpt2 <- c('q1', 0.5)),

    FSOpt1 <- c('t0001', 'q0',0.25),
    FSOpt2 <- c('t0001', 'q1',0.25),
    FSOpt3 <- c('t10', 'q0',0.25),
    FSOpt4 <- c('t10', 'q1',0.25)),
  mu    = c(422,0.89),  #distribution means to aim for
  #mu    = 0.89,  # B/BMSY mean to attain (roughly)
  sigma = c(3.*refMSYcv,3.*refBYoBMSYcv),  #lognormal sampling sigma (~CV)
  sigmaTrunc = 3, #truncate sample distribution at this many sigma
  nBins = 3, #number of bins covering distribution is a grid of nBins X nBins
  logNorm=T)


OMref17.2List <- filtered[[2]]
details <- filtered[[1]]
details[,1] <- as.factor(details[,1])
samp2 <- details[sample(details[,1], size=2000, replace=T, prob=details[,'wtModList']),]
samp2[,1] <- as.character(samp2[,1])

titleText <- "\n OM-ref17.2"
#titleText <- "\n Alternate OM with sampling CVs 2X SA-ref"

N <- nrow(samp2)

#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
layout(matrix(c(1,4,2,4,3,5,6,6), 4, 2, byrow = TRUE))
#par(mar = c(3,4,2,1))
par(mar = c(4,4,4,4))

pDat <- as.numeric(samp2[,'MSY'])
pDat[pDat>799] <- 799
pDat[pDat<201] <- 201

#h <- hist(pDat, breaks = c(200:800),
#h <- hist(pDat, breaks = c(20:80)*10,
h <- hist(pDat, breaks = c(20:80)*10,
  main="MSY Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="MSY", probability=T)
#lines(200:800, dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)/sum(dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)), col=3)
points(c(refMSY), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'MSY']))

#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:300)/100,
#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:30)/10,
h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:40)/20,
  main="SSB(2016)/SSB(MSY) Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="B(2016)/B(MSY)", probability=T)
points(c(refB_B.MSY), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'B_B.MSY']))

h <- hist(as.numeric(samp2[,'B_B0']), breaks = c(0:40)/40,
  main="SSB(2016)/SSB0 Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="B(2016)/B0", probability=T)
points(c(refB_B0), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'B_B0']))

X <-cbind(as.numeric(samp2[,'MSY']),as.numeric(samp2[,'B_B.MSY']))
print(summary(X))
print(cor(X[,1],X[,2]))
plot(X, xlab="MSY", ylab="SSB/SSBMSY", pch=19, cex=0.5, main="Bivariate Sample Plot")
points(jitter(X[,1], amount=mean(X[,1])/50),jitter(X[,2], amount=mean(X[,2])/50), pch=19, cex=0.01, col='grey')

#plot(table(samp2[,'modID']), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')
plot(sort(table(samp2[,'modID']))/sum(table(samp2[,'modID'])), main="Proportion of each model in OM ensemble", xlab="", ylab='Proportion', xaxt='n')
#plot(cumsum(sort(table(samp2[,'modID']))), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')

gridOptions <- unlist(c(strsplit(gridList, "_")))
sampledOptions <- unlist(c(strsplit(samp2[,'modID'], "_")))
plot(table(sampledOptions)/sum(table(sampledOptions)),type='p', col=3, pch=15, main = "Grid option frequency before and after filtering", ylab="Proportion")
lines(table(gridOptions)/sum(table(gridOptions)))

title(titleText, outer=TRUE)

plotIndices2.f(modList=as.character(samp2[,1]), mfrowLayout = c(4,2), MSYyLim=c(0,700))


#need to ensure that the "correct", i.e. assessment-preferred CPUE is the one used for the MP
#this is achieved by making sure this series is the first model in the list... (iH and q0)
i <- 1
while(!prod((c("iH","q0") %in% unlist(strsplit(names(OMref17.2List[i]), split="_"))))) {
  i <- i+1
  if(i>length(OMref17.2List)){print("oops - better check your list for CPUE series..."); break}
}
if(i>1){
  tmp <- OMref17.2List[1]
  OMref17.2List[1] <- OMref17.2List[i]
  OMref17.2List[i] <- tmp
  names(OMref17.2List) <- names(OMref17.2List[c(i,1:(i-1),(i+1):length(OMref17.2List))])
}

OMref17.2WtList <- OMref17.2List
#save just the list of  weights (and model names as names)
save(OMref17.2WtList,file="OMref17.2WtList.RDA")
#load(OMref17.2List,file="OMref17.2WtList.RDA")

# save the full SS3 files, so they don't have to be reloaded from ASCII to recreate OMs ...too big
#gridDir <- rootDir %&% 'OMconditioning//YFT//gridY17.2//'
#importGrid.f(gridList=names(OMref17.2List), gridDir=gridDir, covar=F, keepEverything=T)
#save(list=names(OMref17.2List),file="OMref17.2ListMods.RDA")





