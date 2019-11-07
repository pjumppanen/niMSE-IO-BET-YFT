#=============================================================================================================================
# R script for making and inspecting the conditioned BET Operating Model OMrefB19.6 and associated robustness scenarios
#  OM gridB19.6 uses a fractional factorial design which only calculates a grid for main effects estimation (no interactions)
#  it was originally run as two separate grids, so has to be merged
#  OM fitting was repeated from jittered starting condiitons with a target of 3 independent convergences (or 10 failures)
#  Results are based on the lowest OBJfn value among the converged models
#=============================================================================================================================
#online
mainRootDir  <- "H:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path
OMRootDir   <- "E:\\KOL018\\MSE-IO-BET-YFT\\"
OMRootDir   <- mainRootDir

#offline
#rootDir <- "M:\\C-offline\\MSE-IO-BET-YFT\\"
#rootDir <- "C:\\MSE-IO-BET-YFT\\"

library(TinnRcom)                             # if using TinnR IDE
library (r4ss)                                # R package supporting Stock Synthesis - if something's broken it might be becuase this changed
library(planor)                               # fractional factorial design
library(PerformanceAnalytics)                 # for chart.Correlation
library(planor)                               # for fractional factorial design
library(MASS)

source(paste(mainRootDir,"OMconditioning\\RStuff\\phase2\\pasteOperator.R",sep=""))

#source(paste(rootDir,"Source\\pasteOperator.R",sep=""))
source(mainRootDir %&% "OMconditioning\\RStuff\\seasAsYrToDecYr.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.0.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.1.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.3.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.4b.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.5.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.5SD.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB18.2.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\importGrid.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\SS_output2.R") #temporarily bypasses file inconsistency error caused by tangled batch files
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\importGrid2.f.R") #temporarily bypasses file inconsistency error caused by tangled batch files

#source(mainRootDir %&% "OMconditioning\\RStuff\\plotIndices.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\plotIndices.f.DK.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\timeSeriesPlots.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\gridSamplerBivar.f.R")


#extract some reference case assessment values

#For comparison: Langley 2016 BET assessment ref case (provided by Dan Fu with nod from Adam) is a grid of 6
#So these figs are not strictly correct...
refB2017 <- SS_output(dir = mainRootDir %&% "OMconditioning\\BET\\AssessmentFiles2017\\TagLambda1", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
ref <- refB2017

# these are based on the aggregate of the six model assessments, however they were produced for the WPTT report
refMSY   <- 104 # from WPTT (derived from Langley 2016 assessment)
refMSYcv <- 0.127  #STD TotYield_MSY 3.67914e+03 (times 4 = 14716.56 )... CV = 3.5% ?!
refB_B.MSY   <- 1.29 # from Langley report
refB_B0 <- 0.38
#ref2017SSBCurrent    <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SPB_356","Value"]  #
#ref2017SSBCurSD      <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SPB_355","StdDev"]  #
#ref2017SSBMSY        <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SSB_MSY","Value"]  #
#ref2017SSBMSYSD      <- ref2017[['derived_quants']][ref2017[['derived_quants']]$"LABEL"=="SSB_MSY","StdDev"]  #
# SPB_MSY is not included in covar
#ref2017SSBCurMSYcorr <- ref2017[["CoVar"]][ref2017[['CoVar']]$"label.i"=="SPB_276" & ref2017[['CoVar']]$"label.j"=="SPB_MSY","corr"]
# Use correlation from MSY filtered grid (SPB_276, SPB_MSY) = 0.59
# then using cavar calcns (at bottom) of BY/BMSY
refBYoBMSYcv <- 0.136  # assuming 0 correlation yields cv = 0.075

#SS_plots(refB2017, uncertainty=F)
#ref$derived_quants[ref$derived_quants$LABEL == 'TotYield_MSY',]$Value*4/1000
#max(ref$equil_yield$Catch)



##########################################################################################################################################
# Omgrid19.6 was a merger of 2 OMgrid19.5 Lists, renamed (to prevent having to unnecessarily rerun half of the grid)

##########################################################################################################################################
# grid 19.5 = fractional factorial design grid with WPM/WPTT 2018 uncertainty requests (fixes regional scaling error) and omits robustness test dimensions
path=OMRootDir %&% 'OMconditioning\\BET\\'
OMgridB19.5SL.List <- makeGridB19.5.f(path=path, gridName= 'gridB19.5', makeGrid=F, makeBatFiles=F, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.5.List <- makeGridB19.5.f(path=path, gridName= 'gridB19.5', makeGrid=F, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.5.List <- makeGridB19.5.f(path=path, gridName= 'gridB19.5', makeGrid=T, makeBatFiles=T, splitMasterBatch=20, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure

#for (i in 1:length(OMgridB19.5SL.List)){
#  OMgridB19.5SL.List[i] <- OMgridB19.5SL.List[i] %&% "_SL" 
#}



#make the list into a uniformaly weighted list with OM specs as names
#names(OMgridB19.5SL.List) <- OMgridB19.5SL.List
#OMgridB19.5SL.List[] <- 1/length(OMgridB19.5SL.List)
#print(OMgridB19.5SL.List)

#save(OMgridB19.5.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.5.List.RDA")
#load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.6.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.5SL.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.5SD.List.RDA")

# import summary results & diagnostics from the gridB19.5List of models
gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB19.5\\'




#importGrid.f(gridList=names(OMgridB19.5SL.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
#outSL <- importGrid.f(gridList=names(OMgridB19.5SL.List), gridDir=gridDir, convergedNum=2, covar=F, stdOnly=F, doLoad=T)
#out <- importGrid.f(gridList=names(OMgridB19.5.List[names(OMgridB19.5.List) %in% c("h80_M10_t0001_q0_iH_i3_iR2_gr1_CLRW", "h80_M08_t0001_q0_iC_i1_iR2_gr1_ess10")]), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

#gridB19.5SLconvergedList <- outSL$loadList
#names(gridB19.5SLconvergedList) <- gridB19.5SLconvergedList








################################################################################
#examine convergence characteristics
#conv <- as.data.frame(type.convert(outSL$conStats[,2:6]))
#conv$modID <- outSL$conStats[,'modID']

#plot some summary stuff
#B19.5SL.sumStats <- #cbind(omList,
#   plotIndices2.f(doPlots=F, modList=names(gridB19.5SLconvergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
#   inputRefLines=F, #override list below which are extracted from reports etc
#   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

# sort out conflicting plotIndices versions...Paavp's version looks okay, except for duplicate convergence is ignored (?)
# plotIndices.PJ
#B19.5SL.sumStats <- #cbind(omList,
#  plotIndices.f(doPlots=T, modList=names(gridB19.5SLconvergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
#                 inputRefLines=F, #override list below which are extracted from reports etc
#                 refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
#                 refModel="ref", repeatedMin=3)

# revised plotIndices importing routine internalizes the convergence test
# 1 model not run for some reason?
# E:\KOL018\MSE-IO-BET-YFT\OMconditioning\BET\gridB19.5\h80_M06_t0001_q1_iH_iR1_ess10_SL  
B19.5SLconverged.sumStats <- #cbind(omList,
  plotIndices.f(SSRootDir=gridDir, doPlots=T, modList=names(OMgridB19.5SL.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
                 inputRefLines=F, #override list below which are extracted from reports etc
                 refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
                 refModel="ref", repeatedMin=3)


# check bounds
print("loBounds warnings")
table(unlist(B19.5SLconverged.sumStats[[2]]))
sum(table(unlist(B19.5SLconverged.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(B19.5SLconverged.sumStats[[3]]))
print(B19.5SLconverged.sumStats[[4]]) #identify specific problem models
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


