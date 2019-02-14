#=============================================================================================================================
# R script for making and inspecting the conditioned BET Operating Model OMrefB19.1 and associated robustness scenarios
#  OM gridB19.1 uses a fractional factorial design which only calculates a grid for main effects estimation (no interactions)
#
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
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.1.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.3.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB19.4b.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\makeGridB18.2.f.R")
#source(mainRootDir %&% "OMconditioning\\RStuff\\importGrid.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\SS_output2.R") #temporarily bypasses file inconsistency error caused by tangled batch files
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\importGrid2.f.R") #temporarily bypasses file inconsistency error caused by tangled batch files

source(mainRootDir %&% "OMconditioning\\RStuff\\plotIndices.f.R")
source(mainRootDir %&% "OMconditioning\\RStuff\\phase2\\plotIndices2.f.R")
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


#fix the missing growth issue in a one off half-grid merged back into gridB19.1
# nt so easy - fractional seems to use stochastic search which makes incompatible half-grids?
# grid 19.1 = fractional factorial design grid with WPM/WPTT 2018 uncertainty requests
path=OMRootDir %&% 'OMconditioning\\BET\\'
#OMgridB19.1.List <- makeGridB19.1.f(path=path, gridName= 'gridB19.1b', makeBatFiles=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure
OMgridB19.1.List <- makeGridB19.1.f(path=path, gridName= 'gridB19.1b', makeGrid=T, makeBatFiles=T, splitMasterBatch=T, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure
OMgridB19.1.List <- makeGridB19.1c.f(path=path, gridName= 'gridB19.1b', makeGrid=F, makeBatFiles=F, splitMasterBatch=36, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure




##########################################################################################################################################
# grid 19.1 = fractional factorial design grid with WPM/WPTT 2018 uncertainty requests
path=OMRootDir %&% 'OMconditioning\\BET\\'
OMgridB19.1.List <- makeGridB19.1.f(path=path, gridName= 'gridB19.1', makeBatFiles=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.1.List <- makeGridB19.1.f(path=path, gridName= 'gridB19.1', makeGrid=T, makeBatFiles=T, splitMasterBatch=T, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure

#make the list into a uniformaly weighted list with OM specs as names
names(OMgridB19.1.List) <- OMgridB19.1.List
OMgridB19.1.List[] <- 1/length(OMgridB19.1.List)
print(OMgridB19.1.List)

save(OMgridB19.1.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.1.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.1.List.RDA")


# check out some results for convergence sensitivity etc...
#modDir <- "H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.1\\test_h70_M10_t10_q0_iH_i1_iR1_gr1_CLRW"
#modDirName <- "test2."

#dum <- NULL
#for (j in 1:5){
#  tmp <- SS_output(dir = modDir  %&% "\\" %&% "j" %&% j, repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
# assign(modDirName %&% "j" %&% j,  tmp)
#  dum <- rbind(dum,c(j,tmp$likelihoods_used[1,1],
#                tmp$"maximum_gradient_component",
#                tmp$derived_quants[tmp$derived_quants$LABEL == "TotYield_MSY","Value"]*4,
#                tmp$derived_quants[tmp$derived_quants$LABEL == "Bratio_356","Value"]))
#  tmp$"maximum_gradient_component"
#  tmp$warnings
#  table(tmp$estimated_non_rec_devparameters$Status)
#colnames(dum) <- c("j", "ObjFn", "max(Grad)", "MSY", "B/BMSY")
#print(dum)



# import summary results & diagnostics from the gridB19.1List of models
gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB19.1b\\'
importGrid.f(gridList=names(OMgridB19.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
out <- importGrid.f(gridList=names(OMgridB19.1.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)
#out <- importGrid.f(gridList=names(OMgridB19.1.List[names(OMgridB19.1.List) %in% c("h80_M10_t0001_q0_iH_i3_iR2_gr1_CLRW", "h80_M08_t0001_q0_iC_i1_iR2_gr1_ess10")]), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T)

gridB19.1convergedList <- out$loadList
names(gridB19.1convergedList) <- gridB19.1convergedList

################################################################################
#examine convergence characteristics
conv <- as.data.frame(type.convert(out$conStats[,2:6]))
conv$modID <- out$conStats[,'modID']

#plot some summary stuff
B19.1.sumStats <- #cbind(omList,
   plotIndices2.f(doPlots=F, modList=names(gridB19.1convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))


# check bounds
print("loBounds warnings")
table(unlist(B19.1.sumStats[[2]]))
sum(table(unlist(B19.1.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(B19.1.sumStats[[3]]))
print(B19.1.sumStats[[4]]) #identify specific problem models
#print(B19.1.sumStats[[4]][c(85,104)]) # bait boat bound problems relaxed and rerun "AgeSel_11P_1_BB1"
# init_F hi bound tst - h90_M06_t0001_q1_iH_i1_iR2_gr2_CLRW
  tmp <- SS_output(dir = "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.1b\\h90_M06_t0001_q1_iH_i1_iR2_gr2_CLRW" , repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(tmp, uncertainty=F)


#relative OBJ value within modID
conv$relObj <- conv$objFn - ave(conv$objFn, conv$modID, FUN=min)
hist(conv$relObj, breaks=1000, xlim=c(-1,200))
summary(conv$relObj)

#relationship between final gradient and objFn
plot(log10(conv$maxGrad), conv$relObj, xlab="log10(max. gradient)", ylab="Relative -log(likelihood)")
lines(loess.smooth(log10(conv$maxGrad),conv$relObj),col=2)
abline(h=200)

plot(log10(conv$maxGrad), conv$relObj, xlab="log10(max. gradient)", ylab="Relative -log(likelihood)", ylim=c(0,240))
lines(loess.smooth(log10(conv$maxGrad),conv$relObj),col=2)

#mean of the CVs calculated within modIDs
mean(ave(conv$MSY, conv$modID, FUN=sd) / ave(conv$MSY, conv$modID, FUN=mean))
#CV of the best fit models among modIDs
sd(conv$MSY[conv$relObj==0]) / mean(conv$MSY[conv$relObj==0])

#mean of the CVs calculated within modIDs
mean(ave(conv$SSBYoSSBMSY, conv$modID, FUN=sd) / ave(conv$SSBYoSSBMSY, conv$modID, FUN=mean))
#CV of the best fit models among modIDs
sd(conv$SSBYoSSBMSY[conv$relObj==0]) / mean(conv$SSBYoSSBMSY[conv$relObj==0])

#relationship between stock status and objFn ?
plot(conv$SSBYoSSBMSY, 4.*conv$MSY, ylab="MSY",xlab="B/B(MSY)", cex=0.5)
points(conv$SSBYoSSBMSY[conv$relObj==0], 4.*conv$MSY[conv$relObj==0], cex=0.5, col=2, pch=16)







#################################################################################
#OMgridB19.2.List - drop the alternate growth curve and original area-weighting factors
#gridB19.2List <- gridB19.1convergedList[!(grepl("gr2", names(gridB19.1convergedList), fixed=TRUE) | grepl("iR1", names(gridB19.1convergedList), fixed=TRUE))]
#This yields a 36 model grid, but planor seemingly could not identify an equivalent 36 model design that allows all main effects to be estimable.

#subsample from gridB19.4
path=OMRootDir %&% 'OMconditioning\\BET\\'
OMgridB19.2.List <- makeGridB19.2.f(path=path, makeBatFiles=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure

names(OMgridB19.2.List) <- OMgridB19.2.List
OMgridB19.2.List[] <- 1/length(OMgridB19.2.List)
print(OMgridB19.2.List)

save(OMgridB19.2.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.2.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.2.List.RDA")


B19.2.sumStats <-
   plotIndices2.f(doPlots=F, modList=names(OMgridB19.2.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

OMLabel <- rep("B19.2 - 72 mods", nrow(B19.2.sumStats[[1]]))
B19.2.sumStats[[1]] <- cbind(B19.2.sumStats[[1]], OMLabel)



#################################################################################
#OMgridB19.3.List - drop the alternate growth curve and original area-weighting factors and increase the fractional factorial density
path=OMRootDir %&% 'OMconditioning\\BET\\'
OMgridB19.3.List <- makeGridB19.3.f(path=path, makeBatFiles=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.3.List <- makeGridB19.3.f(path=path, gridName= 'gridB19.3', makeGrid=T, makeBatFiles=T, splitMasterBatch=T, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure

names(OMgridB19.3.List) <- OMgridB19.3.List
OMgridB19.3.List[] <- 1/length(OMgridB19.3.List)
print(OMgridB19.3.List)

save(OMgridB19.3.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.3.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.3.List.RDA")

#plot some summary stuff
B19.3.sumStats <- #cbind(omList,
   plotIndices2.f(doPlots=F, modList=names(OMgridB19.3.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

OMLabel <- rep("B19.3 - 144 mods", nrow(B19.3.sumStats[[1]]))
B19.3.sumStats[[1]] <- cbind(B19.3.sumStats[[1]], OMLabel)







#################################################################################
#OMgridB19.4.List - do the full 288 factorial for comparison (in the B19.3 dir to avoid redoing the first 144)
path=OMRootDir %&% 'OMconditioning\\BET\\'
OMgridB19.4.List <- makeGridB19.4.f(path=path, makeBatFiles=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.4.List <- makeGridB19.4.f(path=path, makeBatFiles=T, makeGrid=F, splitMasterBatch=62)    #check the list of grid elements, but don't create the grid DIR structure
#OMgridB19.4.List <- makeGridB19.4.f(path=path, gridName= 'gridB19.3', makeGrid=T, makeBatFiles=T, splitMasterBatch=T, batFile="projBatNoHessJitterLoop")    #check the list of grid elements, but don't create the grid DIR structure

#make the list into a uniformaly weighted list with OM specs as names
names(OMgridB19.4.List) <- OMgridB19.4.List
OMgridB19.4.List[] <- 1/length(OMgridB19.4.List)
print(OMgridB19.4.List)

save(OMgridB19.4.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.4.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridB19.4.List.RDA")


gridDir <- OMRootDir %&% 'OMconditioning\\BET\\gridB19.3\\'
importGrid.f(gridList=names(OMgridB19.4.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=F)
B19.4 <- importGrid.f(gridList=names(OMgridB19.4.List), gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T, ncolSS=140)


#B19.4 <- importGrid.f(gridList=names(OMgridB19.4.List)[c(144,207)], gridDir=gridDir, convergedNum=3, covar=F, stdOnly=F, doLoad=T, ncolSS=140)



gridB19.4convergedList <- B19.4$loadList
names(gridB19.4convergedList) <- gridB19.4convergedList

################################################################################
#examine convergence characteristics
conv <- as.data.frame(type.convert(out$conStats[,2:6]))
conv$modID <- out$conStats[,'modID']

#plot some summary stuff
B19.4.sumStats <- #cbind(omList,
   plotIndices2.f(doPlots=F, modList=names(gridB19.4convergedList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

OMLabel <- rep("B19.4 - 288 mods", nrow(B19.4.sumStats[[1]]))
B19.4.sumStats[[1]] <- cbind(B19.4.sumStats[[1]], OMLabel)


# check bounds
print("loBounds warnings")
table(unlist(B19.4.sumStats[[2]]))
sum(table(unlist(B19.4.sumStats[[2]])))
print("hiBounds warnings")
table(unlist(B19.4.sumStats[[3]]))
print(B19.4.sumStats[[4]]) #identify specific problem models
# bait boat bound problems again - unimportant fishery, tightened prior somewhat and refit "AgeSel_11P_1_BB1"
# "h80_M06_t10_q0_iC_i3_ess10", "h90_M06_t10_q1_iC_i3_CLRW"
# "h80_M06_t0001_q0_iC_i3_CLRW", "h70_M10_t0001_q1_iH_i1_ess10"
# rm(list=ls()[ls() %in% c("h80_M06_t10_q0_iC_i3_ess10", "h90_M06_t10_q1_iC_i3_CLRW", "h80_M06_t0001_q0_iC_i3_CLRW", "h70_M10_t0001_q1_iH_i1_ess10")])
#BBboundErr <- SS_output(dir = "H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.3\\h80_M06_t10_q0_iC_i3_ess10\\converged3", repfile = "Report.sso", compfile = "CompReport.sso", ncol=140, covar=F, forecast=F)
#SS_plots(BBboundErr, uncertainty=F)


#relative OBJ value within modID
conv$relObj <- conv$objFn - ave(conv$objFn, conv$modID, FUN=min)
hist(conv$relObj, breaks=1000, xlim=c(-1,200))
summary(conv$relObj)

#relationship between final gradient and objFn
plot(log10(conv$maxGrad), conv$relObj, xlab="log10(max. gradient)", ylab="Relative -log(likelihood)")
lines(loess.smooth(log10(conv$maxGrad),conv$relObj),col=2)
#abline(h=200)

plot(log10(conv$maxGrad), conv$relObj, xlab="log10(max. gradient)", ylab="Relative -log(likelihood)", ylim=c(0,240))
lines(loess.smooth(log10(conv$maxGrad),conv$relObj),col=2)

#mean of the CVs calculated within modIDs
mean(ave(conv$MSY, conv$modID, FUN=sd) / ave(conv$MSY, conv$modID, FUN=mean))
#CV of the best fit models among modIDs
sd(conv$MSY[conv$relObj==0]) / mean(conv$MSY[conv$relObj==0])

#mean of the CVs calculated within modIDs
mean(ave(conv$SSBYoSSBMSY, conv$modID, FUN=sd) / ave(conv$SSBYoSSBMSY, conv$modID, FUN=mean))
#CV of the best fit models among modIDs
sd(conv$SSBYoSSBMSY[conv$relObj==0]) / mean(conv$SSBYoSSBMSY[conv$relObj==0])

#relationship between stock status and objFn ?
plot(conv$SSBYoSSBMSY, 4.*conv$MSY, ylab="MSY",xlab="B/B(MSY)", cex=0.5)
points(conv$SSBYoSSBMSY[conv$relObj==0], 4.*conv$MSY[conv$relObj==0], cex=0.5, col=2, pch=16)

# distributional characteristics in case we are to mirror BET uncertainty in YFT:
summary(as.numeric(B19.4.sumStats[[1]][,9:10]))

#summary statistics comparing stock status inference distributions for full grid
#MSY
#> mean(as.numeric(B19.4.sumStats[[1]][,9]))
#[1] 115.4426
#> median(as.numeric(B19.4.sumStats[[1]][,9]))
#[1] 109.2596
#> sd(as.numeric(B19.4.sumStats[[1]][,9]))
#[1] 36.37496
#> sd(log(as.numeric(B19.4.sumStats[[1]][,9])))
#[1] 0.3135557
#> mean(log(as.numeric(B19.4.sumStats[[1]][,9])))
#[1] 4.700271

#B(2015)/BMSY
#mean(as.numeric(B19.4.sumStats[[1]][,10]))
#[1] 1.360386
#median(as.numeric(B19.4.sumStats[[1]][,10]))
#[1] 1.287056
#sd(as.numeric(B19.4.sumStats[[1]][,10]))
# 0.4242073
#sd(log(as.numeric(B19.4.sumStats[[1]][,10])))
# 0.2937479
# mean(log(as.numeric(B19.4.sumStats[[1]][,10])))
# 0.2635585

#correlation between the two
#cor(as.numeric(B19.4.sumStats[[1]][,9]), as.numeric(B19.4.sumStats[[1]][,10]))
#[1] 0.8205585


#marginal_plot = function(x, y, group = NULL, data = NULL, lm_show = FALSE, lm_formula = y ~ x, bw = "nrd0", adjust = 1, alpha = 1, plot_legend = T, ...)
marginal_plot(x=as.numeric(plotDat$B_B.MSY), y=as.numeric(plotDat$MSY), group = plotDat$OMLabel,
   ylab="MSY", xlab="B(2015)/BMSY", bw = "nrd0", adjust = 0.75, alpha = 1, plot_legend = T )










skip aggregate distribution plots for BET if convergence problem is avoided and other filtering not required











outdated below here ...




# unexplained error in this model (network problem during batch run?)
#importGrid.f(gridList="R3I1_h70_M06_t0001_q0_x4_iH_SS_ess10", gridDir=gridDir, covar=F)


#save(OMrefB18.2.432List, file = mainRootDir %&% "\\gitMirror\\Objects\\phase2\\"  %&% 'OMrefB18.2.432List.RDA')
#load(file=file = mainRootDir %&% "\\gitMirror\\Objects\\phase2\\" %&% 'OMrefB18.2.432List.RDA')

#use boxplots and pairwise correlations to plot some key model results and diagnostics, marginalized by grid Options for the whole ensemble
#identify potential convergence problems, parameters on bounds etc
#skipPlots=T might be useful if there are stran
si432 <- #cbind(omList,
   plotIndices2.f(doPlots=F, modList=names(grid18.5stdList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))
   #)


summaryChars <- as.data.frame(unlist(si432[[1]]), stringsAsFactors=F)

si <- lapply(summaryChars[,1:13,drop=FALSE],as.numeric)

si$modID <- summaryChars[[14]]
si$stdExists <- as.logical(summaryChars[[15]])
names(si)


sum(as.numeric(si$max.Grad) < -1 & si$stdExists==T)
sum(as.numeric(si$max.Grad) < -2 & si$stdExists==T)
sum(as.numeric(si$max.Grad) < -3 & si$stdExists==T)
sum(as.numeric(si$max.Grad) < -4 & si$stdExists==T)

# Subset the grid to filter out "failed" models; MSY filter is a temporary proxy for failed .std calcs in which old .std was erroneously copied over before reminimization)
#OMrefB18.2.MG1 <- si$modID[as.numeric(si$max.Grad) < -1 & si$stdExists==T & as.numeric(si$MSY) > 1]

OMrefB18.2.MG1 <- si$modID[as.numeric(si$max.Grad) < -1 & si$stdExists==T]
OMrefB18.2.MG2 <- si$modID[as.numeric(si$max.Grad) < -2 & si$stdExists==T]
OMrefB18.2.MG3 <- si$modID[as.numeric(si$max.Grad) < -3 & si$stdExists==T]
OMrefB18.2.MG4 <- si$modID[as.numeric(si$max.Grad) < -4 & si$stdExists==T]

#make OMs based on the filtered uniform grids
names(OMrefB18.2.MG1) <- OMrefB18.2.MG1
OMrefB18.2.MG1[] <- rep(1/length(OMrefB18.2.MG1), length(OMrefB18.2.MG1))

names(OMrefB18.2.MG2) <- OMrefB18.2.MG2
OMrefB18.2.MG2[] <- rep(1/length(OMrefB18.2.MG2), length(OMrefB18.2.MG2))
OMrefB18.5.254.List <- OMrefB18.2.MG2

names(OMrefB18.2.MG3) <- OMrefB18.2.MG3
OMrefB18.2.MG3[] <- rep(1/length(OMrefB18.2.MG3), length(OMrefB18.2.MG3))

names(OMrefB18.2.MG4) <- OMrefB18.2.MG4
OMrefB18.2.MG4[] <- rep(1/length(OMrefB18.2.MG4), length(OMrefB18.2.MG4))

save(OMrefB18.5List, file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% 'OMrefB18.5List.RDA')
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% 'OMrefB18.5List.RDA')

tmp <- plotIndices2.f(doPlots=F, modList=names(OMrefB18.5.254.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

# Remove two models with outlier poor size compsition fits
tmp[[1]]$CL.fit
poorCLMods <- tmp[[1]][as.numeric(tmp[[1]][,"CL.Fit"])>200,"modList"]  #2 X min() = 120 yields same result
OMrefB18.5.252.List <- OMrefB18.5.254.List[!names(OMrefB18.5.254.List) %in% poorCLMods]
OMrefB18.5.252.List[] <- 1/sum(as.numeric(OMrefB18.5.252.List))

tmp <- plotIndices2.f(doPlots=F, modList=names(OMrefB18.5.252.List), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))


save(OMrefB18.5.252.List,file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB18.5.252.List.RDA")
load(file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMrefB18.5.252.List.RDA")


#grid18.2summary <- ccDat
#save(grid18.2summary, file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT-master\\Objects\\"  %&% 'grid18.2summary.RDA')
#load(file=file = mainRootDir %&% "gitMirror\\phase2\\niMSE-IO-BET-YFT-master\\Objects\\" %&% 'grid18.2summary.RDA')

#omList <- NULL
#for(i in 1:length(OMrefB18.2.432List)){
#    omList <- c(omList, names(OMrefB18.2.432List)[i])
#}





# filter gridB18.2.432 for convergence
#omList <- NULL
#for(i in 1:length(OMrefB18.2.432List)){
#  if(get(names(OMrefB18.2.432List)[i])$maximum_gradient_component < 0.01){
#    print(get(names(OMrefB18.2.432List)[i])$maximum_gradient_component)
#    omList <- c(omList, names(OMrefB18.2.432List)[i])
#  }
#}
#length(omList)
#gridB18.2.304List <- omList
#make an OM based on the expanded uniform grid OMrefB18.2
#OMrefB18.2.304WtList <- rep(1/length(omList), length(omList))
#names(OMrefB18.2.304WtList) <- omList
#save(OMrefB18.2.304WtList,file = rootDir %&% "\\gitMirror\\Objects\\phase2\\"  %&% "OMrefB18.2.304WtList.RDA")
#load(file = file = rootDir %&% "\\gitMirror\\Objects\\phase2\\"  %&% "OMrefB18.2.304WtList.RDA")






# below here is only required to produce the summary graphic.
#gridList <- names(OMrefB18.2.MG1)
gridList <- names(OMrefB18.5)

titleText <- "\nOMrefB18.5"

#use boxplots to plot some key model results and diagnostics, marginalized by grid Options for the whole ensemble
ccDat <- plotIndices2.f(modList=c(gridList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))

print("loBounds warnings")
table(unlist(ccDat[[2]]))
print("hiBounds warnings")
table(unlist(ccDat[[3]]))

#extract a list of  mod names with specific bound failures
bfHIList <- ccDat[[4]]
OMrefB18.2.MG2.BoundFailList <- NULL
for(m in 1:length(bfHIList)){
  #first iteration...missed values unless the dubious vars were first in the list
  #if("AgeSel_11P_1_BB1" %in% unlist(bfHIList[[m]]) | "AgeSel_11P_3_BB1" %in% unlist(bfHIList[[m]]) | "AgeSpline_GradHi_PSFS1_5" %in% unlist(bfHIList[[m]])){
  #second iteration
  if("AgeSel_11P_4_BB1" %in% unlist(bfHIList[[m]]) | "AgeSpline_GradHi_PSFS1_5" %in% unlist(bfHIList[[m]]) | "AgeSpline_GradLo_PSFS1_5" %in% unlist(bfHIList[[m]])){
    OMrefB18.2.MG2.BoundFailList <- c(OMrefB18.2.MG2.BoundFailList, bfHIList[[m]][1])
  }
}
omList <- unlist(OMrefB18.2.MG2.BoundFailList)
names(omList) <- omList
plotIndices2.f(modList=c(omList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))





# identify rnd models with sel upper bounds problems for further investigation...
# AgeSel_11P_3_BB1 - "R3I1_h70_M06_t01_q1_x4_i10C_SS_ess10" ,  -rerun with upper bound change 9 -> 100
# PSFS1_5  "R3I1_h70_M10_t0001_q0_x4_iH_SS_CLRW"  -rerun with upper bound change 0.001 -> 100

# "PSFS5ParMax, PSFS5ParMaxMod"
#  "0.000948873" "R3I1_h80_M10_t0001_q1_x4_i10H_SS_CLRW"


# check implications of altering sel parameter bounds ...
# two models with original par bounds
BBOriginalBounds   <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\R3I1_h70_M06_t01_q1_x4_i10C_SS_ess10", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(BBOriginalBounds, uncertainty=F)
PSFSOriginalBounds <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\R3I1_h70_M10_t0001_q0_x4_iH_SS_CLRW", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(PSFSOriginalBounds, uncertainty=F)
PSFSOriginalBoundsMax <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\R3I1_h80_M10_t0001_q1_x4_i10H_SS_CLRW", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(PSFSOriginalBoundsMax, uncertainty=F)

# same models except violated par bounds greatly expanded
BBBoundcheck       <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\newBoundBB1_R3I1_h70_M06_t01_q1_x4_i10C_SS_ess10", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(BBBoundcheck, uncertainty=F)
PSFSBoundcheck     <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\newBoundPSFS1_R3I1_h70_M10_t0001_q0_x4_iH_SS_CLRW", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(PSFSBoundcheck, uncertainty=F)
PSFSBoundcheckMax  <- SS_output(dir = OMRootDir %&% "OMconditioning\\BET\\gridB18.2\\newBoundPSFS1MaxR3I1_h80_M10_t0001_q1_x4_i10H_SS_CLRW", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(PSFSBoundcheckMax, uncertainty=F)

BB1_original <- BBOriginalBounds
BB1_newBound <- BBBoundcheck
PSFS1_original <- PSFSOriginalBoundsMax
PSFS1_newBound <- PSFSBoundcheckMax

boundCheckList <- c('BB1_original','BB1_newBound','PSFS1_original','PSFS1_newBound')

tmp <- plotIndices2.f(modList=c(boundCheckList), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104),
   doPlots=F)
table(unlist(tmp[[2]]))

#make new unbalanced grid to deal with bounds problems only
##########################################################################################################################################
# grid 18.4 ...grid 18.2 selectivity bounds failures adjusted
#path=OMRootDir %&% 'OMconditioning\\BET\\gridB18.4'
path = "H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB18.4"
OMrefB18.2BoundFailList <- makeGridB18.4.f(subList=OMrefB18.2.MG2.BoundFailList ,path=path, doHess=F, makeGrid=F)    #check the list of grid elements, but don't create the grid DIR structure

print(OMrefB18.2BoundFailList)
#OMrefB18.2BoundFailList <- makeGridB18.4.f(subList=OMrefB18.2.MG2.BoundFailList ,path=path, doHess=T, makeGrid=T)    #check the list of grid elements, but don't create the grid DIR structure




ccDat <- plotIndices2.f(modList=names(OMrefB18.5), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))


# model "R3I1_h70_M06_t0001_q0_x4_iH_SS_ess10" has MSY ==0 ......not sure why log10(max.Grad) = -2.3

#summaryChars <- as.data.frame(unlist(ccDat[[3]]), stringsAsFactors=F)
summaryChars <- unlist(ccDat[[1]])

#remove weird result and rerun it...
#summaryChars <- summaryChars[summaryChars[,1] != "R3I1_h70_M06_t0001_q0_x4_iH_SS_ess10",]
summaryChars <- as.data.frame(summaryChars, stringsAsFactors=F)

si <- lapply(summaryChars[,1:13,drop=FALSE],as.numeric)
si$modID <- summaryChars[[14]]
si$stdExists <- summaryChars[[15]]






#colnames(ccDat)[1] <- 'modID'
# drop failed convergence
#cDat <- ccDat[as.numeric(ccDat[,'max.Grad']) < -2, ]

#samp2 <- as.matrix(si)


N <- length(gridList)

#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
layout(matrix(c(1,4,2,4,3,5,6,6), 4, 2, byrow = TRUE))
#par(mar = c(3,4,2,1))
par(mar = c(4,4,4,4))

#pDat <- as.numeric(samp2[,'MSY'])
pDat <- si$MSY
pDat[pDat>299] <- 299
#pDat[pDat<201] <- 201

#h <- hist(pDat, breaks = c(200:800),
#h <- hist(pDat, breaks = c(20:80)*10,
h <- hist(pDat, breaks = c(0:40)*10,
  main="MSY Distribution \n OM Ensemble; nMods = " %&% length(unique(si$modID)),
  xlab="MSY", probability=T)
#lines(200:800, dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)/sum(dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)), col=3)
points(c(refMSY), c(0), col=2, pch=15)
summary(si$MSY)

#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:300)/100,
#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:30)/10,
h <- hist(si$B_B.MSY, #breaks = c(0:40)/20,
  main="SSB(2016)/SSB(MSY) Distribution \n OM Ensemble; nMods = " %&% length(unique(si$modID)),
  xlab="B(2016)/B(MSY)", probability=T)
points(c(refB_B.MSY), c(0), col=2, pch=15)
summary(as.numeric(si$B_B.MSY))

h <- hist(as.numeric(si$B_B0), breaks = c(0:40)/40,
  main="SSB(2016)/SSB0 Distribution \n OM Ensemble; nMods = " %&% length(unique(si$modID)),
  xlab="B(2016)/B0", probability=T)
points(c(refB_B0), c(0), col=2, pch=15)
summary(si$B_B0)

X <-cbind(si$MSY,si$B_B.MSY)
print(summary(X))
print(cor(X[,1],X[,2]))
plot(X, xlab="MSY", ylab="SSB/SSBMSY", pch=19, cex=0.5, main="Bivariate Sample Plot")
points(jitter(X[,1], amount=mean(X[,1])/50),jitter(X[,2], amount=mean(X[,2])/50), pch=19, cex=0.01, col='grey')

#plot(table(samp2[,'modID']), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')
plot(sort(table(si$modID)/sum(table(si$modID))), main="Proportion of each model in OM ensemble", xlab="", ylab='Proportion', xaxt='n')
#plot(cumsum(sort(table(samp2[,'modID']))), main="Frequency of individual models in OM ensemble", xlab="", ylab='Frequency', xaxt='n')

gridOptions <- unlist(c(strsplit(gridList, "_")))
sampledOptions <- unlist(c(strsplit(si$modID, "_")))
plot(table(sampledOptions)/sum(table(sampledOptions)),type='p', col=3, pch=15, main = "Grid option frequency before and after filtering", ylab="Proportion")
lines(table(gridOptions)/sum(table(gridOptions)))

title(titleText, outer=TRUE)











plotIndices2.f(modList=as.character(samp2[,1]), mfrowLayout = c(4,2), MSYyLim=c(0,300), mFile = F,  SPB_Yr="SPB_356",
   inputRefLines=F, #override list below which are extracted from reports etc
   refSSB0=c(1,1,1),refSSBY=c(1,1,1), refSSBMSY=c(525,364,718), refSSBYoSSBMSY=c(1.29,1.07,1.51), refSSBYoSSB0=c(0.38,0.38,0.38),refMSY=c(104))















# CVs inferred from the BET assessment (grid)
refMSYcv = 0.127
refBYoBMSYcv = 0.136

###############################################################################################################################
# OM-refB18.3
# Bivariate sample of OMrefB18.2grid on basis of MSY and B/BMSY consistency with assessment and assumed variance

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

    FSOpt1 <- c('t0001', 'q0',0.1667),
    FSOpt2 <- c('t0001', 'q1',0.1667),
    FSOpt1 <- c('t01', 'q0',0.1667),
    FSOpt2 <- c('t01', 'q1',0.1667),
    FSOpt3 <- c('t10', 'q0',0.1667),
    FSOpt4 <- c('t10', 'q1',0.1667)),
  mu    = c(104,1.29),  #distribution means to aim for
  #mu    = 0.89,  # B/BMSY mean to attain (roughly)
  sigma = c(refMSYcv,refBYoBMSYcv),  #lognormal sampling sigma (~CV)
  sigmaTrunc = 3, #truncate sample distribution at this many sigma
  nBins = 4, #number of bins covering distribution is a grid of nBins X nBins
  logNorm=T)


OMrefB18.3WtList <- filtered[[2]]
details <- filtered[[1]]
details[,1] <- as.factor(details[,1])
samp2 <- details[sample(details[,1], size=2000, replace=T, prob=details[,'wtModList']),]
samp2[,1] <- as.character(samp2[,1])

titleText <- "\n OM-refB18.3"
#titleText <- "\n Alternate OM with sampling CVs 2X SA-ref"

N <- nrow(samp2)

#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
layout(matrix(c(1,4,2,4,3,5,6,6), 4, 2, byrow = TRUE))
#par(mar = c(3,4,2,1))
par(mar = c(4,4,4,4))

pDat <- as.numeric(samp2[,'MSY'])
pDat[pDat>299] <- 299
#pDat[pDat<201] <- 201

#h <- hist(pDat, breaks = c(200:800),
#h <- hist(pDat, breaks = c(20:80)*10,
h <- hist(pDat, breaks = c(0:40)*10,
  main="MSY Distribution \n OM Ensemble; nMods = " %&% length(unique(samp2[,'modID'])),
  xlab="MSY", probability=T)
#lines(200:800, dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)/sum(dnorm(log(200:800), refB_B.MSY, sd=sigmaB_B.MSY)), col=3)
points(c(refMSY), c(0), col=2, pch=15)
summary(as.numeric(samp2[,'MSY']))

#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:300)/100,
#h <- hist(as.numeric(samp2[,'B_B.MSY']), breaks = c(0:30)/10,
h <- hist(as.numeric(samp2[,'B_B.MSY']), #breaks = c(0:40)/20,
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

#plotIndices2.f(modList=as.character(samp2[,1]), mfrowLayout = c(4,2), MSYyLim=c(0,700))


#need to ensure that the "correct", i.e. assessment-preferred CPUE is the one used for the MP
#this is achieved by making sure this series is the first model in the list... (iH and q0)
tmpList <- OMrefB18.3WtList
i <- 1
while(!prod((c("iH","q0") %in% unlist(strsplit(names(tmpList[i]), split="_"))))) {
  i <- i+1
  if(i>length(tmpList)){print("oops - model list does not include the preferred CPUE series..."); break}
}
if(i>1){
  tmp <- tmpList[1]
  tmpList[1] <- tmpList[i]
  tmpList[i] <- tmp
  names(tmpList) <- names(tmpList[c(i,1:(i-1),(i+1):length(tmpList))])
}
OMrefB18.3WtList <- tmpList

#save just the list of  weights (and model names as names) into the DIR with the models in it
gridDir <- rootDir %&% 'OMconditioning//BET//gridB18.2//'
save(OMrefB18.3WtList,file = gridDir %&% "OMrefB18.3WtList.RDA")
load(file = gridDir %&% "OMrefB18.3WtList.RDA")






















