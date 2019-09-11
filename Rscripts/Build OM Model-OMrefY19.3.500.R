# ============================================================================================================================================
# === Operating Model definition object 'MseDef' ============================================================================
# updated from previous iteration:
#    - new grid based on 2018 assessment provided by Dan Fu
#    -CPUE (rec?) CVs and autocorrelation - match BET
#    -bridging catch values
#    -first MP year = 2021
#    -This is a 49 model subset of a 144 model fractional grid removing models that failed to converge 3 times or had a catchLLH > 1E-5

# ============================================================================================================================================
# YFT OM definition prepared for the June 2019 TCMP

# --- Create a blank OM definition object --------------------------------------
MseDef<-new('MseDefinition')

# --- Description --------------------------------------------------------------
MseDef@Name          <- "yftY19.3.500"
MseDef@Label         <- "yftY19.3.500" #useful for changing graphics labels
MseDef@Date          <- "Apr2019"
MseDef@Author        <- "D.Kolody"
MseDef@Notes         <- "49 model YFT reference OMgridY19.3, with catchLLH and repeat convergence filtering"
MseDef@PrimarySource <- "OM spec following 2019 IOTC MSE task force meeting."
MseDef@CppMethod     <- 0 # 1 = Use C++ Baranov solution, 0 = Use R based Popes approximation
MseDef@UseCluster    <- 0 # 1 = Use cluster of R processes, 0 = Use single R process


# --- Specifications -----------------------------------------------------------
MseDef@npop          <- as.integer(1) #multiple stocks not supported at this time
MseDef@nfleets       <- as.integer(25) #number of fisheries; needs to be consistent with SS input files
#MseDef@SSRootDir     <- "M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase1\\OMconditioning\\YFT\\gridY19.2\\"          # Root dir for SS results outputs
MseDef@SSRootDir     <- "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\"          # Root dir for SS results outputs
MseDef@SBlim         <- 0.4
MseDef@Flim          <- 1.4

# Mix of models from 2 pooled, unbalanced grids
# source("..\\OMconditioning\\RStuff\\phase2\\makeGridY17.2.f.R")  # only req'd if lists not aready saved
load("objects\\OMrefY19.3.List.RDA")       #the weighted list of models


# not sure why some models failed to produce wtatage file, did on retest - maybe network problem?
#readline(prompt=" temporary hack removes OMs with missing (or doubled) output files:  Press [enter] to continue")
#fileFail <- NULL
#for (i in 1:length(OMrefY19.2WtList)){
#  if(file.info(tmpDir %&% "\\wtatage.ss_new")$size < 10) fileFail <- c(fileFail, names(OMrefY19.2WtList[i]))
#  if(file.info(tmpDir %&% "\\CumReport.sso")$size < 10 | file.info(tmpDir %&% "\\CumReport.sso")$size >90000) 
#      fileFail <- c(fileFail, names(OMrefY19.2WtList[i]))
#  if(file.info(tmpDir %&% "\\data.ss_new")$size < 2.2e+6 | file.info(tmpDir %&% "\\data.ss_new"  )$size > 3.0e+6 ) fileFail <- c(fileFail, names(OMrefY19.2WtList[i]))
#  
#  print(i)
#}
#OMrefY19.2WtList <- OMrefY19.2WtList[!(names(OMrefY19.2WtList) %in% fileFail)]

# screen the SS output folders for missing files or strange file sizes (e.g. 0 or double  the expected, presumably due to repeat batch appending)
#readline(prompt=" temporary hack removes OMs with missing (or doubled) output files:  Press [enter] to continue")
#okFileSize <- NULL
#tmpDir <- MseDef@SSRootDir %&% names(OMrefY19.2WtList[1]) %&% "\\"
#okFileList <- list.files(tmpDir)
#okFileList <- okFileList[!(okFileList %in% c("warning.sso", "ss3.24Z.log", "projBat.bat", "projBatnoHess.bat"))]
#for (j in okFileList){
#  okFileSize <- c(okFileSize, file.info(tmpDir  %&% j)$size)
#}
#problemList <- NULL
#for (i in c(2:length(OMrefY19.2WtList))){
#  tmpDir <- MseDef@SSRootDir %&% names(OMrefY19.2WtList[i]) %&% "\\"
#  print(i)
#
#  for (j in 1:length(okFileList)){
#    newFileSize <- file.info(tmpDir %&% okFileList[j])$size
#    if(is.na(newFileSize)){ #missing file
#      print(c("file sizes: ", okFileSize[j], newFileSize))
#      problemList <- c(problemList, names(OMrefY19.2WtList[i]), okFileList[j])
#    }
#    else
#    { #file size deviates by >10% of the one that works (not necesssarily an error, but worth checking)
#      if(okFileSize[j]>0){
#        if(abs((newFileSize-okFileSize[j])/okFileSize[j])>0.1){
#          print(c("file sizes: ", okFileSize[j], newFileSize))
#          problemList <- c(problemList, names(OMrefY19.2WtList[i]), okFileList[j])
#        }
#      }
#    }
#  }
#}
#problemList <- c(problemList,"R4MvEst_h90_sr4_M08_t10_q0_x3_iH_SS_ess5")
#OMrefY19.2WtList <- OMrefY19.2WtList[!(names(OMrefY19.2WtList) %in% problemList)]
#OMrefY19.2WtList <- rev(OMrefY19.2WtList)





MseDef@OMList       <- as.list(names(OMrefY19.3.List))
MseDef@modelWeight  <- as.karray(OMrefY19.3.List)  #= "karray",    # Plausibility weighting for given OMFile
MseDef@totalSims        <- 500                 # = "numeric",   # Total number of simulations in MSE run.
# nSimPerOMFile is ignored if totalSims is defined (modelWt requried either way)
MseDef@nsimPerOMFile <- array(rep(1,length(MseDef@OMList)),dim=length(MseDef@OMList))   # Number of simulations per each SS specification file (vector of length OMList allows differental weighting, i.e. c(10,50,25)


MseDef@proyears      <- as.integer(26)              # Number projection years
MseDef@targpop       <- as.integer(1)               # summary stats by population; irrelevant for single stock case
MseDef@seed          <- as.integer(1)               # rnd seed
MseDef@recentPerFirst<- as.integer(0)               # number of most recent seasons to include in "recent" C and E definition counting backward (0 means use last season of assessment)
MseDef@recentPerLast <- as.integer(8)               # number of most recent seasons to include in "recent" C and E definition counting backward
MseDef@seasonCEDist  <- as.integer(0)               # 0/1 - 1=use seasonal pattern of C/E (recentPeriod must be multiple of recentPeriod) or all easons equal

#some assessment specific time mapping requirements to align SS years as quarters with OM year-seasons and real-time
MseDef@nsubyears      <- as.integer(4)
MseDef@lastSeas       <- as.integer(4)      # just in case whole years not used
MseDef@firstSeas      <- as.integer(1)      # just in case whole years not used
MseDef@firstSSYr      <- as.integer(13)    #101     # SS equivalent of firstYrToPlot
MseDef@firstCalendarYr<- as.integer(1950)   # used to convert to actual years
MseDef@lastCalendarYr <- as.integer(2017)   # used to convert to actual years
MseDef@firstMPYr      <- as.integer(2021)   #MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
#MseDef@firstMPYr      <- as.integer(2020)   #MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
MseDef@MPDataLag      <- as.integer(3)      # The lag in number of years between assessment data availability and the timing of the assessment/HCR calculation; +1 means data is one year behind
MseDef@catchBridge    <- as.karray(c(409000))  #2017 catch = 409K  # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
#MseDef@catchBridge    <- as.karray(c(100000.))    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
#MseDef@catchBridge    <- as.karray(c(-999))    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
MseDef@catchBridgeCV  <- 0.001                #error to add onto catch for bridge years with unknown catch

# fleet numbers to include in size comp sampling for MPs, i.e. probably the same as the survey (CPUE) fleets
MseDef@indexFisheries   <- as.integer(c(7,10,11,13))       # LL CPUE fleets

#These are dimmed in the OM
MseDef@ReccvTin <-array(rep(0.42,MseDef@npop), dim=MseDef@npop)       # (annualized now) Temporal variability in recruitment aggregate over regions
MseDef@ReccvRin <- 0.                                          # Spatial variability in recruitment for all pops, areas, sims (i.e. streamlined implemented, because its probably a low priority)
MseDef@RecACTin <-array(rep(0.21,MseDef@npop), dim=MseDef@npop)       # Recruitment autocorrelation (for regional aggregate)
MseDef@NInitCV      <- 0.6
MseDef@NInitCVdecay <- 0.1
MseDef@selExpRange  <- 0.     #0.6  0           # sel temporal variability exponent - oscillates with a sin wave rangeing between +/- exp(selExpRange)
MseDef@selAgeRange  <- 0.      #3.   0           # 0=no age shift, 2 means (discretized) sine wave shift of sel vector between - 2 and + 2 age class
MseDef@selWLRange   <- array(c(0.0625,0.5))   # sel temporal variability wavelength range (0.0625 = quarter wavelength in 25 years, 0.5=2 full cycles in 25 years

#Observation errors (original ABT code had a separate observation class)
MseDef@TACEcv <-array(rep(0.001,MseDef@nfleets)) # fleet-specific lognormal errors on TAC/TAE (independent among fleets and seasons)
MseDef@Ccv    <-c(0., 0.0001)
MseDef@Icv    <-c(0.2, 0.2001)  # annual CV of 0.2 if quarterly series of 4 independent series are averaged
MseDef@IACin   <- 0.5           # cpue autocorrelation

#MseDef@Icv    <-c(0.3, 0.3001)  #c(0., 0.0001) # c(0.3, 0.3001)
MseDef@Dcv    <-c(0., 0.0001)
MseDef@Btcv   <-c(0., 0.0001)
MseDef@Ftcv   <-c(0., 0.0001)
MseDef@Cbcv   <- 0.
MseDef@Mbcv   <- 0.
MseDef@LFCbcv <- 0.
MseDef@LFSbcv <- 0.
MseDef@ageMbcv<- 0.
MseDef@Ftbcv  <- 0.
MseDef@Recbcv <- 0.
MseDef@IMSYbcv<- 0.
MseDef@MSYbcv <- 0.
MseDef@BMSYbcv<- 0.
MseDef@hbcv   <- 0.
MseDef@Btbcv  <- 0.
MseDef@Dbcv   <- 0.
MseDef@Kbcv       <- 0.0001
MseDef@t0bcv      <- 0.0001
MseDef@Linfbcv    <- 0.0001
MseDef@FMSYbcv    <- 0.
MseDef@FMSY_Mbcv  <- 0.
MseDef@BMSY_B0bcv <- 0.
MseDef@nCAAobs    <- c(1000,1001)
MseDef@nCALobs    <- 100
MseDef@Lcv        <- c(0., 0.)
MseDef@Ibeta      <- c(0.999, 1.0001) # hyperstability parm, cv # c(0.66,1.5) #exp(runif(nsim,log(0.75),log(1.25))) #check definition
MseDef@ITrendin   <- -1               # cpue trend % per annum compounded, negative means the trend is extracted from the assessment model filname, i.e. q1 = 1%

#tuning criteria - moved to separate object in phase 2
#MseDef@tunePM           <- "SBoSBMSY0.5"  #"GKmean" = P(Kobe Kobe)
#MseDef@tunePMProjPeriod <- 1001 #1001 = 2019:2039  1002=2024
#MseDef@tunePMTarget     <- 1.0  # 0.75 for P(Green)
#MseDef@tuneTol          <- 0.01



# --- save object --------------------------------------------------------------
#save(MseDef,file=paste(getwd(),"/Objects/MseDef.YFT18.1.250.RDA",sep=""))


# ==========================================================================================================================
# End of build script ========================================================================================================
# ==========================================================================================================================
