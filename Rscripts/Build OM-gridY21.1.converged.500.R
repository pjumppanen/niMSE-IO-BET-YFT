# ============================================================================================================================================
# === Operating Model definition object 'MseDef' ============================================================================
# updated from previous iteration:
#    - 2021 4 area test grid based on 2018 assessment provided by Dan Fu
#    - with new elements derived from Uritberea et al 2019, 2020 assessment explorations
#    -first MP year = 2022
#    -This is a smaple from a 52 model converged subset of a 54 model fractional grid only removing models that failed to converge 

# ============================================================================================================================================
# Exploraory YFT OM definition prepared for the 2021 Task Force Mtg

# --- Create a blank OM definition object --------------------------------------
MseDef<-new('MseDefinition')

# --- Description --------------------------------------------------------------
MseDef@Name          <- "OMrefY21.1.500"
MseDef@Label         <- "Y21.1" #useful for changing graphics labels
MseDef@Date          <- "Dec2020"
MseDef@Author        <- "D.Kolody"
MseDef@Notes         <- "500 model TEST YFT OM from OMgridY21.1, with convergence filtering only"
MseDef@PrimarySource <- "main effects fractional design - minimal filtering"
MseDef@CppMethod     <- 0 # 1 = Use C++ Baranov solution, 0 = Use R based Popes approximation
MseDef@UseCluster    <- 0 # 1 = Use cluster of R processes, 0 = Use single R process


# --- Specifications -----------------------------------------------------------
MseDef@npop          <- as.integer(1) #multiple stocks not supported at this time
MseDef@nfleets       <- as.integer(25) #number of fisheries; needs to be consistent with SS input files
#MseDef@SSRootDir     <- "M:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase1\\OMconditioning\\YFT\\gridY19.2\\"          # Root dir for SS results outputs
MseDef@SSRootDir     <- "X:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY21.1\\"          # Root dir for SS results outputs
MseDef@SBlim         <- 0.4
MseDef@Flim          <- 1.4


#mainRootDir <- "C:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path
mainRootDir <- "C:\\Users\\kol018\\OneDrive - CSIRO\\IOTC-postCovid\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path

load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY21.1.List.Converged.RDA")
#load("Objects\\OMgridY20.1.List.Converged.RDA")

#theseMods <- c(46:52)
#    xxx theseMods and 20 for testing only
# find faulty model
#names(OMgridY21.1.List)=="h80_M08_t01_q1_i1_gr3_x8"
#45


MseDef@OMList       <- as.list(names(OMgridY21.1.List.Converged)) #[theseMods]
MseDef@modelWeight  <- as.karray(OMgridY21.1.List.Converged) #[theseMods]  #= "karray",    # Plausibility weighting for given OMFile
MseDef@totalSims        <- 500 #20                 # = "numeric",   # Total number of simulations in MSE run.
#nSimPerOMFile is ignored if totalSims is defined (modelWt requried either way)
# this option might not be supported anymore?
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
MseDef@firstMPYr      <- as.integer(2022)   #MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
MseDef@MPDataLag      <- as.integer(2)      # The lag in number of years between assessment data availability and the timing of the assessment/HCR calculation; +1 means data is one year behind
MseDef@catchBridge    <- as.karray(c(440834, 427239))    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
# according to 2020 WPTT report best scientific total catch 2017 = 421,825; 2018 = 440,834; 2019 = 427,23
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
MseDef@Icv    <-c(-0.2, -0.2001)  # annual CV of 0.2 if quarterly series of 4 independent series are averaged; negative is max(0.2, model-specific RMSE)
MseDef@IACin   <- 0.5           # cpue autocorrelation

MseDef@IACin      <- 0.5              # this is annualized (quarterly series are not calculated)
MseDef@cpueMP_File <- "Objects\\yftMPcpue2018.dat" # i.e. MP cpue, not  OM cpue
MseDef@cpueMP_NormYrs <- c(1972:2020)
#MseDef@nbackupyears   <- 3

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
