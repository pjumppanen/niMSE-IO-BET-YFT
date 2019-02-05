# ============================================================================================================================================
# === Operating Model definition object 'OMd' ============================================================================
# ============================================================================================================================================
# OMrefB19.1.288 - a 288 model BET OM prepared for the 2019 MSE task force
# variability in selectivity not active
# implementation error not active
# bridging catch for 2016 implemented

# --- Create a blank OM definition object --------------------------------------
MseDef <- new('MseDefinition')

# --- Description --------------------------------------------------------------
MseDef@Name          <- "OMrefB19.1.288"
MseDef@Label         <- "OMrefB19.1.288" #useful for changing graphics labels
MseDef@Date          <- "Jan2019"
MseDef@Author        <- "D.Kolody"
MseDef@Notes         <- "BET reference case OM from OMrefB19.1, fractional factorial screening grid of 144 models "
MseDef@PrimarySource <- "uniform (expanded dimensions, expanded par bounds & filtered) grid derived from 2016 BET assessment"
MseDef@CppMethod     <- 0 # 1 = Use C++ Baranov solution, 0 = Use R based Pope-like approximation
MseDef@UseCluster    <- 0 # 1 = Use cluster of R processes, 0 = Use single R process


# --- Specifications -----------------------------------------------------------
MseDef@npop          <- as.integer(1) #multiple stocks not supported at this time
MseDef@nfleets       <- as.integer(15) #number of fisheries; needs to be consistent with SS input files
MseDef@SSRootDir     <- "H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB19.1\\"          # Root dir for SS results outputs
#MseDef@SSRootDir     <- "M:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridB18.2\\"          # Root dir for SS results outputs
#MseDef@SSRootDir     <- "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\gridY18.5\\"          # Root dir for SS results outputs
MseDef@SBlim         <- 0.5
MseDef@Flim          <- 1.3


load("objects\\OMgridB19.1.List.RDA")       #the weighted list of BET models
MseDef@OMList       <- as.list(names(OMgridB19.1.List))

#MseDef@OMList       <- as.list(names(OMrefB18.2List))
#MseDef@modelWeight  <- as.karray(OMrefB18.0List)  #= "karray",    # Plausibility weighting for given OMFile
#MseDef@totalSims    <- 50                 # = "numeric",   # Total number of simulations in MSE run.
# nSimPerOMFile should now be automatically oweritten based on totalSims and modelWeight (?)
MseDef@nsimPerOMFile <- array(rep(2,length(MseDef@OMList)),dim=length(MseDef@OMList))   # Number of simulations per each SS specification file (vector of length OMList allows differental weighting, i.e. c(10,50,25)


MseDef@proyears      <- as.integer(30)              # Number projection years
MseDef@targpop       <- as.integer(1)               # summary stats by population; irrelevant for single stock case
MseDef@seed          <- as.integer(1)               # rnd seed
MseDef@recentPerFirst<- as.integer(4)               # number of most recent seasons to include in "recent" C and E definition counting backward (0 means use last season of assessment)
MseDef@recentPerLast <- as.integer(19)               # number of most recent seasons to include in "recent" C and E definition counting backward
MseDef@seasonCEDist  <- as.integer(0)               # 0/1 - 1=use seasonal pattern of C/E (recentPeriod must be multiple of recentPeriod) or all easons equal

#some assessment specific time mapping requirements to align SS years as quarters with OM year-seasons and real-time
MseDef@nsubyears      <- as.integer(4)
MseDef@lastSeas       <- as.integer(4)      # just in case whole years not used
MseDef@firstSeas      <- as.integer(1)      # just in case whole years not used
MseDef@firstSSYr      <- as.integer(101)    #101     # SS equivalent of firstYrToPlot
MseDef@firstCalendarYr<- as.integer(1952)   # used to convert to actual years
MseDef@lastCalendarYr <- as.integer(2015)   # used to convert to actual years
MseDef@firstMPYr      <- as.integer(2021)   #MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
#MseDef@firstMPYr      <- as.integer(2020)   #MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
MseDef@MPDataLag      <- as.integer(3)      # The lag in number of years between assessment data availability and the timing of the assessment/HCR calculation; +1 means data is one year behind
MseDef@catchBridge    <- as.karray(c(86589, 90500)) # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)  87K = value in 2016 WPTT report
#MseDef@catchBridge    <- as.karray(c(100000.))    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
#MseDef@catchBridge    <- as.karray(c(-999))    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
MseDef@catchBridgeCV  <- 0.01                #error to add onto catch for bridge years with unknown catch

# fleet numbers to include in size comp sampling for MPs, i.e. probably the same as the survey (CPUE) fleets
MseDef@indexFisheries   <- as.integer(c(2,3,4,13))       # LL CPUE fleet areas ... xxx need to check that OM will not go strange given same popn area indexed by two fisheries

#These are dimmed in the OM
MseDef@ReccvTin <-array(rep(0.6,MseDef@npop), dim=MseDef@npop)       # Temporal variability in recruitment aggregate over regions
MseDef@ReccvRin <- 0.                                          # Spatial variability in recruitment for all pops, areas, sims (i.e. streamlined implemented, because its probably a low priority)
MseDef@RecACTin <-array(rep(0.5,MseDef@npop), dim=MseDef@npop)       # Recruitment autocorrelation (for regional aggregate)
MseDef@NInitCV      <- 0.6
MseDef@NInitCVdecay <- 0.1
MseDef@selExpRange  <- 0.     #0.6  0         # sel temporal variability exponent - oscillates with a sin wave rangeing between +/- exp(selExpRange)
MseDef@selAgeRange  <- 0.      #3.   0        # 0=no age shift, 2 means (discretized) sine wave shift of sel vector between - 2 and + 2 age class
MseDef@selWLRange   <- array(c(0.0625,0.5))   # sel temporal variability wavelength range (0.0625 = quarter wavelength in 25 years, 0.5=2 full cycles in 25 years

#Observation errors (original ABT code had a separate observation class)
MseDef@TACEcv <-array(rep(0.001,MseDef@nfleets)) # fleet-specific lognormal errors on TAC/TAE (independent among fleets and seasons)
MseDef@Ccv    <-c(0., 0.0001)
#MseDef@Icv    <-c(0.2, 0.2001)  #c(0., 0.0001) # c(0.3, 0.3001)
MseDef@Icv    <-c(0.46, 0.46001)  #corresponds to annual CV of 0.2 if quarterly series of 4 independent series are averaged
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
#MseDef@IACin      <- 0.5              # cpue autocorrelation
MseDef@IACin      <- 0.76              # series-specific AC corresponds to annual aggregate AC = 0.5
MseDef@ITrendin   <- -1               # cpue trend % per annum compounded, negative means the trend is extracted from the assessment model filname, i.e. q1 = 1%

#tuning criteria
#MseDef@tunePM           <- "SBoSBMSYmean"  #"GKmean" = P(Kobe Kobe)
#MseDef@tunePMProjPeriod <- 1001 #1001 = 2019:2039  1002=2024
#MseDef@tunePMTarget     <- 1.0  # 0.75 for P(Green)
#MseDef@tuneTol          <- 0.01



# --- save object --------------------------------------------------------------
#save(MseDef,file=paste(getwd(),"/Objects/OMd.OMrefB19.1.288.RDA",sep=""))


# ==========================================================================================================================
# End of build script ========================================================================================================
# ==========================================================================================================================
