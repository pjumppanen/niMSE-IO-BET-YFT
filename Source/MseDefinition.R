# -----------------------------------------------------------------------------
# MseDefinition class
# -----------------------------------------------------------------------------
setClass("MseDefinition",
  slots = c(
    # Description
    Name             = "character",
    Label            = "character",
    Date             = "character",
    Author           = "character",
    Notes            = "character",
    PrimarySource    = "character",
    npop             = "integer",
    proyears         = "integer",
    targpop          = "integer",
    seed             = "integer",
    recentPerFirst   = "integer",
    recentPerLast    = "integer",
    seasonCEDist     = "integer",
    SSRootDir        = "character",
    SBlim            = "numeric",
    Flim             = "numeric",
    OMList           = "list",
    nsimPerOMFile    = "karray",
    modelWeight      = "karray",    # Plausibility weighting for given OMFile
    totalSims        = "numeric",   # Total number of simulations in MSE run.
    CppMethod        = "numeric",
    UseCluster       = "numeric",

    nfleets          = "integer",
    ImplErrBias      = "karray",    # Vector proyears in length. If +ve it is a multiplier on TAC. If -ve it is a multiplier on fishing mortality prior to the
                                    # -ve value coming in. This is used to add implementation error bias.
    RecScale         = "karray",    # Scaling parameter on recuitment. Either a single value or a vector of length proyears
    ReccvTin         = "karray",
    RecACTin         = "karray",
    ReccvRin         = "numeric",
    NInitCV          = "numeric",   # additional noise on initial N(a) (CV on age 1)
    NInitCVdecay     = "numeric",   # exponential decay on CV on initial N(a) = exp(NinitCV*(a-1))
    selExpRange      = "numeric",   # sel temporal variability exponent oscillates with a sin wave ranging between these values
    selAgeRange      = "numeric",   # 0=no age shift, 2 means (discretized) sine wave shift of sel vector between - 2 and + 2 age class
    selWLRange       = "karray",    # sel temporal variability wavelength range (0.0625 = quarter wavelength in 25 years, 0.5=2 full cycles in 25 years

    nsubyears        = "integer",
    firstCalendarYr  = "integer",
    lastCalendarYr   = "integer",
    lastSeas         = "integer",
    firstSeas        = "integer",
    firstSSYr        = "integer",
    firstMPYr        = "integer",   # MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
    MPDataLag        = "integer",   # MP kicks in here (projections in the intervening period may require an observed catch input to handle data lags)
    catchBridge      = "karray",    # known catch history between last assessment year in OM and firstMPYr (length of min 0 to max firstMPYr-lastCalendarYr-1)
    catchBridgeCV    = "numeric",   # error to add onto catch for bridge years with unknown catch

    indexFisheries   = "integer",   # vector of fisheries to aggregate for index calculations (e.g. LL CPUEfleets)
                                    # Observation errors (original ABT code had a separate observation class)
    TACEcv           = "karray",    # fleet-specific lognormal errors on TAC/TAE (independent among fleets and seasons)
    Ccv              = "numeric",   # Observation error
    Cbcv             = "numeric",   # bias in total annual catches
    nCAAobs          = "numeric",   # Number of annual catch at age (CAA) observations
    nCALobs          = "numeric",   # Number of annual catch-at-length (CAL) observations
    Lcv              = "numeric",
    Ibeta            = "numeric",   # Hyperstability parameter I^beta
    Icv              = "numeric",   # Observation error in relative abundance indices
    IACin            = "numeric",   # cpue index autorrelation error
    ITrendin         = "numeric",   # cpue index catchability trend (for observation error only, this parameter does not cause increases in F)
    Mbcv             = "numeric",   # Bias in observation of natural mortality rate
    Kbcv             = "numeric",   # Bias in estimation of growth parameters
    t0bcv            = "numeric",
    Linfbcv          = "numeric",
    LFCbcv           = "numeric",   # Bias in observation of length at first capture (LFC)
    LFSbcv           = "numeric",   # Bias in observation of length at full selection (LFS)
    FMSYbcv          = "numeric",   # Bias in observaton of FMSY
    FMSY_Mbcv        = "numeric",   # Bias in ratio of FMSY/M
    BMSY_B0bcv       = "numeric",   # Bias ratio of BMSY/B0
    ageMbcv          = "numeric",   # Bias in observation of age at 50% maturity and
    Dbcv             = "numeric",   # Bias in observation of current stock depletion
    Dcv              = "numeric",   # Imprecision in observation of current stock depletion
    Btbcv            = "numeric",   # Bias in observation of current stock biomass
    Btcv             = "numeric",   # Imprecision in observation of current stock biomass
    Ftbcv            = "numeric",   # Bias in observation of current fishing mortality rate
    Ftcv             = "numeric",   # Imprecision in observation of current fishing mortality rate
    hbcv             = "numeric",   # Bias in observation of steepness
    Recbcv           = "numeric",   # Bias in observation of recent recrutiment
    IMSYbcv          = "numeric",   # Bias in observation of target CPUE (CPUE @ MSY)
    MSYbcv           = "numeric",   # Bias in observation of target catch (MSY)
    BMSYbcv          = "numeric"    # Bias in observation of target biomass (BMSY)
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "MseDefinition",
  function(.Object)
  {
    .Object@ImplErrBias   = karray(c(NA))
    .Object@RecScale      = karray(c(1))
    .Object@modelWeight   = karray(c(1))
    .Object@totalSims     = 0 # default of 0 means simulations are based only on nsimPerOMFile.
                              # non 0 totalSims and the nsimPerOMFile is calculated based on modelWeight and totalSims.
    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("createMseFramework", function(.Object, ...) standardGeneric("createMseFramework"))

setMethod("createMseFramework", "MseDefinition",
  function(.Object, Report=FALSE, UseCluster=NA, UseMSYss=0)
  {
    if ((.Object@firstMPYr - .Object@lastCalendarYr) < 2)
    {
      print("ERROR: firstMPYr - firstCalendarYr must be >= 2")
      stop()
    }

    return (new("MseFramework", .Object, Report, UseCluster, UseMSYss))
  }
)
