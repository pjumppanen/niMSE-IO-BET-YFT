# -----------------------------------------------------------------------------
# StockSynthesisModelData class
# -----------------------------------------------------------------------------
setClass("StockSynthesisModelData",
  slots = c(
    # dimension variables
    nsim                  = "integer",
    nyears                = "integer",
    nsubyears             = "integer",
    proyears              = "integer",
    nages                 = "integer",
    npop                  = "integer",
    nareas                = "integer",
    nfleets               = "integer",
    nCPUE                 = "integer",
    UseMSYss              = "integer",
    nbackupyears          = "integer",
    which                 = "integer",

    SSB0ss                = "numeric",
    B0ss                  = "numeric",
    MSYss                 = "numeric",
    BMSYss                = "numeric",
    SSBMSYss              = "numeric",
    FMSYss                = "numeric",
    SRrel                 = "integer",
    M                     = "karray",
    h                     = "karray",
    ReccvT                = "karray",
    ReccvR                = "karray",
    Recsubyr              = "integer",
    Recdist               = "karray",
    a                     = "numeric",
    b                     = "numeric",
    Len_age               = "karray",
    Len_age_mid           = "karray",
    Wt_age                = "karray",
    Wt_age_SB             = "karray",
    Wt_age_mid            = "karray",
    mat                   = "karray",
    sel                   = "karray",
    CPUEsel               = "karray",
    mov                   = "karray",
    Idist                 = "karray",
    R0                    = "karray",
    Css                   = "karray",
    CAAFss                = "karray",
    CMbyFss               = "karray",
    SSBAss                = "karray",
    CBss                  = "karray",
    Bss                   = "karray",
    Recss                 = "karray",
    RecYrQtrss            = "karray",
    NLLss                 = "karray",
    NLLIss                = "karray",
    NBeforeInit           = "karray",
    q                     = "karray",
    FAgeRange             = "karray",
    CPUEFleetNums         = "karray",
    CPUEFleetAreas        = "karray",
    CPUEobsMR             = "karray",
    CPUEobsY              = "karray",
    CPUEmpY               = "karray",
    CPUEmpNormYrs         = "numeric",
    UseCPUEfromSS         = "logical",
    UseInitIDevfromSS     = "logical",
    initIDev              = "numeric",
    ECurrent              = "karray",
    CMCurrent             = "karray",
    EByQtrLastYr          = "karray",
    F_FMSYss              = "karray",
    Frepss                = "karray",
    RecACT                = "karray",
    IAC                   = "numeric",
    ITrend                = "karray",
    qCPUE                 = "numeric",

    InitSeed              = "integer",
    ProjectSeed           = "integer"
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "StockSynthesisModelData",
  function(.Object)
  {
    .Object@nbackupyears      = as.integer(0)
    .Object@UseCPUEfromSS     = FALSE
    .Object@UseInitIDevfromSS = FALSE

    return (.Object)
  }
)
