# -----------------------------------------------------------------------------
# ReferenceVars class
# -----------------------------------------------------------------------------
setClass("ReferenceVars",
  slots = c(
    MSY           = "numeric",
    BMSY          = "numeric",
    VBMSY         = "numeric",
    SSBMSY        = "numeric",
    UMSY          = "numeric",
    FMSY1         = "numeric",
    SSBMSY_SSB0   = "numeric",
    SSB0          = "numeric",
    B0            = "numeric"
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "ReferenceVars",
  function(.Object, ssModelData=NULL, MseDef=NULL, Report=FALSE, EffortCeiling=as.double(20.0), TACTime=0.5, rULim=0.5)
  {
    getMSYrefs <- function(.Object, CppMethod, nyears=40)
    {
      TAC  <- sum(ssModelData@CMCurrent)

      # -----------------------------------------------------------------------

      objective <- function(par, reportIndicators)
      {
        environment_MSY <- environment()

        if (reportIndicators)
        {
          environment_MSY$optimisation <- FALSE
        }
        else
        {
          environment_MSY$optimisation <- TRUE
        }

        environment_MSY$TAC <- TAC * exp(par)

        Proj <- new("Projection",
                    ssModelData=ssModelData,
                    RefVars=NULL,
                    MseDef=MseDef,
                    sim=1,
                    MP="",
                    interval=3,
                    Report=Report,
                    CppMethod=CppMethod,
                    EffortCeiling=EffortCeiling,
                    TACTime=TACTime,
                    rULim=rULim,
                    seed=1,
                    tune=1.0,
                    environment_MSY=environment_MSY)

        if (reportIndicators)
        {
          result <- environment_MSY$ReferencePoints
        }
        else
        {
          result <- environment_MSY$Likelihood
        }

        return (result)
      }

      # -----------------------------------------------------------------------

      test <- optimize(objective,
                       interval=log(c(0.01, 100.0)),
                       reportIndicators=FALSE,
                       tol=1e-1)

      best <- objective(test$minimum, reportIndicators=TRUE)

      print(sprintf("TAC at MSY: %.2f, MSY: %.2f, MSY / TAC: %.2f", TAC * exp(test$minimum), best[1], best[1] / (TAC * exp(test$minimum))))
      flush.console()

      return (best)
    }

    # -------------------------------------------------------------------------
    # start of constructor code
    # -------------------------------------------------------------------------
    if (is.null(ssModelData))
    {
      # do nothing. This is an empty constructor for object upgrading
    }
    else
    {
      if (class(ssModelData) != "StockSynthesisModelData")
      {
        print(paste("ERROR: Could not create ReferenceVars.",deparse(substitute(ssModelData)),"not of class StockSynthesisModelData"))
        stop()
      }

      if (class(MseDef) != "MseDefinition")
      {
        print(paste("ERROR: Could not create ReferenceVars.",deparse(substitute(MseDef)),"not of class MseDefinition"))
        stop()
      }

      if (ssModelData@UseMSYss == 0)
      {
        # Do MSYref projections
        MSYrefs <- getMSYrefs(ssModelData, MseDef@CppMethod != 0, nyears=70)

        .Object@MSY         <- MSYrefs[1]
        .Object@BMSY        <- MSYrefs[2]
        .Object@VBMSY       <- MSYrefs[3]
        .Object@SSBMSY      <- MSYrefs[4]
        .Object@UMSY        <- MSYrefs[5]
        .Object@FMSY1       <- MSYrefs[6]
        .Object@SSBMSY_SSB0 <- MSYrefs[7]
        .Object@SSB0        <- MSYrefs[8]
        .Object@B0          <- MSYrefs[9]

      }
      else
      {
        .Object@MSY         <- ssModelData@MSYss
        .Object@BMSY        <- NA #ssModelData@BMSYss
        .Object@VBMSY       <- NA
        .Object@SSBMSY      <- ssModelData@SSBMSYss
        .Object@UMSY        <- NA
        .Object@FMSY1       <- ssModelData@FMSYss
        .Object@SSBMSY_SSB0 <- ssModelData@SSBMSYss / ssModelData@SSB0ss
        .Object@SSB0        <- ssModelData@SSB0ss
        .Object@B0          <- ssModelData@B0ss
      }
    }

    return (.Object)
  }
)
