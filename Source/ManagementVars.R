# -----------------------------------------------------------------------------
# MP_Spec class
# -----------------------------------------------------------------------------
setClass("MP_Spec",
  slots = c(
    MP      = "character", # MP to run
    MP_Name = "character", # name given to MP run
    tune    = "numeric"    # Tuning parameter for MP run
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "MP_Spec",
  function(.Object, MP, MP_Name=NA, tune=1.0)
  {
    .Object@MP      = MP
    .Object@MP_Name = if (is.na(MP_Name)) MP else MP_Name
    .Object@tune    = tune

    return (.Object)
  }
)


# -----------------------------------------------------------------------------
# ManagementVars class
# -----------------------------------------------------------------------------
setClass("ManagementVars",
  slots = c(
    nsim          = "integer",
    npop          = "integer",
    nfleets       = "integer",
    nareas        = "integer",
    nyears        = "integer",
    nsubyears     = "integer",
    seed          = "integer",
    FlastYr       = "numeric",
    MP            = "MP_Spec",
    which         = "integer",

    F             = "karray",
    SSB           = "karray",
    B             = "karray",
    CM            = "karray",
    CMbyF         = "karray",
    Rec           = "karray",
    RecYrQtr      = "karray",
    IobsArchive   = "karray",
    IobsRArchive  = "karray",
    TAC           = "karray",
    TAEbyF        = "karray"
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "ManagementVars",
  function(.Object, ssModelData, bHistoric, which)
  {
    if (class(ssModelData) != "StockSynthesisModelData")
    {
      print(paste("ERROR: Could not create ManagementVars.",deparse(substitute(ssModelData)),"not of class StockSynthesisModelData"))
      stop()
    }

    nsim      <- as.integer(if (bHistoric) 1 else ssModelData@nsim)
    npop      <- ssModelData@npop
    nfleets   <- ssModelData@nfleets
    nareas    <- ssModelData@nareas
    nyears    <- if (bHistoric) ssModelData@nyears else ssModelData@proyears
    nsubyears <- ssModelData@nsubyears

    .Object@nsim      <- nsim
    .Object@npop      <- npop
    .Object@nfleets   <- nfleets
    .Object@nareas    <- nareas
    .Object@nyears    <- nyears
    .Object@nsubyears <- nsubyears

    .Object@seed      <- as.integer(runif(ssModelData@nsim, 0, .Machine$integer.max))
    .Object@FlastYr   <- as.double(NA)

    empty             <- NA
    class(empty)      <-"MP_Spec"
    .Object@MP        <- empty
    .Object@which     <- as.integer(which)

    if (bHistoric)
    {
      NLLR  <- apply(ssModelData@NLLss[1:nyears,,], sum, MARGIN=c(1,3))
      IobsR <- ssModelData@qCPUE * NLLR # Is this correct? removed influence on Ibeta and Ierr for historic case.

      .Object@F             <- karray(ssModelData@Frepss[1:nyears], dim=c(nyears))
      .Object@SSB           <- karray(ssModelData@SSBAss[,1:nyears], dim=c(npop, nyears))
      .Object@B             <- karray(ssModelData@Bss[,1:nyears], dim=c(npop, nyears))
      .Object@CM            <- karray(ssModelData@CBss[,1:nyears], dim=c(npop, nyears))
      .Object@CMbyF         <- karray(ssModelData@CMbyFss[,1:nyears,], dim=c(npop, nyears, nfleets))
      .Object@Rec           <- karray(apply(ssModelData@Recss[,1:nyears], c(2), sum), dim=c(nyears))
      .Object@RecYrQtr      <- karray(apply(ssModelData@RecYrQtrss[,1:(nyears * nsubyears)], c(2), sum), dim=c(nyears * nsubyears))
      .Object@IobsArchive   <- karray(ssModelData@CPUEobsY[1:nyears], dim=c(nyears))
      .Object@IobsRArchive  <- karray(IobsR[1:nyears,], dim=c(nyears, nareas))
      .Object@TAC           <- karray(NA, dim=c(nyears))
      .Object@TAEbyF        <- karray(NA, dim=c(nyears, nfleets))
    }
    else
    {
      .Object@F             <- karray(NA, dim=c(nsim, nyears))
      .Object@SSB           <- karray(NA, dim=c(nsim, npop, nyears))
      .Object@B             <- karray(NA, dim=c(nsim, npop, nyears))
      .Object@CM            <- karray(NA, dim=c(nsim, npop, nyears))
      .Object@CMbyF         <- karray(NA, dim=c(nsim, npop, nyears, nfleets))
      .Object@Rec           <- karray(NA, dim=c(nsim, nyears))
      .Object@RecYrQtr      <- karray(NA, dim=c(nsim, nyears * nsubyears))
      .Object@IobsArchive   <- karray(NA, dim=c(nsim, nyears))
      .Object@IobsRArchive  <- karray(NA, dim=c(nsim, nyears, nareas))
      .Object@TAC           <- karray(NA, dim=c(nsim, nyears))
      .Object@TAEbyF        <- karray(NA, dim=c(nsim, nyears, nfleets))
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("f_fmsy", function(.Object, RefVars) standardGeneric("f_fmsy"))

setMethod("f_fmsy", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    F_FMSY <- .Object@F / RefVars@FMSY1

    return (F_FMSY)
  }
)

# -----------------------------------------------------------------------------

setGeneric("c_msy", function(.Object, RefVars) standardGeneric("c_msy"))

setMethod("c_msy", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    C_MSY <- .Object@CM / RefVars@MSY

    return (C_MSY)
  }
)

# -----------------------------------------------------------------------------

setGeneric("b_bmsy", function(.Object, RefVars) standardGeneric("b_bmsy"))

setMethod("b_bmsy", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    if (length(dim(.Object@B)) == 3)
    {
      B_BMSY <- apply(.Object@B, c(1,3), sum) / RefVars@BMSY

    } else
    {
      B_BMSY <- apply(.Object@B, c(2), sum) / RefVars@BMSY
    }

    return (B_BMSY)
  }
)

# -----------------------------------------------------------------------------

setGeneric("ssb_ssbmsy", function(.Object, RefVars) standardGeneric("ssb_ssbmsy"))

setMethod("ssb_ssbmsy", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    if (length(dim(.Object@B)) == 3)
    {
      SSB_SSBMSY <- apply(.Object@SSB, c(1,3), sum) / RefVars@SSBMSY

    } else
    {
      SSB_SSBMSY <- apply(.Object@SSB, c(2), sum) / RefVars@SSBMSY
    }

    return (SSB_SSBMSY)
  }
)

# -----------------------------------------------------------------------------

setGeneric("ssb_ssb0", function(.Object, RefVars) standardGeneric("ssb_ssb0"))

setMethod("ssb_ssb0", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    SSB_SSB0 <- .Object@SSB / RefVars@SSB0

    return (SSB_SSB0)
  }
)

# -----------------------------------------------------------------------------

setGeneric("b_b0", function(.Object, RefVars) standardGeneric("b_b0"))

setMethod("b_b0", c("ManagementVars", "ReferenceVars"),
  function(.Object, RefVars)
  {
    B_B0 <- .Object@B / RefVars@B0

    return (B_B0)
  }
)

# -----------------------------------------------------------------------------

setGeneric("runProjection", function(.Object, RefVars, ssModelData, MseDef, ...) standardGeneric("runProjection"))

setMethod("runProjection", c("ManagementVars", "ReferenceVars", "StockSynthesisModelData", "MseDefinition"),
  function(.Object, RefVars, ssModelData, MseDef, MP, MP_Name, tune, interval, Report, CppMethod, cluster, EffortCeiling, TACTime, rULim)
  {
    runJob <- function(sim, ssModelData, RefVars, MseDef, MP, interval, Report, CppMethod, EffortCeiling, TACTime, rULim, seed, tune, UseCluster)
    {
      if (UseCluster)
      {
        require(keep)

        # In a cluster context we re-direct output to file so we can then play
        # it back in the host
        StdOutFileName <- paste("projStdOutFile", sim, ".txt", sep="")
        sink(file=StdOutFileName, append=FALSE, type=c("output", "message"))
      }

      Proj <- NA

      tryCatch(
      {
        Proj <- new("Projection", ssModelData, RefVars, MseDef, sim, MP, interval, Report, CppMethod, EffortCeiling, TACTime, rULim, seed[sim], tune)
      },
      error=function(e)
      {
        print(e)
        traceback()
      })

      if (UseCluster)
      {
        print("\n")
        sink()
      }

      return (Proj)
    }

    printJobOutput <- function(sim)
    {
      StdOutFileName <- paste("projStdOutFile", sim, ".txt", sep="")

      if (file.exists(StdOutFileName))
      {
        Con <- file(StdOutFileName, "rt")

        writeLines(readLines(Con))
        close(Con)
        unlink(StdOutFileName)
      }
    }

    sims       <- 1:length(.Object@seed)
    UseCluster <- !is.na(cluster)

    if (UseCluster)
    {
      results <- parSapply(cluster,
                           sims,
                           FUN=runJob,
                           ssModelData,
                           RefVars,
                           MseDef,
                           MP,
                           interval,
                           Report,
                           CppMethod,
                           EffortCeiling,
                           TACTime,
                           rULim,
                           .Object@seed,
                           tune,
                           UseCluster)

      sapply(sims, FUN=printJobOutput)
    }
    else
    {
      results <- sapply(sims,
                        FUN=runJob,
                        ssModelData,
                        RefVars,
                        MseDef,
                        MP,
                        interval,
                        Report,
                        CppMethod,
                        EffortCeiling,
                        TACTime,
                        rULim,
                        .Object@seed,
                        tune,
                        FALSE)
    }

    years   <- (ssModelData@nyears + 1):(ssModelData@nyears + ssModelData@proyears)
    months  <- ((ssModelData@nyears) * ssModelData@nsubyears + 1):((ssModelData@nyears + ssModelData@proyears) * ssModelData@nsubyears)

    .Object@MP <- new("MP_Spec", MP, MP_Name, tune)

    for (res in results)
    {
      if (class(res) == "Projection")
      {
        sim <- res@which

        if (sim == 1)
        {
          .Object@FlastYr <- res@F[ssModelData@nyears]
        }

        .Object@F[sim,]             <- res@F[years]
        .Object@SSB[sim,,]          <- res@SSB[,years]
        .Object@B[sim,,]            <- res@B[,years]
        .Object@CM[sim,,]           <- res@CM[,years]
        .Object@CMbyF[sim,,,]       <- res@CMbyF[,years,]
        .Object@Rec[sim,]           <- res@Rec[years]
        .Object@RecYrQtr[sim,]      <- res@RecYrQtr[months]
        .Object@IobsArchive[sim,]   <- res@IobsArchive[years]
        .Object@IobsRArchive[sim,,] <- res@IobsRArchive[years,]
        .Object@TAC[sim,]           <- res@TAC[years]
        .Object@TAEbyF[sim,,]       <- res@TAEbyF[years,]
      }
    }

    return (.Object)
  }
)
