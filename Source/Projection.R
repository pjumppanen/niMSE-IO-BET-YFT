# -----------------------------------------------------------------------------
# Projection class
# -----------------------------------------------------------------------------
setClass("Projection",
  slots = c(
    npop          = "integer",
    nfleets       = "integer",
    nareas        = "integer",
    nyears        = "integer",
    nsubyears     = "integer",
    which         = "integer",
    Log           = "list",

    F             = "karray",
    SSB           = "karray",
    B             = "karray",
    CM            = "karray",
    CMbyF         = "karray",
    Rec           = "karray",
    RecYrQtr      = "karray",
    IobsArchive   = "karray",
    IobsRArchive  = "karray",
    CPUEobsY      = "karray",
    TAC           = "karray",
    TAEbyF        = "karray"
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "Projection",
  function(.Object, ssModelData, RefVars, MseDef, sim, MP, interval, Report, CppMethod, EffortCeiling, TACTime, rULim, seed, tune, environment_MSY=NULL)
  {
    if (class(ssModelData) != "StockSynthesisModelData")
    {
      print(paste("ERROR: Could not create Projection.",deparse(substitute(ssModelData)),"not of class StockSynthesisModelData"))
      stop()
    }

    if (is.null(environment_MSY) && (class(RefVars) != "ReferenceVars"))
    {
      print(paste("ERROR: Could not create Projection.",deparse(substitute(RefVars)),"not of class ReferenceVars"))
      stop()
    }

    if (class(MseDef) != "MseDefinition")
    {
      print(paste("ERROR: Could not create Projection.",deparse(substitute(MseDef)),"not of class MseDefinition"))
      stop()
    }

    isMSY_projection <- !is.null(environment_MSY)

    # --------------------------------------------------------------------

    projection.domov <- function(Ntemp, movtemp)
    {
      # P A R  x  P A R R
      nareas <- dim(movtemp)[length(dim(movtemp))]

      #return dim = PAR
      apply(karray(Ntemp, c(dim(Ntemp),nareas)) * movtemp, MARGIN=c(1,2,4), sum)
    }

    # --------------------------------------------------------------------

    findFrep <- function(NsoRbyPAM, M_Y, npop, nages, nsubyears, FAgeRange)
    {
      ZsoRbyPAM                <- karray(NA, dim=c(npop,nages-2,nsubyears))
      ZsoRbyPAM[,,1:nsubyears] <- -log(NsoRbyPAM[,2:(nages - 1),2:(nsubyears + 1)] / NsoRbyPAM[,1:(nages - 2),1:nsubyears])
      FsoRbyPAM                <- ZsoRbyPAM - karray(M_Y[,1:(nages-2)] / nsubyears, dim=c(npop,nages-2,nsubyears))

      Frep <- mean(FsoRbyPAM[,FAgeRange[1]:FAgeRange[2],])  # 2:27 = true ages 1:26 (1:26 used by SS)

      return (Frep)
    }

    # --------------------------------------------------------------------

    sampCatch <- function(Csamp, nSamp)
    {
      out    <- karray(NA, dim(Csamp))
      nages  <- dim(Csamp)[1]
      nyears <- dim(Csamp)[2]

      for (yy in 1:nyears)
      {
        Csampo <- Csamp[,yy]

        if (sum(Csampo < 0.0) > 0.0) browser()
        if (sum(Csampo) == 0) Csampo <- rep(1.0 / nages, nages)

        out[,yy] <- ceiling(rmultinom(1, size=nSamp, Csampo)) #small value added for reproducibility in rnd number table position
      }

      return (out)
    }

    # --------------------------------------------------------------------

    sdconv <- function(m, sd)
    {
      # get log normal standard deviation from transformed space mean and standard deviation
      return ((log(1.0 + ((sd ^ 2) / (m ^ 2)))) ^ 0.5)
    }

    # --------------------------------------------------------------------

    mconv <- function(m, sd)
    {
      # get log normal mean from transformed space mean and standard deviation
      return (log(m) - 0.5 * log(1.0 + ((sd ^ 2) / (m ^ 2))))
    }

    # --------------------------------------------------------------------

    trlnorm <- function(reps, mu, cv)
    {
      return (rlnorm(reps, mconv(mu, mu * cv), sdconv(mu, mu * cv)))
    }

    # --------------------------------------------------------------------

    makeCAL <- function(CAA, LenAtAge, CAL_bins, CALsd=0.05)
    {
      ny       <- dim(CAA)[2]
      na       <- dim(CAA)[1]
      CALmu    <- -0.5 * CALsd ^ 2
      nCALbins <- length(CAL_bins)
      CAL      <- karray(NA, dim=c(nCALbins,ny))

      for (j in 1:ny)
      {
        ages    <- rep(1:na, CAA[,j]) + runif(sum(CAA[,j]), -0.5, 0.5)  # vector of the age of the samples (with error)
        lengths <- LenAtAge[round(ages)] * exp(rnorm(sum(CAA[,j]), CALmu, CALsd))
        CAL[,j] <- c(0, hist(lengths, breaks=CAL_bins, plot=Report)$counts)
      }

      return(CAL)
    }

    # --------------------------------------------------------------------

    MSYreferencePoints <- function(ECurrent,
                                   sel,
                                   M,
                                   C,
                                   SSB0,
                                   B0,
                                   N,
                                   NBefore,
                                   SSN,
                                   Wt_age,
                                   #Wt_age_SB,
                                   targpop,
                                   nsubyears,
                                   npop,
                                   nages,
                                   nareas,
                                   nfleets,
                                   FAgeRange)
    {
      # Assume M,C,SSN,Wt_age are single year slices. In addition assume N and NBefore
      # have nsubyear + 1 entries for the subyear index with the +1 th entry
      # corresponding to the first time step of the following year. In all cases the
      # simulation index is removed / not present.
      MSY    <- sum(karray(C[targpop,,,,], c(length(targpop), nages, nsubyears, nareas, nfleets)) * karray(Wt_age[targpop,,nyears], c(length(targpop),nages,nsubyears,nareas,nfleets)))
      BMSY   <- sum(karray((NBefore[targpop,,1,]), c(length(targpop),nages,nareas)) * karray(Wt_age[targpop,,nyears], c(length(targpop),nages,nareas)))
      softmp <- karray(NA, dim=c(nages,nsubyears,nareas,nfleets))

      for(im in 1:nsubyears)
      {
        for(ir in 1:nareas)
        {
          for(ifleets in 1:nfleets)
          {
            softmp[,im,ir,ifleets] <- ECurrent[im,ir,ifleets] * sel[ifleets,]
          }
        }
      }

      sof <- apply(softmp, sum, MARGIN=c(1:3))

      rm(softmp)

      # DK: prod(N, sel, ECurrent for MSY year)   TC uses season 1 only; take average over seasons instead
      # this is probably wrong and not a good option anyway
      VBMSY <- sum(karray(rep(sof,each=length(targpop)), c(length(targpop),nages,nsubyears,nareas)) *
                   karray((N[targpop,,1:nsubyears,]), c(length(targpop),nages,nsubyears,nareas)) *
                   karray(rep(Wt_age[targpop,,nyears],nsubyears), c(length(targpop),nages,nsubyears,nareas)))

      # All this stuff used to calculate FMSY (summed over regions, mean over age and season)
      NsoRbyPAM <- apply(NBefore[,,,], FUN=sum, MARGIN=c(1:3))

      # Note that as we are in steady state numbers at y + 1 subyear 1 should be the same as
      # numbers at y subyear 1 . The values at nsubyears + 1 are incomplete because they need
      # to wrap around to the next year first. As such, we base the nsubyears + 1 case on
      # the first subyear.
      NsoRbyPAM[,,nsubyears + 1] <- apply(NBefore[,,1,], FUN=sum, MARGIN=c(1:2))

      ZsoRbyPAM <- karray(NA, dim=c(npop,nages-2,nsubyears))

      ZsoRbyPAM[,,1] <- -log(NsoRbyPAM[,2:(nages-1),2] / NsoRbyPAM[,1:(nages-2),1])
      ZsoRbyPAM[,,2] <- -log(NsoRbyPAM[,2:(nages-1),3] / NsoRbyPAM[,1:(nages-2),2])
      ZsoRbyPAM[,,3] <- -log(NsoRbyPAM[,2:(nages-1),4] / NsoRbyPAM[,1:(nages-2),3])
      ZsoRbyPAM[,,4] <- -log(NsoRbyPAM[,2:(nages-1),5] / NsoRbyPAM[,1:(nages-2),4])

      MbyPAM    <- karray(rep(M[,1:(nages-2),nyears],times=nsubyears), dim=c(npop,nages-2,nsubyears))
      FsoRbyPAM <- ZsoRbyPAM - MbyPAM / nsubyears

      FMSY1     <- mean(FsoRbyPAM[,FAgeRange[1]:FAgeRange[2],])  # 2:27 = true ages 1:26 (1:26 used by SS)

      #potential change to integrated biomass calculation
      SSBMSY    <- sum(karray(SSN[targpop,,1,],c(length(targpop),nages,nareas)) * karray(Wt_age[targpop,,nyears], c(length(targpop),nages,nareas)))
      #SSBMSY     <- sum(karray(NBefore[targpop,,1,],c(length(targpop),nages,nareas)) * karray(Wt_age_SB[targpop,,nyears], c(length(targpop),nages,nareas)))

      UMSY      <- MSY / VBMSY
      SSBMSY_B0 <- SSBMSY / sum(SSB0[targpop])

      return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSY1,SSBMSY_B0,SSB0,B0))
    }

    # --------------------------------------------------------------------
    # Start of contructor code
    # --------------------------------------------------------------------

    .Object@Log <- list()

    npop          <- ssModelData@npop
    nareas        <- ssModelData@nareas
    nyears        <- ssModelData@nyears
    nsubyears     <- ssModelData@nsubyears
    proyears      <- ssModelData@proyears
    nages         <- ssModelData@nages
    nfleets       <- ssModelData@nfleets
    nCPUE         <- ssModelData@nCPUE
    targpop       <- as.integer(MseDef@targpop)
    allyears      <- nyears + proyears
    FAgeRange     <- c(ssModelData@FAgeRange[1], ssModelData@FAgeRange[2])
    nbackupyears  <- ssModelData@nbackupyears

    .Object@npop      <- npop
    .Object@nfleets   <- nfleets
    .Object@nareas    <- nareas
    .Object@nyears    <- allyears
    .Object@nsubyears <- nsubyears
    .Object@which     <- as.integer(sim)

    .Object@F             <- karray(as.double(NA), dim=c(allyears))
    .Object@SSB           <- karray(as.double(NA), dim=c(npop, allyears))
    .Object@B             <- karray(as.double(NA), dim=c(npop, allyears))
    .Object@CM            <- karray(as.double(NA), dim=c(npop, allyears))
    .Object@CMbyF         <- karray(as.double(NA), dim=c(npop, allyears, nfleets))
    .Object@Rec           <- karray(as.double(NA), dim=c(allyears))
    .Object@RecYrQtr      <- karray(as.double(NA), dim=c(allyears * nsubyears))
    .Object@IobsArchive   <- karray(as.double(NA), dim=c(allyears))
    .Object@IobsRArchive  <- karray(as.double(NA), dim=c(allyears, nareas))
    .Object@CPUEobsY      <- karray(as.double(NA), dim=c(allyears))
    .Object@TAC           <- karray(as.double(NA), dim=c(allyears))
    .Object@TAEbyF        <- karray(as.double(NA), dim=c(allyears, nfleets))

    MP_environment        <- new.env()

    initFn <- function(.Object)
    {
      # Repeat the same stochastic history for each MP to keep the comparison fair
      set.seed(seed)

      if (is.na(CppMethod))
      {
        CppMethod <- MseDef@CppMethod
      }

      NMass     <- karray(as.double(NA),c(npop,nages,allyears+1,nsubyears,nareas))  # N in mass
      Z         <- karray(as.double(NA),c(npop,nages,allyears+1,nsubyears,nareas))  # Z aggregated over fleets
      NLLI      <- karray(as.double(NA),c(allyears))                                # Longline-selected Numbers over all ages, areas, populations for aggregate abundance index
      NLL       <- karray(as.double(NA),c(allyears,nsubyears,nareas))               # Longline-selected Numbers over all ages, areas, populations for aggregate abundance index
      Fdist     <- karray(as.double(NA),c(npop,nfleets,nareas))                     # current F dist by fleet
      CAAF      <- karray(as.double(NA),c(nages,allyears,nfleets))                  # Catch at age by fleets

      initYear  <- nyears  # last assessment year

      # subset all model parameters needed by the model run
      Len_age       <- ssModelData@Len_age
      Len_age_mid   <- ssModelData@Len_age_mid
      Wt_age        <- ssModelData@Wt_age
      #Wt_age_SB     <- ssModelData@Wt_age_SB
      Wt_age_mid    <- ssModelData@Wt_age_mid
      h             <- ssModelData@h
      R0            <- ssModelData@R0
      Idist         <- ssModelData@Idist
      M             <- ssModelData@M
      mat           <- ssModelData@mat
      Recdist       <- ssModelData@Recdist
  #    Linf          <- ssModelData@Linf
  #    K             <- ssModelData@K
      q             <- ssModelData@q
      EByQtrLastYr  <- ssModelData@EByQtrLastYr
      ECurrent      <- ssModelData@ECurrent
      CMCurrent     <- ssModelData@CMCurrent
      sel           <- ssModelData@sel
      mov           <- ssModelData@mov
      CPUEsel       <- ssModelData@CPUEsel
      qCPUE         <- ssModelData@qCPUE

      CAAF[,1:initYear,]                        <- ssModelData@CAAFss[,1:initYear,]
      .Object@SSB[,1:initYear]                  <- ssModelData@SSBAss[,1:initYear]
      .Object@B[,1:initYear]                    <- ssModelData@Bss[,1:initYear]
      .Object@Rec[1:initYear]                   <- apply(ssModelData@Recss[,1:initYear], c(2), sum)
      .Object@RecYrQtr[1:(initYear*nsubyears)]  <- apply(ssModelData@RecYrQtrss[,1:(initYear*nsubyears)], c(2), sum)
      .Object@CM[,1:initYear]                   <- ssModelData@CBss[,1:initYear]
      .Object@CMbyF[,1:initYear,]               <- ssModelData@CMbyFss[,1:initYear,]
      .Object@CPUEobsY[1:initYear]              <- ssModelData@CPUEobsY[1:initYear]
      NLLI[1:initYear]                          <- ssModelData@NLLIss[1:initYear]
      NLL[1:initYear,,]                         <- ssModelData@NLLss[1:initYear,,]

      NullRecSpatialDevs                        <- karray(as.double(1.0),c(npop,nareas))
      NBefore_Y                                 <- karray(NA,c(npop,nages,nsubyears + 1,nareas))  # this N should be the start of timestep
      N_Y                                       <- karray(NA,c(npop,nages,nsubyears + 1,nareas))  # this N is updated within the timestep

      SSN_Y                                     <- karray(as.double(NA),c(npop,nages,nsubyears,nareas))
      NLL_Y                                     <- karray(as.double(NA),c(nsubyears,nCPUE))
      NLLI_Y                                    <- as.double(NA)
      C_Y                                       <- karray(as.double(NA),c(npop,nages,nsubyears,nareas,nfleets))
      SSBA_Y                                    <- karray(as.double(NA),c(npop))

      if (isMSY_projection)
      {
        # if running a projection for MSY estimation purposes it means that
        # we are calculation RefVars and therefore cannot use it to obtain
        # B0 and SSB0 values.

        # Find survivorship
        # -----------------
        # Need to get initial equilibrium numbers right because SSB0 and rec depend on it
        # NBefore recorded before M, hence the M=0 for a=1; integral for lastAge added (important for low M scenatios)
        Madvanced           <- karray(as.double(NA), c(npop, nages))
        Madvanced[1:npop,1] <- 0.0
        Madvanced[,2:nages] <- M[, 1:(nages - 1), 1]
        surv                <- t(exp(-apply(Madvanced[, 1:nages], c(1), cumsum) / nsubyears))

        #infinite sum for plus group
        surv[,nages] <- surv[,nages - 1] * exp(-Madvanced[,nages] / nsubyears) / (1.0 - exp(-Madvanced[,nages] / nsubyears))

        y <- initYear
        m <- 1

        PAYMR <- as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas))    # Set up some karray indexes
        PAMR  <- PAYMR[,c(1,2,4,5)]
        PA    <- PAYMR[,c(1,2)]
        P     <- PAYMR[,c(1)]
        PAR   <- PAYMR[,c(1,2,5)]
        PAY   <- PAYMR[,c(1,2,3)]

        SSB_Y <- karray(as.double(NA),c(npop,nages,nsubyears,nareas))
        B_Y   <- karray(as.double(NA),c(npop,nages,nsubyears,nareas))

        if (nareas > 1)
        {
          # Calculate virgin Numbers
          NBefore_Y[PAMR] <- surv[PA] * R0[P] * Idist[PAR]
        }
        else
        {
          NBefore_Y[PAMR] <- surv[PA] * R0[P]
        }

        SSN_Y[PAMR] <- NBefore_Y[PAMR] * mat[PAY]

        # potential change to combined maturity and mass-at-age
        # Calculate spawning stock biomass
        SSB_Y[PAMR] <- SSN_Y[PAMR] * Wt_age[PAY]

        # Calculate total biomass
        B_Y[PAMR] <- NBefore_Y[PAMR] * Wt_age[PAY]

        SSB0 <- apply(SSB_Y[,,m,], 1, sum)
        B0   <- apply(B_Y[,,m,], 1, sum)
      }
      else
      {
        SSB0 <- RefVars@SSB0
      }

      # Calculate spawning stock biomass per recruit
      SSBpR   <- SSB0 / R0

      # Ricker SR params
      bR      <- log(5.0 * h) / (0.8 * SSB0)
      aR      <- exp(bR * SSB0) / SSBpR


      # Determine autocorrelated rec dev quarterly parameters based on annual specification
      # --------------------------------------------
      if (ssModelData@RecACT < 0.0)
      {
        warning("For negatively correlated recruitment deviation the parameter RecACT is assumed a quarterly figure rather than annual")
      }

      recruitment_params <- findWindowedAR_params(ssModelData@RecACT)
      Recdevs            <- karray(as.double(NA), dim=c(ssModelData@npop, allyears * length(ssModelData@Recsubyr)))
      PTm                <- as.matrix(expand.grid(1:ssModelData@npop, 1:(allyears * length(ssModelData@Recsubyr))))

      if (isMSY_projection)
      {
        Recdevs[PTm] <- 1.0

        # Initial population at the beginning of projection before movement and mortality for reporting with noise
        # --------------------------------------------------------------------------------------------------------
        NBeforeInit <- ssModelData@NBeforeInit

        # take out selectivity temporal variability as this is a MSY projection
        # ---------------------------------------------------------------------
        selTSSign     <- karray(1.0, dim=c(ssModelData@nfleets, 2))
        selTSWaveLen  <- karray(0.0, dim=c(ssModelData@nfleets, 2))
        selAgeRange   <- 0
        selExpRange   <- 0
      }
      else
      {
        # Autocorrelated rec devs for projection years
        # --------------------------------------------
        rndDevs <- karray(rep(ssModelData@ReccvT, times=ssModelData@npop * allyears * length(ssModelData@Recsubyr)) * rnorm(ssModelData@npop * allyears * length(ssModelData@Recsubyr)),
                          dim=c(ssModelData@npop, allyears * length(ssModelData@Recsubyr)))

        for (t in (ssModelData@nyears * length(ssModelData@Recsubyr) + 1):(allyears * length(ssModelData@Recsubyr)))
        {
          rndDevs[,t] <- recruitment_params$Alpha * rndDevs[,t - 1] + rndDevs[,t] * recruitment_params$Beta
        }

        # We attach any recruitment scaling (recuitment shock implementation) to the recruitment deviates.
        P <- PTm[,c(1)]
        Y <- floor((PTm[,c(2)] - 1) / length(ssModelData@Recsubyr)) + 1  # Calculate year from timestep

        if (length(MseDef@RecScale) == 1)
        {
          RecScale <- karray(c(rep(1.0, times=ssModelData@nyears), rep(MseDef@RecScale, times=MseDef@proyears)))
        }
        else if (length(MseDef@RecScale) == MseDef@proyears)
        {
          RecScale <- karray(c(rep(1.0, times=ssModelData@nyears), MseDef@RecScale))
        }
        else
        {
          print("ERROR: MSE definition RecScale vector is wrong length. It must be proyears in length")
          stop()
        }

        Recdevs[PTm] <- RecScale[Y] * exp(rndDevs[PTm] - 0.5 * ssModelData@ReccvT[P] ^ 2)

        rm(rndDevs, PTm, P, Y)

        # Initial population at the beginning of projection before movement and mortality for reporting with noise
        # --------------------------------------------------------------------------------------------------------
        CVMatByPA   <- rep(MseDef@NInitCV * exp(-MseDef@NInitCVdecay * (0:(ssModelData@nages - 1))), each=ssModelData@npop)
        CVMatByPAR  <- karray(rep(CVMatByPA, times=ssModelData@nareas), dim=c(ssModelData@npop, ssModelData@nages, ssModelData@nareas))
        Ndevs       <- exp(CVMatByPAR * rnorm(prod(dim(ssModelData@NBeforeInit))) - 0.5 * CVMatByPAR * CVMatByPAR)
        NBeforeInit <- ssModelData@NBeforeInit * Ndevs

        rm(CVMatByPA, CVMatByPAR, Ndevs)


        # selectivity temporal variability
        # --------------------------------
        selTSSign                 <- karray(round(runif(ssModelData@nfleets * 2)), dim=c(ssModelData@nfleets, 2))
        selTSWaveLen              <- karray(runif(ssModelData@nfleets * 2) * (MseDef@selWLRange[2] - MseDef@selWLRange[1]) + MseDef@selWLRange[1], dim=c(ssModelData@nfleets, 2))
        selTSSign[selTSSign < 1]  <- -1 # -1 or 1 indicate initial sin wave trend
        selAgeRange               <- MseDef@selAgeRange
        selExpRange               <- MseDef@selExpRange
      }

      if (!isMSY_projection)
      {
        # historical simulation of one year to re-create N for first projection year
        # ---------------------------------------------------------------------------
        cat("\nRe-running final year of OM")
        cat("\n")
      }

      y           <- initYear - nbackupyears

      mm          <- nsubyears
      nSpawnPerYr <- length(ssModelData@Recsubyr)  # rec Index for more than 1 rec per year

      # Initialise starting population. Note N_Y initialisation redundant for R case but
      # needed for C++ case. For MSY projection we increase the starting population size
      # to ensure the stock doesn't crash on high F when the starting state is overfished.
      # Without accounting for this the MSY projection could become biased low.
      if (isMSY_projection)
      {
        NBefore_Y[,,1,] <- NBeforeInit * 3
        N_Y[,,1,]       <- NBeforeInit * 3
      }
      else
      {
        NBefore_Y[,,1,] <- NBeforeInit
        N_Y[,,1,]       <- NBeforeInit
      }

      # Matrix index arrays used in historic and projection R code
      PAYMRF <- as.matrix(expand.grid(1:npop, 1:nages, y, 1, 1:nareas, 1:nfleets))
      PAMRF  <- PAYMRF[,c(1,2,4,5,6)]
      MRF    <- PAYMRF[,c(4,5,6)]
      PARF   <- PAYMRF[,c(1,2,5,6)]
      FA     <- PAYMRF[,c(6,2)]
      F      <- PAYMRF[,c(6)]
      ARF    <- PAYMRF[,c(2,5,6)]
      AR     <- PAYMRF[,c(2,5)]
      RF     <- PAYMRF[,c(5,6)]

      PAYMR  <- as.matrix(expand.grid(1:npop, 1:nages, y, 1, 1:nareas))
      PAMR   <- PAYMR[,c(1,2,4,5)]
      PAY    <- PAYMR[,c(1,2,3)]
      PAR    <- PAYMR[,c(1,2,5)]
      A      <- PAYMR[,c(2)]

      if (CppMethod != 0) # use C++ Baranov sub-routine
      {
        Obj <- Om.create(npop,
                         nages,
                         nsubyears,
                         nareas,
                         nfleets,
                         ssModelData@Recsubyr)

        OmB.nt.initialiseParameters(Obj,
                                    M[,,y],
                                    R0,
                                    mat[,,y],
                                    Idist,
                                    Wt_age[,,y],
                                    h)

        RecdevIndex <- (y - 1) * nSpawnPerYr + 1
        Recdevs_Y   <- Recdevs[,RecdevIndex:(RecdevIndex + nSpawnPerYr - 1)]

        OmB.nt.runHistoric(Obj,
                           as.double(1.0),
                           q,
                           EByQtrLastYr,
                           R0,
                           M[,,y],
                           mat[,,y],
                           Idist,
                           Len_age[,,y],
                           Wt_age[,,y],
                           sel,
                           mov,
                           h,
                           Recdist,
                           Recdevs_Y,
                           NullRecSpatialDevs,
                           ssModelData@SRrel,
                           N_Y,
                           NBefore_Y,
                           SSN_Y,
                           C_Y,
                           SSBA_Y)
      }
      else # Use R-based projection code
      {
        FM <- karray(NA, dim=c(npop,nages,nareas,nfleets))
        Z  <- karray(NA, dim=c(npop,nages,nareas))

        #Initial projection year application
        for (mm in 1:nsubyears)
        {
          PAMRF[,3] <- mm
          PAMR[,3]  <- mm
          MRF[,1]   <- mm

          SSN_Y[,,mm,] <- NBefore_Y[,,mm,] * karray(rep(mat[,,y],times=nareas), c(npop,nages,nareas))
          #potential change to _SB
          SSBA_Y        <- apply(SSN_Y[,,mm,] * karray(Wt_age[,,y], dim=c(npop,nages,nareas)), c(1), sum, na.rm=T)
          #SSBA_Y        <- apply(NBefore_Y[,,mm,] * karray(Wt_age_SB[,,y], dim=c(npop,nages,nareas)), c(1), sum, na.rm=T)

          if (mm %in% ssModelData@Recsubyr)
          {
            for(pp in 1:npop)
            {
              # ie every qtr for YFT
              RecdevInd <- (y - 1) * nSpawnPerYr + mm

              # recruit fish
              if (ssModelData@SRrel[pp] == 1)
              {
                # Beverton-Holt recruitment
                rec <- Recdevs[pp,RecdevInd] * ((0.8 * R0[pp] * h[pp] * SSBA_Y[pp]) / (0.2 * SSBpR[pp] * R0[pp] * (1 - h[pp]) + (h[pp] - 0.2) * SSBA_Y[pp]))
              }
              else
              {
                # Most transparent form of the Ricker uses alpha and beta params
                rec <- Recdevs[pp,RecdevInd] * aR[pp] * SSBA_Y[pp] * exp(-bR[pp] * SSBA_Y[pp])
              }

              NBefore_Y[pp,1,mm,] <- rec * Recdist[pp,]
            }
          }

          # move fish
          if (nareas > 1)
          {
            N_Y[,,mm,] <- projection.domov(Ntemp=karray(NBefore_Y[,,mm,], dim=c(npop,nages,nareas)),
                                           movtemp=karray(mov[,,mm,,], dim=c(npop,nages,nareas,nareas)))
          }
          else
          {
            N_Y[,,mm,] <- NBefore_Y[,,mm,]
          }

          # Apply M and F
          FM[PARF] <- EByQtrLastYr[MRF] * sel[FA] * q[F]
          Ftot     <- apply(FM, c(1,2,3), sum)
          Z[PAR]   <- Ftot[PAR] + M[PAY] / nsubyears

          C_Y[PAMRF] <- N_Y[PAMR] * (1.0 - exp(-Z[PAR])) * (FM[PARF] / Z[PAR])

          N_Y[,,mm,] <- N_Y[,,mm,] * exp(-Z[,,])

          # Age fish
          NBefore_Y[,nages,mm + 1,]          <- N_Y[,nages - 1,mm,] + N_Y[,nages,mm,]
          NBefore_Y[,2:(nages - 1),mm + 1,]  <- N_Y[,1:(nages - 2),mm,]
          NBefore_Y[,1,mm + 1,]              <- 0

        } #Initial projection year mm loop
      }  #R option initial year set up

      UpdateRecentEbyF <- FALSE
      RecentEbyF       <- karray(1.0, dim=c(nfleets))

      # Calculate final year Frep
      NsoRbyPAM    <- apply(NBefore_Y[,,keep(1:(nsubyears+1)),], FUN=sum, MARGIN=c(1:3))
      .Object@F[y] <- findFrep(NsoRbyPAM, M[,,y], npop, nages, nsubyears, FAgeRange)

      # calculate LL selected numbers for the year for the abundance index
      NLLbyAMR <- apply(N_Y[,keep(1:nages),1:nsubyears,], MARGIN=c(2,3,4), FUN=sum, na.rm=T)

      for (isubyears in 1:nsubyears)
      {
        for (iCPUE in 1:nCPUE)
        {
          NLL_Y[isubyears,iCPUE] <- sum(NLLbyAMR[,isubyears,ssModelData@CPUEFleetAreas[iCPUE]] * CPUEsel[,iCPUE], na.rm=T)
        }
      }

      NLLI_Y <- sum(NLL_Y, na.rm=T)

      # copy results back into historical data
      .Object@SSB[,y] <- SSBA_Y
      NLLI[y]         <- NLLI_Y
      NLL[y,,]        <- NLL_Y

      E <- apply(ssModelData@ECurrent, sum, MARGIN=c(1,3))

      # Store results ...
      .Object@CM[y]      <- apply(C_Y * karray(Wt_age_mid[,,y], c(npop,nages,nsubyears,nareas,nfleets)), c(1), sum)
      .Object@CMbyF[,y,] <- apply(C_Y * karray(Wt_age_mid[,,y], c(npop,nages,nsubyears,nareas,nfleets)), c(1,5), sum)

      CAAF[,y,]      <- apply(C_Y, c(2,5), sum)
      .Object@Rec[y] <- apply(NBefore_Y[,1,keep(1:nsubyears),], c(1), sum)
      BbyS           <- apply(NBefore_Y[,,keep(1:nsubyears),] * karray(Wt_age[,,y], c(npop,nages,nsubyears,nareas)), c(1,3), sum)
      .Object@B[,y]  <- apply(BbyS, c(1), mean)

      FirstIdx <- (y - 1) * nsubyears + 1
      LastIdx  <- y * nsubyears

      .Object@RecYrQtr[FirstIdx:LastIdx] <- apply(NBefore_Y[,1,keep(1:nsubyears),], c(1,2), sum)

      # Run projections
      # ---------------------------------------------------------------------------

      if (!isMSY_projection)
      {
        cat("\n Running projections \n")
      }

      firstMPy <- nyears + MseDef@firstMPYr - MseDef@lastCalendarYr
      upyrs    <- firstMPy + (0:(floor(ssModelData@proyears / interval))) * interval  # the years in which there are MP updates (e.g. every three years)

      #xxx might need to load historical Catch and CL (depending on MP), bypassed for now

      CAAInit     <- karray(NA, dim=c(nages, nyears - nbackupyears))
      CAAIndInit  <- karray(NA, dim=c(nages, nyears - nbackupyears)) #Catch-at-age for the abudnance index fleets

      if (min(apply(CAAF, c(1,2), sum) < 0, na.rm=T))
      {
        print("CA<0 a")
        browser()
      }

      CAAInit[,1:y]    <- sampCatch(apply(CAAF[,keep(1:y),], c(1,2),sum), MseDef@nCAAobs)                       # need time series for MP
      CAAIndInit[,1:y] <- sampCatch(apply(CAAF[,keep(1:y),MseDef@indexFisheries], c(1,2), sum), MseDef@nCAAobs) # need time series for MP

      # xxx zzz need to load historical from OM eventually
      nCALbins <- 30

      #DK: quick and dirty fix
      #CAL_bins   <- seq(0, max(Linf), length.out=nCALbins)
      CAL_bins   <- seq(0, 200, length.out=nCALbins - 1)
      CAL_bins   <- c(CAL_bins, CAL_bins[nCALbins - 1] * 2)     #add big +bin
      CALInit    <- karray(NA, dim=c(nCALbins, y))
      CALIndInit <- karray(NA, dim=c(nCALbins, y))

      #DK change to use Len_age rather than recalc
      #CAL               <- makeCAL(CAA, Linf=Linf[1,1:y], K=K[1,1:y], t0=t0[1], CAL_bins)
      #CAL[,1:y]         <- makeCAL(CAA[,1:y,drop=FALSE], Len_age[1,,1:y], CAL_bins)
      CALInit[,1:y]     <- makeCAL(CAAInit[,1:y], Len_age_mid[1,,1:y], CAL_bins)
      #CALLL10[,1:y]     <- makeCAL(CAALL10[,1:y,drop=FALSE], Len_age[1,,1:y], CAL_bins)
      CALIndInit[,1:y]  <- makeCAL(CAAIndInit[,1:y], Len_age_mid[1,,1:y], CAL_bins)

      CAA    <- CAAInit
      CAAInd <- CAAIndInit
      CAL    <- CALInit
      CALInd <- CALIndInit
      Ibeta  <- exp(runif(1, log(MseDef@Ibeta[1]), log(MseDef@Ibeta[2])))

      # Initialise observation model errors
      Cimp <- runif(1, MseDef@Ccv[1], MseDef@Ccv[2])
      Cb   <- trlnorm(1, MseDef@Cbmean, MseDef@Cbcv)
      Cerr <- karray(trlnorm(allyears, rep(Cb, allyears), rep(Cimp, allyears)), c(allyears))

      # Calculate the CPUE CV and auto-correlation for the given MP CPUE series
      if (any(!is.na(ssModelData@CPUEmpY)))
      {
        # Initialise IrndDevs based on supplied CPUE sequence using determined Icv, IAC, initIDev and qCPUE
        .Object@CPUEobsY[1:initYear] <- ssModelData@CPUEmpY[1:initYear]

        # Re-normalization by pre-defined period
        if (any(is.na(ssModelData@CPUEmpNormYrs)))
        {
          CPUEmpNormYrs <- c(1, nyears)
        }
        else
        {
          CPUEmpNormYrs <- ssModelData@CPUEmpNormYrs - MseDef@firstCalendarYr + 1
        }

        MeanMP_CPUE   <- mean(ssModelData@CPUEmpY[CPUEmpNormYrs[1]:CPUEmpNormYrs[2]], na.rm=TRUE)
        MeanObsCPUE   <- mean(ssModelData@CPUEobsY[CPUEmpNormYrs[1]:CPUEmpNormYrs[2]], na.rm=TRUE)
        NormMP_CPUE   <- ssModelData@CPUEmpY[1:initYear] / MeanMP_CPUE
        NormObsCPUE   <- ssModelData@CPUEobsY[1:initYear] / MeanObsCPUE
        DevsCPUE      <- log(NormMP_CPUE) - log(NormObsCPUE)
        MPcpueRMSE    <- sqrt(mean(DevsCPUE ^ 2, na.rm = TRUE))

        # remove missing observations calculations
        IdxA          <- CPUEmpNormYrs[1]:(CPUEmpNormYrs[2] - 1)
        IdxB          <- (CPUEmpNormYrs[1] + 1):CPUEmpNormYrs[2]
        ValidA        <- which(!is.na(NormMP_CPUE[IdxA]))
        ValidB        <- which(!is.na(NormMP_CPUE[IdxB]))
        valid         <- intersect(ValidA, ValidB)
        lenValid      <- length(valid)

        # calculate the starting point for the CPUE deviations
        lastYrIndices <- c(valid[lenValid - 3], valid[lenValid - 2], valid[lenValid - 1], valid[lenValid])
        norm          <- exp(mean(log(.Object@CPUEobsY[1:nyears] / ssModelData@CPUEobsY[1:nyears]), na.rm=TRUE))
        initIDev      <- log(norm * sum(ssModelData@CPUEobsY[IdxB[lastYrIndices]]) / sum(ssModelData@CPUEmpY[IdxB[lastYrIndices]]))

        # If Icv[1] > 0 then assume we a doing robustness testing and will use the Icv and IAC from
        # the MseDef rather thanthe calculated values.
        if (MseDef@Icv[1] >= 0)
        {
          Iimp        <- runif(1, MseDef@Icv[1], MseDef@Icv[2])
          IAC         <- ssModelData@IAC
        }
        else
        {
          Iimp        <- MPcpueRMSE

          if (Iimp < 0.2) Iimp <- 0.2

          # calculate the auto-correlation
          IAC         <- cor(DevsCPUE[IdxA[valid]], DevsCPUE[IdxB[valid]])
        }

        # q to keep CPUE on original scale
        #   CPUEobsY = qCPUE * (NLLI ^ Ibeta) * Ierr
        #
        qCPUE         <- ssModelData@qCPUE * norm
      }
      else
      {
        # Initialise IrndDevs based on MseDef parameters and keep SS CPUE sequence
        Iimp     <- runif(1, MseDef@Icv[1], MseDef@Icv[2])
        initIDev <- ssModelData@initIDev
        IAC      <- ssModelData@IAC
        qCPUE    <- ssModelData@qCPUE
      }

      IrndDevs         <- karray(rnorm(allyears, 0, rep(Iimp, allyears)), c(allyears))
      IrndDevs[nyears] <- initIDev

      if (length(IAC) != 0)
      {
        for (t in (nyears + 1):allyears)
        {
          IrndDevs[t] <- IAC * IrndDevs[t - 1] + IrndDevs[t] * sqrt(1.0 - IAC ^ 2)
        }
      }

      Ierr      <- exp(IrndDevs) * ssModelData@ITrend
      Btimp     <- runif(1, MseDef@Btcv[1], MseDef@Btcv[2])
      Btb       <- trlnorm(1, 1, MseDef@Btbcv)
      Bterr     <- karray(trlnorm(allyears, rep(Btb, allyears), rep(Btimp, allyears)), c(allyears))

      Mb        <- trlnorm(1, 1, MseDef@Mbcv)
      Kb        <- trlnorm(1, 1, MseDef@Kbcv)
      Linfb     <- trlnorm(1, 1, MseDef@Linfbcv)
      t0b       <- 1

      MSYb      <- trlnorm(1, 1, MseDef@MSYbcv)
      BMSYb     <- trlnorm(1, 1, MseDef@BMSYbcv)
      IMSYb     <- trlnorm(1, 1, MseDef@IMSYbcv)
      FMSYb     <- trlnorm(1, 1, MseDef@FMSYbcv)
      FMSY_Mb   <- trlnorm(1, 1, MseDef@FMSY_Mbcv)

      nCAAobs   <- ceiling(runif(1, MseDef@nCAAobs[1], MseDef@nCAAobs[2]))

      ageMb     <- trlnorm(1, 1, MseDef@ageMbcv)

      # Fleet-specific Error multiplier for TAC and TAE applications
      TACEError <- karray(exp(rnorm(MseDef@nfleets) * MseDef@TACEcv - 0.5 * MseDef@TACEcv ^ 2), c(nfleets))


      # print a progress report
      if (!isMSY_projection)
      {
        cat(paste("Running MSE for ",MP, sep=""))
        cat("\n")

        # update the console
        flush.console()
      }

      # initial TAC and TAE values required for first MP application
      if (isMSY_projection)
      {
        TAC <- environment_MSY$TAC
      }
      else
      {
        TAC <- sum(CMCurrent) # refresh the MP store of TAC among simulations
      }

      TAE   <- karray(rep(0, nfleets), dim=c(nfleets))
      TACE  <- list(TAEbyF=TAE, TAC=TAC)

      .Object@TAC[initYear]     <- TAC
      .Object@TAEbyF[initYear,] <- TAE

      if (CppMethod != 0)
      {
        Om.nt.beginProjection(Obj, as.double(rep(log(0.001), nfleets)))
      }

      for (y in (nyears + 1 - nbackupyears):(nyears + proyears))     # redo initl year for reporting; test implications
      {
        PAYMRF[,3] <- y
        PAY[,3]    <- y

        #projection years loop includes repeat of first year for rec and initializtion ... seemingly not anymore?
        if (!isMSY_projection && (CppMethod == 0))
        {
          cat(".")
        }

        selTS <- sel

        if (selAgeRange >= 1)
        {
          #add an age shift to the selectivity noise
          selAge <- karray(rep(round(selTSSign[,2] * sin((y - nyears + nbackupyears - 1) * selTSWaveLen[,2]) * MseDef@selAgeRange), nages), dim=dim(sel))

          for (fi in 1:nfleets)
          {
            if (selAge[fi] > 0)
            {
              selTS[fi,(selAge[fi] + 1):nages]  <- sel[fi,1:(nages - selAge[fi])]
              selTS[fi,1:selAge[fi]]            <- sel[fi,1]
            }
            else
            {
              if (selAge[fi] < 0)
              {
                selTS[fi,1:(nages + selAge[fi])]          <- sel[fi,(1 - selAge[fi]):nages]
                selTS[fi,(nages + selAge[fi] + 1):nages]  <- sel[fi,nages]
              }
            }
          }
        }

        selExp    <- exp(selTSSign[,1] * sin((y - nyears  + nbackupyears - 1) * selTSWaveLen[,1]) * (selExpRange))
        selTS     <- selTS ^ rep(selExp, nages)
        CPUEselTS <- CPUEsel

        for (ai in 1:nareas)
        {
          CPUEselTS[,ai] <- selTS[MseDef@indexFisheries[ai],]
        }

        if ((y %in% c(nyears + proyears)))
        {
          if (Report)
          {
            #plot some original and  modified selectivities
            par(mfrow=c(4,4))

            # plot of selectivity temporal variability
            for (fi in 1:nfleets)
            {
              plot(sel[fi,], type='l', main='sel')
              lines(selTS[fi,], col=2)
            }

            for (ai in 1:min(16,nCPUE))
            {
              plot(CPUEsel[,ai], type='l', main="CPUE sel")
              lines(CPUEselTS[,ai], col=3)
            }
          }
        }

        if (!isMSY_projection)
        {
          # set the bridging Catches between the last OM year and the first MP year
          firstMPy <- nyears + MseDef@firstMPYr - MseDef@lastCalendarYr

          if (y < nyears + 1)
          {
            # run projection model from nyears - nbackupyears till now using historic catch data for TAC
            TAC   <- .Object@CM[y]
            TAE   <- karray(rep(0, nfleets), dim=c(nfleets))
            TACE  <- list(TAEbyF=TAE, TAC=TAC)

            # update CAA and CAL
            nuy     <- (y - 2):(y - 1)
            nCAA    <- sampCatch(apply(CAAF[,keep(nuy),], c(1,2), sum), nCAAobs)
            nCAAInd <- sampCatch(apply(CAAF[,keep(nuy),MseDef@indexFisheries], c(1,2), sum), nCAAobs)

            CAA     <- abind(CAA, nCAA, along=2)
            CAAInd  <- abind(CAAInd, nCAAInd, along=2)
            CAL     <- abind(CAL, makeCAL(nCAA, Len_age_mid[1,,nuy], CAL_bins), along=2)
            CALInd  <- abind(CALInd, makeCAL(nCAAInd, Len_age_mid[1,,nuy], CAL_bins), along=2)
          }
          else if (y < firstMPy)
          {
            yBridge <- y - nyears

            if ((yBridge <= length(MseDef@catchBridge)) && (sum(MseDef@catchBridge) > 0))
            {
              # use the known aggregate catch imported from the OMd
              TAC   <- MseDef@catchBridge[yBridge]
            }
            else
            {
              #use the previous aggregate catch+error
              TAC   <- TAC * exp(rnorm(length(TAC)) * MseDef@catchBridgeCV - 0.5 * MseDef@catchBridgeCV ^ 2)
            }

            TAE   <- karray(rep(0, nfleets), dim=c(nfleets))
            TACE  <- list(TAEbyF=TAE, TAC=TAC)
          }
        }

        # Calculate new CMCurrent based on recommended TAC by fishery.
        # Note that this only works for all fisheries under TAC.
        if ((y                                   == firstMPy) &&
            (length(MseDef@recommendedTACbyF)    == nfleets ) &&
            all(!is.na(MseDef@recommendedTACbyF) == TRUE))
        {
          SAFl <- as.matrix(expand.grid(1:nsubyears,1:nareas,1:nfleets))
          Fl   <- SAFl[,c(3)]

          # Calculate a new CMCurrent that has the allocation of recommendedTACbyF
          # by the subyear-area distribution of the original CMCurrent and set
          # the new starting TAC
          if (isMSY_projection)
          {
            recommendedTACbyF <- TAC * MseDef@recommendedTACbyF / sum(MseDef@recommendedTACbyF)
          }
          else
          {
            recommendedTACbyF <- MseDef@recommendedTACbyF
          }

          CMCurrent_byFl  <- apply(ssModelData@CMCurrent, sum, MARGIN=c(3))
          CMCurrent[SAFl] <- recommendedTACbyF[Fl] * ssModelData@CMCurrent[SAFl] / (CMCurrent_byFl[Fl] + 1e-3)
          TAC             <- sum(CMCurrent)
          TAE             <- karray(rep(0, nfleets), dim=c(nfleets))
          TACE            <- list(TAEbyF=TAE, TAC=TAC)
        }

        if (!isMSY_projection && (y %in% upyrs))
        {
          #CPUEobsY based on merger of observed and simulated CPUE (proportional to IObs in projections)
          #calculate CPUE up to but not including the current year (MP does not necessarily have access to this value depending on data lag)
          .Object@CPUEobsY[(nyears - nbackupyears):(y - 1)] <- qCPUE * (NLLI[(nyears - nbackupyears):(y - 1)] ^ Ibeta) * Ierr[(nyears - nbackupyears):(y - 1)]
          Iobs                                              <- .Object@CPUEobsY[1:(y - 1)]

          # Operate P A Y M R          #MP applied in these years only

          # Simulate imperfect information --------------------------------------

          if (y >= firstMPy)
          {
            #update data
            if (y == firstMPy)
            {
              nuy <- (nyears + 1):(firstMPy - 1)
            }
            else
            {
              nuy <- (upyrs[match(y, upyrs) - 1]):(y - 1)
            }

            nCAA    <- sampCatch(apply(CAAF[,keep(nuy),], c(1,2), sum), nCAAobs)
            nCAAInd <- sampCatch(apply(CAAF[,keep(nuy),MseDef@indexFisheries], c(1,2), sum), nCAAobs)

            CAA     <- abind(CAA, nCAA, along=2)
            CAAInd  <- abind(CAAInd, nCAAInd, along=2)
            CAL     <- abind(CAL, makeCAL(nCAA, Len_age_mid[1,,nuy], CAL_bins), along=2)
            CALInd  <- abind(CALInd, makeCAL(nCAAInd, Len_age_mid[1,,nuy], CAL_bins), along=2)
          }

          Cobs <- apply(.Object@CM[,1:(y - 1)], c(2), sum) * Cerr[1:(y - 1)]

          # xxx zzz MP rate parameters presumably need to be harmonized for YFT as well
          # xxx zzz need to finish growth curve substitution flagged ###
          # missing dimension problem temporarily commented out...
          pset<-list("Cobs"     = Cobs[1:(y - MseDef@MPDataLag)],
                     #"K"        = K[1,y - 1] * Kb,
                     #"Linf"     = Linf[1,y - 1] * Kb,
                     "t0"       = t0b[1],
                     "M"        = M[1,,(y - 1)] * Mb,
                     "MSY"      = RefVars@MSY * MSYb,
                     "BMSY"     = RefVars@BMSY * BMSYb,
                     "UMSY"     = RefVars@UMSY * FMSYb,
                     "a"        = ssModelData@a,
                     "b"        = ssModelData@b,
                     "nages"    = nages,
                     "Mat"      = mat[1,,1:(y - 1)],

                     #need to lag data available for HCR by appropriate amount (OM@MPDataLag)
                     "CMCsum"   = sum(CMCurrent),
                     "UMSY_PI"  = RefVars@UMSY,
                     "Iobs"     = Iobs[keep(1:(y - MseDef@MPDataLag))],
                     "CAA"      = CAA[,keep(1:(y - MseDef@MPDataLag))],
                     "CAL"      = CAL[,keep(1:(y - MseDef@MPDataLag))],
                     "CALInd"   = CALInd[,keep(1:(y - MseDef@MPDataLag))],
                     "CAL_bins" = CAL_bins,
                     "prevTACE" = TACE,
                     "y"        = y - MseDef@MPDataLag,
                     "tune"     = tune,
                     "interval" = interval,
                     "env"      = MP_environment) # env provides historic data storage for MP implementations

          # run the MP
          TACE <- get(MP)(pset)
        }

        # Start of annual projection
        #---------------------------------------------------------------------
        if (isMSY_projection)
        {
          TAC    <- TACE$TAC
          TAEbyF <- karray(TACE$TAEbyF, dim=c(nfleets))

          recSpatialDevs   <- karray(1.0, dim=dim(Recdist))
          UpdateRecentEbyF <- TRUE
        }
        else
        {
          # Unpack MP TACs and TAEs
          TAC    <- TACE$TAC
          TAEbyF <- karray(TACE$TAEbyF, dim=c(nfleets))
          #if the fleet has a TAE, this vector is used to exclude the fleet from the TAC extractions

          .Object@TAC[y]     <- TAC
          .Object@TAEbyF[y,] <- TAEbyF

          if ((length(MseDef@ImplErrBias) > 1) && (y >= firstMPy))
          {
            if (MseDef@ImplErrBias[y - nyears] < 0)
            {
              TAC              <- 0.0
              TAEbyF           <- RecentEbyF * abs(MseDef@ImplErrBias[y - nyears])
              UpdateRecentEbyF <- FALSE
            }
            else if (MseDef@ImplErrBias[y - nyears] > 0)
            {
              TAC              <- TAC * MseDef@ImplErrBias[y - nyears]
              UpdateRecentEbyF <- TRUE
            }
            else
            {
              UpdateRecentEbyF <- TRUE
            }
          }

          #Spatial devs in rec (affect spatial distribution but not total; streamlined implementationsame for all sims, pops, area)
          recSpatialDevs <- karray(exp(ssModelData@ReccvR * rnorm(length(Recdist))), dim=dim(Recdist))
          recSpatialDevs <- recSpatialDevs / karray(rep(apply(recSpatialDevs, FUN=mean, MARGIN=c(1)), nareas),dim=dim(Recdist))
        }

        if (CppMethod != 0)
        {
          RecdevIndex <- (y - 1) * nSpawnPerYr + 1
          Recdevs_Y   <- Recdevs[,RecdevIndex:(RecdevIndex + nSpawnPerYr - 1)]

          Om.nt.projection(Obj,
                           y,
                           as.integer(if (Report) 1 else 0),
                           EffortCeiling,
                           TAC,
                           TAEbyF,
                           TACEError,
                           ECurrent,
                           CMCurrent,
                           q,
                           R0,
                           M[,,y],
                           mat[,,y],
                           Idist,
                           Len_age[,,y],
                           Wt_age[,,y],
                           Wt_age_mid[,,y],
                           selTS,
                           mov,
                           h,
                           Recdist,
                           Recdevs_Y,
                           recSpatialDevs,
                           ssModelData@SRrel,
                           N_Y,
                           NBefore_Y,
                           SSN_Y,
                           C_Y,
                           SSBA_Y,
                           as.integer(100))

          if (UpdateRecentEbyF)
          {
            RecentEbyF <- Om.get.nt.LastEbyF(Obj)
          }
        }
        else  # R roption
        {
          # distribute TAC by season and fleet for all sims, for all fishries that do not have TAEs
          isTACFleet        <- karray(NA, dim=dim(CMCurrent))
          MRFim             <- as.matrix(expand.grid(1:nsubyears, 1:nareas, 1:nfleets))
          Fim               <- MRFim[,c(3)]
          isTACFleet[MRFim] <- karray((!TAEbyF) * 1.0, dim=c(nfleets))[Fim]  # exclude TAC fleets
          TACbyMRF          <- TAC * CMCurrent * isTACFleet / sum(CMCurrent * isTACFleet)

          if (UpdateRecentEbyF)
          {
            RecentEbyF[1:nfleets] <- 0.0
            TotalCbyF             <- rep(1.0e-10, times=nfleets) # Initialise to non-zero but small value to avoid NA
          }

          for (mm in 1:nsubyears)
          {

            PAYMRF[,4]  <- mm
            PAMRF[,3]   <- mm
            PAMR[,3]    <- mm
            MRF[,1]     <- mm

            N_Y[,,mm,]   <- NBefore_Y[,,mm,]
            SSN_Y[,,mm,] <- NBefore_Y[,,mm,] * karray(rep(mat[,,y],times=nareas), c(npop,nages,nareas))
            #potential change
            SSBA_Y        <- apply(SSN_Y[,,mm,] * karray(rep(Wt_age[,,y],times=nareas), dim=c(npop,nages,nareas)), c(1), sum, na.rm=T)
            #SSBA_Y        <- apply(NBefore_Y[,,mm,] * karray(rep(Wt_age_SB[,,y],times=nareas), dim=c(npop,nages,nareas)), c(1), sum, na.rm=T)

            # do recruitment
            if(mm %in% ssModelData@Recsubyr)
            {
              for(pp in 1:npop)
              {
                # ie every qtr for YFT
                RecdevInd <- (y - 1) * nSpawnPerYr + mm

                # recruit fish
                if (ssModelData@SRrel[pp] == 1)
                {
                  # Beverton-Holt recruitment
                  rec <- Recdevs[pp,RecdevInd] * ((0.8 * R0[pp] * h[pp] * SSBA_Y[pp]) / (0.2 * SSBpR[pp] * R0[pp] * (1 - h[pp]) + (h[pp] - 0.2) * SSBA_Y[pp]))
                }
                else
                {
                  # Most transparent form of the Ricker uses alpha and beta params
                  rec <- Recdevs[pp,RecdevInd] * aR[pp] * SSBA_Y[pp] * exp(-bR[pp] * SSBA_Y[pp])
                }

                N_Y[pp,1,mm,]       <- rec * Recdist[pp,] * recSpatialDevs[pp,]
                NBefore_Y[pp,1,mm,] <- N_Y[pp,1,mm,]
              }
            }

            # move fish (order of events altered from original)
            if (nareas > 1)
            {
              N_Y[,,mm,] <- projection.domov(Ntemp=karray(N_Y[,,mm,], dim=c(npop,nages,nareas)),
                                             movtemp=karray(mov[,,mm,,], dim=c(npop,nages,nareas,nareas)))
            }

            if (UpdateRecentEbyF)
            {
              N_Ystart <- apply(N_Y[,,mm,], FUN=sum, MARGIN=c(2,3))
            }

            #---------------------------------------------------------------------
            # Use the new population dynamics for mix of TACs and TAEs
            # Pope's approximation; values up to ~0.6 may be substantially closer
            # to Baranov depending on TAC, TAE and M

            #TACTime <- 0.5

            CNTACbyPARF <- karray(0, dim=c(npop,nages,nareas,nfleets))
            CNTAEbyPARF <- karray(0, dim=c(npop,nages,nareas,nfleets))

            # Fishing mort for TAE-managed fleets
            FTAE    <- karray(0, dim=c(npop,nages,nareas,nfleets)) #by fleet
            FTAEAgg <- karray(0, dim=c(npop,nages,nareas))         #aggregated over fleets

            # Define some index matrices
            PARFim <- PAYMRF[,c(1,2,5,6)]
            Fim    <- PAYMRF[,c(6)]
            MRFim  <- PAYMRF[,c(4,5,6)]

            # TAE Fs by fleet
            FTAE[PARFim] <- ECurrent[MRFim] * TAEbyF[Fim] * selTS[FA] * q[F] * TACEError[Fim]

            #sum TAE Fs over fleets
            FTAEAgg <- apply(FTAE, MARGIN=c(1:3), sum)

            #---------------------------------------------------------------------
            # first half timestep natural mort and F for TAEs (before TAC)

            CNTAEbyPARF[PARFim] <- FTAE[PARFim] / (FTAEAgg[PAR] + M[PAY] / nsubyears) * (1.0 - exp(-TACTime * (M[PAY] / nsubyears + FTAEAgg[PAR]))) * N_Y[PAMR]
            N_Y[PAMR]           <- N_Y[PAMR] * exp(-TACTime * (M[PAY] / nsubyears + FTAEAgg[PAR]))

            #---------------------------------------------------------------------
            # mid-year TAC extraction

            CNTACbyPAR <- karray(0.0, dim=c(npop,nages,nareas))

            # skip if all TACs = 0
            if ((sum(isTACFleet) > 0) && (TAC > 0.0))
            {
              # xxx still need to add TAC implementation error
              # xxx some of this is probably incorrect for multi-population case

              # Vulnerable biomass and numbers for each fishery by pop
              VBbyPARF    <- karray((N_Y[PAMR] * Wt_age_mid[PAY] * selTS[FA]), dim=c(npop,nages,nareas,nfleets))

              # VB summed over ages (and populations)
              VBbyRF      <- apply(VBbyPARF,sum,MARGIN=c(3,4))

              # U for each fishery and region independently (TAC could be unachievable , i.e. U>1)
              UbyRFtest                <- TACbyMRF[mm,,] / VBbyRF[]
              UbyRFtest[UbyRFtest > 9] <- 9 #bound ridiculous U to prevent exp(U) -> inf

              # U for each age by region and fishery  (could be unachievable, i.e. U>1)
              UbyARFtest      <- karray(NA, dim=c(nages,nareas,nfleets))
              UbyARF          <- karray(NA, dim=c(nages,nareas,nfleets))
              UbyARFtest[ARF] <- UbyRFtest[RF] * selTS[FA] * q[F] * TACEError[Fim]

              # U for each age by region aggregated over fisheries
              UbyARtest <- apply(UbyARFtest, sum, MARGIN=c(1:2))

              # ad hoc limit on U
              UbyAR                    <- UbyARtest
              UbyAR[UbyARtest > rULim] <- rULim+(1-rULim-0.3)*(1-exp(-UbyARtest[UbyARtest > rULim]+rULim))

              # rULim=50% original: proportional to U=0.5, then approaches an asymptote of 0.88;
              #if(rULim == 0.5) UbyAR[UbyARtest > 0.5] <- exp(UbyARtest[UbyARtest > 0.5]) / (1.0 + exp(UbyARtest[UbyARtest > 0.5])) - 0.122
              # rULim=30% - seemingly best (coupled with the TACtime=0.01); proportional to U=0.3, then approaches an asymptote of 0.28
              #if(rULim == 0.3) UbyAR[UbyARtest > 0.3] <- exp(UbyARtest[UbyARtest > 0.3]) / (1.0 + exp(UbyARtest[UbyARtest > 0.3])) - 0.28
              #test  H65
              #UbyAR[UbyARtest > 0.65] <- exp(UbyARtest[UbyARtest > 0.65]) / (1.0 + exp(UbyARtest[UbyARtest > 0.65])) - 0.01
              #test  H10
              #UbyAR[UbyARtest > 0.1] <- exp(UbyARtest[UbyARtest > 0.1]) / (1.0 + exp(UbyARtest[UbyARtest > 0.1])) - 0.43
              #test  H99
              #UbyAR[UbyARtest > 0.99] <- 0.99

              # rescale U for each fishery as a proportion of the fishery-aggregate U (only relevant for those that exceed U limit)
              UbyARF[ARF] <- UbyAR[AR] * UbyARFtest[ARF] / UbyARtest[AR]

              # TAC catch in numbers (U identical for all pops, xxx probably not correct for multiple pops)
              CNTACbyPARF[PARFim] <- UbyARF[ARF] * N_Y[PAMR]

              # aggregate catch over fleets
              CNTACbyPAR <- karray(apply(CNTACbyPARF, sum, MARGIN=c(1:3)), dim=c(npop,nages,nareas))

              # Update N post-TAC extraction
              N_Y[PAMR] <- N_Y[PAMR] - CNTACbyPAR[PAR]
            }

            #---------------------------------------------------------------------
            # Second TAE extraction (and M), following TAC extraction

            # Catch from first TAE + second TAE extraction
            CNTAEbyPARF[PARFim] <- CNTAEbyPARF[PARFim] + FTAE[PARFim] / (FTAEAgg[PAR] + M[PAY] / nsubyears) * (1.0 - exp(-(1-TACTime) * (M[PAY] / nsubyears + FTAEAgg[PAR]))) * N_Y[PAMR]
            N_Y[PAMR]           <- N_Y[PAMR] * exp(-(1 - TACTime) * (M[PAY] / nsubyears + FTAEAgg[PAR]))
            CNTAEbyPAR          <- karray(apply(CNTAEbyPARF, sum, MARGIN=c(1:3)), dim=c(npop,nages,nareas))

            # Total Catch in numbers from TAE and TAC
            C_Y[PAMRF] <- CNTAEbyPARF[PARF] + CNTACbyPARF[PARFim]

            if (Report)
            {
              # aggregate catch in mass for rep 1
              CMTACsum <- sum(CNTACbyPAR * karray(rep(Wt_age_mid[,,y],nareas),dim=c(npop, nages, nareas)))
              CMTAEsum <- sum(CNTAEbyPAR * karray(rep(Wt_age_mid[,,y],nareas),dim=c(npop, nages, nareas)))

              print("CMass TAC, TAE, combined:")
              print(CMTACsum)
              print(CMTAEsum)
              print(CMTACsum + CMTAEsum)
            }

            if (UpdateRecentEbyF)
            {
              ARF2   <- as.matrix(expand.grid(1:nages, 1:nareas, 1:nfleets))
              AR2    <- ARF2[,c(1,2)]
              RF2    <- ARF2[,c(2,3)]
              FA2    <- ARF2[,c(3,1)]
              F2     <- ARF2[,c(3)]
              EbyARF <- karray(0.0, dim=c(nages,nareas,nfleets))

              # Need to figure out what the recent effective EbyF is. Find Z and
              # fish deaths (Mort) by pop,age and area; find catch by age, area
              # and fleet; find total catch by fleet; find F by age, area and
              # fleet; finally find RecentEbyF by doing a weighted sum of E by
              # age, area and fishery. We weight it according to catch so that
              # large catch EbyARF contributes the most to the final result. This
              # should help minimise any bias in the estimate.
              N_Yend       <- apply(N_Y[,,mm,], FUN=sum, MARGIN=c(2,3))
              Ztot         <- -log(N_Yend / N_Ystart)
              Mort         <- N_Ystart - N_Yend
              CbyARF       <- apply(C_Y[,,mm,,], FUN=sum, MARGIN=c(2,3,4))
              SumCbyF      <- apply(CbyARF, FUN=sum, MARGIN=c(3))
              TotalCbyF    <- TotalCbyF + SumCbyF
              Fest         <- Ztot[AR2] * CbyARF[ARF2] / Mort[AR2]
              EbyARF[ARF2] <- (CbyARF[ARF2] / SumCbyF[F2]) * (Fest / (ECurrent[mm,,][RF2] * selTS[FA2] * q[F2] * TACEError[F2]))
              RecentEbyF   <- RecentEbyF + (SumCbyF * apply(EbyARF, FUN=sum, MARGIN=c(3), na.rm=TRUE))

              if (mm == nsubyears)
              {
                RecentEbyF <- RecentEbyF / TotalCbyF
                selectTAE  <- (TAEbyF != 0) * 1.0
                selectTAC  <- (TAEbyF == 0) * 1.0
                RecentEbyF <- (TAEbyF * selectTAE) + (RecentEbyF * selectTAC)
              }
            }

            # end harvest calculations
            #---------------------------------------------------------------------
            #  age individuals
            NBefore_Y[,nages,mm + 1,]          <- N_Y[,nages - 1,mm,] + N_Y[,nages,mm,]
            NBefore_Y[,2:(nages - 1),mm + 1,]  <- N_Y[,1:(nages - 2),mm,]
            NBefore_Y[,1,mm + 1,]              <- 0
          } # mm (season) loop
        } # R option in R/cpp cnditional

        # calculate LL selected numbers for the year for the abundance index
        NLLbyAMR <- apply(N_Y[,keep(1:nages),1:nsubyears,], MARGIN=c(2,3,4), FUN=sum, na.rm=T)

        for (isubyears in 1:nsubyears)
        {
          for (iCPUE in 1:nCPUE)
          {
            NLL_Y[isubyears,iCPUE] <- sum(NLLbyAMR[,isubyears,ssModelData@CPUEFleetAreas[iCPUE]] * CPUEselTS[,iCPUE])
          }
        }

        NLLI_Y <- sum(NLL_Y)

        # copy results back into historic data
        .Object@SSB[,y] <- SSBA_Y
        NLLI[y]         <- NLLI_Y
        NLL[y,,]        <- NLL_Y

        # Store results ...
        .Object@CM[,y]     <- apply(C_Y * karray(Wt_age_mid[,,y], c(npop,nages,nsubyears,nareas,nfleets)), c(1), sum)
        .Object@CMbyF[,y,] <- rep(apply(C_Y * karray(Wt_age_mid[,,y], c(npop,nages,nsubyears,nareas,nfleets)), c(1,5), sum))

        CAAF[,y,]      <- apply(C_Y, c(2,5), sum)
        .Object@Rec[y] <- apply(NBefore_Y[,1,keep(1:nsubyears),], c(1), sum)
        BbyS           <- apply(NBefore_Y[,,keep(1:nsubyears),] * karray(Wt_age[,,y], c(npop,nages,nsubyears,nareas)), c(1,3), sum)
        .Object@B[,y]  <- apply(BbyS, c(1), mean)

        FirstIdx <- (y - 1) * nsubyears + 1
        LastIdx  <- y * nsubyears

        .Object@RecYrQtr[FirstIdx:LastIdx] <- apply(NBefore_Y[,1,keep(1:nsubyears),], c(1,2), sum)

        # Calculate Frep
        NsoRbyPAM    <- apply(NBefore_Y[,,keep(1:(nsubyears+1)),], FUN=sum, MARGIN=c(1:3))
        .Object@F[y] <- findFrep(NsoRbyPAM, M[,,y], npop, nages, nsubyears, FAgeRange)

        # set up next year starting point
        NBefore_Y[,,1,] <- NBefore_Y[,,nsubyears + 1,]

        #---------------------------------------------------------------------
        # End of annual projection

      } # projection year loop

      if (isMSY_projection)
      {
        if (environment_MSY$optimisation)
        {
          MSY <- sum(karray(C_Y[targpop,,,,], c(length(targpop),nages,nsubyears,nareas,nfleets)) *
                     karray(Wt_age[targpop,,nyears], c(length(targpop),nages,nsubyears,nareas,nfleets)))

          environment_MSY$Likelihood <- -log(MSY) + 100.0 * log(TAC / MSY)
        }
        else
        {
          SAFl <- as.matrix(expand.grid(1:nsubyears,1:nareas,1:nfleets))
          Fl   <- SAFl[,c(3)]

          # Update ECurrent to include required effort scaling to obtain TAC
          ECurrent[SAFl] <- RecentEbyF[Fl] * ECurrent[SAFl]

          environment_MSY$ReferencePoints <- MSYreferencePoints(ECurrent,
                                                                sel,
                                                                M,
                                                                C_Y,
                                                                SSB0,
                                                                B0,
                                                                N_Y,
                                                                NBefore_Y,
                                                                SSN_Y,
                                                                Wt_age,
                                                                #Wt_age_SB,
                                                                targpop,
                                                                nsubyears,
                                                                npop,
                                                                nages,
                                                                nareas,
                                                                nfleets,
                                                                FAgeRange)
        }
      }
      else
      {
        # call the MP as a hook to plot diagnostics at the end of a run.
        # "complete" key will be defined.
        pset$complete <- TRUE
        get(MP)(pset)
      }

      # Store results ...
      # archive timing may not be entirely consistent with SS for all quantitities,
      # but should be internally consistent
      # recalculate CPUE so last years can be reported even if they are outside of MP years

      .Object@CPUEobsY[(nyears - nbackupyears):y] <- qCPUE * (NLLI[(nyears - nbackupyears):y] ^ Ibeta) * Ierr[(nyears - nbackupyears):y]
      .Object@IobsArchive                         <- .Object@CPUEobsY[1:y]

      NLLR            <- apply(NLL[1:y,,], sum, MARGIN=c(1,3))
      IobsR           <- qCPUE * (NLLR ^ Ibeta) * rep(Ierr, nCPUE)

      .Object@IobsRArchive[1:y,] <- IobsR[1:y,]

      # Debugging code to visualise the CPUE fit of supplied CPUE series after scaling
      if (!isMSY_projection & FALSE)
      {
        p2 <- qCPUE * (NLLI ^ Ibeta) #* Ierr

        plot((1:allyears) + MseDef@firstCalendarYr - 1, .Object@CPUEobsY, yaxs="i", ylim=c(0,37), main=MseDef@OMList[[ssModelData@which]] %&% " \n%CV " %&% round(MPcpueRMSE * 100) %&% "     rho " %&% (round(IAC * 100) / 100))
        lines((1:allyears) + MseDef@firstCalendarYr - 1, ssModelData@CPUEobsY)
        points((1:allyears) + MseDef@firstCalendarYr - 1, p2, pch=2, col=2)

        if (max(abs(.Object@CPUEobsY[nyears:(y - 1)] - .Object@CPUEobsY[(nyears + 1):y])) > 5)
        {
          browser()
        }
      }

      if (CppMethod != 0)
      {
        Om.destroy(Obj)
      }

      flush.console()

      return (.Object)
    }

    ErrorLog <- NULL

    errorHandler <- function(e)
    {
      # this code truncates the call list because in parallel contexts
      # it ends up going to a really high context and then quotes the
      # entire root object content. This keeps the content small
      # but with enough context to be useful.
      Calls <- sys.calls()
      Len   <- length(Calls)

      for (Idx in 1:Len)
      {
        if (regexpr("withCallingHandlers", Calls[Idx]) > 0)
        {
          Calls <- Calls[Idx:Len]
          break
        }
      }

      # archive truncated call list
      ErrorLog <<- paste(paste(Calls), collapse="\n")
    }

    # We put the tryCatch here so we can have the object initialised to a
    # decent state and to capture the error output into the object instance
    Result <- tryCatch(withCallingHandlers(initFn(.Object), error=errorHandler),
                       error=function(e) print(e))

    if (isS4(Result))
    {
      .Object <- Result
    }

    if (!is.null(ErrorLog))
    {
      .Object@Log[["error"]] <- ErrorLog
    }

    if (!is.null(MP_environment$save) && (MP_environment$save == TRUE))
    {
      .Object@Log[["env"]] <- MP_environment
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------
# Helper function to determine parameters for correlated random deviations
# -----------------------------------------------------------------------------
findWindowedAR_params <- function(Alpha)
{
  if (Alpha > 0.0)
  {
    WindowedAR_Params <- function(Alpha)
    {
      # Auto-regressive filter impulse response estimate
      limit <- 1e-12
      Count <- max(floor(log(limit) / log(abs(Alpha) + 0.001)), 20)
      IR    <- array(0.0, dim=c(Count))
      IR[1] <- 1.0

      for (t in 2:Count)
      {
        IR[t] <- Alpha * IR[t - 1] + IR[t]
      }

      # convolve with 4 point summing window (ie. effect of summing over quarters)
      IR2in <- c(0,0,0,IR,0,0,0,0)
      IR2   <- array(0.0, dim=c(Count))

      for (t in 1:Count)
      {
        IR2[t] <- sum(IR2in[t:(t+3)])
      }

      # Variance gain for AR filter plus 4 point summing window
      Gain <- sum(IR2 * IR2)
      Beta <- 1.0 / sqrt(Gain)

      # normalise Impulse response and find quarterly correlation
      IR2 <- IR2 * Beta
      Cor <- sum(IR2[1:(Count - 4)] * IR2[5:Count])

      return (list(Beta=Beta, Cor=Cor))
    }

    res <- optim(Alpha,
                 function(x)
                 {
                   (Alpha - WindowedAR_Params(x)$Cor) ^ 2
                 },
                 method = "L-BFGS-B")

    params <- WindowedAR_Params(res$par)

    return (list(Alpha=res$par, Beta=params$Beta, Cor=params$Cor))
  }
  else
  {
    # negative correlation can't be re-scaled according to annual so
    # leave as is.
    return (list(Alpha=Alpha, Beta=(1 - Alpha ^ 2) ^ 0.5, Cor=Alpha))
  }
}
