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
  function(.Object, ssModelData, MseDef, Report = FALSE)
  {
    getMSYrefs <- function(.Object, CppMethod, nyears=40, toly=1e-1)
    {
      # -----------------------------------------------------------------------
      # Common reference point calculation function
      # -----------------------------------------------------------------------
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
        MSY    <- sum(karray(C[targpop,,,,], c(length(targpop), nages, nsubyears, nareas, nfleets)) * karray(Wt_age[targpop,], c(length(targpop),nages,nsubyears,nareas,nfleets)))
        BMSY   <- sum(karray((NBefore[targpop,,1,]), c(length(targpop),nages,nareas)) * karray(Wt_age[targpop,], c(length(targpop),nages,nareas)))
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
                     karray(rep(Wt_age[targpop,],nsubyears), c(length(targpop),nages,nsubyears,nareas)))

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

        MbyPAM    <- karray(rep(M[,1:(nages-2)],times=nsubyears), dim=c(npop,nages-2,nsubyears))
        FsoRbyPAM <- ZsoRbyPAM - MbyPAM / nsubyears

        FMSY1     <- mean(FsoRbyPAM[,FAgeRange[1]:FAgeRange[2],])  # 2:27 = true ages 1:26 (1:26 used by SS)

        #potential change to integrated biomass calculation
        SSBMSY    <- sum(karray(SSN[targpop,,1,],c(length(targpop),nages,nareas)) * karray(Wt_age[targpop,], c(length(targpop),nages,nareas)))
        #SSBMSY     <- sum(karray(NBefore[targpop,,1,],c(length(targpop),nages,nareas)) * karray(Wt_age_SB[targpop,], c(length(targpop),nages,nareas)))

        UMSY      <- MSY / VBMSY
        SSBMSY_B0 <- SSBMSY / sum(SSB0[targpop])

        return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSY1,SSBMSY_B0,SSB0,B0))
      }

      if (CppMethod)
      {
        Obj <- Om.create(.Object@npop,
                         .Object@nages,
                         .Object@nsubyears,
                         .Object@nareas,
                         .Object@nfleets,
                         .Object@Recsubyr)

        MinPar        <- as.double(rep(NA, 1))
        MSY           <- as.double(rep(NA, 1))
        BMSY          <- as.double(rep(NA, 1))
        SSBMSY        <- as.double(rep(NA, 1))
        SSBMSY_B0     <- as.double(rep(NA, 1))
        N             <- karray(as.double(NA),c(.Object@npop, .Object@nages, .Object@nsubyears + 1, .Object@nareas))
        NBefore       <- karray(as.double(NA),c(.Object@npop, .Object@nages, .Object@nsubyears + 1, .Object@nareas))
        SSN           <- karray(as.double(NA),c(.Object@npop, .Object@nages, .Object@nsubyears, .Object@nareas))
        C             <- karray(as.double(NA),c(.Object@npop, .Object@nages, .Object@nsubyears, .Object@nareas, .Object@nfleets))
        SSBA          <- karray(as.double(NA),c(.Object@npop))

        Om.nt.findMSYrefs(Obj,
                          as.integer(if (Report) 1 else 0),
                          .Object@ECurrent,
                          .Object@q,
                          .Object@R0,
                          .Object@M[,,1],
                          .Object@mat[,,1],
                          .Object@Idist,
                          .Object@Len_age[,,1],
                          .Object@Wt_age[,,1],
                          .Object@sel,
                          .Object@mov,
                          .Object@h,
                          .Object@Recdist,
                          .Object@SRrel,
                          N,
                          NBefore,
                          SSN,
                          C,
                          SSBA,
                          as.integer(length(MseDef@targpop)),
                          as.integer(MseDef@targpop),
                          as.integer(nyears),
                          MinPar,
                          MSY,
                          BMSY,
                          SSBMSY,
                          SSBMSY_B0,
                          as.integer(1000))

        B0   <- OmB.get.nt.B0(Obj)
        SSB0 <- OmB.get.nt.SSB0(Obj)

        print("Effort multiplier at MSY:")
        print(exp(MinPar))

        refs <- MSYreferencePoints(.Object@ECurrent,
                                   .Object@sel,
                                   .Object@M[,,1],
                                   C,
                                   SSB0,
                                   B0,
                                   N,
                                   NBefore,
                                   SSN,
                                   .Object@Wt_age[,,1],
                                   #.Object@Wt_age_SB[,,1],
                                   MseDef@targpop,
                                   .Object@nsubyears,
                                   .Object@npop,
                                   .Object@nages,
                                   .Object@nareas,
                                   .Object@nfleets,
                                   .Object@FAgeRange)

        # delete the object
        Om.destroy(Obj)

        return (refs)
      } else
      {
        # -----------------------------------------------------------------------
        # R based population dynamics function
        # -----------------------------------------------------------------------
        popdyn <- function(par,
                           reportIndicators,
                           npop,
                           nages,
                           nyears,
                           nsubyears,
                           nareas,
                           nfleets,
                           R0,
                           M,
                           mat,
                           Idist,
                           Len_ageVec,
                           Wt_ageVec,
                           #Wt_age_SBVec,
                           q,
                           nSpawnPerYr=4,
                           a,
                           b,
                           Recdist,
                           ECurrent,
                           sel,
                           mov,
                           Recsubyr,
                           h,
                           Recdevs,
                           SRrel,
                           targpop=NA,
                           MSYyear=1,
                           loud=FALSE,
                           FAgeRange)
        {
          # movement function
          popdyn.domov <- function(Ntemp, movtemp)
          {
            # P A R  x  P A R R
            nareas <- dim(movtemp)[4]

            apply(karray(Ntemp, c(dim(Ntemp),nareas)) * movtemp, c(1,2,4), sum)
          }

          #Effort multiplier for ECurrent
          totF <- exp(par)

          # Need to get initial equilibrium numbers right because SSB0 and rec depend on it
          # NBefore recorded before M, hence the M=0 for a=1; integral for lastAge added (important for low M scenatios)
          Madvanced           <- karray(as.double(NA),c(npop,nages))
          Madvanced[1:npop,1] <- 0.0
          Madvanced[,2:nages] <- M[,1:(nages-1),1]
          surv                <- t(exp(-apply(Madvanced[,1:nages], c(1), cumsum) / nsubyears))

          #infinite sum for plus group
          surv[,nages] <- surv[,nages-1]*exp(-Madvanced[,nages]/nsubyears)/(1-exp(-Madvanced[,nages]/nsubyears))

          Wt_age    <- karray(NA, c(npop,nages,nyears))
          #Wt_age_SB <- karray(NA, c(npop,nages,nyears))

          ind    <- as.matrix(expand.grid(1:npop, 1:nages, 1:nyears))
          indo   <- as.matrix(expand.grid(1:npop, 1:nages, 1:nyears))

          indo[,3] <- rep(MSYyear, npop * nages * nyears)
          M        <- karray(M[indo], c(npop,nages,nyears))
          mat      <- karray(mat[indo], c(npop,nages,nyears))

          indAPY      <- as.matrix(expand.grid(1:nages, 1:npop, 1:nyears))
          Wt_age[ind]    <- karray(rep(Wt_ageVec, times=npop * nyears), dim=c(nages,npop,nyears))[indAPY]
          #Wt_age_SB[ind] <- karray(rep(Wt_age_SBVec, times=npop * nyears), dim=c(nages,npop,nyears))[indAPY]

          #NBefore probably not required in equilibrium calcs, but added for ease of reading
          N_Y       <- karray(NA,c(npop,nages,nsubyears + 1,nareas))      # only need aggregated catch for these purposes
          NBefore_Y <- karray(NA,c(npop,nages,nsubyears + 1,nareas))      # only need aggregated catch for these purposes
          SSN_Y     <- karray(NA,c(npop,nages,nsubyears,nareas))          # only need aggregated catch for these purposes
          SSB_Y     <- karray(NA,c(npop,nages,nsubyears,nareas))          # only need aggregated catch for these purposes
          B_Y       <- karray(NA,c(npop,nages,nsubyears,nareas))          # only need aggregated catch for these purposes
          C_Y       <- karray(NA,c(npop,nages,nsubyears,nareas,nfleets))  # Catch

          Z    <- karray(NA,c(npop,nages,nareas))
          FD   <- karray(NA,c(nfleets,nareas))            # F distribution
          FM   <- karray(NA,c(npop,nages,nareas,nfleets)) # Fishing Mortality

          y <- 1
          m <- 1

          PAYMR <- as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas))    # Set up some karray indexes
          PAMR  <- PAYMR[,c(1,2,3,5)]
          PA    <- PAYMR[,c(1,2)]
          P     <- PAYMR[,c(1)]
          PAR   <- PAYMR[,c(1,2,5)]
          PAY   <- PAYMR[,c(1,2,3)]

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
          #SSB_Y[PAMR] <- NBefore_Y[PAMR] * Wt_age_SB[PAY]

          # Calculate total biomass
          B_Y[PAMR] <- NBefore_Y[PAMR] * Wt_age[PAY]

          SSB0 <- apply(SSB_Y[,,m,], 1, sum)
          B0   <- apply(B_Y[,,m,], 1, sum)

          # Calculate spawning stock biomass per recruit
          SSBpR <- SSB0 / R0

          # Ricker SR params
          bR <- log(5.0 * h) / (0.8 * SSB0)

          # Ricker SR params
          aR <- exp(bR * SSB0) / SSBpR

          #minimizer quite robust to this value
          NBefore_Y[PAMR] <- NBefore_Y[PAMR] * 0.3
          SSB_Y[PAMR]     <- SSB_Y[PAMR] * 0.3

          PAYMRF2 <- as.matrix(expand.grid(1:npop, 1:nages, y, m, 1:nareas, 1:nfleets))
          PAMRF2  <- PAYMRF2[,c(1,2,4,5,6)]
          PAMR2   <- PAYMRF2[,c(1,2,4,5)]
          PARF2   <- PAYMRF2[,c(1,2,5,6)]
          PAR2    <- PAYMRF2[,c(1,2,5)]
          PAY2    <- PAYMRF2[,c(1,2,3)]
          FA2     <- PAYMRF2[,c(6,2)]
          F2      <- PAYMRF2[,c(6)]
          MRF2    <- PAYMRF2[,c(4,5,6)]

          for (y in 1:nyears)
          {
            PAYMRF2[,3] <- y
            PAY2[,3]    <- y

            for (m in 1:nsubyears)
            {
              PAYMRF2[,4] <- m
              PAMRF2[,3]  <- m
              PAMR2[,3]   <- m
              MRF2[,1]    <- m

              # do recruitment
              if (m %in% Recsubyr)
              {
                # ie every qtr for YFT
                SSN_Y[,,m,] <- NBefore_Y[,,m,] * karray(rep(mat[,,y],times=nareas), dim=c(npop,nages,nareas))

                #potential change to combined spawning and Wt_age
                SSBA_Y      <- apply(SSN_Y[,,m,] * karray(Wt_age[,,y], dim=c(npop,nages,nareas)), 1, sum, na.rm=T)
                #SSBA_Y      <- apply(NBefore_Y[,,m,] * karray(Wt_age_SB[,,y], dim=c(npop,nages,nareas)), 1, sum, na.rm=T)

                # recruit fish
                for(pp in 1:npop)
                {
                  if (SRrel[pp] == 1)
                  {
                    # Beverton-Holt recruitment
                    rec <- ((0.8 * R0[pp] * h[pp] * SSBA_Y[pp]) / (0.2 * SSBpR[pp] * R0[pp] * (1.0 - h[pp]) + (h[pp] - 0.2) * SSBA_Y[pp]))
                  }
                  else
                  {
                    #readline("warning: Ricker not tested")
                    # Most transparent form of the Ricker uses alpha and beta params
                    rec <- aR[pp] * SSBA_Y[pp] * exp(-bR[pp] * SSBA_Y[pp])
                  }

                  NBefore_Y[pp,1,m,] <- rec * Recdist[pp,]
                }
              }

              # move fish (moved to before F+M)
              if (nareas > 1)
              {
                N_Y[,,m,] <- popdyn.domov(Ntemp=NBefore_Y[,,m,], movtemp=mov[,,m,,])
              }
              else
              {
                N_Y[,,m,] <- NBefore_Y[,,m,]
              }

              FM[PARF2]     <- totF * ECurrent[MRF2] * sel[FA2] * q[F2]
              FM[is.na(FM)] <- 0
              Ftot          <- apply(FM, c(1,2,3), sum)
              Z[PAR2]       <- Ftot[PAR2] + M[PAY2] / nsubyears

              C_Y[PAMRF2]  <- N_Y[PAMR2] * (1.0 - exp(-Z[PAR2])) * (FM[PARF2] / Z[PAR2])

              N_Y[,,m,] <- N_Y[,,m,] * exp(-Z)

              # Age fish
              NBefore_Y[,nages,m + 1,]          <- N_Y[,nages - 1,m,] + N_Y[,nages,m,]
              NBefore_Y[,2:(nages - 1),m + 1,]  <- N_Y[,1:(nages - 2),m,]
              NBefore_Y[,1,m + 1,]              <- 0
              N_Y[,,m + 1,]                     <- NBefore_Y[,,m + 1,]

            } # season loop

            if (y != nyears)
            {
              NBefore_Y[,,1,] <- NBefore_Y[,,nsubyears + 1,]
            }
          } # year loop

          if (reportIndicators)
          {
            return (MSYreferencePoints(ECurrent,
                                       sel,
                                       M[,,1],
                                       C_Y,
                                       SSB0,
                                       B0,
                                       N_Y,
                                       NBefore_Y,
                                       SSN_Y,
                                       Wt_age[,,1],
                                       #Wt_age_SB[,,1],
                                       targpop,
                                       nsubyears,
                                       npop,
                                       nages,
                                       nareas,
                                       nfleets,
                                       FAgeRange))
          }
          else
          {
            BMSY <- sum(karray(C_Y[targpop,,,,], c(length(targpop),nages,nsubyears,nareas,nfleets)) *
                        karray(Wt_age[targpop,,nyears], c(length(targpop),nages,nsubyears,nareas,nfleets)))

            return(-log(BMSY))
          }
        }

        # -----------------------------------------------------------------------

        test<-optimize(popdyn,
                       interval=log(c(0.001,10.)),
                       reportIndicators=FALSE,
                       npop=.Object@npop,
                       nages=.Object@nages,
                       nyears=nyears,
                       nsubyears=.Object@nsubyears,
                       nareas=.Object@nareas,
                       nfleets=.Object@nfleets,
                       R0=.Object@R0,
                       M=.Object@M,
                       mat=.Object@mat,
                       Idist=.Object@Idist,
                       Len_ageVec=.Object@Len_age[1,,1],
                       Wt_ageVec=.Object@Wt_age[1,,1],
                       #Wt_age_SBVec=.Object@Wt_age_SB[1,,1],
                       q=.Object@q,
                       nSpawnPerYr = length(.Object@Recsubyr),
                       a=.Object@a,
                       b=.Object@b,
                       Recdist=.Object@Recdist,
                       ECurrent=.Object@ECurrent,
                       sel=.Object@sel,
                       mov=.Object@mov,
                       Recsubyr=.Object@Recsubyr,
                       h=.Object@h,
                       Recdevs=.Object@Recdevs,
                       SRrel=.Object@SRrel,
                       targpop=MseDef@targpop,
                       MSYyear=.Object@nyears,
                       tol=toly,
                       FAgeRange=.Object@FAgeRange)

        best <- popdyn(test$minimum,
                       reportIndicators=TRUE,
                       npop=.Object@npop,
                       nages=.Object@nages,
                       nyears=nyears,
                       nsubyears=.Object@nsubyears,
                       nareas=.Object@nareas,
                       nfleets=.Object@nfleets,
                       R0=.Object@R0,
                       M=.Object@M,
                       mat=.Object@mat,
                       Idist=.Object@Idist,
                       Len_ageVec=.Object@Len_age[1,,1],
                       Wt_ageVec=.Object@Wt_age[1,,1],
                       #Wt_age_SBVec=.Object@Wt_age_SB[1,,1],
                       q=.Object@q,
                       nSpawnPerYr = length(.Object@Recsubyr),
                       a=.Object@a,
                       b=.Object@b,
                       Recdist=.Object@Recdist,
                       ECurrent=.Object@ECurrent,
                       sel=.Object@sel,
                       mov=.Object@mov,
                       Recsubyr=.Object@Recsubyr,
                       h=.Object@h,
                       Recdevs=.Object@Recdevs,
                       SRrel=.Object@SRrel,
                       targpop=MseDef@targpop,
                       MSYyear=.Object@nyears,
                       FAgeRange=.Object@FAgeRange)

        print("Effort multiplier at MSY:")
        print(exp(test$minimum))

        return(best)
      }
    }

    # -------------------------------------------------------------------------
    # start of constructor code
    # -------------------------------------------------------------------------
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

    return (.Object)
  }
)
