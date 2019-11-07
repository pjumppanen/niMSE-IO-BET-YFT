# -----------------------------------------------------------------------------
# StockSynthesisModel class
# -----------------------------------------------------------------------------
setClass("StockSynthesisModel",
  slots = c(
    ModelData             = "StockSynthesisModelData",
    RefVars               = "ReferenceVars",
    HistoricVars          = "ManagementVars",
    ProjectedVars         = "list"            # list of ManagementVars objects one per MP
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "StockSynthesisModel",
  function(.Object, MseDef=NULL, which=-1, seed=-1, Report=FALSE, UseMSYss=0)
  {
    if (is.null(MseDef))
    {
      # do nothing. This is an empty constructor for object upgrading
    }
    else
    {
      if (class(MseDef) != "MseDefinition")
      {
        stop(paste("Could not create StockSynthesisModel.",deparse(substitute(MseDef)),"not of class MseDefinition"))
      }

      if (which == -1)
      {
        stop("'which' not initialised")
      }

      if (seed == -1)
      {
        stop("'seed' not initialised")
      }

      ModelData <- new("StockSynthesisModelData")

      .Object@ModelData@InitSeed <- as.integer(seed)
      .Object@ModelData@which    <- as.integer(which)

      set.seed(.Object@ModelData@InitSeed)

      .Object@ModelData@ProjectSeed <- as.integer(runif(1,0,.Machine$integer.max))

      print(c("importing ", MseDef@OMList[which]))

      if (Report)
      {
        par(mfrow=c(3,3)) #Plot some SS results while importing
      }

      # ssoutput.f no longer req'd, r4ss fixed
      ssMod <- SS_output2(dir=MseDef@SSRootDir %&% MseDef@OMList[which] %&% "\\converged0", covar=FALSE, ncols=213,forecast=FALSE)

      # P nfleets excluding surveys (and some fleets are misleading - should be re-configured as time blocks in selectivity)
      .Object@ModelData@nsim      <- as.integer(MseDef@nsimPerOMFile[which])
      .Object@ModelData@nyears    <- as.integer((ssMod$endyr - MseDef@firstSSYr + 1) / MseDef@nsubyears)
      .Object@ModelData@nsubyears <- MseDef@nsubyears
      .Object@ModelData@proyears  <- MseDef@proyears
      .Object@ModelData@nareas    <- as.integer(ssMod$nareas)
      .Object@ModelData@npop      <- MseDef@npop
      .Object@ModelData@nfleets   <- as.integer(sum(ssMod$IsFishFleet))
      .Object@ModelData@nCPUE     <- ssMod$nfleets - ssMod$nfishfleets
      .Object@ModelData@UseMSYss  <- as.integer(UseMSYss)

      # note MSE age 1 = SS age 0
      nagesss                     <- length(ssMod$endgrowth$Age)
      allyears                    <- .Object@ModelData@nyears + .Object@ModelData@proyears

      .Object@ModelData@nages     <- as.integer(nagesss) #instead of plus group

      .Object@ModelData@M         <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@h         <- karray(as.double(NA), dim=c(.Object@ModelData@npop))
      .Object@ModelData@ReccvT    <- karray(as.double(NA), dim=c(.Object@ModelData@npop))
      .Object@ModelData@ReccvR    <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nareas))

      .Object@ModelData@Recsubyr  <- c(1:4)  #recruit every quarter

      .Object@ModelData@Recdist   <- karray(NA, dim=c(.Object@ModelData@npop, ssMod$nareas)) #recruitment distribution by areas

      .Object@ModelData@Len_age      <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))      #cm
      .Object@ModelData@Len_age_mid  <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))      #cm
      .Object@ModelData@Wt_age       <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))      #kg
      .Object@ModelData@Wt_age_SB    <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))      #kg
      .Object@ModelData@Wt_age_mid   <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))      #kg
      .Object@ModelData@mat          <- karray(as.double(NA), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@sel          <- karray(as.double(NA), dim=c(.Object@ModelData@nfleets, .Object@ModelData@nages))
      .Object@ModelData@CPUEsel      <- karray(as.double(NA), dim=c(.Object@ModelData@nages, .Object@ModelData@nCPUE))

       # Initialise to 1 because C++ model requires valid mov
      .Object@ModelData@mov          <- karray(as.double(1.0), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nareas))

       # Initialise to 1 because C++ model requires valid Idist
      .Object@ModelData@Idist        <- karray(as.double(1.0), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nareas))

      .Object@ModelData@R0           <- karray(as.double(NA), dim=c(.Object@ModelData@npop))

      # Catch numbers
      .Object@ModelData@Css          <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears,.Object@ModelData@nfleets))

      # Catch at age by fleets
      .Object@ModelData@CAAFss       <- karray(as.double(NA), dim=c(.Object@ModelData@nages, allyears,.Object@ModelData@nfleets))

      # Catch mass by npop and fleets
      .Object@ModelData@CMbyFss      <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears,.Object@ModelData@nfleets))

      # age aggregated SSB
      .Object@ModelData@SSBAss       <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears))

      # Catch biomass
      .Object@ModelData@CBss         <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears))

      # biomass
      .Object@ModelData@Bss          <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears))

      # recruitment
      .Object@ModelData@Recss        <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears))

      # quarterly recruitment
      .Object@ModelData@RecYrQtrss   <- karray(as.double(NA), dim=c(.Object@ModelData@npop, allyears*.Object@ModelData@nsubyears))

      # Longline-selected Numbers over all ages, areas, populations for aggregate abundance index
      .Object@ModelData@NLLss        <- karray(as.double(NA), dim=c(allyears, .Object@ModelData@nsubyears, .Object@ModelData@nareas))

      # Longline-selected Numbers over all ages, areas, populations for aggregate abundance index
      .Object@ModelData@NLLIss       <- karray(as.double(NA), dim=c(allyears))

      .Object@ModelData@q            <- karray(as.double(0.0), dim=c(.Object@ModelData@nfleets))

      .Object@ModelData@FAgeRange    <- karray(as.double(NA), dim=c(2))


      #should probably force nCPUE = nareas (?)
      .Object@ModelData@CPUEFleetNums   <- karray(ssMod$fleet_ID[!ssMod$IsFishFleet], dim=c(.Object@ModelData@nCPUE))
      .Object@ModelData@CPUEFleetAreas  <- karray(ssMod$fleet_area[!ssMod$IsFishFleet], dim=c(.Object@ModelData@nCPUE))

      .Object@ModelData@CPUEobsMR       <- karray(as.double(NA), dim=c(allyears, .Object@ModelData@nsubyears, .Object@ModelData@nCPUE))
      .Object@ModelData@CPUEobsY        <- karray(as.double(NA), dim=c(allyears))

      .Object@ModelData@ECurrent        <- karray(as.double(NA), dim=c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))
      .Object@ModelData@CMCurrent       <- karray(as.double(NA), dim=c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))
      .Object@ModelData@EByQtrLastYr    <- karray(as.double(NA), dim=c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))

      .Object@ModelData@F_FMSYss        <- karray(as.double(NA), dim=c(allyears))
      .Object@ModelData@Frepss          <- karray(as.double(NA), dim=c(allyears))

      .Object@ModelData@ITrend          <- karray(as.double(1.0), dim=c(allyears))

      # --- set up M -------
      #assumes the last year vector is the one to use in projections, ignores sex, morphs, temporal varying growth)
      #for some reason SS M matrix exceeds the final year and begins to deviate...must be used for something else
      # not sure why the original YFT specification seems unduly complicated, check its still ok
      nColOffset <- 3

      # SS does not report M in final time step for some reason (i.e. should be ssMod$endyr - ssMod$startyr + 1)
      .Object@ModelData@M <- .Object@ModelData@nsubyears * karray(as.numeric(ssMod$M_at_age[ssMod$endyr - ssMod$startyr, nColOffset + 1:(nagesss)]), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))

      #SS reported M(last age)=NA for some reason
      .Object@ModelData@M[,.Object@ModelData@nages,] <- .Object@ModelData@M[,.Object@ModelData@nages - 1,]


      # ---- Stock-recruit relationships -------
      .Object@ModelData@SRrel <- as.integer(1) #BH: steepness ssMod$parameters$Label="SR_BH_steep"

      #assumes all sims and pops identical for given OMFile
      .Object@ModelData@h <- karray(ssMod$parameters[ssMod$parameters$Label == "SR_BH_steep",]$Value, dim=c(.Object@ModelData@npop))

      tmp <- as.numeric(unlist(strsplit(ssMod$F_report_basis,"_")))
      .Object@ModelData@FAgeRange <- karray(tmp[!is.na(tmp)], dim=2) + 1 #  MSE columns add 1 because column 1 is SS age 0
      rm(tmp)

      #assumes all sims and pops identical for given OMFile
      #.Object@ModelData@ReccvT <- karray(ssMod$parameters[ssMod$parameters$Label == "SR_sigmaR",]$Value, dim=c(.Object@ModelData@npop))

      # Rec sigma - if ReccvTin is negative use the SS file, if non-positive use ReccvTin
      if (MseDef@ReccvTin[1] >= 0)
      {
        .Object@ModelData@ReccvT <- karray(MseDef@ReccvTin, dim=c(.Object@ModelData@npop))
      }
      else
      {
        .Object@ModelData@ReccvT <- karray(ssMod$parameters[ssMod$parameters$Label == "SR_sigmaR",]$Value, dim=c(.Object@ModelData@npop))
      }

      # check recruitment scaling parameter so errors are picked up early
      if ((length(MseDef@RecScale) != 1) && (length(MseDef@RecScale) != MseDef@proyears))
      {
        print("ERROR: MSE definition RecScale vector is wrong length. It must be proyears in length")
        stop()
      }

      # check implementation error bias parameter so errors are picked up early
      if ((length(MseDef@ImplErrBias) != 1) && (length(MseDef@ImplErrBias) != MseDef@proyears))
      {
        print("ERROR: MSE definition ImplErrBias vector is wrong length. It must be proyears in length")
        stop()
      }

      #assumes all sims and pops identical for given OMFile
      if (.Object@ModelData@nareas > 1)
      {
        tmp <- NULL

        for(r in 1:.Object@ModelData@nareas)
        {
          tmp <- c(tmp, ssMod$parameters$Value[ssMod$parameters$Label == paste("RecrDist_Area_", r, sep="")])
        }

        # note that lower bound parameter value is input for areas which have zero rec by definition in SS (~0.1%) - keep this to prevent NA issues
        tmp <- exp(tmp)
        tmp <- tmp / sum(tmp)

        .Object@ModelData@Recdist <- karray(tmp, dim=c(.Object@ModelData@npop, .Object@ModelData@nareas))
      }
      else
      {
        .Object@ModelData@Recdist <- karray(1.0, dim=c(.Object@ModelData@npop, ssMod$nareas))
      }

      .Object@ModelData@ReccvR    <- karray(MseDef@ReccvRin, dim=c(.Object@ModelData@npop, .Object@ModelData@nareas))
      .Object@ModelData@RecACT    <- MseDef@RecACTin  #input value; could extract from each OM file

      # Trend in CPUE observation error (Multiplier)
      if (MseDef@ITrendin < 0)
      {
        # The catchability trend is determined from the q value in each file name independently
        print("WARNING: file-specific index trends assume the single digit following a single q in the filename defines the trend")

        qVal <- as.numeric(substr(MseDef@OMList[which], str_locate(MseDef@OMList[which], "q") + 1, str_locate(MseDef@OMList[which], "q") + 1)) #Should probably revise to 2 digits on next grid iteration

        .Object@ModelData@ITrend[(.Object@ModelData@nyears + 1):(.Object@ModelData@nyears + .Object@ModelData@proyears)] <- cumprod(rep(1 + 0.01 * qVal, .Object@ModelData@proyears))
      }

      # Len_age relationships
      .Object@ModelData@Len_age     <- karray(rep(ssMod$endgrowth$Len_Beg, each=.Object@ModelData@npop), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@Len_age_mid <- karray(rep(ssMod$endgrowth$Len_Mid, each=.Object@ModelData@npop), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))

      # weight-length parameters
      # appears that SS does not apply them to the mean Len(a), but integrates to account for the non-linearity - use SS mass inputs instead
      .Object@ModelData@a <- ssMod$parameters[ssMod$parameters$Label == "Wtlen_1_Fem",]$Value
      .Object@ModelData@b <- ssMod$parameters[ssMod$parameters$Label == "Wtlen_2_Fem",]$Value

      # This mass-at-age conversoin appears to be overly simplified:
      #.Object@ModelData@Wt_age     <- .Object@ModelData@a * .Object@ModelData@Len_age ^ .Object@ModelData@b
      #.Object@ModelData@Wt_age_mid <- .Object@ModelData@a * .Object@ModelData@Len_age_mid ^ .Object@ModelData@b

      # Wt_age_SB includes Wt_age * maturity (useful for dispoportional fecundity)
      .Object@ModelData@Wt_age_SB   <- karray(rep(as.numeric(ssMod$wtatage[1,7:(7+.Object@ModelData@nages-1)]), each=.Object@ModelData@npop), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@Wt_age      <- karray(rep(as.numeric(ssMod$wtatage[2,7:(7+.Object@ModelData@nages-1)]), each=.Object@ModelData@npop), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@Wt_age_mid  <- karray(rep(as.numeric(ssMod$wtatage[3,7:(7+.Object@ModelData@nages-1)]), each=.Object@ModelData@npop), dim=c(.Object@ModelData@npop, .Object@ModelData@nages, allyears))

      # Maturity - product of age and length-based maturity seems to cover both options
      # But does not add up to the calculation below; presumably SS does an integration for maturity as well)
      #.Object@ModelData@mat[firstSimNum:lastSimNum,,,] <- karray(rep(ssMod$endgrowth$Age_Mat * ssMod$endgrowth$Len_Mat, each=.Object@ModelData@nsimPerOMFile[iom] * .Object@ModelData@npop),
      #                                                 dim=c(.Object@ModelData@nsimPerOMFile[iom], .Object@ModelData@npop, .Object@ModelData@nages, allyears))
      .Object@ModelData@mat <- .Object@ModelData@Wt_age_SB / .Object@ModelData@Wt_age

      # Extract CPUE Observations
      print("WARNING: missing CPUE values (after time series start) are substituted with preceding value(i.e. not a good idea if there are many missing obs)")
      print("WARNING: Observed CPUE series are adopted from the first SS model in the OMList")
      # substitute missing CPUE values for expedience (assumes SS output in temporal order)

      # all OMs use the CPUE data defined in the first SS model list
      # This is required because catchability trends in the SS assessment models are implemented with fake CPUE series
      # but the MP only has one set of data to use
      for (i in 1:nrow(ssMod$cpue))
      {
        yrSeas <- seasAsYrToMSEYrSeas.f(seasAsYr=ssMod$cpue$Yr[i], endSeasAsYr = ssMod$endyr, numSeas = MseDef@nsubyears, endYr = MseDef@lastCalendarYr, endSeas = MseDef@lastSeas, firstSeasAsYr = MseDef@firstSSYr, firstSeas = MseDef@firstSeas) #generic

        .Object@ModelData@CPUEobsMR[yrSeas[1]:.Object@ModelData@nyears,yrSeas[2]:.Object@ModelData@nsubyears, as.numeric(substr(ssMod$cpue$Name[i], 7, 7))] <- ssMod$cpue$Obs[i]
      }

      # Annual
      .Object@ModelData@CPUEobsY[] <- apply(.Object@ModelData@CPUEobsMR, FUN=sum, MARGIN=c(1))

      # Calculate initial aggregate annual CPUE index deviate for aggregate autocorrelation of index
      lastYrIndices              <- (max(ssMod$cpue$Yr) - 3):max(ssMod$cpue$Yr)
      .Object@ModelData@initIDev <- log(sum(ssMod$cpue$Exp[(ssMod$cpue$Yr %in% lastYrIndices)]) / sum(ssMod$cpue$Obs[(ssMod$cpue$Yr %in% lastYrIndices)]))

      sel     <- karray(NA, c(.Object@ModelData@nfleets, .Object@ModelData@nages))
      CPUEsel <- karray(NA, c(.Object@ModelData@nages, .Object@ModelData@nCPUE))

      # Fishery selectivity
      for (ff in 1:.Object@ModelData@nfleets)
      {
        nColOffset <- 7
        sel[ff,]   <- karray(as.numeric(ssMod$ageselex[ssMod$ageselex$label == ssMod$endyr %&% "_"  %&% ff %&% "Asel", c(nColOffset + 1:nagesss)]),
                             dim=c(.Object@ModelData@nages))
        if (Report)
        {
          if (ff == 1)
          {
            plot(sel[ff,], type='l', main="Fishery Sel, which=" %&% which)
          }
          else
          {
            lines(sel[ff,], col=ff)
          }
        }
      }

      .Object@ModelData@sel <- sel

      rm(sel)

      #CPUE selectivity
      for(iCPUE in 1:.Object@ModelData@nCPUE)
      {
        nColOffset      <- 7
        CPUEsel[,iCPUE] <- karray(as.numeric(ssMod$ageselex[ssMod$ageselex$label == ssMod$endyr %&% "_"  %&% .Object@ModelData@CPUEFleetNums[iCPUE] %&% "Asel",c(nColOffset + 1:nagesss)]),
                                  dim=c(.Object@ModelData@nages))

        if (Report)
        {
          if (iCPUE == 1)
          {
            plot(CPUEsel[,iCPUE], type='l', main="CPUE Sel, which=" %&% which)
          }
          else
          {
            lines(CPUEsel[,iCPUE], col=iCPUE)
          }
        }
      }

      .Object@ModelData@CPUEsel <- CPUEsel

      rm(CPUEsel)

      mov <- karray(0, dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nareas))

      if (.Object@ModelData@nareas > 1)
      {
        # Import SS movement parms
        ssmov <- ssMod$movement

        for (i in 1:nrow(ssmov))
        {
          from        <- ssmov$Source_area[i]
          to          <- ssmov$Dest_area[i]
          nColOffSet  <- 6
          moveVec     <- ssmov[i, (nColOffSet + 1):(nColOffSet + nagesss)]
          moveVec     <- as.numeric(moveVec) # MSE maxage older than SS

          for (ip in 1:.Object@ModelData@npop)
          {
            # Note that OM is configured for seasonal movement, but SS uses mean annual migration; hence the repetition for each OM season
            for(im in 1:.Object@ModelData@nsubyears)
            {
              mov[ip,,im,from,to] <- moveVec
            }
          }
        }

        .Object@ModelData@mov <- mov

        rm(mov)

        # movement function
        projection.domov <- function(Ntemp, movtemp)
        {
          # P A R  x  P A R R
          nareas <- dim(movtemp)[length(dim(movtemp))]

          #return dim = PAR
          apply(karray(Ntemp, c(dim(Ntemp),nareas)) * movtemp, MARGIN=c(1,2,4), sum)
        }

        # Initial distribution of N, not really relevant when adopting SS initial N
        Idist <- karray(0.0, dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nareas))

        for (i in 1:30)
        {
          # At least nages/nsubyears
          for (mm in 1:.Object@ModelData@nsubyears)
          {
            # Mean unfished equilibrium distribution (extra age calculaitons are redundant)
            if (i == 1 & mm == 1)
            {
              Idist[ip,1,] <- .Object@ModelData@Recdist[ip,]
            }

            Idist <- projection.domov(Ntemp=karray(Idist, dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nareas)),
                                      movtemp=karray(.Object@ModelData@mov[,,mm,,], dim=c(.Object@ModelData@npop, .Object@ModelData@nages, .Object@ModelData@nareas, .Object@ModelData@nareas)))

            for(ip in 1:.Object@ModelData@npop)
            {
              Idist[ip,2:(.Object@ModelData@nages),] <- Idist[ip,1:(.Object@ModelData@nages - 1),]
              Idist[ip,1,]                 <- as.double(.Object@ModelData@Recdist[ip,])
            }
          }
        }

        if (Report)
        {
          plot(Idist[1,,1], col=1, type='l', ylim=c(0, max(Idist[1,,])), xlab="age", ylab="Prop", main="Init N Dist by region")
          lines(Idist[1,,2], col=2)
          lines(Idist[1,,3], col=3)
          lines(Idist[1,,4], col=4)
        }

        .Object@ModelData@Idist <- Idist

        rm(Idist)
      }  # end if (nareas > 1)

      # R0 extracted from SS
      R0ss                 <- ssMod$parameters[ssMod$parameters$Label == "SR_LN(R0)",]$Value  # in thousands
      .Object@ModelData@R0 <- karray(exp(R0ss), dim=c(.Object@ModelData@npop))

      # Total numbers and spawning stock numbers
      SSN     <- karray(NA, c(.Object@ModelData@npop, .Object@ModelData@nages, allyears + 1, .Object@ModelData@nsubyears, .Object@ModelData@nareas))
      NBefore <- karray(NA, c(.Object@ModelData@npop, .Object@ModelData@nages, allyears + 1, .Object@ModelData@nsubyears, .Object@ModelData@nareas))

      # Catch (numbers, then mass in MSE framework)
      CssTmp <- karray(NA, c(.Object@ModelData@npop, .Object@ModelData@nages, allyears, .Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))

      #YFT input is in mass, except LL fleets in numbers (3,7,10,11,13) (though not #25 fresh tuna LL apparently?)
      #check if catage in numbers or mass or mixed: appears to be in numbers:
      #sum(ssMod$catage[ssMod$catage$Fleet==1 & ssMod$catage$Yr==272, 11:39])  = 936.9    # YFT.dat = 13598 = 15kg per gillnet fish
      #sum(ssMod$catage[ssMod$catage$Fleet==3 & ssMod$catage$Yr==272, 11:39])  = 1.355    # YFT.dat = 1.355

      # SS Catch
      CssTmp[,,1:.Object@ModelData@nyears,,,] <- 0.0

      # SS Catch-at-age
      CAss <- ssMod$catage[as.numeric(ssMod$catage$"Yr") >= MseDef@firstSSYr,] # "Area"   "Fleet"  "Gender" "XX"     "XX"     "Morph"  "Yr"     "Seas"   "XX"     "Era"    "0"

      for (i in 1:nrow(CAss))
      {
        # Convert SS season (defined as year) to MSE year, season
        yrSeas     <- seasAsYrToMSEYrSeas.f(seasAsYr = CAss$Yr[i], endSeasAsYr = ssMod$endyr, numSeas = MseDef@nsubyears, endYr = MseDef@lastCalendarYr, endSeas = MseDef@lastSeas, firstSeasAsYr = MseDef@firstSSYr, firstSeas = MseDef@firstSeas) #generic
        nColOffset <- 10

        # PAYMRF
        #CssTmp[1,1:nagesss,yrSeas[1],yrSeas[2],CAss$Area[i],CAss$Fleet[i]] <- as.numeric(CAss[i,nColOffset + 1:nagesss])

        # Drop catch in first time slot because SS evidently reports an aggregate here if early equilibrium exploitation history otion is used
        if (CAss$Yr[i] > min(CAss$Yr))
        {
          CssTmp[1,1:nagesss,yrSeas[1],yrSeas[2],CAss$Area[i],CAss$Fleet[i]] <- as.numeric(CAss[i,nColOffset + 1:nagesss])
        }
      }

      .Object@ModelData@Css     <- apply(CssTmp, c(1,3,6), sum)
      .Object@ModelData@CAAFss  <- apply(CssTmp, c(2,3,6), sum)
      .Object@ModelData@CBss    <- apply(CssTmp * karray(.Object@ModelData@Wt_age_mid[,,], c(.Object@ModelData@npop,.Object@ModelData@nages, allyears,.Object@ModelData@nsubyears,.Object@ModelData@nareas,.Object@ModelData@nfleets)), c(1,3), sum)
      .Object@ModelData@CMbyFss <- karray(apply(apply(CssTmp, c(1,2,3,6), sum) * karray(.Object@ModelData@Wt_age_mid[,,], c(.Object@ModelData@npop, .Object@ModelData@nages, allyears, .Object@ModelData@nfleets)), c(1,3,4), sum), c(.Object@ModelData@npop, allyears, .Object@ModelData@nfleets))

      # SS N(age)
      # YFT Nss <- ssMod$natage[as.numeric(ssMod$natage$"Yr")>=101 & ssMod$natage$"Beg/Mid"=="B",]  #in thousands

      Nss <- ssMod$natage[as.numeric(ssMod$natage$"Yr") >= MseDef@firstSSYr & ssMod$natage$"Beg/Mid"=="B",]  #in thousands

      for (i in 1:nrow(Nss))
      {
        # Convert SS season (defined as year) to MSE year, season
        yrSeas     <- seasAsYrToMSEYrSeas.f(seasAsYr=Nss$Yr[i], endSeasAsYr = ssMod$endyr, numSeas = MseDef@nsubyears, endYr = MseDef@lastCalendarYr, endSeas = MseDef@lastSeas, firstSeasAsYr = MseDef@firstSSYr, firstSeas = MseDef@firstSeas) #generic
        nColOffset <- 11

        #PAYMR
        NBefore[,1:nagesss,yrSeas[1],yrSeas[2],Nss$Area[i]] <- as.numeric(Nss[i,nColOffset + 1:nagesss])
        SSN[,,yrSeas[1],yrSeas[2],Nss$Area[i]] <- NBefore[,,yrSeas[1],yrSeas[2],Nss$Area[i]] * .Object@ModelData@mat[,,yrSeas[1]]
      }

      # This is the N by SPAYMR in the last year of the assessment, that is re-iterated in the first year of projections
      .Object@ModelData@NBeforeInit <- NBefore[,,.Object@ModelData@nyears,1,]

      .Object@ModelData@SSBAss[] <- apply(karray(SSN[,,keep(1:allyears),.Object@ModelData@nsubyears,], c(.Object@ModelData@npop,.Object@ModelData@nages,allyears,.Object@ModelData@nareas)) * karray(.Object@ModelData@Wt_age[,,1:allyears], c(.Object@ModelData@npop,.Object@ModelData@nages,allyears,.Object@ModelData@nareas)), c(1,3), sum)

      # Bss mean over seasons
      BbyM                      <- apply(NBefore[,,keep(1:allyears),,] * karray(.Object@ModelData@Wt_age[,,keep(1:allyears)], c(.Object@ModelData@npop,.Object@ModelData@nages,allyears,.Object@ModelData@nsubyears,.Object@ModelData@nareas)), c(1,3,4), sum)
      BbyY                      <- apply(BbyM, c(1,2), mean)
      .Object@ModelData@Bss     <- BbyY
      .Object@ModelData@Recss[] <- apply(NBefore[,1,keep(1:allyears),,], c(2), sum)

      for (yyy in 1:allyears)
      {
        FirstIdx <- (yyy - 1) * .Object@ModelData@nsubyears + 1
        LastIdx  <- yyy * .Object@ModelData@nsubyears

        .Object@ModelData@RecYrQtrss[,FirstIdx:LastIdx] <- apply(NBefore[,1,yyy,keep(1:.Object@ModelData@nsubyears),], c(1,2), sum)
      }

      # All this stuff is used to calculate Frep (summed over regions, arithmetic mean over age and season)
      NsoRbyPAYM   <- karray(NA, dim=c(.Object@ModelData@npop,.Object@ModelData@nages,allyears + 1,.Object@ModelData@nsubyears))
      NsoRbyPAYM[] <- apply(NBefore, FUN=sum, MARGIN=c(1:4))

      ZsoRbyPAYM       <- karray(NA, dim=c(.Object@ModelData@npop,.Object@ModelData@nages - 2,allyears,.Object@ModelData@nsubyears))
      ZsoRbyPAYM[,,,1] <- -log(NsoRbyPAYM[,2:(.Object@ModelData@nages - 1),1:allyears,2]       / NsoRbyPAYM[,1:(.Object@ModelData@nages - 2),1:allyears,1])
      ZsoRbyPAYM[,,,2] <- -log(NsoRbyPAYM[,2:(.Object@ModelData@nages - 1),1:allyears,3]       / NsoRbyPAYM[,1:(.Object@ModelData@nages - 2),1:allyears,2])
      ZsoRbyPAYM[,,,3] <- -log(NsoRbyPAYM[,2:(.Object@ModelData@nages - 1),1:allyears,4]       / NsoRbyPAYM[,1:(.Object@ModelData@nages - 2),1:allyears,3])
      ZsoRbyPAYM[,,,4] <- -log(NsoRbyPAYM[,2:(.Object@ModelData@nages - 1),2:(allyears + 1),1] / NsoRbyPAYM[,1:(.Object@ModelData@nages - 2),1:allyears,4])

      FsoRbyPAYM <- ZsoRbyPAYM - karray(.Object@ModelData@M[,1:(.Object@ModelData@nages - 2),] / .Object@ModelData@nsubyears, dim=c(.Object@ModelData@npop,.Object@ModelData@nages - 2,allyears,.Object@ModelData@nsubyears))

      # Columns 2:27 = true ages 1:26 (1:26 used by SS YFT)
      a1 <- .Object@ModelData@FAgeRange[1]
      a2 <- .Object@ModelData@FAgeRange[2]

      .Object@ModelData@Frepss[] <- apply(FsoRbyPAYM[,a1:a2,,], mean, MARGIN=c(3))

      # Alternate Frep calculation from ss Kobe output ... they really ought to be the same!!

      rm(NsoRbyPAYM, ZsoRbyPAYM)

      # Includes two abundance index karrays:
      # Iobs is LL selected annual numbers aggregated over seasons and regions
      # CPUEobs is partitioned by season and fishery (one fishery per region in initial YFT implementation)
      NLLssByAYMR <- apply(NBefore[,keep(1:.Object@ModelData@nages),keep(1:allyears),,], MARGIN=c(2,3,4,5), FUN=sum,na.rm=T)

      # Longline-selected Numbers by area and season for partitioned abundance index
      for(isubyears in 1:.Object@ModelData@nsubyears)
      {
        for(iCPUE in 1:.Object@ModelData@nCPUE)
        {
          .Object@ModelData@NLLss[,isubyears,iCPUE] <- apply(NLLssByAYMR[,,isubyears,.Object@ModelData@CPUEFleetAreas[iCPUE]] * karray(rep(.Object@ModelData@CPUEsel[,iCPUE], times=allyears), dim=c(.Object@ModelData@nages,allyears)), MARGIN=c(2), FUN=sum)
        }
      }

      .Object@ModelData@NLLIss[] <- apply(.Object@ModelData@NLLss, MARGIN=c(1), FUN=sum)

      # q to keep CPUE on original scale; should move to OM initialization
      .Object@ModelData@qCPUE <- sum(.Object@ModelData@CPUEobsY[1:.Object@ModelData@nyears][!is.na(.Object@ModelData@CPUEobsY[1:.Object@ModelData@nyears])]) / sum(.Object@ModelData@NLLIss[1:.Object@ModelData@nyears][!is.na(.Object@ModelData@CPUEobsY[1:.Object@ModelData@nyears])])

      rm(NLLssByAYMR)

      # SS F/FMSY - xxx need to resolve inconsistencies if series to be usefully merged
      for (ymse in 1:.Object@ModelData@nyears)
      {
        .Object@ModelData@F_FMSYss[ymse] <- mean(ssMod$Kobe$F.Fmsy[seasAsYrToMSEYr.f(seasAsYr=ssMod$Kobe$Year, endSeasAsYr = ssMod$endyr, numSeas = MseDef@nsubyears, endYr = MseDef@lastCalendarYr, endSeas = MseDef@lastSeas, firstSeasAsYr = MseDef@firstSSYr, firstSeas = MseDef@firstSeas)==ymse])
      }

      # MSE works in individuals and kg (or 1000 individuals and tonnes)
      # SS MSY, SSBMSY, etc (no obvious BMSY in SS outputs)
      MSYss           <- 4.0 * ssMod$derived_quants$Value[ssMod$derived_quants$LABEL == "TotYield_MSY"]  #SS MSY is quarterly
      SSBMSYss        <- ssMod$derived_quants$Value[ssMod$derived_quants$LABEL == "SSB_MSY"]

      .Object@ModelData@SSB0ss  <- ssMod$derived_quants$Value[ssMod$derived_quants$LABEL == "SSB_Unfished"]
      .Object@ModelData@B0ss    <- ssMod$derived_quants$Value[ssMod$derived_quants$LABEL == "TotBio_Unfished"]
      .Object@ModelData@FMSYss  <- ssMod$derived_quants$Value[ssMod$derived_quants$LABEL == "Fstd_MSY"]

      # ABT MSE assumes TAC split envenly among quarters
      # YFT q defined as 1 (or zero for fisheries that are time blocks that do not operate in 2014)
      # YFT assumes a default annual distribution for Effort with a constant proportion by season, fishery and area strata
      # This is true for aggregate annual TAC or TAE-managed fisheries, but not if there is a mix (or fishery-specific controls)
      q <- karray(1.0, dim=c(.Object@ModelData@nfleets))

      # YFT Effort and Catch-in-Mass distribution (by season region and fishery for one year)  for future effort/catch allocations
      EByQtrLastYr  <- karray(0.0, c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))
      ECurrent      <- karray(0.0, c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))
      CMCurrent     <- karray(0.0, c(.Object@ModelData@nsubyears, .Object@ModelData@nareas, .Object@ModelData@nfleets))

      # Note that YFT fisheries are always defined on the basis of a specific region, such that
      # fishery and area distinctions are redundant for many purposes. The F definitions below
      # appear to be scaled relative to the season duration, such that:
      # if duration is 3 months, ssMod$seasDuration = 0.25
      # Fs reported as "F:_fleetnumber" and plotted are annuallized, but only applied for seasDuration
      # meaning that the F value in absolute practical terms is F * seasduration!
      # Ms and growth appear to be re-scaled separate to this, and the seasduration introduces an
      # error in terms of mid-year sizes:
      # i.e. midyearSize is t=t+0.125 rather than t+0.5

      lastSSyrSeas  <- ssMod$endyr
      numRecentSeas <- MseDef@recentPerLast - MseDef@recentPerFirst + 1 # last timestep indicated by recentPerFirst=0

      # Calculate mean catch and effort by season and fleet
      if ((MseDef@seasonCEDist) & (numRecentSeas %% MseDef@nsubyears == 0))
      {
        recentPeriodYrs <- numRecentSeas / .Object@ModelData@nsubyears

        for (ifleets in 1:.Object@ModelData@nfleets)
        {
          for (iy in 1:recentPeriodYrs)
          {
            for (im in 1:.Object@ModelData@nsubyears)
            {
              r <- ssMod$"fleet_area"[ifleets]

              YrRows                      <- lastSSyrSeas - .Object@ModelData@recentPerFirst - (recentPeriodYrs * MseDef@nsubyears) + ((iy - 1) * MseDef@nsubyears) + im
              EByQtrLastYr[im,r,ifleets]  <- ssMod$timeseries[ssMod$timeseries$Yr == YrRows & ssMod$timeseries$Area == r, "F:_" %&% ifleets == names(ssMod$timeseries)]
              ECurrent[im,r,ifleets]      <- ECurrent[im,r,ifleets] + (1.0 / recentPeriodYrs) * EByQtrLastYr[im,r,ifleets]

              yrSeas <- seasAsYrToMSEYrSeas.f(seasAsYr      = YrRows,
                                              endSeasAsYr   = ssMod$endyr,
                                              numSeas       = MseDef@nsubyears,
                                              endYr         = MseDef@lastCalendarYr,
                                              endSeas       = MseDef@lastSeas,
                                              firstSeasAsYr = MseDef@firstSSYr,
                                              firstSeas     = MseDef@firstSeas) #generic

              CMCurrent[im,r,ifleets] <- CMCurrent[im,r,ifleets] +
                                           sum((1.0 / recentPeriodYrs) *
                                               colSums(CssTmp[,,yrSeas[1], yrSeas[2], r, ifleets]) *
                                               colSums(.Object@ModelData@Wt_age_mid[,,seasAsYrToMSEYrSeas.f(YrRows)[1]]))
            }
          }
        }
      }
      else #calculate mean catch and effort that remains constant over seasons
      {
        if ((MseDef@seasonCEDist) & (numRecentSeas %% MseDef@nsubyears != 0))
        {
          readline("WARNING: recent C&E dists are not seasonal because recentPeriod is not a multiple of nsubyears. Press ENTER to continue")
        }

        for (ifleets in 1:.Object@ModelData@nfleets)
        {
          for (iseason in MseDef@recentPerLast:MseDef@recentPerFirst)
          {
            r <- ssMod$"fleet_area"[ifleets]

            YrRows                                     <- lastSSyrSeas - iseason
            EThisSeason                                <- ssMod$timeseries[ssMod$timeseries$Yr == YrRows & ssMod$timeseries$Area == r, "F:_" %&% ifleets == names(ssMod$timeseries)]
            EByQtrLastYr[1:MseDef@nsubyears,r,ifleets] <- EThisSeason
            ECurrent[1:MseDef@nsubyears,r,ifleets]     <- ECurrent[1:MseDef@nsubyears,r,ifleets] + (1.0 /numRecentSeas) * EThisSeason

            yrSeas <- seasAsYrToMSEYrSeas.f(seasAsYr      = YrRows,
                                            endSeasAsYr   = ssMod$endyr,
                                            numSeas       = MseDef@nsubyears,
                                            endYr         = MseDef@lastCalendarYr,
                                            endSeas       = MseDef@lastSeas,
                                            firstSeasAsYr = MseDef@firstSSYr,
                                            firstSeas     = MseDef@firstSeas) #generic

            CMCurrent[1:MseDef@nsubyears,r,ifleets] <- CMCurrent[1:MseDef@nsubyears,r,ifleets] +
                                                         sum(1.0 / (numRecentSeas) *
                                                             colSums(CssTmp[,,yrSeas[1], yrSeas[2], r, ifleets]) *
                                                             colSums(.Object@ModelData@Wt_age_mid[,,seasAsYrToMSEYrSeas.f(YrRows)[1]]))
          }
        }
      }

      rm(CssTmp, SSN, NBefore)

      EByQtrLastYr  <- EByQtrLastYr * ssMod$seasdurations
      ECurrent      <- ECurrent * ssMod$seasdurations

      if (Report)
      {
        # Plot the F by fleet time series to see if the SS output is sensible
        par(mfrow=c(1,1), ask=FALSE)

        for (ifleets in c(1:.Object@ModelData@nfleets))
        {
          f <- "F:_" %&% ifleets
          y <- "Yr"
          d <- ssMod$timeseries[, c(f,y) ]
          d <- d[d[,1] > 0,] * ssMod$seasdurations

          if (ifleets == 1)
          {
            plot(d[,2], d[,1], type='l', ylim=c(0,2.5))
          }
          else
          {
            lines(d[,2], d[,1], col=ifleets)
          }
        }
      }

      .Object@ModelData@q[]            <- q
      .Object@ModelData@ECurrent[]     <- ECurrent
      .Object@ModelData@CMCurrent[]    <- CMCurrent
      .Object@ModelData@EByQtrLastYr[] <- EByQtrLastYr

      .Object@ModelData@MSYss    <- MSYss
      .Object@ModelData@SSBMSYss <- SSBMSYss

      # Do MSY projections
      .Object@RefVars <- new("ReferenceVars", .Object@ModelData, MseDef, Report)

      .Object@HistoricVars <- new("ManagementVars", .Object@ModelData, TRUE, 0, seeds=rep(-999,.Object@ModelData@nsim))

      rm(ssMod)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setMethod("initCPUE_SeriesFrom", c("StockSynthesisModel"),
  function(.Object, RefModel)
  {
    # copy over CPUE related stuff in ModelData
#    .Object@ModelData@NLLss[]           <- RefModel@ModelData@NLLss[]
#    .Object@ModelData@NLLIss[]          <- RefModel@ModelData@NLLIss[]
#    .Object@ModelData@qCPUE             <- RefModel@ModelData@qCPUE
#    .Object@ModelData@CPUEsel           <- RefModel@ModelData@CPUEsel
#    .Object@ModelData@CPUEobsMR[]       <- RefModel@ModelData@CPUEobsMR[]
#    .Object@ModelData@CPUEobsY[]        <- RefModel@ModelData@CPUEobsY[]
#    .Object@ModelData@CPUEFleetNums[]   <- RefModel@ModelData@CPUEFleetNums[]
#    .Object@ModelData@CPUEFleetAreas[]  <- RefModel@ModelData@CPUEFleetAreas[]

    # copy over CPUE related stuff in HistoricVars
    .Object@HistoricVars@IobsArchive[]  <- RefModel@HistoricVars@IobsArchive[]
    .Object@HistoricVars@IobsRArchive[] <- RefModel@HistoricVars@IobsRArchive[]

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setMethod("runMse", c("StockSynthesisModel"),
  function(.Object, MseDef, MPs, tune=NA, interval=3, Report=FALSE, CppMethod=NA, cluster=NA, EffortCeiling = as.double(20.0), TACTime = 0.5, rULim = 0.5)
  {
    if (class(MseDef) != "MseDefinition")
    {
      print(paste("ERROR: Could not create StockSynthesisModel.",deparse(substitute(MseDef)),"not of class MseDefinition"))
      stop()
    }

    # CPUE auto-correlation
    .Object@ModelData@IAC <- MseDef@IACin     #input value; could extract from each OM file

    # Trend in CPUE observation error (Multiplier)
    if (MseDef@ITrendin >= 0)
    {
      # Use the input value for all sims
      .Object@ModelData@ITrend[(.Object@ModelData@nyears + 1):(.Object@ModelData@nyears + .Object@ModelData@proyears)] <- cumprod(rep(1 + 0.01 * MseDef@ITrendin, .Object@ModelData@proyears))
    }

    .Object@ProjectedVars <- list()

    set.seed(.Object@ModelData@ProjectSeed)

    idx <- 1

    if ((length(tune) == 1) && is.na(tune))
    {
      tune <- rep(1.0, times=length(MPs))
    }

    seeds <- runif(.Object@ModelData@nsim, 0, .Machine$integer.max)

    for (MP in MPs)
    {
      # need to initialise this first without running the projection because it
      # initialises seeds with random numbers and needs to be done prior to any
      # cluster processing.
      .Object@ProjectedVars[[idx]] <- new("ManagementVars", .Object@ModelData, FALSE, idx, seeds)

      idx <- idx + 1
    }

    idx      <- 1
    MP_Names <- names(MPs)

    if (is.null(MP_Names))
    {
      MP_Names <- rep("", times=length(MPs))
    }

    for (MP in MPs)
    {
      MPn        <- MP
      MP_Name    <- MP_Names[idx]
      tune_value <- tune[idx]

      if (class(MP) == "MP_Spec")
      {
        tune_value <- MP@tune
        MPn        <- MP@MP
      }
      else
      {
        if (nchar(MP_Name) == 0)
        {
          MP_Name <- MP
        }

        tune_value <- tune_value
        tune_error <- as.numeric(NA)
        MP         <- new("MP_Spec", MP, MP_Name, tune, tune_error)
      }

      MP_class      <- class(get(MPn))
      ProjectedVars <- .Object@ProjectedVars[[idx]]

      if ((MP_class == "IO_MP") || (MP_class == "IO_MP_tune"))
      {
        ProjectedVars <- runProjection(ProjectedVars, .Object@RefVars, .Object@ModelData, MseDef, MP, tune_value, interval, Report, CppMethod, cluster, EffortCeiling, TACTime, rULim)

      } else
      {
        print(paste("ERROR: Could not run MSE:",deparse(substitute(MP)),"not of class IO_MP or IO_MP_tune"))
        stop()
      }

      .Object@ProjectedVars[[idx]] <- ProjectedVars

      if (is.na(.Object@HistoricVars@F[.Object@ModelData@nyears]))
      {
        .Object@HistoricVars@F[.Object@ModelData@nyears] <- ProjectedVars@FlastYr
      }

      idx <- idx + 1
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setMethod("getMPs", c("StockSynthesisModel"),
  function(.Object)
  {
    lapply(.Object@ProjectedVars, function(ProjectedVar) {return (ProjectedVar@MP)})
  }
)

# -----------------------------------------------------------------------------

setMethod("changeMP_Names", c("StockSynthesisModel"),
  function(.Object, namedList)
  {
    for (idx in 1:length(.Object@ProjectedVars))
    {
      .Object@ProjectedVars[[idx]] <- changeMP_Names(.Object@ProjectedVars[[idx]], namedList)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setMethod("msevizPerformanceData", c("StockSynthesisModel"),
  function(.Object, mseFramework, df, AvgYears, prefix = "", model_name=NA)
  {
    if (class(mseFramework) != "MseFramework")
    {
      print(paste("ERROR: Could not extract performance data.",deparse(substitute(MseFramework)),"not of class MseFramework"))
      stop()
    }

    extractData <- function(ManagementVars, HistoricVars, RefVars, AvgYears)
    {
      Sims <- 1:ManagementVars@nsim
      C2   <- rep(mseFramework@MseDef@firstMPYr + AvgYears[1], times=ManagementVars@nsim)
      C4   <- Sims
      C6   <- rep(paste(prefix, ManagementVars@MP@MP_Name, sep=""), times=ManagementVars@nsim)
      df   <- NULL

      if (is.na(model_name))
      {
        addRows <- function(df, data, indicator, name)
        {
          C1 <- rep(indicator, times=ManagementVars@nsim)
          C3 <- data
          C5 <- rep(name, times=ManagementVars@nsim)

          df <- rbind(data.frame(df), data.frame(indicator=C1, year=C2, data=C3, iter=C4, name=C5, mp=C6))

          return (df)
        }
      }
      else
      {
        addRows <- function(df, data, indicator, name)
        {
          C1 <- rep(indicator, times=ManagementVars@nsim)
          C3 <- data
          C5 <- rep(name, times=ManagementVars@nsim)
          C7 <- rep(model_name, times=ManagementVars@nsim)

          df <- rbind(data.frame(df), data.frame(indicator=C1, year=C2, data=C3, iter=C4, name=C5, mp=C6, model=C7))

          return (df)
        }
      }

      AvgYearsm1 <- AvgYears - 1


      # Renumbered all performance indicators relative to the SC2017 table expectations
      # Order is not consistent because of order of caluclations requirement
      # S8 Pr(SB>0.2SB0)
      SSB_SSB0    <- ssb_ssb0(ManagementVars, RefVars)
      PrSBgtp2SB0 <- round(apply(as.karray(SSB_SSB0)[keep(Sims), mseFramework@MseDef@targpop, AvgYears] > 0.2, MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, PrSBgtp2SB0, "S8", "Pr(SB>0.2SB0)")

      # S9 Pr(SB>SBlim) where SBlim is species dependent
      SSB_SSBMSY  <- ssb_ssbmsy(ManagementVars, RefVars)
      PrSBgtSBlim <- round(apply(as.karray(SSB_SSBMSY)[keep(Sims), AvgYears] > mseFramework@MseDef@SBlim, MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, PrSBgtSBlim, "S9", "Pr(SB>SBlim)")

      # S16 Pr(C<0.1MSY)
      C_MSY       <- c_msy(ManagementVars, RefVars)
      PrCltp1MSY  <- round(apply(as.karray(C_MSY)[keep(Sims), , AvgYears] < 0.1, MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, PrCltp1MSY, "S16", "Pr(C<0.1MSY)")

      # S1 mean(SB/SB_0)
      SBoSB0      <- round(apply(as.karray(SSB_SSB0)[keep(Sims), , AvgYears], MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, SBoSB0, "S1", "mean(SB/SB_0)")

      # S2 min(SB/SB0)
      minSBoSB0   <- round(apply(as.karray(SSB_SSB0)[keep(Sims), , AvgYears], MARGIN=c(1), min), digits=3)
      df          <- addRows(df, minSBoSB0, "S2", "min(SB/SB_0)")

      # S3 mean(SB/SB_MSY)
      SBoSBMSY    <- round(apply(as.karray(SSB_SSBMSY)[keep(Sims), AvgYears], MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, SBoSBMSY, "S3", "mean(SB/SB_MSY)")

      # S4 mean(F/F_target), in this case...Ftarget = FMSY - should generalize this
      F_FMSY      <- f_fmsy(ManagementVars, RefVars)
      FoFtarg     <- round(apply(as.karray(F_FMSY)[keep(Sims), AvgYears], MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, FoFtarg, "S4", "mean(F/F_target)")

      # S5 mean(F/F_MSY)
      FoFMSY      <- round(apply(as.karray(F_FMSY)[keep(Sims), AvgYears], MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, FoFMSY, "S5", "mean(F/F_MSY)")

      # S6 Pr(Green)
      PrGreen     <- round(apply(as.karray(F_FMSY)[keep(Sims), AvgYears] < 1 & as.karray(SSB_SSBMSY)[keep(Sims), AvgYears] > 1, c(1), sum) / length(AvgYears), 3)
      df          <- addRows(df, PrGreen, "S6", "Pr(Green)")

      # S7 Pr(Red)
      PrRed       <- round(apply(as.karray(F_FMSY)[keep(Sims), AvgYears] > 1 & as.karray(SSB_SSBMSY)[keep(Sims), AvgYears] < 1, c(1), sum) / length(AvgYears), 3)
      df          <- addRows(df, PrRed, "S7", "Pr(Red)")

      # S8 Pr(SB>SB_MSY) defined above
      # S9 Pr(SB>SBlim) defined above

      # S10 (Y1) mean(C)
      C           <- round(apply(ManagementVars@CM[keep(Sims), mseFramework@MseDef@targpop, AvgYears], MARGIN=c(1), mean), 0) / 1000.0
      df          <- addRows(df, C, "S10", "mean(C)")

      # S11 mean(C) by fishery



      # S12 mean(C/MSY)
      CoMSY       <- round(apply(as.karray(C_MSY)[keep(Sims), , AvgYears], MARGIN=c(1), mean), digits=2)
      df          <- addRows(df, CoMSY, "S12", "mean(C/MSY)")

      # S13 CPUE for index fleet relative to 2011-2015 average
      relCPUE     <- round(apply(ManagementVars@IobsArchive[keep(Sims), keep(AvgYears)], MARGIN=c(1), mean) /  mean(HistoricVars@IobsArchive[(HistoricVars@nyears-4):HistoricVars@nyears]), digits=2)
      df          <- addRows(df, relCPUE, "S13", "mean(CPUE)")

      # S14 AAVY
      AAVY        <- 100*apply(abs((as.karray(ManagementVars@CM)[keep(Sims),mseFramework@MseDef@targpop,AvgYears] -
                                as.karray(ManagementVars@CM)[keep(Sims),mseFramework@MseDef@targpop,AvgYearsm1]) /
                                as.karray(ManagementVars@CM)[keep(Sims),mseFramework@MseDef@targpop,AvgYearsm1]) , 1, mean)
      df          <- addRows(df, AAVY, "S14", "Catch_Variablity")

      # S15 CV(C)
      cvC         <- round(apply(as.karray(ManagementVars@CM)[keep(Sims), mseFramework@MseDef@targpop, AvgYears], MARGIN=c(1), sd), 2)/
                     round(apply(as.karray(ManagementVars@CM)[keep(Sims), mseFramework@MseDef@targpop, AvgYears], MARGIN=c(1), mean), 2)
      df          <- addRows(df, cvC, "S15", "cv(C)")

      # T1 mean(C(t)/C(t-1))
      CtonCtm1    <- apply((ManagementVars@CM[keep(Sims), mseFramework@MseDef@targpop, AvgYears] / ManagementVars@CM[keep(Sims), mseFramework@MseDef@targpop, AvgYearsm1]) , c(1), mean, na.rm = TRUE)
      df          <- addRows(df, CtonCtm1, "T1", "mean(C(t)/C(t-1))")

      # T3 var(F)
#      varF        <- round(apply(as.karray(F_FMSY)[keep(Sims), mseFramework@MseDef@targpop, AvgYears] * RefVars@FMSY1, MARGIN=c(1), var), 2)
#      df          <- addRows(df, varF, "T3", "var(F)")

      return (df)
    }

    for (ProjVar in .Object@ProjectedVars)
    {
      df <- rbind(data.frame(df), data.frame(extractData(ProjVar, .Object@HistoricVars, .Object@RefVars, AvgYears)))
    }

    return (df)
  }
)
