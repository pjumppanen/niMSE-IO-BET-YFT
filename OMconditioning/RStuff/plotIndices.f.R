

require("PerformanceAnalytics")


# Instantiate cache
SSModelCache <- new.env()

# -----------------------------------------------------------------------------

loadSSModel <- function(Model, SSRootDir, force=FALSE)
{
  ModelPath    <- SSRootDir %&% Model
  ModifiedTime <- file.mtime(ModelPath)
  Valid        <- FALSE

  if (!is.na(ModifiedTime))
  {
    CacheTime <- NA

    if (exists(Model, envir=SSModelCache))
    {
      CacheTime <- SSModelCache[[Model]]$ModifiedTime
    }

    if ((!is.na(CacheTime)) && (ModifiedTime > CacheTime))
    {
      force <- TRUE
    }
  }

  if (!exists(Model, envir=SSModelCache) || force)
  {
    ErrorLog <- NULL

    errorHandler <- function(e)
    {
      ErrorLog <<- paste(paste(sys.calls()), collapse="\n")
    }

    # We put the tryCatch here in case we can't load the model
    FullModel <- tryCatch(withCallingHandlers(SS_output(dir=ModelPath, covar=FALSE, ncols=213, forecast=FALSE, checkcor=FALSE, warn=FALSE, printstats=FALSE, verbose=FALSE), error=errorHandler), error=function(e) print(e))

    if (is.null(ErrorLog))
    {
      SimplifiedModel <- list(cpue                            = FullModel$cpue,
                              Length_comp_Eff_N_tuning_check  = FullModel$Length_comp_Eff_N_tuning_check,
                              derived_quants                  = FullModel$derived_quants,
                              stdExists                       = FullModel$stdExists,
                              equil_yield                     = FullModel$equil_yield,
                              catchc1.0                       = FullModel$catchc1.0,
                              timeseries                      = FullModel$timeseries,
                              parameters                      = FullModel$parameters,
                              maximum_gradient_component      = FullModel$maximum_gradient_component,
                              likelihoods_used                = FullModel$likelihoods_used,
                              likelihoods_raw_by_fleet        = FullModel$likelihoods_raw_by_fleet,
                              recruit                         = FullModel$recruit,
                              ModifiedTime                    = ModifiedTime)

      assign(Model, SimplifiedModel, envir=SSModelCache)
      rm(FullModel)
      gc()

      Valid <- TRUE
    }
    else
    {
      print(ErrorLog)
    }
  }

  return (Valid)
}

# -----------------------------------------------------------------------------

getSSModel <- function(Model)
{
  if (exists(Model, envir=SSModelCache))
  {
    SimplifiedModel <- get(Model, envir=SSModelCache)

    return (SimplifiedModel)
  }
  else
  {
    return (NA)
  }
}

# -----------------------------------------------------------------------------

chooseBestModelFit <- function(modelList, SSRootDir)
{
  GoodModels <- c()
  BadModels  <- c()
  Warnings   <- c()
  SSRootDir  <- gsub("\\\\", "/", SSRootDir)

  for (model in modelList)
  {
    modelDir <- SSRootDir %&% model %&% "/"

    BestLLH   <- 9.99e+256
    BestModel <- -1

    for (convergedNum in 0:5)
    {
      convergedDir  <- modelDir %&% "converged" %&% convergedNum %&% "/"
      CumReportFile <- convergedDir %&% "CumReport.sso"

      if (file.exists(CumReportFile))
      {
        con  <- file(CumReportFile, "rt")
        Text <- paste(readLines(con), collapse="\n")
        close(con)

        # Look for:
        # 2 Like_Value Total 17590.2
        Pattern <- "Like_Value\\s+Total\\s+\\d+\\.\\d*[eE][+-]?\\d+|Like_Value\\s+Total\\s+\\d+[eE][+-]?\\d+|Like_Value\\s+Total\\s+\\d+\\.\\d*|Like_Value\\s?Total\\s?\\d+"
        MatchA  <- regexpr(Pattern, Text, perl=TRUE)

        if (MatchA[1] >= 0)
        {
          LikelihoodText <- substr(Text, MatchA[1], MatchA[1] + attr(MatchA, "match.length") - 1)
          MatchB         <- regexpr("Like_Value\\s+Total\\s+", LikelihoodText, perl=TRUE)
          Likelihood     <- as.double(substr(LikelihoodText, MatchB[1] + attr(MatchB, "match.length"), MatchB[1] + attr(MatchB, "match.length") + nchar(LikelihoodText)))

          if (Likelihood < BestLLH)
          {
            BestLLH   <- Likelihood
            BestModel <- convergedNum
          }
        }
      }
    }

    if (BestModel >= 0)
    {
      GoodModels <- c(GoodModels, model)

      # copy the relevant files to the parent directory
      if (.Platform$OS.type == "windows")
      {
        copyCommand <- "copy " %&% modelDir %&% "converged" %&% BestModel %&% "/*.* " %&% modelDir
        copyCommand <- gsub("/", "\\\\", copyCommand)
      }
      else
      {
        copyCommand <- "cp " %&% modelDir %&% "converged" %&% BestModel %&% "/*.* " %&% modelDir
      }

      print(paste("Copying ", model %&% "/converged" %&% BestModel, "to parent"))

      shell(copyCommand)

      WarningFile <- modelDir %&% "converged" %&% BestModel %&% "/warning.sso"

      con  <- file(WarningFile, "rt")
      Text <- paste(readLines(con), collapse="\n")
      close(con)

      Warnings <- c(Warnings, "Text")
    }
    else
    {
      print(paste("Model", model, "Failed to converge"))

      BadModels <- c(BadModels, model)
    }
  }

  return (list(GoodModels=GoodModels, BadModels=BadModels, Warnings=Warnings))
}


# -----------------------------------------------------------------------------
# plotIndices.f
# -----------------------------------------------------------------------------
# Function to plot diagnostics for stock synthesis grids. Note that this updated
# version has slightly different call semantics than the original one. In this
# version a "reduced" stock synthesis output is loaded using the loadSSModel()
# function which caches loads to the SSModelCache environment for later use.
# This ensures that if the model data has been loaded recently it will load it
# from cache rather than use SS_output to load it to improve load speed.
#
# Previous implementation assumed that the global variable ref was loaded with
# the reference model statistics. In this version we specify the reference model
# by the stock synthesis data/model name which typically corresponds to the folder
# name the data files exist in. It is assumed that reference data has been
# previously loaded with a call to loadSSModel() and is in the cache. We have
# implemented it this way to avoid an extra parameter in the plotIndices.f()
# argument list (the path to the folder containing the model folder) which would
# be needed, as the reference data and the will likely be in different parent
# folders. An example of code loading the reference case would be,
#
# loadSSModel("TagLambda1", "Z:/MSE-IO-BET-YFT/OMconditioning/BET/AssessmentFiles2017/")
#
# should this function require access to other statistics not currently cached
# simply modify the loadSSModel() function above. More specifically, add the
# extra sub lists required to the SimplifiedModel list.
#
# If you want to keep the cache data between sessions then use the save() and
# load() R functions to do so. For instance,
#
# save(SSModelCache, file="ss_model_cache.RDA")
#
# to save and,
#
# load(file="ss_model_cache.RDA")
#
# to re-load the cache. The cache includes file time stamp checks to ensure
# that newer versions of the existing model are re-loaded.
# -----------------------------------------------------------------------------
plotIndices.f <- function(modList = gridZList,
                          MSYyLim=c(0,300),
                          mFile = F,
                          mfrowLayout = c(3,2),
                          SPB_Yr="SPB_276",
                          inputRefLines=T, #override list below which are extracted from reports etc
                          refSSB0=c(3421,2492,4628),
                          refSSBY=c(786,457,1009),
                          refSSBMSY=c(1200,751,1483),
                          refSSBYoSSBMSY=c(0.66,0.48,0.72),
                          refSSBYoSSB0=c(0.23,0.17,0.32),
                          refMSY=c(423),
                          doPlots=T,
                          SSRootDir="",
                          refModel="TagLambda1",
                          maxAverageRecruitThreshold=3.0)
{
  # temporary check to figure out how 4 and 7 CPUE series configurations got tangled together?!
  ok <- notok <- NULL

  for (i in 1:length(modList))
  {
    print(c(i,modList[i]))

    # load the model data
    if (loadSSModel(modList[i], SSRootDir))
    {
      m <- getSSModel(modList[i])

      print(c("nCPUE  ", length(levels(as.factor(m$cpue$FleetName)))))

      if (length(levels(as.factor(m$cpue$FleetName))) == 4)
        ok <- c(ok, modList[i])

      if (length(levels(as.factor(m$cpue$FleetName))) == 7)
        notok <- c(notok, modList[i])

      rm(m)
    }
    else
    {
      # model missing
      notok <- c(notok, modList[i])
    }
  }

  print(notok)

  modList <- ok

  if (inputRefLines)
  {
    ref <- getSSModel(refModel)

    refMSY         <- ref$derived_quants[ref$derived_quants$LABEL == 'TotYield_MSY',]$Value * 4 / 1000
    refSSB0        <- ref$derived_quants[ref$derived_quants$LABEL == 'SPB_Virgin',]$Value / 1000
    refSSBY        <- ref$derived_quants[ref$derived_quants$LABEL == SPB_Yr,]$Value / 1000
    refSSBMSY      <- ref$derived_quants[ref$derived_quants$LABEL == 'SSB_MSY',]$Value / 1000
    refSSBYoSSBMSY <- refSSBY / refSSBMSY
    refSSBYoSSB0   <- refSSBY / refSSB0
  }

  par(mar = c(4, 4, 3, 1) + 0.1)

  factorList <- array(NA, dim = c(length(modList), length(unlist(strsplit(modList[1], split = "_")))))

  for (mi in 1:length(modList))
  {
    factors <- strsplit(modList[mi], split = "_")

    #one-off kludge:
    #if(length(unlist(factors))==8) factors[[9]] <- "ess5"

    factorList[mi, ] <- unlist(factors)
  }

  print(factorList)

  numMod    <- length(modList)
  m         <- getSSModel(modList[1])
  numFleets <- length(m$Length_comp_Eff_N_tuning_check$mean_effN)
  numCPUE   <- length(levels(as.factor(m$cpue$FleetName)))

  rm(m)

  pBoundall         <- NULL
  MSYall            <- NULL
  SSBYall           <- NULL
  BMinall           <- NULL
  BMinoSSBMSYall    <- NULL
  SSBYoSSBMSYall    <- NULL
  SSBMSYall         <- NULL
  SSBYoSSB0all      <- NULL
  SSBMSYoSSB0all    <- NULL
  FYoFMSYall        <- NULL
  CYoMSYall         <- NULL
  essall            <- array(NA, dim = c(numMod, numFleets))
  cpueRMSEall       <- array(NA, dim = c(numMod, numCPUE))
  cpueRMSEByYrall   <- array(NA, dim = c(numMod, numCPUE))  # use means of CPUE to emphasize interannual variability
  gradall           <- NULL
  stdExistsall      <- NULL
  boundCheckall     <- NULL
  LLHall            <- NULL
  tagFit            <- NULL
  recFit            <- NULL # rec LLH from SS3, including whatever treatment of constants and bias corrections
  recFit2           <- NULL #
  recFit2ByYr       <- NULL # annualized rec CV to focus on internannual variability
  rTrendall         <- NULL
  boundFailLOList   <- NULL
  boundFailHIList   <- NULL
  boundFailHIList2  <- list()
  PSFS5ParMax       <- -999
  PSFS5ParMaxMod    <- NA
  crashPen          <- NULL
  catchLLH          <- NULL

  # these arrays are for a recuitment diagnostic checking for recruitment spikes
  maxEstRecruitDev  <- NA
  minEstRecruitDev  <- NA
  meanEstRecruitDev <- NULL
  qtrlyRecruitment  <- c()
  quarters          <- c()
  models            <- c()
  nmodels           <- 0
  recruitWarnings   <- list()

  for (i in 1:length(modList))
  {
    if (modList[i] != "nullMod")
    {
      m <- getSSModel(modList[i])

      # update recruitment spike diagnostics
      Recruit           <- m$recruit$pred_recr
      SmoothedRecruit   <- lowess(Recruit)$y
      RecruitDevEst     <- Recruit / SmoothedRecruit
      qtrlyRecruitment  <- c(qtrlyRecruitment, Recruit)
      quarters          <- c(quarters, 1:length(Recruit))
      models            <- rep(i, times=length(Recruit))

      if (!any(is.na(RecruitDevEst)))
      {
        nmodels           <- nmodels + 1
        maxEstRecruitDev  <- pmax(maxEstRecruitDev, RecruitDevEst, na.rm=TRUE)
        minEstRecruitDev  <- pmin(minEstRecruitDev, RecruitDevEst, na.rm=TRUE)

        if (is.null(meanEstRecruitDev))
        {
          meanEstRecruitDev <- RecruitDevEst
        }
        else
        {
          meanEstRecruitDev <- meanEstRecruitDev + RecruitDevEst
        }

        # local check for spike
        maxRecruitDev     <- max(RecruitDevEst, na.rm=TRUE)
        avgRecruitDev     <- mean(RecruitDevEst, na.rm=TRUE)
        maxAvgRatio       <- maxRecruitDev / avgRecruitDev
        PossibleSpike     <- (maxAvgRatio > maxAverageRecruitThreshold)

        if (PossibleSpike)
        {
          recruitWarnings[[length(recruitWarnings) + 1]] <- paste("possible recruitment spike in model", modList[i], ". Max to Average ratio =", maxAvgRatio)
        }
      }

      # .std file exists (does not distinguish between a Hessian inversion failure, or whether it was not calculated in the interest of time)
      stdExistsall <- c(stdExistsall, m$stdExists)

      #MSY       <- max(m$equil_yield$Catch)
      MSY       <- m$derived_quants[m$derived_quants$LABEL == 'TotYield_MSY',]$Value
      MSYall    <- c(MSYall, MSY)
      #BMin      <- min(m$equil_yield$Depletion)
      #BMinall   <- c(BMinall, BMin)
      #SSBY      <- as.numeric(m$derived_quants[substring(m$derived_quants$LABEL, 1, 8) == SPB_Yr, ]$Value)
      SSBY      <- m$derived_quants[m$derived_quants$LABEL == SPB_Yr,]$Value
      SSBYall   <- c(SSBYall,SSBY)
      #SSB0      <- as.numeric(subset(m$derived_quants, m$derived_quants$LABEL == "SPB_Virgin")$Value)
      SSB0      <- m$derived_quants[m$derived_quants$LABEL == 'SPB_Virgin',]$Value

      #SSBMSY    <- m$equil_yield$Depletion[m$equil_yield$Catch == MSY][1] * SSB0
      SSBMSY    <- m$derived_quants[m$derived_quants$LABEL == 'SSB_MSY',]$Value
      SSBMSYall <- c(SSBMSYall,SSBMSY)

      #BMinoSSBMSY     <- BMin/m$equil_yield$Depletion[m$equil_yield$Catch == MSY][1]
      #BMinoSSBMSYall  <- c(BMinoSSBMSYall, BMinoSSBMSY)
      SSBYoSSBMSYall  <- c(SSBYoSSBMSYall, SSBY/SSBMSY)
      SSBMSYoSSB0all  <- c(SSBMSYoSSB0all, SSBMSY/SSB0)
      SSBYoSSB0all    <- c(SSBYoSSB0all, SSBY/SSB0)

      #CY         <- sum(m$catchc1.0[m$timeseries$Yr == 2009])
      #CYoMSYall  <- c(CYoMSYall, CY/MSY)

      #print(c("BMin", BMin))
      print(c("MSY", MSY))
      print(c("SSBY", SSBY))
      print(c("SSB0", SSB0))
      #print(c("dep", m$equil_yield$Depletion[m$equil_yield$Catch ==  MSY]))
      print(c("SSBMSY", SSBMSY))
      #print(c("CY", CY))

      #pBound      <- nrow(subset(m$parameters, (m$parameters[9] ==     "HI" | m$parameters[9] == "LO"))[1:9])
      #pBoundall   <- c(pBound, pBoundall)
      ess         <- m$Length_comp_Eff_N_tuning_check$mean_effN
      names(ess)  <- m$Length_comp_Eff_N_tuning_check$Fleet

      colnames(essall) <- m$Length_comp_Eff_N_tuning_check$Fleet
      essall[i, ]      <- as.numeric(ess)

      tmp             <- aggregate(m$cpue$Dev^2, by = list(fleetName = m$cpue$FleetName), FUN = mean)
      cpueRMSE        <- sqrt(tmp[, 2])
      names(cpueRMSE) <- tmp[, 1]

      #CPUE RMSE among years
      cpueCalendarYr <- floor(seasAsYrToDecYr.f(seasAsYr=m$cpue$Yr, endSeasAsYr=356, numSeas=4, endYr=2015, endSeas=4))

      #remove years with some missing seasons
      tmp <- table(m$cpue$FleetName, cpueCalendarYr)

      #how many years with obs for all seasons and fleets?
      sum(colSums(tmp) == numCPUE * 4)

      #identify years with CPUE for all seasons and fleets simultaneously
      fullCPUEYrs    <- names(colSums(tmp)[colSums(tmp)==numCPUE*4])
      cpueDevFullSet <- cpueCalendarYr %in% fullCPUEYrs
      cpueDevSet     <- logical(length(m$cpue$Dev))

      for (ii in 1:length(m$cpue$Dev))
      {
        print(c(ii,m$cpue$FleetName[ii], as.character(cpueCalendarYr[ii]), tmp[m$cpue$FleetName[ii],as.character(cpueCalendarYr[ii])], tmp[m$cpue$FleetName[ii],as.character(cpueCalendarYr[ii])]==4))
        cpueDevSet[ii] <- tmp[m$cpue$FleetName[ii],as.character(cpueCalendarYr[ii])]==4
      }

      #cpueDevByYrByFleet  <- aggregate(m$cpue$Dev[cpueDevFullSet], by = list(fleetName = m$cpue$FleetName[cpueDevFullSet], year=cpueCalendarYr[cpueDevFullSet]), FUN = mean)
      cpueDevByYrByFleet  <- aggregate(m$cpue$Dev[cpueDevSet], by = list(fleetName = m$cpue$FleetName[cpueDevSet], year=cpueCalendarYr[cpueDevSet]), FUN = mean)
      tmp                 <- aggregate(cpueDevByYrByFleet$x^2, by = list(fleetName = cpueDevByYrByFleet$fleetName), FUN = mean)
      cpueRMSEByYr        <- sqrt(tmp[, 2])
      names(cpueRMSEByYr) <- tmp[, 1]

      #tmp2             <- aggregate(m$cpue$Dev^2, by = list(fleetName = m$cpue$FleetName), FUN = mean)
      #cpueRMSE         <- sqrt(tmp[, 2])
      #names(cpueRMSE)  <- tmp[, 1]

      #adapt this and include autocorrelation
      #if ("SURVEY1" %in% names(cpueRMSE)) {
      #    tmp <- m$cpue
      #    tmp <- subset(tmp, tmp$FleetName %in% c("SURVEY1",
      #      "SURVEY2"))
      #    tmp <- sqrt(mean(tmp$Dev^2))
      #    cpueRMSE[names(cpueRMSE) == "SURVEY1"] <- tmp
      #}
      #cpueRMSE <- cpueRMSE[names(cpueRMSE) != "UJ91p_SW"]

      if ("JS" %in% factorList[i, ])
      {
      }

      cpueRMSEall[i, ]     <- as.numeric(cpueRMSE)
      cpueRMSEByYrall[i, ] <- as.numeric(cpueRMSEByYr)
      gradall              <- c(gradall, m$maximum_gradient_component)
      boundCheckall        <- c(boundCheckall, sum(m$parameters$Status=="HI" | m$parameters$Status=="LO", na.rm=T))

      if (boundCheckall[i] > 0)
      { #various one-off things done for testing...
        print("bound check high")
        print(c(modList[i], boundCheckall[i]))
        #print(subset(m$parameters, subset=m$parameters$Status=="HI" | m$parameters$Status=="LO", select=c(Label, Min, Max, Value)))
        print(subset(m$parameters, subset=m$parameters$Status=="HI", select=c(Label, Min, Max, Value)))

        boundFailHIListLab    <- c(subset(m$parameters, subset=m$parameters$Status=="HI", select=c(Label)))
        boundFailLOList       <- c(boundFailLOList, subset(m$parameters, subset=m$parameters$Status=="LO", select=c(Label)))
        boundFailHIList       <- c(boundFailHIList, subset(m$parameters, subset=m$parameters$Status=="HI", select=c(Label)))
        boundFailHIList2[[i]] <- c(modList[i], subset(m$parameters, subset=m$parameters$Status=="HI", select=c(Label)))
        PSFS5ParTest          <- -999
        tmp                   <- subset(m$parameters, subset=m$parameters$Status=="HI" & m$parameters$Label=="AgeSpline_GradHi_PSFS1_5", select=c(Value))

        if (nrow(tmp) == 1)
          PSFS5ParTest <- unlist(tmp)

        if (PSFS5ParTest > PSFS5ParMax)
        {
          PSFS5ParMax    <- PSFS5ParTest
          PSFS5ParMaxMod <- modList[i]
        }

        #if("AgeSel_11P_3_BB1" %in% boundFailHIListLab) browser()
      }

      LLHall   <- c(LLHall, m$likelihoods_used$values[1])

      tmp      <- m$likelihoods_raw_by_fleet[m$likelihoods_raw_by_fleet[,1]=="Tag_comp_Like" | m$likelihoods_raw_by_fleet[,1]=="Tag_negbin_Like",]
      tagFit   <- c(tagFit, sum(as.numeric(tmp[1,3:length(tmp)])) + sum(as.numeric(tmp[2,3:length(tmp)])))
      recFit   <- c(recFit, m$likelihoods_used['Recruitment','values'])
      crashPen <- c(crashPen, m$likelihoods_used['Crash_Pen','values'])
      catchLLH <- c(catchLLH, m$likelihoods_used['Catch','values'])

      rdev     <- m$recruit$dev[m$recruit$era == "Main"]
      nrdev    <- length(rdev)
      rSlope   <- unname(coefficients(lm(rdev ~ c(1:nrdev)))[2])
      #rTrend   <- length(m$recruit$dev[m$recruit$era == "Main"]) * rSlope * 100

      #seasAsYrToDecYr.f(seasAsYr=101, endSeasAsYr=272, numSeas=4, endYr=2014, endSeas=4)

      # possibly misaligned by small amount, but this is irrelevant for CV calculation...
      rDevCalendarYr <- seasAsYrToDecYr.f(seasAsYr=m$recruit$year[m$recruit$era == "Main"], endSeasAsYr=356, numSeas=4, endYr=2015, endSeas=4)
      rdevByYr       <- tapply(rdev, as.factor(floor(rDevCalendarYr)), FUN=mean)
      recFit2        <- c(recFit2,sqrt(sum(rdev^2)/nrdev))
      recFit2ByYr    <- c(recFit2ByYr,sqrt(sum(rdevByYr^2)/(nrdev/4)))
      rTrendall      <- c(rTrendall, rSlope)

      rm(m)
    }
    else
    {
      MSYall                <- c(MSYall, NA)
      SSBMSYall             <- c(SSBMSYall, NA)
      SSBYoSSBMSYall        <- c(SSBYoSSBMSYall, NA)
      SSBYoSSB0all          <- c(SSBYoSSB0all, NA)
      SSBMSYoSSB0all        <- c(SSBMSYoSSB0all, NA)
      FYoFMSYall            <- c(FYoFMSYall, NA)
      CYoMSYall             <- c(CYoMSYall, NA)
      SSBYall               <- c(SSBYall, NA)
      essall[i, ]           <- NA
      cpueRMSEall[i, ]      <- NA
      cpueRMSEByYrall[i, ]  <- NA
      gradall               <- c(gradall, NA)
      stdExistsall          <- c(stdExistsall, NA)
      boundCheckall         <- c(boundCheckall, NA)
      LLHall                <- c(LLHall, NA)
      tagFit                <- c(tagFit, NA)
      recFit                <- c(recFit, NA)
      recFit2               <- c(recFit2, NA)
      recFit2ByYr           <- c(recFit2ByYr, NA)
      rTrendall             <- c(rTrendall, NA)
      crashPen              <- c(crashPen, NA)
      catchLLH              <- c(catchLLH, NA)
    }
  }   # modList i

  meanEstRecruitDev <- meanEstRecruitDev / nmodels

  print("pre-plots done")

  if (doPlots)
  {
    # plot recruitment deviates summary
    dt <- data.table(qtr=1:length(maxEstRecruitDev), max=maxEstRecruitDev, min=minEstRecruitDev, mean=meanEstRecruitDev)

    print(ggplot(dt, aes(x = qtr, ymin = 0)) +
                 ylab("recruitment deviates") +
                 theme_bw() +
                 ggtitle("Estimated recruitment\n deviates summary (max, mean & min).") +
                 geom_line(aes(y = `min`), size=0.5, colour="blue") +
                 geom_line(aes(y = `max`), size=0.5, colour="red") +
                 geom_ribbon(aes(ymin = `min`, ymax = `mean`), fill = "grey", alpha = 0.6) +
                 geom_ribbon(aes(ymin = `mean`, ymax = `max`), fill = "grey", alpha = 0.8) +
                 geom_line(aes(y = `mean`), size=1))

    # plot recruitment time series distribution
    dt <- data.table(qtr=quarters, model=models, recruitment=qtrlyRecruitment)

    dt <- dt[, as.list(quantile(recruitment, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)), keyby = list(qtr)]

    print(ggplot(dt, aes(x = qtr, ymin = 0)) +
                 ylab("recruitment") +
                 theme_bw() +
                 ggtitle("Recruitment") +
                 geom_line(aes(y = `10%`), size=0.5, colour="blue") +
                 geom_line(aes(y = `90%`), size=0.5, colour="red") +
                 geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "grey", alpha = 0.6) +
                 geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "grey", alpha = 0.8) +
                 geom_line(aes(y = `50%`), size=1))

    print(sum(stdExistsall))
    print(length(stdExistsall))
    par(mfrow=c(2,1))
    hist(log10(gradall)[stdExistsall==T], main = "max(gradient) - Hessian OK", breaks=-10:20)
    hist(log10(gradall)[stdExistsall==F], main = "max(gradient) - Hessian not pos def - ", breaks=-10:20)

    par(mfrow=c(3,1))
    hist(boundCheckall, main = "Par bounds exceptions - all", breaks=-10:20)
    hist(boundCheckall[stdExistsall==T], main = "Par bounds exceptions - Hessian OK", breaks=-10:20)
    plot(log10(gradall), boundCheckall, main = "Par bounds exceptions")

    table(unlist(boundFailLOList))
    table(unlist(boundFailHIList))

    print(LLHall)
    LLHall <- LLHall - min(LLHall, na.rm = T)
    fList  <- NULL

    for (i in 1:ncol(factorList))
    {
      if (length(unique(factorList[, i])) > 1)
        fList <- cbind(fList, factorList[, i])
    }

    print(LLHall)
    print(dim(fList))
    par(mfrow = mfrowLayout)
    boxplot(LLHall, ylab = "Likelihood units")

    if (!is.null(fList))
    {
      for (i in 1:ncol(fList))
      {
        boxplot(LLHall ~ fList[, i], ylab = "Likelihood units")
      }
    }

    par(mfrow = c(1, 1))
    boxplot(LLHall ~ fList[, 1]:fList[, 2], ylab = "Likelihood units")

    if (ncol(fList) > 2)
      boxplot(LLHall ~ fList[, 1]:fList[, 3], ylab = "Likelihood units")

    #par(mfrow = c(1,1))
    #boxplot(tagFit ~ fList[,], ylab = "Tag likelihood (no lambda)", col=8)
    #first <- length(unique(fList[,1]))+1
    #
    #for (i in 2:ncol(fList)) {
    #  last <- first + length(unique(fList[,i]))-1
    #  boxplot(tagFit ~ fList[,i], col=i, add=T, at=first:last)
    #  first <- last+1
    #}

    par(mfrow = c(1,1))
    spacer <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))

    boxplot(cbind(tagFit,spacer), ylab = "Tag likelihood (no lambda)", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2
    colList=c(8,2:100)

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(tagFit ~ fList[,i], col=colList[i], add=T, at=first:last)

      first <- last+1
    }

    #tagLLHs for full mixing, t10 only - one off code not robust
    tagPartition <- F

    if (tagPartition)
    {
      # partition by x3 and x8
      par(mfrow = c(1,1))

      dropCol <- which.max(colSums(fList[] == "x3"))
      tagFit2 <- tagFit[fList[,dropCol]=="x3"]
      fList2  <- fList[fList[,dropCol]=="x3",]
      fList2  <- fList2[,fList2[1,] != "x3"]

      boxplot(tagFit2 ~ fList2[,], ylab = "Tag likelihood (no lambda)", main="x3 - 3 qtr tag mixing", names=rep(" ",length(unique(as.vector(fList2)))))

      first   <- 1
      colList <- c(8,2:ncol(fList2))

      for (i in 1:ncol(fList2))
      {
        last <- first + length(unique(fList2[,i]))-1

        boxplot(tagFit2 ~ fList2[,i], col=colList[i], add=T, at=first:last)

        first <- last+1
      }

      dropCol <- which.max(colSums(fList[] == "x8"))
      tagFit2 <- tagFit[fList[,dropCol]=="x8"]
      fList2  <- fList[fList[,dropCol]=="x8",]
      fList2  <- fList2[,fList2[1,] != "x8"]

      boxplot(tagFit2 ~ fList2[,], ylab = "Tag likelihood (no lambda)", main="x8 - 8 qtr tag mixing", names=rep(" ",length(unique(as.vector(fList2)))))

      first   <- 1

      for (i in 1:ncol(fList2))
      {
        last <- first + length(unique(fList2[,i]))-1

        boxplot(tagFit2 ~ fList2[,i], col=colList[i], add=T, at=first:last)

        first <- last+1
      }

      par(mfrow = c(1,1))

      dropCol <- which.max(colSums(fList[] == c("x3")))
      tagFit2 <- tagFit[fList[,dropCol]=="x3"]
      fList2  <- fList[fList[,dropCol]=="x3",]
      fList2  <- fList2[,fList2[1,] != "x3"]

      dropCol <- which.max(colSums(fList2[] == c("t10")))
      tagFit3 <- tagFit2[fList2[,dropCol]=="t10"]
      fList3  <- fList2[fList2[,dropCol]=="t10",]
      fList3  <- fList3[,fList3[1,] != "t10"]

      boxplot(tagFit3 ~ fList3[,], ylab = "Tag likelihood (no lambda)", main="(x3, t10) 3 qtr tag mixing and lambda=1.0", names=rep(" ",length(unique(as.vector(fList3)))))

      first   <- 1
      colList <- c(8,2:ncol(fList3))

      for (i in 1:ncol(fList3))
      {
        last <- first + length(unique(fList3[,i]))-1

        boxplot(tagFit3 ~ fList3[,i], col=colList[i], add=T, at=first:last)

        first <- last+1
      }
    } #tagPartiion

    par(mfrow = mfrowLayout)
    boxplot(gradall, ylab = "max. gradient")

    if (!is.null(fList))
    {
      for (i in 1:ncol(fList))
      {
        boxplot(gradall ~ fList[, i], ylab = "max. gradient xxx")
      }
    }

    par(mfrow = mfrowLayout)

    boxplot(log10(gradall), ylab = "max. gradient")

    if (!is.null(fList))
    {
      for (i in 1:ncol(fList))
      {
          boxplot(log10(gradall) ~ fList[, i], ylab = "log10(max Grad)")
      }
    }

    #par(mfrow = c(1,1))
    #boxplot(gradall ~ fList[,], ylab = "log10(max. gradient)", col=8)
    #3first <- length(unique(fList[,1]))+1
    #
    #for (i in 2:ncol(fList)) {
    #  last <- first + length(unique(fList[,i]))-1
    #  boxplot(log10(gradall) ~ fList[,i], col=i, add=T, at=first:last)
    #      first <- last+1
    #}

    par(mfrow = c(1,1))

    spacer <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))

    boxplot(cbind(log10(gradall),spacer), ylab = "log10(max. gradient)", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    #boxplot(log10(gradall) ~ fList[,], ylab = "log10(max. gradient)", names=rep(" ",length(unique(as.vector(fList)))))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(log10(gradall) ~ fList[,i], col=colList[i], add=T, at=first:last, outcol=colList[i])

      first <- last+1
    }

    #check status on bounds - relies on r4ss definition of close to bounds Status = HI or LO(soft limit presumably)
    par(mfrow = c(1,1))

    spacer <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))

    boxplot(cbind(boundCheckall,spacer), ylab = "Par bounds issues", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    #boxplot(boundCheckall ~ fList[,], ylab = "Par bounds issues", names=rep(" ",length(unique(as.vector(fList)))))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(boundCheckall ~ fList[,i], col=colList[i], add=T, at=first:last, outcol=colList[i])

      first <- last+1
    }

    #par(mfrow = mfrowLayout)
    #
    #boxplot(pBoundall, ylab = "N par near bounds")
    #
    #for (i in 1:ncol(fList)) {
    #    boxplot(pBoundall ~ fList[, i], ylab = "N par near bounds")
    #}
    #
    #par(mfrow = mfrowLayout)
    #boxplot(BMinall, ylab = "Minimum depletion at high F (B/B0)",
    #    ylim = c(0, 0.25))
    #for (i in 1:ncol(fList)) {
    #    boxplot(BMinall ~ fList[, i], ylab = "B/B0 (at high F)", ylim = c(0, 0.25))
    #}
    #par(mfrow = mfrowLayout)
    #boxplot(BMinoSSBMSYall, ylab = "BMin/BMSY)", ylim = c(0,
    #    1))
    #for (i in 1:ncol(fList)) {
    #    boxplot((BMinoSSBMSYall) ~ fList[, i], ylab = "BMin/BMSY ", ylim = c(0, 1))
    #}

    for (f in 1:numFleets)
    {
      par(mfrow = mfrowLayout)

      boxplot(essall[, f], ylab = "mean ESS", main = "ESS " %&% colnames(essall)[f])

      if (!is.null(fList))for (i in 1:ncol(fList))
      {
        boxplot(essall[, f] ~ fList[, i], ylab = "mean ESS", main = "ESS " %&% colnames(essall)[f])
      }
    }

    par(mfrow = c(1,1))

    boxplot(essall[,],ylim=c(0,150),xlab="Fishery", ylab="ESS")
    abline(h=5)
    boxplot(essall[,],ylim=c(0,600),xlab="Fishery", ylab="ESS")
    abline(h=5)
    boxplot(log(essall[,]),ylim=c(0,7),xlab="Fishery", ylab="log(ESS)")
    abline(h=log(5))

    par(mfrow = mfrowLayout)

    boxplot(4*MSYall/1000, ylab = "MSY (1000 t) (X4)", ylim=MSYyLim)
    abline(h=refMSY,col=3)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(4*MSYall/1000 ~ fList[, i], ylab = "MSY (1000 t) (X4)", ylim=MSYyLim)
      abline(h=refMSY,col=3)
    }

    par(mfrow = c(1,1))

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(crashPen,spacer)

    boxplot(plotDat, ylab = "LLH units", main="SS3 Crash Penalty", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    for (i in 1:ncol(fList))
    {
      last  <- first + length(unique(fList[,i])) - 1

      boxplot(crashPen ~ fList[,i], col=colList[i], add=T, at=first:last)

      first <- last+1
    }

    par(mfrow = c(1,1))
    spacer <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(catchLLH,spacer)
    boxplot(plotDat, ylab = "LLH units", main="SS3 Catch LLH", names=c("all",rep(" ",length(unique(as.vector(fList))))))
    first <- 2
    #boxplot(4*MSYall/1000 ~ fList[,], ylab = "MSY (1000 t)", names=rep(" ",length(unique(as.vector(fList)))), ylim=c(0,1000))
    #first <- 1
    for (i in 1:ncol(fList)) {
      last <- first + length(unique(fList[,i]))-1
      boxplot(catchLLH ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last),1.01*max(catchLLH), round(table(fList[,i])/sum(table(fList[,i])),2))
      first <- last+1
    }

    hist(log10(catchLLH), breaks=100, main='SS3 Catch likelihood')

    #par(mfrow = c(1,1))
    #
    #boxplot(4*MSYall/1000 ~ fList[,], ylab = "MSY", col=8, ylim=c(0,1000))
    #
    #first <- length(unique(fList[,1]))+1
    #
    #for (i in 2:ncol(fList)) {
    #  last <- first + length(unique(fList[,i]))-1
    #  boxplot(4*MSYall/1000 ~ fList[,i], col=i, add=T, at=first:last)
    #  first <- last+1
    #}
    #
    #abline(h=refMSY,col=1)

    par(mfrow = c(1,1))

    hist(4*MSYall/1000, breaks=20, main = "MSY (1000 t)",xlab="")
    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(4*MSYall/1000,spacer)

    boxplot(plotDat, ylab = "MSY (1000 t)", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    #boxplot(4*MSYall/1000 ~ fList[,], ylab = "MSY (1000 t)", names=rep(" ",length(unique(as.vector(fList)))), ylim=c(0,1000))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(4*MSYall/1000 ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last),1.01 * max(4 * MSYall / 1000), round(table(fList[,i]) / sum(table(fList[,i])), 2))

      first <- last+1
    }

    abline(h=refMSY,col=1)

    par(mfrow = mfrowLayout)

    boxplot(SSBYall/1000, ylab = "SSBY (1000 t)", ylim = c(0, 2500))
    abline(h=refSSBY,col=3)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(SSBYall/1000 ~ fList[, i], ylab = "SSBY (1000 t)", ylim = c(0, 2500))
      abline(h=refSSBY,col=3)
    }

    par(mfrow = mfrowLayout)

    boxplot(SSBMSYall/1000, ylab = "SSBMSY (1000 t)", ylim = c(0, 3000))
    abline(h=refSSBMSY,col=3)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(SSBMSYall/1000 ~ fList[, i], ylab = "SSBMSY (1000 t)", ylim = c(0, 3000))
      abline(h=refSSBMSY,col=3)
    }

    par(mfrow = mfrowLayout)
    boxplot(SSBYoSSBMSYall, ylab = "SSBY / SSBMSY", ylim=c(0,3.5))
    abline(h=refSSBYoSSBMSY,col=3)
    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(SSBYoSSBMSYall ~ fList[, i], ylab = "SSBY / SSBMSY", ylim=c(0,3.5))
      lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
      abline(h=refSSBYoSSBMSY,col=3)
    }

    #par(mfrow = c(1,1))
    #boxplot(SSBYoSSBMSYall ~ fList[,], ylab = "SSB(2014)/SSBMSY", col=8, ylim=c(0,1.5))
    #first <- length(unique(fList[,1]))+1
    #for (i in 2:ncol(fList)) {
    #  last <- first + length(unique(fList[,i]))-1
    #  boxplot(SSBYoSSBMSYall ~ fList[,i], col=i, add=T, at=first:last)
    #  first <- last+1
    #}
    #abline(h=refSSBYoSSBMSY,col=1)

    par(mfrow = c(1,1))
    hist(SSBYoSSBMSYall, breaks=20, main = "SSB(Current) / SSBMSY",xlab="")

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(SSBYoSSBMSYall,spacer)

    boxplot(plotDat, ylab = "SSB(2015)/SSBMSY", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first   <- 2

    #boxplot(SSBYoSSBMSYall ~ fList[,], ylab = "SSB(2014)/SSBMSY", names=rep(" ",length(unique(as.vector(fList)))), ylim=c(0,1.5))
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(SSBYoSSBMSYall ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last), 1.01 * max(SSBYoSSBMSYall), round(table(fList[,i]) / sum(table(fList[,i])), 2))

      first <- last+1
    }

    abline(h=refSSBYoSSBMSY,col=1)

    par(mfrow = mfrowLayout)

    boxplot(SSBMSYoSSB0all, ylab = "SSBMSY / SSB0", ylim = c(0, 1))
    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(SSBMSYoSSB0all ~ fList[, i], ylab = "SSBMSY / SSB0", ylim = c(0, 1))
      text((first:last), 1.01 * max(SSBMSYoSSB0all), round(table(fList[,i]) / sum(table(fList[,i])), 2))
      lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    }

    par(mfrow = mfrowLayout)

    boxplot(SSBYoSSB0all, ylab = "SSBY / SSB0", ylim = c(0, 1))
    abline(h=refSSBYoSSB0,col=3)
    lines(c(0, 10000), c(0.2, 0.2), lty = 2, col = 2)
    lines(c(0, 10000), c(0.4, 0.4), lty = 2, col = 2)

    if (!is.null(fList))
    {
      for (i in 1:ncol(fList))
      {
        boxplot(SSBYoSSB0all ~ fList[, i], ylab = "SSBY / SSB0", ylim = c(0, 1))
        text((first:last), 1.01 * max(SSBYoSSB0all), round(table(fList[,i]) / sum(table(fList[,i])), 2))
        lines(c(0, 10000), c(0.2, 0.2), lty = 2, col = 2)
        lines(c(0, 10000), c(0.4, 0.4), lty = 2, col = 2)
        abline(h=refSSBYoSSB0,col=3)
      }
    }

    #par(mfrow = c(1,1))
    #boxplot(SSBYoSSB0all ~ fList[,], ylab = "SSB(2014)/SSB0", col=8, ylim=c(0,1))
    #first <- length(unique(fList[,1]))+1
    #for (i in 2:ncol(fList)) {
    #  last <- first + length(unique(fList[,i]))-1
    #  boxplot(SSBYoSSB0all ~ fList[,i], col=i, add=T, at=first:last)
    #  first <- last+1
    #}
    #abline(h=refSSBYoSSB0,col=1)

    par(mfrow = c(1,1))

    hist(SSBYoSSB0all, breaks=20, main = "SSB(current) / SSB0",xlab="")

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(SSBYoSSB0all,spacer)

    boxplot(plotDat, ylab = "SSB(current)/SSB0", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first   <- 2

    #boxplot(SSBYoSSB0all ~ fList[,], ylab = "SSB(current)/SSB0", names=rep(" ",length(unique(as.vector(fList)))), ylim=c(0,1))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(SSBYoSSB0all ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last), 1.01 * max(SSBYoSSB0all), round(table(fList[,i]) / sum(table(fList[,i])), 2))

      first <- last+1
    }

    abline(h=refSSBYoSSB0,col=1)

    #par(mfrow = mfrowLayout)
    #boxplot(CYoMSYall, ylab = "C(2009) / MSY")
    #lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    #for (i in 1:ncol(fList)) {
    #    boxplot(CYoMSYall ~ fList[, i], ylab = "C(2009) / MSY")
    #    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    #}

    par(mfrow = mfrowLayout)

    boxplot(recFit2ByYr, ylab = "Observed Annual Rec RMSE ", ylim = c(0, 1))

    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(recFit2ByYr ~ fList[, i], ylab = "Rec RMSE", ylim = c(0, 0.8))
      lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    }

    par(mfrow = mfrowLayout)

    boxplot(cpueRMSEall, ylab = "CPUE RMSE All", ylim = c(0, 0.8))
    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)

    if (!is.null(fList))
    {
      for (i in 1:ncol(fList))
      {
        boxplot(cpueRMSEall ~ fList[, i], ylab = "CPUE RMSE All", ylim = c(0, 0.8))
        lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
      }
    }

    par(mfrow = c(2,1))

    for (r in 1:4)
    {  #region
      boxplot(cpueRMSEall[,r] ~ fList[,], ylab = "CPUE RMSE", main="Region " %&% r, col=8)

      first <- length(unique(fList[,1]))+1

      for (i in 2:ncol(fList))
      {
        last <- first + length(unique(fList[,i]))-1

        boxplot(cpueRMSEall[,r] ~ fList[,i], col=i, add=T, at=first:last)

        first <- last+1
      }
    }

    par(mfrow = c(1,1))

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(recFit2ByYr,spacer)

    boxplot(plotDat, ylab = "Rec RMSE", ylim=c(0,0.9), main="Obs Recruitment RMSE (annual)", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first   <- 2
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(recFit2ByYr ~ fList[,i], col=colList[i], add=T, at=first:last)

      first <- last+1
    }

    par(mfrow = c(1,1))

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(rowMeans(cpueRMSEall),spacer)

    boxplot(plotDat, ylab = "CPUE RMSE", main="mean(all regions) ", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    #boxplot(cpueRMSEall[,r] ~ fList[,], ylab = "CPUE RMSE", main="Region " %&% r, names=rep(" ",length(unique(as.vector(fList)))))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(rowMeans(cpueRMSEall) ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last),1.01*max(rowMeans(cpueRMSEall)), round(table(fList[,i])/sum(table(fList[,i])),2))

      first <- last+1
    }

    par(mfrow = c(1,1))

    for(r in 1:4)
    {  #region
      spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
      plotDat <- cbind(cpueRMSEall[,r],spacer)

      boxplot(plotDat, ylab = "CPUE RMSE", main="Region " %&% r, names=c("all",rep(" ",length(unique(as.vector(fList))))))

      first <- 2

      #boxplot(cpueRMSEall[,r] ~ fList[,], ylab = "CPUE RMSE", main="Region " %&% r, names=rep(" ",length(unique(as.vector(fList)))))
      #
      #first   <- 1
      colList <- c(8,2:ncol(fList))

      for (i in 1:ncol(fList))
      {
        last <- first + length(unique(fList[,i]))-1

        boxplot(cpueRMSEall[,r] ~ fList[,i], col=colList[i], add=T, at=first:last)

        first <- last+1
      }
    }

    par(mfrow = c(1,1))

    spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
    plotDat <- cbind(rowMeans(cpueRMSEByYrall),spacer)

    boxplot(plotDat, ylab = "CPUE RMSE (annualized) ", main="mean(all regions) ", names=c("all",rep(" ",length(unique(as.vector(fList))))))

    first <- 2

    #boxplot(cpueRMSEall[,r] ~ fList[,], ylab = "CPUE RMSE", main="Region " %&% r, names=rep(" ",length(unique(as.vector(fList)))))
    #
    #first   <- 1
    colList <- c(8,2:ncol(fList))

    for (i in 1:ncol(fList))
    {
      last <- first + length(unique(fList[,i]))-1

      boxplot(rowMeans(cpueRMSEByYrall) ~ fList[,i], col=colList[i], add=T, at=first:last)
      text((first:last),1.01*max(rowMeans(cpueRMSEByYrall)), round(table(fList[,i])/sum(table(fList[,i])),2))

      first <- last+1
    }

    par(mfrow = c(1,1))

    for(r in 1:4)
    {  #region
      spacer  <- array(NA, dim=c(dim(fList)[1], length(unique(as.vector(fList)))))
      plotDat <- cbind(cpueRMSEByYrall[,r],spacer)

      boxplot(plotDat, ylab = "CPUE RMSE (annualized)", main="Region " %&% r, names=c("all",rep(" ",length(unique(as.vector(fList))))))

      first <- 2

      #boxplot(cpueRMSEall[,r] ~ fList[,], ylab = "CPUE RMSE", main="Region " %&% r, names=rep(" ",length(unique(as.vector(fList)))))
      #
      #first   <- 1
      colList <- c(8,2:ncol(fList))

      for (i in 1:ncol(fList))
      {
        last <- first + length(unique(fList[,i]))-1

        boxplot(cpueRMSEByYrall[,r] ~ fList[,i], col=colList[i], add=T, at=first:last)

        first <- last+1
      }
    }

    #par(mfrow = mfrowLayout)
    #for (c in 1:numCPUE) {
    #    boxplot(cpueRMSEall[, c], ylab = "CPUE RMSE", ylim = c(0,
    #        0.8))
    #    lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    #    for (i in 1:ncol(fList)) {
    #        boxplot(cpueRMSEall[, c] ~ fList[, i], ylab = "CPUE RMSE",
    #            ylim = c(0, 0.8), main = "CPUE RMSE " %&% names(cpueRMSE)[c])
    #        lines(c(0, 10000), c(1, 1), lty = 2, col = 2)
    #    }
    #}
    #par(mfrow = c(4, 2))
    #for (f in 1:numFleets) {
    #    boxplot(essall[, f] ~ fList[, 5], ylab = "mean ESS",
    #        main = "ESS " %&% colnames(essall)[f])
    #}
    #par(mfrow = c(4, 2)) #what's this?
    #for (f in 1:numFleets) {
    #    boxplot(essall[, f] ~ fList[, 2], ylab = "mean ESS",
    #        main = "ESS " %&% colnames(essall)[f])
    #}

    par(mfrow = mfrowLayout)

    boxplot(rTrendall, ylab = " Slope in rec devs (per qtr)")

    if (!is.null(fList))for (i in 1:ncol(fList))
    {
      boxplot(rTrendall ~ fList[, i], ylab = " Slope in rec devs (per qtr)")
      lines(c(0, 10000), c(0, 0), lty = 2, col = 2)
    }

    par(mar = c(5, 4, 4, 2) + 0.1)
  } #doPlots

  CPUE.fit      <- apply(cpueRMSEall[,], FUN=mean, MARGIN=1) # mean CPUE fit
  CL.Fit        <- apply(essall[,], FUN=mean, MARGIN=1)     # mean CL ESS
  tag.Fit       <- tagFit
  rec.Fit       <- recFit
  rec.RMSE      <- recFit2
  recByYr.RMSE  <- recFit2ByYr
  #tag.Fit3      <- tagFit[fList[,5]=="x3"]
  #tag.Fit8      <- tagFit[fList[,5]=="x8"]

  max.Grad  <- log10(gradall)
  rec.Trend <- rTrendall
  MSY       <- 4.*MSYall/1000.
  B_B.MSY   <- SSBYoSSBMSYall
  B_B0      <- SSBYoSSB0all
  F_F.MSY   <- FYoFMSYall
  C_Pen     <- crashPen
  C_LLH     <- catchLLH

  if (doPlots)
  {
    corrDat   <- cbind(jitter(CPUE.fit, amount=diff(range(CPUE.fit, na.rm=T))/50),
                       jitter(CL.Fit, amount=diff(range(CL.Fit, na.rm=T))/50),
                       jitter(tag.Fit, amount=diff(range(tag.Fit, na.rm=T))/50),
                       jitter(rec.Trend, amount=diff(range(rec.Trend, na.rm=T))/50),
                       jitter(rec.Fit, amount=diff(range(rec.Fit, na.rm=T))/50),
                       jitter(MSY, amount=diff(range(MSY, na.rm=T)/50)),
                       jitter(B_B.MSY, amount=diff(range(B_B.MSY, na.rm=T))/50))

    colnames(corrDat) <- c('CPUE.fit', 'CL.Fit', 'tag.Fit', 'rec.Trend', 'rec.Fit', 'MSY', 'B_B.MSY')
    rownames(corrDat) <- NULL

    for (i in 1:ncol(fList))
    {
      #dev.new() - need to do something to graphics pars here of legend layout screws up
      reset_graphics_par()

      f   <- fList[,i]
      ff  <- as.factor(f)

      chart.Correlation2(corrDat, pch=16, col=seq(1:length(levels(ff)))[ff])
      par(xpd=T)
      legend( .85,.95,sort(as.vector(unique(ff))), fill=seq(1:length(levels(ff))),bg='white')
    }

    corrDat           <- cbind(rec.Trend, rec.Fit, tag.Fit, MSY)
    colnames(corrDat) <- c('rec.Trend', 'rec.Fit', 'tag.Fit','MSY')
    rownames(corrDat) <- NULL

    for (i in 1:ncol(fList))
    {
      #dev.new() - need to do something to graphics pars here of legend layout screws up
      reset_graphics_par()

      f   <- fList[,i]
      ff  <- as.factor(f)
      chart.Correlation2(corrDat,pch=16, col=seq(1:length(levels(ff)))[ff])
      par(xpd=T)
      legend( .85,.95,sort(as.vector(unique(ff))), fill=seq(1:length(levels(ff))),bg='white')
    }

    corrDat<- cbind(MSY, B_B.MSY, C_Pen, C_LLH)
    colnames(corrDat) <- c('MSY', 'B_B.MSY', 'C_Pen', 'C_LLH')
    rownames(corrDat) <- NULL

    for (i in 1:ncol(fList))
    {
      reset_graphics_par()

      f  <- fList[,i]
      ff <- as.factor(f)

      chart.Correlation2(corrDat,pch=16, col=seq(1:length(levels(ff)))[ff])

      par(xpd=T)

      legend( .85,.95,sort(as.vector(unique(ff))), fill=seq(1:length(levels(ff))), bg='white')
    }

    reset_graphics_par()

    #look at ESS by fishery vs MSY
    #pairs(cbind(MSY, essall[,1:5]))
    #pairs(cbind(MSY, essall[,6:10]))
    #pairs(cbind(MSY, essall[,11:15]))
    #pairs(cbind(MSY, essall[,16:20]))
    #pairs(cbind(MSY, essall[,21:23]))

    #look at CPUE by area vs MSY
    #corrDat<- cbind(MSY, cpueRMSEall)
    #pairs(corrDat)

    plot(MSY, rec.Trend, ylab='slope (dev/qtr)', main="Recruitment Deviation Trend" )

    plot(x=jitter(MSY, amount=mean(MSY)/100),y=jitter(rec.Trend, amount=max(rec.Trend)/25+0.001), ylab='slope (dev/qtr)', main="Recruitment Deviation Trend", cex=0.1)
    par(mfrow=c(2,2))

    for (r in 1:4)
    {
      plot(MSY, cpueRMSEall[,r], ylab='RMSE', main="CPUE Region " %&% r)
    }

    par(mfrow=c(3,3))

    for (f in 1:numFleets)
    {
      plot(MSY, essall[,f], ylab='post-fit ESS', main="Fishery " %&% f)
    }
  } #doPlots

  BY   <- SSBYall
  BMSY <- SSBMSYall

  print(recruitWarnings)

  return (list(cbind(CPUE.fit, CL.Fit, tag.Fit, max.Grad, rec.Trend, rec.Fit, rec.RMSE,recByYr.RMSE, MSY, B_B.MSY, B_B0, BY, BMSY, modList, stdExistsall, crashPen, catchLLH), boundFailLOList, boundFailHIList, boundFailHIList2))
}

# this fixes a par problem introduced by chart.Correlation
reset_graphics_par <- function()
{
  op <- structure(list(xlog       = FALSE,
                       ylog       = FALSE,
                       adj        = 0.5,
                       ann        = TRUE,
                       ask        = FALSE,
                       bg         = "transparent",
                       bty        = "o",
                       cex        = 1,
                       cex.axis   = 1,
                       cex.lab    = 1,
                       cex.main   = 1.2,
                       cex.sub    = 1,
                       col        = "black",
                       col.axis   = "black",
                       col.lab    = "black",
                       col.main   = "black",
                       col.sub    = "black",
                       crt        = 0,
                       err        = 0L,
                       family     = "",
                       fg         = "black",
                       fig        = c(0, 1, 0, 1),
                       fin        = c(6.99999895833333, 6.99999895833333),
                       font       = 1L,
                       font.axis  = 1L,
                       font.lab   = 1L,
                       font.main  = 2L,
                       font.sub   = 1L,
                       lab        = c(5L, 5L, 7L),
                       las        = 0L,
                       lend       = "round",
                       lheight    = 1,
                       ljoin      = "round",
                       lmitre     = 10,
                       lty        = "solid",
                       lwd        = 1,
                       mai        = c(1.02, 0.82, 0.82, 0.42),
                       mar        = c(5.1, 4.1, 4.1, 2.1),
                       mex        = 1,
                       mfcol      = c(1L, 1L),
                       mfg        = c(1L, 1L, 1L, 1L),
                       mfrow      = c(1L, 1L),
                       mgp        = c(3, 1, 0),
                       mkh        = 0.001,
                       new        = FALSE,
                       oma        = c(0, 0, 0, 0),
                       omd        = c(0, 1, 0, 1),
                       omi        = c(0, 0, 0, 0),
                       pch        = 1L,
                       pin        = c(5.75999895833333, 5.15999895833333),
                       plt        = c(0.117142874574832, 0.939999991071427, 0.145714307397962, 0.882857125425167),
                       ps         = 12L,
                       pty        = "m",
                       smo        = 1,
                       srt        = 0,
                       tck        = NA_real_,
                       tcl        = -0.5,
                       usr        = c(0.568, 1.432, 0.568, 1.432),
                       xaxp       = c(0.6, 1.4, 4),
                       xaxs       = "r",
                       xaxt       = "s",
                       xpd        = FALSE,
                       yaxp       = c(0.6, 1.4, 4),
                       yaxs       = "r",
                       yaxt       = "s",
                       ylbias     = 0.2),
                  .Names = c("xlog",
                             "ylog",
                             "adj",
                             "ann",
                             "ask",
                             "bg",
                             "bty",
                             "cex",
                             "cex.axis",
                             "cex.lab",
                             "cex.main",
                             "cex.sub",
                             "col",
                             "col.axis",
                             "col.lab",
                             "col.main",
                             "col.sub",
                             "crt",
                             "err",
                             "family",
                             "fg",
                             "fig",
                             "fin",
                             "font",
                             "font.axis",
                             "font.lab",
                             "font.main",
                             "font.sub",
                             "lab",
                             "las",
                             "lend",
                             "lheight",
                             "ljoin",
                             "lmitre",
                             "lty",
                             "lwd",
                             "mai",
                             "mar",
                             "mex",
                             "mfcol",
                             "mfg",
                             "mfrow",
                             "mgp",
                             "mkh",
                             "new",
                             "oma",
                             "omd",
                             "omi",
                             "pch",
                             "pin",
                             "plt",
                             "ps",
                             "pty",
                             "smo",
                             "srt",
                             "tck",
                             "tcl",
                             "usr",
                             "xaxp",
                             "xaxs",
                             "xaxt",
                             "xpd",
                             "yaxp",
                             "yaxs",
                             "yaxt",
                             "ylbias"))

  par(op)
}



# -----------------------------------------------------------------------------
# chart.Correlation2
# -----------------------------------------------------------------------------
# This is a modified version of the chart.Correlation() function in the
# PerformanceAnalytics package which is broken in the current release.
# More specifically, the orginal implementation would pass on ellipsis (...)
# parameters to the pairs() functions allowing the colour of the data plotted
# to be changed but in the latest implementation the ellipses is no longer
# passed. This version simply reverses that change and restores colour control.
# -----------------------------------------------------------------------------
chart.Correlation2 <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...)
{
    x = checkData(R, method = "matrix")
    if (missing(method))
        method = method[1]
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
        method = "pearson", cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")

        if (missing(cex.cor))
          cex <- 0.8/strwidth(txt)

        test <- cor.test(as.numeric(x), as.numeric(y), method = method)
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
                         "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
        text(0.8, 0.8, Signif, cex = cex, col = 2)
    }
    f <- function(t) {
        dnorm(t, mean = mean(x), sd = sd.xts(x))
    }
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)
    hist.panel = function(x, ... = NULL) {
        par(new = TRUE)
        hist(x, col = "light gray", probability = TRUE, axes = FALSE, main = "", breaks = "FD")
        lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
        rug(x)
    }
    if (histogram)
      pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = hist.panel, ...)
    else
      pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, ...)
}
