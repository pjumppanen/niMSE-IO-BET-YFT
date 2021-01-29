# -----------------------------------------------------------------------------
# TuningParameters class
# -----------------------------------------------------------------------------
setClass("TuningParameters",
  slots = c(
    # management procedure tuning
    performanceMeasureYears = "numeric",   # years over which to calculate the performance measure
    performanceMeasure      = "character", # name of performance measure to tune to
    performanceMeasureClass = "character", # name of performance measure class to tune to. Could be "mean" or any of the percentiles
    tuningTarget            = "numeric",   # level of performance measure that tuning should achieve
    tuningTolerance         = "numeric",   # relative level of precision required in tuning
    tuningLogDomain         = "numeric",   # log base 10 of search domain for tuning solution. It is a log domain to improve search dynamic range
    bisectMethod            = "logical"
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "TuningParameters",
  function(.Object)
  {
    .Object@performanceMeasureYears = 20
    .Object@tuningTolerance         = 0.01
    .Object@tuningLogDomain         = c(-3,0.5) # -3, 0.5; maybe should transform relevant MP paramters so this domain can remain unchanged
    .Object@bisectMethod            = TRUE

    return (.Object)
  }
)


# -----------------------------------------------------------------------------
# MseFramework class
# -----------------------------------------------------------------------------
setClass("MseFramework",
  slots = c(
    MseDef                = "MseDefinition",  # parent MSE definition
    tune                  = "numeric",        # vector of tuning parameters from last runMse() call
    tuneError             = "numeric",        # vector of tuning error / degree fit parameters from last runMse() call
    MP_SourceFilePaths    = "list",           # vector of source file paths to MP implementations
    StockSynthesisModels  = "list"            # list of StockSynthesisModel objects
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "MseFramework",
  function(.Object, MseDef=NULL, Report=FALSE, UseCluster=NA, UseMSYss=0)
  {
    if (is.null(MseDef))
    {
      # do nothing. This is an empty constructor for object upgrading but we
      # initialise the MPs.R because it requires a valid initial empty list
      .Object@MP_SourceFilePaths <- list()
    }
    else
    {
      if (class(MseDef) != "MseDefinition")
      {
        print(paste("ERROR: Could not create MseFramework.",deparse(substitute(MseDef)),"not of class MseDefinition"))
        stop()
      }

      initialiseMseDef <- function(.Object)
      {
        # Check if we are using model plausibility weighting
        if (length(.Object@modelWeight) == 1)
        {
          # do nothing. Default behaviour if modelWeight unspecified.
        }
        else if (length(.Object@modelWeight) == length(.Object@OMList))
        {
          if (.Object@totalSims < 1)
          {
            print("ERROR: MSE definition totalSims is too small. It must be at least 1")
            stop()
          }

          # -1 totalSims to ensure there is always 1 in the first OM spec for CPUE data vector filling purposes
          .Object@nsimPerOMFile     <- karray(as.vector(t(rmultinom(1, size=.Object@totalSims - 1, prob=.Object@modelWeight))))
          .Object@nsimPerOMFile[1]  <- .Object@nsimPerOMFile[1] + 1

          print("first model always included at least once, because it includes the correct CPUE data")
          print(.Object@nsimPerOMFile)
          hist(.Object@nsimPerOMFile)

          # redefine some slots so as not to import or conduct MSY calcs for models with zero samples
          .Object@OMList        <- .Object@OMList[.Object@nsimPerOMFile > 0]
          .Object@nsimPerOMFile <- .Object@nsimPerOMFile[.Object@nsimPerOMFile > 0]

          print(.Object@nsimPerOMFile)
        }
        else
        {
          print("ERROR: MSE definition modelWeight vector is wrong length. It must be length(OMList) in length")
          stop()
        }

        return (.Object)
      }

      set.seed(MseDef@seed)

      # Initialise MseDef via initialiseMseDef(). Note that this function will
      # change the definition if a plausibility weighting specification is used.
      .Object@MseDef <- initialiseMseDef(MseDef)

      # initialise the list of MP source files
      .Object@MP_SourceFilePaths <- list()

      idx <- 0

      makeJobList <- function(x)
      {
        idx <<- idx + 1;

        return (list(file=x, which=idx, seed=runif(1,0,.Machine$integer.max)))
      }

      # Create a job list as a context for initialising the StockSynthesisModel
      # instances. Once as a list it is then a simple matter to parallelise the
      # initialisation process.
      JobList <- lapply(.Object@MseDef@OMList, FUN=makeJobList)

      runJob <- function(Job, MseDef, Report, UseMSYss, UseCluster)
      {
        if (UseCluster)
        {
          beginLog(Job$which)
        }

        StockSynthesisModel <- new("StockSynthesisModel", MseDef, Job$which, Job$seed, Report, UseMSYss, .Object@MseDef@nbackupyears)

        if (UseCluster)
        {
          endLog()
        }

        return (StockSynthesisModel)
      }

      nJobs <- length(JobList)

      if (is.na(UseCluster))
      {
        UseCluster <- .Object@MseDef@UseCluster
      }

      if (UseCluster && (nJobs > 1))
      {
        cl <- openCluster(nJobs)

        clusterEvalQ(cl, eval(parse("Source/MseMain.R")))

        .Object@StockSynthesisModels <- parLapply(cl, JobList, fun=runJob, .Object@MseDef, Report, UseMSYss, UseCluster)

        closeCluster()

        lapply(JobList, FUN=function(Job) {printLog(Job$which)})
      }
      else
      {
        .Object@StockSynthesisModels <- lapply(JobList, FUN=runJob, .Object@MseDef, Report, UseMSYss, FALSE)
      }
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("initCPUE_SeriesFrom", function(.Object, ...) standardGeneric("initCPUE_SeriesFrom"))

setMethod("initCPUE_SeriesFrom", c("MseFramework"),
  function(.Object, RefModelName)
  {
    idx <- which(.Object@MseDef@OMList == RefModelName)

    if (length(idx) == 0)
    {
      stop(paste("The model", RefModelName, "is not in the OMList"))
    }

    refModel <- .Object@StockSynthesisModels[[idx]]

    for (cn in 1:length(.Object@StockSynthesisModels))
    {
      if (cn != idx)
      {
        .Object@StockSynthesisModels[[cn]] <- initCPUE_SeriesFrom(.Object@StockSynthesisModels[[cn]], refModel)
      }
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("runMse", function(.Object, ...) standardGeneric("runMse"))

setMethod("runMse", c("MseFramework"),
  function(.Object, MPs, TuningPars=NA, interval=3, Report=FALSE, CppMethod=NA, UseCluster=NA, EffortCeiling = as.double(20.0), TACTime = 0.5, rULim = 0.5)
  {
    CPUEmpY       <- NA
    CPUEmpNormYrs <- NA

    if (!is.na(.Object@MseDef@cpueMP_File))
    {
      # Initialise MP CPUE series for use in projection
      # Assumes saved data table with yr and cpue columns saved with write.table()
      #
      cpueTable     <- read.table(.Object@MseDef@cpueMP_File, header=TRUE)
      CPUEmpNormYrs <- .Object@MseDef@cpueMP_NormYrs
      idxs          <- cpueTable$yr - .Object@MseDef@firstCalendarYr + 1
      lengthYrs     <- max(cpueTable$yr) - .Object@MseDef@firstCalendarYr + 1
      CPUEmpY       <- karray(NA, dim=lengthYrs)
      CPUEmpY[idxs] <- cpueTable$cpue
    }

    cluster <- NA

    if (is.na(UseCluster))
    {
      UseCluster <- .Object@MseDef@UseCluster
    }

    # Source external MP files to current session
    for (SourcePath in .Object@MP_SourceFilePaths)
    {
      SourcePath <- normalizePath(SourcePath, winslash="/", mustWork=FALSE)
      
      if (file.exists(SourcePath))
      {
        source(SourcePath)
      }
      else
      {
        cat(paste("WARNING: cannot source MP source file ", SourcePath))
      }
    }

    # Compile dependent MP sources. Must happen after sourcing MP source
    # otherwise it might be undefined and crash out. Note that we do not
    # need to load either TMB or BuildSys explicitely in niMSE-IO-BET-YFT
    # because this should be included in the supplied source code for the
    # custom MP so will get instantiated when the source code is loaded
    # as in above. This avoids making TMB and BuildSys a necessary 
    # dependency when it isn't used.
    for (MP in MPs)
    {
      if (class(MP) == "MP_Spec")
      {
        MP_Name <- MP@MP_Name
      }
      else
      {
        MP_Name <- MP
      }

      MP_function <- get(MP_Name)
      BSysProject <- attr(MP_function, "BSysProject")

      if (!is.null(BSysProject))
      {
        BSysProject <- make(BSysProject)
      }
    }
    
    if (UseCluster)
    {
      cluster <- openCluster()

      clusterEvalQ(cluster, eval(parse("Source/MseMain.R")))

      # Source external MP files to each cluster process
      for (SourcePath in .Object@MP_SourceFilePaths)
      {
        SourcePath <- normalizePath(SourcePath, winslash="/", mustWork=FALSE)
      
        if (file.exists(SourcePath))
        {
          # This is the only way I have found to do this. It seems impossible to do a simple "literal" substituition
          # from a variable as in the clusterEvalQ() above. I can develop something that runs but in fails to instantiate
          # the code where it needs to be, thus rendering it useless. Seems to be tied up in the cryptic ways R handles
          # scoping through environments so I'm just going to use a global "SourcePath" instead.
          clusterExport(cluster, list("SourcePath"), envir=environment())
          clusterEvalQ(cluster, eval(parse(SourcePath)))
        }
      }
    }

    runModels <- function(StockSynthesisModels, MPs, tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)
    {
      if (UseCluster)
      {
        runJob <- function(om)
        {
          beginLog(om@ModelData@which)

          om <- runMse(om, .Object@MseDef, MPs, tune, interval, Report, CppMethod, cluster=NA, EffortCeiling, TACTime, rULim, CPUEmpY, CPUEmpNormYrs)

          print("\n")

          endLog()

          return (om)
        }

        StockSynthesisModels <- parLapply(cluster, StockSynthesisModels, fun=runJob)
        lapply(StockSynthesisModels, FUN=function(om) {printLog(om@ModelData@which)})
      }
      else
      {
        StockSynthesisModels <- lapply(StockSynthesisModels, FUN=function(om) {return(runMse(om, .Object@MseDef, MPs, tune, interval, Report, CppMethod, cluster=NA, EffortCeiling, TACTime, rULim, CPUEmpY, CPUEmpNormYrs))})
      }

      return (StockSynthesisModels)
    }

    .Object@tune      <- rep(1.0, times=length(MPs))
    .Object@tuneError <- rep(0.0, times=length(MPs))

    if (class(TuningPars) == "TuningParameters")
    {
      HasTuning <- !identical(TuningPars@performanceMeasure, character(0))

      # Do tuning
      if (HasTuning)
      {
        TunedMPs <- c()
        tuneIdx  <- list()
        MP_Names <- names(MPs)
        idx      <- 1

        for (MP in MPs)
        {
          if (class(MP) == "MP_Spec")
          {
            print("ERROR: Running tuning on a tuned MP. Remove TuningPars from call to runMse()\n")
            stop()
          }

          MP_class <- class(get(MP))

          if (MP_class == "IO_MP_tune")
          {
            TunedMPs      <- c(TunedMPs, MP)
            tuneIdx[[MP]] <- idx
          }

          idx <- idx + 1
        }

        AvgFirstYr <- TuningPars@performanceMeasureYears
        AvgLastYr  <- NA

        if (length(TuningPars@performanceMeasureYears) == 2)
        {
          AvgFirstYr <- TuningPars@performanceMeasureYears[1]
          AvgLastYr  <- TuningPars@performanceMeasureYears[2]
        }

        statname <- paste(TuningPars@performanceMeasure, TuningPars@performanceMeasureClass, sep="")

        getPerformanceMeasure <- function(.Object, MP)
        {
          statistic <- performanceStatistics(.Object, TuningPars@performanceMeasure, AvgFirstYr, AvgLastYr, thisMP=MP)
          index     <- which(names(statistic) == statname)

          if (length(index) == 0)
          {
            print("Unknown performanceMeasureClass")
            stop()
          }

          return (statistic[[index]])
        }

        # -----------------------------------------------------------------------
        # objective function for optimise() method. This uses a log10 based
        # tuning argument to provide a wide dynamic range for the minimisation
        # domain
        # -----------------------------------------------------------------------
        optimiseObjFn <- function(tuneLog10, MP)
        {
          tune                         <- 10 ^ tuneLog10
          .Object@StockSynthesisModels <- runModels(.Object@StockSynthesisModels, MP, tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)

          tuneValue <- getPerformanceMeasure(.Object, MP)
          tuneError <- abs((TuningPars@tuningTarget - tuneValue) / TuningPars@tuningTarget)

          print(paste("target:", TuningPars@tuningTarget, ", Current value:", tuneValue, ", tuning error:", tuneError, ", tune (MP-scale Par value): ", tune))

          return (tuneError)
        }

        # -----------------------------------------------------------------------
        # objective function for Dale's bisect method of tuning. This uses a
        # log10 based tuning argument to provide a wide dynamic range for the
        # minimisation domain
        # -----------------------------------------------------------------------
        bisectObjFn <- function(tuneLog10, MP)
        {
          tune                         <- 10 ^ tuneLog10
          .Object@StockSynthesisModels <- runModels(.Object@StockSynthesisModels, MP, tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)

          tuneValue <- getPerformanceMeasure(.Object, MP)
          tuneError <- abs((TuningPars@tuningTarget - tuneValue) / TuningPars@tuningTarget)

          print(paste("bisect - target:", TuningPars@tuningTarget, ", Current value:", tuneValue, ", tuning error:", tuneError, ", tune (MP-scale Par value): ", tune))

          return (tuneValue)
        }

        MP_Names <- names(TunedMPs)

        for (MP in TunedMPs)
        {
          ErrorMessage <- ""

          print(paste("tuning ", MP))

          if (TuningPars@bisectMethod)
          {
            Par1  <- TuningPars@tuningLogDomain[1]
            Par2  <- TuningPars@tuningLogDomain[2]
            Val1  <- bisectObjFn(Par1, MP)
            Val2  <- bisectObjFn(Par2, MP)

            # Check which way tuning response operates
            if (Val1 < Val2)
            {
              loPar <- Par1
              hiPar <- Par2
              loVal <- Val1
              hiVal <- Val2

            } else
            {
              loPar <- Par2
              hiPar <- Par1
              loVal <- Val2
              hiVal <- Val1
            }

            #simple bisection
            tmpPar <- (loPar + hiPar) / 2
            tmpVal <- bisectObjFn(tmpPar, MP)

            #check if desired target is bracketed
            if ((loVal < TuningPars@tuningTarget & hiVal < TuningPars@tuningTarget & tmpVal < TuningPars@tuningTarget) ||
                (loVal > TuningPars@tuningTarget & hiVal > TuningPars@tuningTarget & tmpVal > TuningPars@tuningTarget))
            {
              ErrorMessage <- "Aborting tuning because Par values do not bracket the target. This could be because the par bounds are too narrow, tuning objective is not attainable, or function is non-monotonic"

              print(ErrorMessage)

              .Object@tune[tuneIdx[[MP]]] <- NA

            } else
            {
              print("Bisection minimization")
              print(c("loPar   ", loPar,   "     loVal ", loVal), digits=4)
              print(c("hiPar   ", hiPar,   "     hiVal ", hiVal), digits=4)
              print(c("tmpPar  ", tmpPar,  "     tmpVal ", tmpVal), digits=4)

              nFnEval   <- 4
              tuneError <- abs((tmpVal - TuningPars@tuningTarget) / TuningPars@tuningTarget)

              bestTuneError <- tuneError
              bestPar <- tmpPar
              bestVal <- tmpVal

              while (bestTuneError > TuningPars@tuningTolerance & abs(10 ^ hiPar - 10 ^ loPar) / (10 ^ loPar) > 0.0001)
              {
                if (tmpVal > TuningPars@tuningTarget)
                {
                  hiPar <- tmpPar
                  hiVal <- tmpVal
                }

                if (tmpVal < TuningPars@tuningTarget)
                {
                  loPar <- tmpPar
                  loVal <- tmpVal
                }

                #Bisection - probably more consistent - linear interpolation seems slow under particular circumstances
                tmpPar <- (loPar + hiPar) / 2
                tmpVal <- bisectObjFn(tmpPar, MP)

                nFnEval   <- nFnEval + 1
                tuneError <- abs((tmpVal - TuningPars@tuningTarget) / TuningPars@tuningTarget)

                if (tuneError < bestTuneError)
                {
                  bestTuneError <- tuneError
                  bestPar       <- tmpPar
                  bestVal       <- tmpVal
                }

                print("Bisection minimization")
                print("nFnEval  " %&% nFnEval)
                print("loPar    " %&%  round(loPar, digits=4)   %&%  "   loVal   " %&%  round(loVal, digits=4))
                print("hiPar    " %&%  round(hiPar, digits=4)   %&%  "   hiVal   " %&%  round(hiVal, digits=4))
                print("tmpPar   " %&%  round(tmpPar, digits=4)  %&%  "   tmpVal  " %&%  round(tmpVal, digits=4) %&%    "  tuneError " %&%  round(tuneError, digits=4))
                print("bestPar  " %&%  round(bestPar, digits=4) %&%  "   bestVal " %&%  round(bestVal, digits=4) %&%   "  bestError " %&%  round(bestTuneError, digits=4))

              }

              .Object@tune[tuneIdx[[MP]]]      <- 10 ^ bestPar
              .Object@tuneError[tuneIdx[[MP]]] <- bestTuneError
            }

          } else
          {
            res <- optimise(optimiseObjFn, interval=TuningPars@tuningLogDomain, MP, tol=TuningPars@tuningTolerance)

            print(res)

            .Object@tune[tuneIdx[[MP]]]      <- 10 ^ res$minimum
            .Object@tuneError[tuneIdx[[MP]]] <- res$objective
          }
        }
      }
    }

    # re-run all models with final tuning
    .Object@StockSynthesisModels <- runModels(.Object@StockSynthesisModels, MPs, .Object@tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)

    if (UseCluster)
    {
      closeCluster()
    }

    printRunLogs(.Object)

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("setProjectionYears", function(.Object, ...) standardGeneric("setProjectionYears"))

setMethod("setProjectionYears", c("MseFramework"),
  function(.Object, proyears)
  {
    .Object@MseDef@proyears <- as.integer(proyears)

    for (cn in 1:length(.Object@StockSynthesisModels))
    {
      .Object@StockSynthesisModels[[cn]] <- setProjectionYears(.Object@StockSynthesisModels[[cn]], proyears)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("setRecommendedTACbyF", function(.Object, ...) standardGeneric("setRecommendedTACbyF"))

setMethod("setRecommendedTACbyF", c("MseFramework"),
  function(.Object, recommendedTACbyF, Report=FALSE, UseCluster=NA, EffortCeiling=as.double(20.0), TACTime=0.5, rULim=0.5)
  {
    # update catch distribution recommendation vector
    .Object@MseDef@recommendedTACbyF <- as.karray(recommendedTACbyF)

    # callback function to update the MSY projection
    runMSY_projection <- function(StockSynthesisModel, MseDef, Report, EffortCeiling, TACTime, rULim, UseCluster)
    {
      if (UseCluster)
      {
        beginLog(StockSynthesisModel@ModelData@which)
      }

      StockSynthesisModel@RefVars <- new("ReferenceVars", StockSynthesisModel@ModelData, MseDef, Report, EffortCeiling, TACTime, rULim)

      if (UseCluster)
      {
        endLog()
      }

      return (StockSynthesisModel)
    }

    # update MSY projections using cluster if requested
    nJobs <- length(.Object@StockSynthesisModels)

    if (is.na(UseCluster))
    {
      UseCluster <- .Object@MseDef@UseCluster
    }

    if (UseCluster && (nJobs > 1))
    {
      cl <- openCluster(nJobs)

      clusterEvalQ(cl, eval(parse("Source/MseMain.R")))

      .Object@StockSynthesisModels <- parLapply(cl, .Object@StockSynthesisModels, fun=runMSY_projection, .Object@MseDef, Report, EffortCeiling, TACTime, rULim, UseCluster)

      closeCluster()

      lapply(.Object@StockSynthesisModels, FUN=function(StockSynthesisModel) {printLog(StockSynthesisModel@ModelData@which)})
    }
    else
    {
      .Object@StockSynthesisModels <- lapply(.Object@StockSynthesisModels, FUN=runMSY_projection, .Object@MseDef, Report, EffortCeiling, TACTime, rULim, FALSE)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("getMPs", function(.Object, ...) standardGeneric("getMPs"))

setMethod("getMPs", c("MseFramework"),
  function(.Object)
  {
    return (getMPs(.Object@StockSynthesisModels[[1]]))
  }
)

# -----------------------------------------------------------------------------

setMethod("changeMP_Names", c("MseFramework"),
  function(.Object, namedList)
  {
    for (idx in 1:length(.Object@StockSynthesisModels))
    {
      .Object@StockSynthesisModels[[idx]] <- changeMP_Names(.Object@StockSynthesisModels[[idx]], namedList)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("msevizPerformanceData", function(.Object, ...) standardGeneric("msevizPerformanceData"))

setMethod("msevizPerformanceData", c("MseFramework"),
  function(.Object, AvgFirstYr, AvgLastYr=NA, prefix = "")
  {
    df <- NULL

    if (is.na(AvgLastYr))
    {
      firstMPy    <- .Object@MseDef@firstMPYr - .Object@MseDef@lastCalendarYr
      AvgDuration <- AvgFirstYr
      AvgYears    <- firstMPy:(firstMPy + AvgDuration - 1)
    }
    else
    {
      AvgYears    <- (AvgFirstYr - .Object@MseDef@lastCalendarYr):(AvgLastYr - .Object@MseDef@lastCalendarYr)
    }

    for (cn in 1:length(.Object@StockSynthesisModels))
    {
      om         <- .Object@StockSynthesisModels[[cn]]
      model_name <- .Object@MseDef@OMList[[cn]]

      df <- msevizPerformanceData(om, .Object, df, AvgYears, prefix=prefix, model_name=model_name)
    }

    return (as.data.table(df))
  }
)

# -----------------------------------------------------------------------------

setGeneric("msevizTimeSeriesData", function(.Object, ...) standardGeneric("msevizTimeSeriesData"))

setMethod("msevizTimeSeriesData", c("MseFramework"),
  function(.Object, bHistoric, prefix = "", Indicators=NULL)
  {
    statHandlers <- list()

    # define handlers for all statistics
    # ----------------------------------
    statHandlers[["CPUE(aggregate)"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                # Aggregate CPUE series skips final year
                return(addRows(context, HistoricVars@IobsArchive, "CPUE(aggregate)"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["Recruitment"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                # Recruitment
                return (addRows(context, HistoricVars@Rec, "Recruitment"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["B/B0"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #B/B0
                B_B0 <- b_b0(HistoricVars, RefVars)

                if (bHistoric)
                {
                  return (addRows(context, apply(B_B0, MARGIN=c(2), FUN=sum), "B/B0"))

                } else
                {
                  return (addRows(context, apply(B_B0, MARGIN=c(1,3), FUN=sum), "B/B0"))
                }
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["B/BMSY"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #B/BMSY
                B_BMSY <- b_bmsy(HistoricVars, RefVars)
                return (addRows(context, B_BMSY, "B/BMSY"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["SSB/SSB0"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #SSB/SSB0
                SSB_SSB0 <- ssb_ssb0(HistoricVars, RefVars)

                if (bHistoric)
                {
                  return (addRows(context, apply(SSB_SSB0, MARGIN=c(2), FUN=sum), "SSB/SSB0"))

                } else
                {
                  return (addRows(context, apply(SSB_SSB0, MARGIN=c(1,3), FUN=sum), "SSB/SSB0"))
                }
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["SSB/SSBMSY"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #SSB/SSBMSY
                SSB_SSBMSY <- ssb_ssbmsy(HistoricVars, RefVars)
                return (addRows(context, SSB_SSBMSY, "SSB/SSBMSY"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["F/FMSY"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #F/FMSY
                F_FMSY <- f_fmsy(HistoricVars, RefVars)
                return (addRows(context, F_FMSY, "F/FMSY"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["C"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #Catch
                if (bHistoric)
                {
                  return (addRows(context, apply(HistoricVars@CM, MARGIN=c(2), FUN=sum), "C"))

                } else
                {
                  return (addRows(context, apply(HistoricVars@CM, MARGIN=c(1,3), FUN=sum), "C"))
                }
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["TAC"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #Catch
                if (bHistoric)
                {
                  return (addRows(context, HistoricVars@TAC, "TAC"))

                } else
                {
                  return (addRows(context, HistoricVars@TAC, "TAC"))
                }
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["C/TAC"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {

                #Catch
                if (bHistoric)
                {
                  return (addRows(context, apply(HistoricVars@CM, MARGIN=c(2), FUN=sum) / HistoricVars@TAC, "C/TAC"))

                } else
                {
                  return (addRows(context, apply(HistoricVars@CM, MARGIN=c(1,3), FUN=sum) / HistoricVars@TAC, "C/TAC"))
                }
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["PrGreen"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                B_BMSY  <- b_bmsy(HistoricVars, RefVars)
                F_FMSY  <- f_fmsy(HistoricVars, RefVars)
                isGreen <- B_BMSY > 1 & F_FMSY < 1

                storage.mode(isGreen) <- "double"

                return (addRows(context, isGreen, "PrGreen"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["PrRed"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                B_BMSY <- b_bmsy(HistoricVars, RefVars)
                F_FMSY <- f_fmsy(HistoricVars, RefVars)
                isRed  <- B_BMSY < 1 & F_FMSY > 1

                storage.mode(isRed) <- "double"

                return (addRows(context, isRed, "PrRed"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["PrOrange"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                B_BMSY   <- b_bmsy(HistoricVars, RefVars)
                F_FMSY   <- f_fmsy(HistoricVars, RefVars)
                isOrange <- B_BMSY > 1 & F_FMSY > 1

                storage.mode(isOrange) <- "double"

                return (addRows(context, isOrange, "PrOrange"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["PrYellow"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                B_BMSY   <- b_bmsy(HistoricVars, RefVars)
                F_FMSY   <- f_fmsy(HistoricVars, RefVars)
                isYellow <- B_BMSY < 1 & F_FMSY < 1

                storage.mode(isYellow) <- "double"

                return (addRows(context, isYellow, "PrYellow"))
              },

      countFn = function(HistoricVars)
                {
                  return (HistoricVars@nyears * HistoricVars@nsim)
                }
      )


    statHandlers[["Recruitment by Qtr"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                # Rec by Qtr
                QtrYrBases  <- (0:(HistoricVars@nyears - 1)) * 4

                #Recruitment by Qtr
                if (bHistoric)
                {
                  context <- addRows(context, HistoricVars@RecYrQtr[QtrYrBases + 1], "Recruitment Q1")
                  context <- addRows(context, HistoricVars@RecYrQtr[QtrYrBases + 2], "Recruitment Q2")
                  context <- addRows(context, HistoricVars@RecYrQtr[QtrYrBases + 3], "Recruitment Q3")
                  context <- addRows(context, HistoricVars@RecYrQtr[QtrYrBases + 4], "Recruitment Q4")

                } else
                {
                  context <- addRows(context, HistoricVars@RecYrQtr[,QtrYrBases + 1], "Recruitment Q1")
                  context <- addRows(context, HistoricVars@RecYrQtr[,QtrYrBases + 2], "Recruitment Q2")
                  context <- addRows(context, HistoricVars@RecYrQtr[,QtrYrBases + 3], "Recruitment Q3")
                  context <- addRows(context, HistoricVars@RecYrQtr[,QtrYrBases + 4], "Recruitment Q4")
                }

                return (context)
              },

      countFn = function(HistoricVars)
                {
                  return (4 * HistoricVars@nyears * HistoricVars@nsim)
                }
      )

    statHandlers[["CbyRF"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #Catch by fishery
                if (HistoricVars@nfleets > 1)
                {
                  for (fi in 1:HistoricVars@nfleets)
                  {
                    if (bHistoric)
                    {
                      context <- addRows(context, HistoricVars@CMbyF[.Object@MseDef@targpop,,fi], paste("C by Fleet",fi))

                    } else
                    {
                      context <- addRows(context, HistoricVars@CMbyF[,.Object@MseDef@targpop,,fi], paste("C by Fleet",fi))
                    }
                  }
                }

                return (context)
              },

      countFn = function(HistoricVars)
                {
                  count <- 0

                  if (HistoricVars@nfleets > 1)
                  {
                    count <- HistoricVars@nfleets * HistoricVars@nyears * HistoricVars@nsim
                  }

                  return (count)
                }
      )

    statHandlers[["C/TACbyRF"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #Catch by fishery
                if ((HistoricVars@nfleets >= 1) && !bHistoric)
                {
                  # Calculate the TAC by F to be used against catch
                  isTACFleet  <- (HistoricVars@TAEbyF == 0)
                  dims        <- dim(isTACFleet)
                  nsim        <- dims[1]
                  proyears    <- dims[2]
                  nfleets     <- dims[3]
                  CMCurrent   <- apply(om@ModelData@CMCurrent, c(3), sum)
                  SYF         <- as.matrix(expand.grid(1:nsim, 1:proyears, 1:nfleets))
                  SY          <- SYF[,c(1,2)]
                  F           <- SYF[,3]
                  CRefbyF     <- karray(CMCurrent[F] * isTACFleet[SYF], dim=c(nsim, proyears, nfleets))
                  SumCRefbyF  <- apply(CRefbyF, c(1,2), sum)
                  TACbyF      <- karray(HistoricVars@TAC[SY] * CRefbyF[SYF] / SumCRefbyF[SY], dim=c(nsim, proyears, nfleets))

                  for (fi in 1:HistoricVars@nfleets)
                  {
                    context <- addRows(context, HistoricVars@CMbyF[,.Object@MseDef@targpop,,fi] / TACbyF[,,fi], paste("C/TAC by Fleet",fi))
                  }
                }

                return (context)
              },

      countFn = function(HistoricVars)
                {
                  count <- 0

                  if ((HistoricVars@nfleets >= 1) && !bHistoric)
                  {
                    count <- HistoricVars@nfleets * HistoricVars@nyears * HistoricVars@nsim
                  }

                  return (count)
                }
      )

    statHandlers[["CPUEbyArea"]] <- list(
      addFn = function(HistoricVars, RefVars, context, om)
              {
                #CPUE by region
                if (HistoricVars@nareas > 1)
                {
                  for (ri in 1:HistoricVars@nareas)
                  {
                    if (bHistoric)
                    {
                      context <- addRows(context, HistoricVars@IobsRArchive[,ri], paste("CPU by Area", ri))

                    } else
                    {
                      context <- addRows(context, HistoricVars@IobsRArchive[,,ri], paste("CPU by Area", ri))
                    }
                  }
                }

                return (context)
              },

      countFn = function(HistoricVars)
                {
                  count <- 0

                  if (HistoricVars@nareas > 1)
                  {
                    count <- HistoricVars@nareas * HistoricVars@nyears * HistoricVars@nsim
                  }

                  return (count)
                }
      )

    # count number of result rows
    row_count <- 0

    if (is.null(Indicators))
    {
      Indicators <- names(statHandlers)
    }

    for (Indicator in Indicators)
    {
      if (Indicator %in% names(statHandlers))
      {
        handler <- statHandlers[[Indicator]]

        for (om in .Object@StockSynthesisModels)
        {
          if (bHistoric)
          {
            row_count <- row_count + handler$countFn(om@HistoricVars) * om@ModelData@nsim

          } else
          {
            for (ProjVar in om@ProjectedVars)
            {
              row_count <- row_count + handler$countFn(ProjVar)
            }
          }
        }
      }
    }

    row_count <- row_count - 1

    # create result data.table
    if (bHistoric)
    {
      dt <- data.table(year=as.integer(rep(0, times=row_count)),
                       data=as.double(rep(0, times=row_count)),
                       iter=as.integer(rep(0, times=row_count)),
                       qname=rep("", times=row_count),
                       model=rep("", times=row_count),
                       stringsAsFactors=TRUE)

    } else
    {
      dt <- data.table(mp=rep("", times=row_count),
                       year=as.integer(rep(0, times=row_count)),
                       data=as.double(rep(0, times=row_count)),
                       iter=as.integer(rep(0, times=row_count)),
                       qname=rep("", times=row_count),
                       model=rep("", times=row_count),
                       stringsAsFactors=TRUE)
    }


    # fill result data.table with data
    res <- list(dt=dt, origin=0, iter=0)

    for (Indicator in Indicators)
    {
      if (Indicator %in% names(statHandlers))
      {
        handler  <- statHandlers[[Indicator]]
        res$iter <- 0

        for (cn in 1:length(.Object@StockSynthesisModels))
        {
          om         <- .Object@StockSynthesisModels[[cn]]
          model_name <- .Object@MseDef@OMList[[cn]]

          if (bHistoric)
          {
            SY  <- as.matrix(expand.grid(nsims=1:om@ModelData@nsim, nyrs=1:om@HistoricVars@nyears))
            Yrs <- .Object@MseDef@firstCalendarYr + (0:(om@HistoricVars@nyears - 1))

            addRows <- function(context, data, name)
            {
              next_origin <- (context$origin + length(Yrs) * om@ModelData@nsim)
              rows        <- context$origin:(next_origin - 1)

              C1          <- Yrs[SY[,2]]
              C2          <- data[SY[,2]]
              C3          <- as.integer(context$iter + SY[,1])
              C4          <- rep(name, times=length(SY[,2]))
              C5          <- rep(model_name, times=length(SY[,2]))

              set(context$dt, rows, "year",  C1)
              set(context$dt, rows, "data",  C2)
              set(context$dt, rows, "iter",  C3)
              set(context$dt, rows, "qname", C4)
              set(context$dt, rows, "model", C5)

              context$origin <- next_origin

              return (context)
            }

            res <- handler$addFn(om@HistoricVars, om@RefVars, res, om)

            res$iter <- res$iter + om@ModelData@nsim

          } else
          {
            for (ProjVar in om@ProjectedVars)
            {
              SY   <- as.matrix(expand.grid(nsims=1:ProjVar@nsim, nyrs=1:ProjVar@nyears))
              Yrs  <- .Object@MseDef@firstCalendarYr + om@ModelData@nyears + (0:(ProjVar@nyears - 1))
              C1   <- rep(paste(prefix, ProjVar@MP@MP_Name, sep=""), times=length(SY[,1]))
              C2   <- Yrs[SY[,2]]

              addRows <- function(context, data, name)
              {
                next_origin <- (context$origin + length(Yrs) * ProjVar@nsim)
                rows        <- context$origin:(next_origin - 1)

                C3          <- data[SY]
                C4          <- as.integer(context$iter + SY[,1])
                C5          <- rep(name, times=length(SY[,1]))
                C6          <- rep(model_name, times=length(SY[,1]))

                set(context$dt, rows, "mp",    C1)
                set(context$dt, rows, "year",  C2)
                set(context$dt, rows, "data",  C3)
                set(context$dt, rows, "iter",  C4)
                set(context$dt, rows, "qname", C5)
                set(context$dt, rows, "model", C6)

                context$origin <- next_origin

                return (context)
              }

              res <- handler$addFn(ProjVar, om@RefVars, res, om)
            }

            res$iter <- res$iter + om@ModelData@nsim
          }
        }
      }
    }

    return (res$dt)
  }
)

# -----------------------------------------------------------------------------

setGeneric("msevizHistoricTimeSeriesData", function(.Object, ...) standardGeneric("msevizHistoricTimeSeriesData"))

setMethod("msevizHistoricTimeSeriesData", c("MseFramework"),
  function(.Object, prefix="", Indicators=NULL)
  {
    return (msevizTimeSeriesData(.Object, TRUE, prefix, Indicators))
  }
)

# -----------------------------------------------------------------------------

setGeneric("msevizProjectedTimeSeriesData", function(.Object, ...) standardGeneric("msevizProjectedTimeSeriesData"))

setMethod("msevizProjectedTimeSeriesData", c("MseFramework"),
  function(.Object, prefix="", Indicators=NULL)
  {
    return (msevizTimeSeriesData(.Object, FALSE, prefix, Indicators))
  }
)

# -----------------------------------------------------------------------------

setGeneric("performanceStatistics", function(.Object, ...) standardGeneric("performanceStatistics"))

setMethod("performanceStatistics", c("MseFramework"),
  function(.Object, Statistics, AvgFirstYr, AvgLastYr=NA, percentiles=c(0.1,0.25,0.5,0.75,0.8,0.9), thisMP=NA, prefix="", appendTo=NULL)
  {
    if (is.na(AvgLastYr))
    {
      firstMPy    <- .Object@MseDef@firstMPYr - .Object@MseDef@lastCalendarYr
      AvgDuration <- AvgFirstYr
      AvgYears    <- karray(firstMPy:(firstMPy + AvgDuration - 1), dim=c(AvgDuration))
    }
    else
    {
      AvgYears    <- karray((AvgFirstYr - .Object@MseDef@lastCalendarYr):(AvgLastYr - .Object@MseDef@lastCalendarYr), dim=c(AvgLastYr - AvgFirstYr + 1))
    }

    # Define stat handlers
    statHandlers = list()

    statHandlers[["SBoSB0"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S1 mean(SB/SB_0)
        SSB_SSB0 <- ssb_ssb0(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSB0)[ , , keep(AvgYears)], MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["minSBoSB0"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S2 min(SB/SB0)
        SSB_SSB0 <- ssb_ssb0(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSB0)[ , , keep(AvgYears)], MARGIN=c(1), min), digits=3))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["SBoSBMSY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S3 mean(SB/SB_MSY)
        SSB_SSBMSY <- ssb_ssbmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSBMSY)[ , keep(AvgYears)], MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["FoFMSY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S5 mean(F/F_MSY)
        F_FMSY <- f_fmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(F_FMSY)[ , keep(AvgYears)], MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["FoFtarg"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S4 mean(F/F_target), in this case...Ftarget = FMSY
        F_FMSY <- f_fmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(F_FMSY)[ , keep(AvgYears)], MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["GK"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S6 Pr(Green)
        F_FMSY     <- f_fmsy(ManagementVars, RefVars)
        SSB_SSBMSY <- ssb_ssbmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(F_FMSY)[ , keep(AvgYears)] < 1 & as.karray(SSB_SSBMSY)[ , keep(AvgYears)] > 1, c(1), sum) / length(AvgYears), 3))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["RK"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S7 Pr(Red)
        F_FMSY     <- f_fmsy(ManagementVars, RefVars)
        SSB_SSBMSY <- ssb_ssbmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(F_FMSY)[ , keep(AvgYears)] > 1 & as.karray(SSB_SSBMSY)[ , keep(AvgYears)] < 1, c(1), sum) / length(AvgYears), 3))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["PrSBgtSBMSY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # S8 Pr(SB>SB_MSY)
        SSB_SSBMSY <- ssb_ssbmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSBMSY)[ , keep(AvgYears)] > 1.0, MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["PrSBgt0.2SB0"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # F1 Pr(SB>0.2SB0)
        SSB_SSB0 <- ssb_ssb0(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSB0)[ , .Object@MseDef@targpop, keep(AvgYears)] > 0.2, MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["PrSBgtSBlim"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # F2 Pr(SB>SBlim) where SBlim = 0.4SSBMSY
        SSB_SSBMSY <- ssb_ssbmsy(ManagementVars, RefVars)

        return (round(apply(as.karray(SSB_SSBMSY)[ , keep(AvgYears)] > .Object@MseDef@SBlim, MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["Y"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # Y1 mean(C)
        return (round(apply(ManagementVars@CM[ , .Object@MseDef@targpop, keep(AvgYears)], MARGIN=c(1), mean), 0) / 1000.0)
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["relCPUE"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # mean catch rates relative to catch rates over four last data years
        return (round(apply(ManagementVars@IobsArchive[ , keep(AvgYears)], MARGIN=c(1), mean) /  mean(HistoricVars@IobsArchive[(HistoricVars@nyears-4):HistoricVars@nyears]), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["YoMSY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # Y3 mean(C/MSY)
        C_MSY <- c_msy(ManagementVars, RefVars)

        return (round(apply(as.karray(C_MSY)[ , , keep(AvgYears)], MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["APCY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        AvgYearsm1 <- AvgYears - 1

        # T1 mean(C(t)/C(t-1))
        return (apply((ManagementVars@CM[ , .Object@MseDef@targpop, keep(AvgYears)] / ManagementVars@CM[ , .Object@MseDef@targpop, AvgYearsm1]) , c(1), mean, na.rm = TRUE))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["AAVY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        AvgYearsm1 <- AvgYears - 1

        # Tx average(abs(change in catch))
        AAVY <- 100*apply(abs(ManagementVars@CM[ , .Object@MseDef@targpop, keep(AvgYears)] - ManagementVars@CM[ , .Object@MseDef@targpop, AvgYearsm1]) /
             ManagementVars@CM[ , .Object@MseDef@targpop, AvgYearsm1], c(1), mean, na.rm=T)

        return (AAVY)
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["YcvPct"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # T2 var(C)
        Y <- round(apply(ManagementVars@CM[ , .Object@MseDef@targpop, keep(AvgYears)], MARGIN=c(1), mean), 0);

        return (round(apply(as.karray(ManagementVars@CM)[ , .Object@MseDef@targpop, keep(AvgYears)], MARGIN=c(1), sd), 2)/Y)
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["PrYlt0.1MSY"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # T4 Pr(C<0.1MSY)
        C_MSY <- c_msy(ManagementVars, RefVars)

        return (round(apply(as.karray(C_MSY)[ , , keep(AvgYears)] < 0.1, MARGIN=c(1), mean), digits=2))
      },
      partsFn = function()
      {
        return (1)
      }
    )

    statHandlers[["Yf"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        #11 mean Catch by fishery
        return (round(apply(as.karray(ManagementVars@CMbyF)[ , keep(.Object@MseDef@targpop), keep(AvgYears), npart], MARGIN=c(1), mean), 0))
      },
      partsFn = function()
      {
        return (.Object@MseDef@nfleets)
      }
    )

    statHandlers[["CPUEr"]] <- list(
      statFn = function(ManagementVars, RefVars, HistoricVars, npart)
      {
        # mean catch rates relative to catch rates over four last data years
        return (round(apply(ManagementVars@IobsRArchive[ , keep(AvgYears), npart], MARGIN=c(1), mean) /  mean(HistoricVars@IobsRArchive[(HistoricVars@nyears-4):HistoricVars@nyears], npart), digits=2))
      },
      partsFn = function()
      {
        return (.Object@StockSynthesisModels[[1]]@ProjectedVars[[1]]@nareas)
      }
    )

    # extract MP names
    MPs         <- NA
    AllMPs      <- NA
    nmodels     <- length(.Object@StockSynthesisModels)
    ntotal_sims <- 0

    if (nmodels > 0)
    {
      AllMPs <- getMPs(.Object@StockSynthesisModels[[1]])
      nMPs   <- length(AllMPs)

      if (nMPs > 0)
      {
        MPs <- AllMPs

        if (!is.na(thisMP))
        {
          MPs <- thisMP
        }

      } else
      {
        print("ERROR: No MP projections in Stock Synthesis models.")
        stop()
      }

      for (stockSynthesisModel in .Object@StockSynthesisModels)
      {
        ntotal_sims <- ntotal_sims + stockSynthesisModel@ModelData@nsim
      }

    } else
    {
      print("ERROR: No Stock Synthesis models in mseFramework.")
      stop()
    }

    # create result data.table. This needs to be done by building a code
    # expression, parsing and evaluating it.
    nMPs     <- length(MPs)
    codeExpr <- "data.table(MP=rep('', times=nMPs),"
    cnames   <- c()

    for (Statistic in Statistics)
    {
      nparts <- statHandlers[[Statistic]]$partsFn()

      if (nparts > 1)
      {
        for (cn in 1:nparts)
        {
          cnames <- c(cnames, paste(Statistic, cn, ".", sep="") %&% c("mean", percentiles))
        }

      } else
      {
        cnames <- c(cnames, Statistic %&% c("mean", percentiles))
      }
    }

    extraColumns <- sapply(cnames, function(name)
                                   {
                                     return (paste(name, "=as.double(rep(NA, times=nMPs))", sep=""))
                                   })

    codeExpr <- paste(codeExpr, paste(extraColumns, collapse=","), ", stringsAsFactors=TRUE)", sep="")
    dt       <- eval(parse(text=codeExpr))
    nrow     <- 1
    LUT      <- list()

    for (idx in 1:length(AllMPs))
    {
      LUT[[AllMPs[[idx]]@MP_Name]] <- idx
    }

    for (MP in MPs)
    {
      if (class(MP) == "MP_Spec")
      {
        MP_Name <- MP@MP_Name
      }
      else
      {
        MP_Name <- MP
      }

      values <- list(MP=MP_Name)

      for (Statistic in Statistics)
      {
        if (Statistic %in% names(statHandlers))
        {
          nparts <- statHandlers[[Statistic]]$partsFn()

          for (cn in 1:nparts)
          {
            cnames     <- if (nparts > 1) (paste(Statistic, cn, ".", sep="") %&% c("mean", percentiles)) else (Statistic %&% c("mean", percentiles))
            SourceData <- karray(as.double(NA), ntotal_sims)
            nsim       <- 1

            for (stockSynthesisModel in .Object@StockSynthesisModels)
            {
              nproj_sims <- stockSynthesisModel@ModelData@nsim
              nendsim    <- nsim + nproj_sims - 1

              SourceData[nsim:nendsim] <- statHandlers[[Statistic]]$statFn(stockSynthesisModel@ProjectedVars[[LUT[[MP_Name]]]], stockSynthesisModel@RefVars, stockSynthesisModel@HistoricVars, cn)

              nsim <- nsim + nproj_sims
            }

            data <- c(mean(SourceData, na.rm=TRUE), quantile(SourceData, percentiles, na.rm=TRUE))

            for (cn in 1:length(cnames))
            {
              values[[cnames[cn]]] <- data[cn]
            }
          }

        } else
        {
          print(paste("ERROR: Statistic", Statistic, "not supported. Supported Statistics include:", names(statHandlers)))
          stop()
        }
      }

      set(dt, i=as.integer(nrow), j=names(values), values)

      nrow <- nrow + 1
    }

    if (!is.null(appendTo))
    {
      dt <- merge(appendTo, dt, all=TRUE)
    }

    return (dt)
  }
)

# -----------------------------------------------------------------------------

setGeneric("getParameters", function(.Object, ...) standardGeneric("getParameters"))

setMethod("getParameters", c("MseFramework"),
  function(.Object)
  {
    parameters <- list(CppMethod      = .Object@MseDef@CppMethod,
                       targpop        = .Object@MseDef@targpop,
                       RecScale       = .Object@MseDef@RecScale,
                       NInitCV        = .Object@MseDef@NInitCV,
                       NInitCVdecay   = .Object@MseDef@NInitCVdecay,
                       selWLRange     = .Object@MseDef@selWLRange,
                       nCAAobs        = .Object@MseDef@nCAAobs,
                       indexFisheries = .Object@MseDef@indexFisheries,
                       Ccv            = .Object@MseDef@Ccv,
                       Cbcv           = .Object@MseDef@Cbcv,
                       Cbmean         = .Object@MseDef@Cbmean,
                       Icv            = .Object@MseDef@Icv,
                       IACin          = .Object@MseDef@IACin,
                       Ibeta          = .Object@MseDef@Ibeta,
                       Btcv           = .Object@MseDef@Btcv,
                       Btbcv          = .Object@MseDef@Btbcv,
                       Mbcv           = .Object@MseDef@Mbcv,
                       Kbcv           = .Object@MseDef@Kbcv,
                       Linfbcv        = .Object@MseDef@Linfbcv,
                       MSYbcv         = .Object@MseDef@MSYbcv,
                       BMSYbcv        = .Object@MseDef@BMSYbcv,
                       IMSYbcv        = .Object@MseDef@IMSYbcv,
                       FMSYbcv        = .Object@MseDef@FMSYbcv,
                       FMSY_Mbcv      = .Object@MseDef@FMSY_Mbcv,
                       ageMbcv        = .Object@MseDef@ageMbcv,
                       TACEcv         = .Object@MseDef@TACEcv,
                       selAgeRange    = .Object@MseDef@selAgeRange,
                       selExpRange    = .Object@MseDef@selExpRange,
                       catchBridge    = .Object@MseDef@catchBridge,
                       catchBridgeCV  = .Object@MseDef@catchBridgeCV,
                       MPDataLag      = .Object@MseDef@MPDataLag,
                       ImplErrBias    = .Object@MseDef@ImplErrBias,
                       ITrendin       = .Object@MseDef@ITrendin)

    return (parameters)
  }
)

# -----------------------------------------------------------------------------

setGeneric("setParameters", function(.Object, ...) standardGeneric("setParameters"))

setMethod("setParameters", c("MseFramework"),
  function(.Object, parameters)
  {
    parameterHandlers <- list(CppMethod = function(param)
                              {
                                return (param)
                              },

                              targpop = function(param)
                              {
                                return (param)
                              },

                              RecScale = function(param)
                              {
                                if ((length(param) != 1) && (length(param) != .Object@MseDef@proyears))
                                {
                                  print("ERROR: RecScale vector is wrong length. It must be singular or proyears in length")
                                  stop()
                                }

                                return (param)
                              },

                              NInitCV = function(param)
                              {
                                return (param)
                              },

                              NInitCVdecay = function(param)
                              {
                                return (param)
                              },

                              nCAAobs = function(param)
                              {
                                return (param)
                              },

                              indexFisheries = function(param)
                              {
                                if ((length(param) < 1) || (length(param) > .Object@MseDef@nfleets))
                                {
                                  print("ERROR: indexFisheries vector has too few or too many values")
                                  stop()
                                }

                                return (param)
                              },

                              Ccv = function(param)
                              {
                                if (length(param) != 2)
                                {
                                  print("ERROR: Ccv vector is wrong length. It must be 2 in length")
                                  stop()
                                }

                                return (param)
                              },

                              Cbcv = function(param)
                              {
                                return (param)
                              },

                              Cbmean = function(param)
                              {
                                return (param)
                              },

                              Icv = function(param)
                              {
                                if (length(param) != 2)
                                {
                                  print("ERROR: Icv vector is wrong length. It must be 2 in length")
                                  stop()
                                }

                                return (param)
                              },

                              IACin = function(param)
                              {
                                return (param)
                              },

                              Ibeta = function(param)
                              {
                                if (length(param) != 2)
                                {
                                  print("ERROR: Ibeta vector is wrong length. It must be 2 in length")
                                  stop()
                                }

                                return (param)
                              },

                              Btcv = function(param)
                              {
                                if (length(param) != 2)
                                {
                                  print("ERROR: Btcv vector is wrong length. It must be 2 in length")
                                  stop()
                                }

                                return (param)
                              },

                              Btbcv = function(param)
                              {
                                return (param)
                              },

                              Mbcv = function(param)
                              {
                                return (param)
                              },

                              Kbcv = function(param)
                              {
                                return (param)
                              },

                              Linfbcv = function(param)
                              {
                                return (param)
                              },

                              MSYbcv = function(param)
                              {
                                return (param)
                              },

                              BMSYbcv = function(param)
                              {
                                return (param)
                              },

                              IMSYbcv = function(param)
                              {
                                return (param)
                              },

                              FMSYbcv = function(param)
                              {
                                return (param)
                              },

                              FMSY_Mbcv = function(param)
                              {
                                return (param)
                              },

                              ageMbcv = function(param)
                              {
                                return (param)
                              },

                              TACEcv = function(param)
                              {
                                return (param)
                              },

                              selAgeRange = function(param)
                              {
                                return (param)
                              },

                              selExpRange = function(param)
                              {
                                return (param)
                              },

                              selWLRange = function(param)
                              {
                                if (length(param) != 2)
                                {
                                  print("ERROR: selWLRange vector is wrong length. It must be 2 in length")
                                  stop()
                                }

                                return (param)
                              },

                              catchBridge = function(param)
                              {
                                if (length(param) > .Object@MseDef@firstMPYr - .Object@MseDef@lastCalendarYr - 1)
                                {
                                  print("ERROR: catchBridge vector too many values")
                                  stop()
                                }

                                return (param)
                              },

                              catchBridgeCV = function(param)
                              {
                                return (param)
                              },

                              MPDataLag = function(param)
                              {
                                return (param)
                              },

                              ImplErrBias = function(param)
                              {
                                if ((length(param) != 1) && (length(param) != .Object@MseDef@proyears))
                                {
                                  print("ERROR: ImplErrBias vector is wrong length. It must be singular or proyears in length")
                                  stop()
                                }

                                return (param)
                              },

                              ITrendin = function(param)
                              {
                                return (param)
                              })

    if (class(parameters) != "list")
    {
      print("ERROR: parameters must be in a named list\n")
      stop()
    }

    srcNames  <- names(parameters)
    destNames <- names(parameterHandlers)

    for (paramName in srcNames)
    {
      if (!(paramName %in% destNames))
      {
        print("ERROR: parameter not set. unknown parameter\n")
        stop()
      }

      handler <- parameterHandlers[[paramName]]

      slot(.Object@MseDef, paramName) <- handler(parameters[[paramName]])
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("printRunLogs", function(.Object, ...) standardGeneric("printRunLogs"))

setMethod("printRunLogs", "MseFramework",
  function(.Object)
  {
    dummy <- lapply(.Object@StockSynthesisModels, function(model)
                                                  {
                                                    dummy <- lapply(model@ProjectedVars,  function(projVars)
                                                                                          {
                                                                                            # print all the logged errors
                                                                                            LogIdx <- which(!is.na(projVars@Log))

                                                                                            if (length(LogIdx) > 0)
                                                                                            {
                                                                                              LogIdx <- which(sapply(projVars@Log[LogIdx], function(x) !is.null(x$error)))
                                                                                            }

                                                                                            if (length(LogIdx) > 0)
                                                                                            {
                                                                                              cat(paste("\n\n\n\nLogs reported in projecting model", .Object@MseDef@OMList[projVars@which], "\n--------------------------------------------------------------------------------\n"))

                                                                                              dummy <- sapply(LogIdx, function(ix)
                                                                                                                      {
                                                                                                                        if (!is.null(projVars@Log[[ix]]) && !is.null(projVars@Log[[ix]]$error))
                                                                                                                        {
                                                                                                                          cat(paste(projVars@Log[[ix]]$error, "\n--------------------------------------------------------------------------------\n"))
                                                                                                                        }
                                                                                                                      })
                                                                                            }

                                                                                            cat("\n")
                                                                                         })
                                                  })
  }
)

# -----------------------------------------------------------------------------

setGeneric("execRunCallback", function(.Object, ...) standardGeneric("execRunCallback"))

setMethod("execRunCallback", "MseFramework",
  function(.Object, callbackFn)
  {
    with_env <- function(Fn, envir)
    {
      stopifnot(is.function(Fn))
      environment(Fn) <- envir
      return (Fn)
    }

    dummy <- lapply(.Object@StockSynthesisModels, function(model)
                                                  {
                                                    dummy <- lapply(model@ProjectedVars,  function(projVars)
                                                                                          {
                                                                                            # print all the logged errors
                                                                                            LogIdx <- which(!is.na(projVars@Log))

                                                                                            if (length(LogIdx) > 0)
                                                                                            {
                                                                                              LogIdx <- which(sapply(projVars@Log[LogIdx], function(x) !is.null(x$env)))
                                                                                            }

                                                                                            if (length(LogIdx) > 0)
                                                                                            {
                                                                                              dummy <- sapply(LogIdx, function(ix)
                                                                                                                      {
                                                                                                                        if (!is.null(projVars@Log[[ix]]) && !is.null(projVars@Log[[ix]]$env))
                                                                                                                        {
                                                                                                                          with_env(callbackFn, projVars@Log[[ix]]$env)(model, projVars, ix)
                                                                                                                        }
                                                                                                                      })
                                                                                            }
                                                                                         })
                                                  })
  }
)

# -----------------------------------------------------------------------------

setGeneric("forAllModelDataDo", function(.Object, ...) standardGeneric("forAllModelDataDo"))

setMethod("forAllModelDataDo", "MseFramework",
  function(.Object, callbackFn)
  {
    for (ix in 1:length(.Object@StockSynthesisModels))
    {
      ModelData <- .Object@StockSynthesisModels[[ix]]@ModelData
      RefVars   <- .Object@StockSynthesisModels[[ix]]@RefVars

      .Object@StockSynthesisModels[[ix]]@ModelData <- callbackFn(ModelData, RefVars, ix)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("subsetModels", function(.Object, ...) standardGeneric("subsetModels"))

setMethod("subsetModels", "MseFramework",
  function(.Object, ModelList)
  {
    if (length(ModelList > 0))
    {
      if (class(ModelList[[1]]) == "numeric")
      {
        .Object@StockSynthesisModels <- .Object@StockSynthesisModels[ModelList]
      }
      else
      {
        SelectList                   <- which(.Object@MseDef@OMList %in% ModelList)
        .Object@StockSynthesisModels <- .Object@StockSynthesisModels[SelectList]
      }
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("addMP_SourceCode", function(.Object, ...) standardGeneric("addMP_SourceCode"))

setMethod("addMP_SourceCode", "MseFramework",
  function(.Object, SourceFiles)
  {
    if (length(SourceFiles > 0))
    {
      for (SourceFile in SourceFiles)
      {
        if (!file.exists(SourceFile))
        {
          cat(paste("Cannot find source file", SourceFile, ". Please provide an explicit path to file."))
        }

        if (any(lapply(.Object@MP_SourceFilePaths, function(item) {return (SourceFile == item)})))
        {
          cat(paste(SourceFile, "is already added to this framework."))
        }
        else
        {
          idx <- length(.Object@MP_SourceFilePaths) + 1

          .Object@MP_SourceFilePaths[[idx]] <- SourceFile
        }
      }      
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("excludeFailedProjections", function(.Object, ...) standardGeneric("excludeFailedProjections"))

setMethod("excludeFailedProjections", "MseFramework",
  function(.Object)
  {
    FilteredModels <- list()
    Idy            <- 0

    for (Model in .Object@StockSynthesisModels)
    {
      FilteredProjVars <- list()
      Idx              <- 0

      for (ProjVars in Model@ProjectedVars)
      {
        ValidIdxs <- 1:ProjVars@nsim
        LogIdx    <- which(!is.na(ProjVars@Log))

        if (length(LogIdx) > 0)
        {
          ValidIdxs <- which(sapply(ProjVars@Log[LogIdx], function(x) is.null(x$error)))
        }

        nsims     <- length(ValidIdxs)

        if (nsims > 0)
        {
          if (nsims < ProjVars@nsim)
          {
            ProjVars@nsim          <- nsims
            ProjVars@Log           <- ProjVars@Log[ValidIdxs]
            ProjVars@F             <- ProjVars@F[keep(ValidIdxs),]
            ProjVars@SSB           <- ProjVars@SSB[keep(ValidIdxs),,]
            ProjVars@B             <- ProjVars@B[keep(ValidIdxs),,]
            ProjVars@CM            <- ProjVars@CM[keep(ValidIdxs),,]
            ProjVars@CMbyF         <- ProjVars@CMbyF[keep(ValidIdxs),,,]
            ProjVars@Rec           <- ProjVars@Rec[keep(ValidIdxs),]
            ProjVars@RecYrQtr      <- ProjVars@RecYrQtr[keep(ValidIdxs),]
            ProjVars@IobsArchive   <- ProjVars@IobsArchive[keep(ValidIdxs),]
            ProjVars@IobsRArchive  <- ProjVars@IobsRArchive[keep(ValidIdxs),,]
            ProjVars@TAC           <- ProjVars@TAC[keep(ValidIdxs),]
            ProjVars@TAEbyF        <- ProjVars@TAEbyF[keep(ValidIdxs),,]
          }

          Idx <- Idx + 1

          FilteredProjVars[[Idx]] <- ProjVars
        }
      }

      if (Idx > 0)
      {
        Model@ProjectedVars <- FilteredProjVars

        Idy <- Idy + 1

        FilteredModels[[Idy]] <- Model
      }
    }

    .Object@StockSynthesisModels <- FilteredModels

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("referenceVarData", function(.Object, ...) standardGeneric("referenceVarData"))

setMethod("referenceVarData", c("MseFramework"),
  function(.Object)
  {
    dataList <- list()
    colNames <- slotNames("ReferenceVars")
    nModels  <- length(.Object@StockSynthesisModels)

    for (cn in 1:nModels)
    {
      for (colName in colNames)
      {
        dataList[[colName]] <- c(dataList[[colName]], slot(.Object@StockSynthesisModels[[cn]]@RefVars, colName))
      }
    }

    return (as.data.table(dataList))
  }
)

