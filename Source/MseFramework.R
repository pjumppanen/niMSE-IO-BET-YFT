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
  ),

  prototype = list(
    performanceMeasureYears = 20,
    tuningTolerance         = 0.01,
    tuningLogDomain         = c(-3,0.5),   # -3, 0.5; maybe should transform relevant MP paramters so this domain can remain unchanged
    bisectMethod            = TRUE
  )
)


# -----------------------------------------------------------------------------
# MseFramework class
# -----------------------------------------------------------------------------
setClass("MseFramework",
  slots = c(
    MseDef                = "MseDefinition",  # parent MSE definition
    tune                  = "numeric",        # vector of tuning parameters from last runMse() call
    StockSynthesisModels  = "list"            # list of StockSynthesisModel objects
  )
)

# -----------------------------------------------------------------------------

setMethod("initialize", "MseFramework",
  function(.Object, MseDef, Report=FALSE, UseCluster=NA, UseMSYss=0)
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
        require(keep)

        # In a cluster context we re-direct output to file so we can then play
        # it back in the host
        StdOutFileName <- paste("msyStdOutFile", Job$which, ".txt", sep="")
        sink(file=StdOutFileName, append=FALSE, type=c("output", "message"))
      }

      StockSynthesisModel <- new("StockSynthesisModel", MseDef, Job$which, Job$seed, Report, UseMSYss)

      if (UseCluster)
      {
        sink()
      }

      return (StockSynthesisModel)
    }

    printJobOutput <- function(Job)
    {
      StdOutFileName <- paste("msyStdOutFile", Job$which, ".txt", sep="")

      if (file.exists(StdOutFileName))
      {
        Con <- file(StdOutFileName, "rt")

        writeLines(readLines(Con))
        close(Con)
        unlink(StdOutFileName)
      }
    }

    nCores  <- detectCores()
    nJobs   <- length(JobList)

    if (is.na(UseCluster))
    {
      UseCluster <- .Object@MseDef@UseCluster
    }

    if (nJobs < nCores)
    {
      nCores <- nJobs
    }

    if (UseCluster && (nCores > 1))
    {
      cl <- makeCluster(nCores)

      clusterEvalQ(cl, eval(parse("Source/MseMain.R")))

      .Object@StockSynthesisModels <- parLapply(cl, JobList, FUN=runJob, .Object@MseDef, Report, UseMSYss, UseCluster)

      stopCluster(cl)

      lapply(JobList, FUN=printJobOutput)
    }
    else
    {
      .Object@StockSynthesisModels <- lapply(JobList, FUN=runJob, .Object@MseDef, Report, UseMSYss, FALSE)
    }

    return (.Object)
  }
)

# -----------------------------------------------------------------------------

setGeneric("runMse", function(.Object, ...) standardGeneric("runMse"))

setMethod("runMse", c("MseFramework"),
  function(.Object, MPs, TuningPars=NA, interval=3, Report=FALSE, CppMethod=NA, UseCluster=NA, EffortCeiling = as.double(20.0), TACTime = 0.5, rULim = 0.5)
  {
    cluster <- NA

    if (is.na(UseCluster))
    {
      UseCluster <- .Object@MseDef@UseCluster
    }

    if (UseCluster)
    {
      nCores  <- detectCores()
      cluster <- makeCluster(nCores)

      clusterEvalQ(cluster, eval(parse("Source/MseMain.R")))
    }

    runModels <- function(StockSynthesisModels, MPs, tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)
    {
      if (UseCluster)
      {
        runJob <- function(om)
        {
          # In a cluster context we re-direct output to file so we can then play
          # it back in the host
          StdOutFileName <- paste("projStdOutFile", om@ModelData@which, ".txt", sep="")
          sink(file=StdOutFileName, append=FALSE, type=c("output", "message"))

          om <- runMse(om, .Object@MseDef, MPs, tune, interval, Report, CppMethod, cluster=NA, EffortCeiling, TACTime, rULim)

          print("\n")
          sink()

          return (om)
        }

        printJobOutput <- function(om)
        {
          StdOutFileName <- paste("projStdOutFile", om@ModelData@which, ".txt", sep="")

          if (file.exists(StdOutFileName))
          {
            Con <- file(StdOutFileName, "rt")

            writeLines(readLines(Con))
            close(Con)
            unlink(StdOutFileName)
          }
        }

        StockSynthesisModels <- parLapply(cluster, StockSynthesisModels, fun=runJob)
        lapply(StockSynthesisModels, FUN=printJobOutput)
      }
      else
      {
        StockSynthesisModels <- lapply(StockSynthesisModels, FUN=function(om) {return(runMse(om, .Object@MseDef, MPs, tune, interval, Report, CppMethod, cluster=NA, EffortCeiling, TACTime, rULim))})
      }

      return (StockSynthesisModels)
    }

    .Object@tune <- rep(1.0, times=length(MPs))

    if (class(TuningPars) == "TuningParameters")
    {
      HasTuning <- !identical(TuningPars@performanceMeasure, character(0))

      # Do tuning
      if (HasTuning)
      {
        TunedMPs <- c()
        tuneIdx  <- list()
        idx      <- 1

        for (MP in MPs)
        {
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

        for (MP in TunedMPs)
        {
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
              print("\n Aborting tuning because Par values do not bracket the target")
              print("This could be because the par bounds are too narrow, tuning objective is not attainable, or function is non-monotonic")
              .Object@tune[tuneIdx[[MP]]] <- NaN

            } else
            {
              print("\n bisection minimization for quantiles")
              print(c("loPar  ",  loPar, "      loVal ", loVal))
              print(c("hiPar  ",  hiPar, "      hiVal ", hiVal))
              print(c("tmpPar ", tmpPar, "     tmpVal ", tmpVal))

              nFnEval <- 4

              while (abs((tmpVal - TuningPars@tuningTarget) / TuningPars@tuningTarget) > TuningPars@tuningTolerance & abs(10 ^ hiPar - 10 ^ loPar) / (10 ^ loPar) > 0.005)
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

                #linear interpolation - should be more effective... predict par value as fn of obj function
                #not much difference over bisection in a handful of trials - sometimes worse
                linMod <- lm(pars ~ vals, data=as.data.frame(cbind(vals=c(loVal, hiVal), pars=c(loPar, hiPar))))
                b      <- linMod$coef[1]
                m      <- linMod$coef[2]
                tmpPar <- m * TuningPars@tuningTarget + b

                print(c("next tmpPar ",tmpPar))
                tmpVal <- bisectObjFn(tmpPar, MP)

                print("                                        bisection minimization for quantiles")
                print(c("nFnEval ",nFnEval))
                print(c("loPar   ",loPar,  "     loVal ", loVal))
                print(c("hiPar   ",hiPar,  "     hiVal ", hiVal))
                print(c("tmpPar  ",tmpPar, "    tmpVal ", tmpVal))

                nFnEval <- nFnEval + 1
              }

              .Object@tune[tuneIdx[[MP]]] <- 10 ^ tmpPar
            }

          } else
          {
            res <- optimise(optimiseObjFn, interval=TuningPars@tuningLogDomain, MP, tol=TuningPars@tuningTolerance)

            print(res)

            .Object@tune[tuneIdx[[MP]]] <- 10 ^ res$minimum
          }
        }
      }
    }

    # re-run all models with final tuning
    .Object@StockSynthesisModels <- runModels(.Object@StockSynthesisModels, MPs, .Object@tune, interval, Report, CppMethod, EffortCeiling, TACTime, rULim)

    if (UseCluster)
    {
      stopCluster(cluster)
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

    for (om in .Object@StockSynthesisModels)
    {
      df <- msevizPerformanceData(om, .Object, df, AvgYears, prefix)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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

    statHandlers[["PrGreen"]] <- list(
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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
      addFn = function(HistoricVars, RefVars, context)
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

    statHandlers[["CPUEbyArea"]] <- list(
      addFn = function(HistoricVars, RefVars, context)
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
    dt <- data.table(year=as.integer(rep(0, times=row_count)),
                     data=as.double(rep(0, times=row_count)),
                     iter=as.integer(rep(0, times=row_count)),
                     qname=rep("", times=row_count))

    # fill result data.table with data
    res <- list(dt=dt, origin=0, iter=0)

    for (Indicator in Indicators)
    {
      if (Indicator %in% names(statHandlers))
      {
        handler  <- statHandlers[[Indicator]]
        res$iter <- 0

        for (om in .Object@StockSynthesisModels)
        {
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

              set(context$dt, rows, "year",  C1)
              set(context$dt, rows, "data",  C2)
              set(context$dt, rows, "iter",  C3)
              set(context$dt, rows, "qname", C4)

              context$origin <- next_origin

              return (context)
            }

            res <- handler$addFn(om@HistoricVars, om@RefVars, res)

            res$iter <- res$iter + om@ModelData@nsim

          } else
          {
            for (ProjVar in om@ProjectedVars)
            {
              SY   <- as.matrix(expand.grid(nsims=1:ProjVar@nsim, nyrs=1:ProjVar@nyears))
              Yrs  <- .Object@MseDef@firstCalendarYr + om@ModelData@nyears + (0:(ProjVar@nyears - 1))
              C1   <- rep(paste(prefix, ProjVar@MP, sep=""), times=length(SY[,1]))
              C2   <- Yrs[SY[,2]]

              addRows <- function(context, data, name)
              {
                next_origin <- (context$origin + length(Yrs) * ProjVar@nsim)
                rows        <- context$origin:(next_origin - 1)

                C3          <- data[SY]
                C4          <- as.integer(context$iter + SY[,1])
                C5          <- rep(name, times=length(SY[,1]))

                set(context$dt, rows, "mp",    C1)
                set(context$dt, rows, "year",  C2)
                set(context$dt, rows, "data",  C3)
                set(context$dt, rows, "iter",  C4)
                set(context$dt, rows, "qname", C5)

                context$origin <- next_origin

                return (context)
              }

              res <- handler$addFn(ProjVar, om@RefVars, res)
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
    nmodels     <- length(.Object@StockSynthesisModels)
    ntotal_sims <- 0

    if (nmodels > 0)
    {
      nMPs <- length(.Object@StockSynthesisModels[[1]]@ProjectedVars)

      if (nMPs > 0)
      {

        MPs <- names(.Object@StockSynthesisModels[[1]]@ProjectedVars)

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

    codeExpr <- paste(codeExpr, paste(extraColumns, collapse=","), ")", sep="")
    dt       <- eval(parse(text=codeExpr))
    nrow     <- 1

    for (MP in MPs)
    {
      values <- list(MP=MP)

      for (Statistic in Statistics)
      {
        if (Statistic %in% names(statHandlers))
        {
          nparts <- statHandlers[[Statistic]]$partsFn()

          for (cn in 1:nparts)
          {
            cnames     <- if (nparts > 1) (paste(Statistic, cn, ".", sep="") %&% c("mean", percentiles)) else (Statistic %&% c("mean", percentiles))
            SourceData <- karray(NA, ntotal_sims)
            nsim       <- 1

            for (stockSynthesisModel in .Object@StockSynthesisModels)
            {
              nproj_sims <- stockSynthesisModel@ModelData@nsim
              nendsim    <- nsim + nproj_sims - 1

              SourceData[nsim:nendsim] <- statHandlers[[Statistic]]$statFn(stockSynthesisModel@ProjectedVars[[MP]], stockSynthesisModel@RefVars, stockSynthesisModel@HistoricVars, cn)

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
