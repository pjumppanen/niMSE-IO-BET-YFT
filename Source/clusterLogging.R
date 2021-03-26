library(parallel)

# -----------------------------------------------------------------------------

MaxCores <- 0

detectCoresWithLimit <- function()
{
  nCores <- detectCores()

  if ((MaxCores > 0) && (nCores > MaxCores))
  {
    nCores <- MaxCores
  }

  return (nCores)
}

# -----------------------------------------------------------------------------

openCluster <- function(Jobs=-1, UseMonitor=TRUE)
{
  beginMonitoring <- function(Id)
  {
    portNum <- 6011 + Id
    con     <- try(socketConnection(host="localhost", port = portNum, blocking=FALSE, server=TRUE, open="wt+", timeout=30), silent=FALSE)

    if (class(con) != "try-error")
    {
      assign("monitorConnection", con, envir=globalenv())
      sink(file=con, append=FALSE, type=c("output", "message"))
    }
    else
    {
      print(paste("connection to port", portNum, "failed"))
    }
  }

  monitorEnv <- new.env(parent=globalenv())

  assign("clusterMonitor", monitorEnv, envir=globalenv())

  nCores <- detectCoresWithLimit()

  if ((Jobs > 0) && (Jobs < nCores))
  {
    nCores <- Jobs
  }

  cluster <- makeCluster(nCores)

  if (UseMonitor)
  {
    WorkerIds <- 1:nCores
    FileName  <- tempfile("runMonitor", fileext=".R")

    if (identical(.Platform$OS.type, "windows"))
    {
      # Backslashes passed to command processor are a pain on Windows so use forward slash instead
      FileName <- gsub("\\\\", "/", FileName)
    }

    assign("cluster",         cluster,   envir=clusterMonitor)
    assign("WorkerIds",       WorkerIds, envir=clusterMonitor)
    assign("monitorFileName", FileName,  envir=clusterMonitor)

    clusterEvalQ(cluster, eval(parse("Source/clusterLogging.R")))

    monitorFile <- file(FileName, "wt")
    writeLines("runMonitor <- ", con=monitorFile)
    writeLines(capture.output(print(runMonitor)), con=monitorFile)
    close(monitorFile)

    CmdLine <- paste("R --vanilla --quiet -e \"source('", FileName, "');runMonitor(", nCores, ")\"", sep="")

    system(CmdLine, wait=FALSE, invisible=FALSE)

    parSapply(cluster, WorkerIds, FUN=beginMonitoring)
  }

  return (cluster)
}

# -----------------------------------------------------------------------------

closeCluster <- function(TimeOutTime=15)
{
  endMonitoring <- function(Id)
  {
    if (exists("monitorConnection", envir=globalenv()))
    {
      con <- get("monitorConnection", envir=globalenv())

      writeLines("\n--CLOSE--", con=con)

      # Wait for acknowledgement of closure
      Again        <- TRUE
      SleepTime    <- 0.1
      TimeoutCount <- as.integer(TimeOutTime / SleepTime)

      while (Again)
      {
        Response <- readLines(con=con, warn=FALSE)

        if (length(Response) > 0)
        {
          Response <- paste(Response, collapse="")

          if (regexpr("C", Response) > 0)
          {
            Again <- FALSE
          }
        }

        if (Again)
        {
          flush(con)
          Sys.sleep(SleepTime)

          TimeoutCount <- TimeoutCount - 1
          Again        <- (TimeoutCount > 0)
        }
      }

      sink()
    }
  }

  clusterMonitor <- get("clusterMonitor",   envir=globalenv())

  if (exists("cluster", envir=clusterMonitor))
  {
    cluster        <- get("cluster",          envir=clusterMonitor)
    WorkerIds      <- get("WorkerIds",        envir=clusterMonitor)
    FileName       <- get("monitorFileName",  envir=clusterMonitor)

    parSapply(cluster, WorkerIds, FUN=endMonitoring)
    stopCluster(cluster)

    unlink(FileName)
  }
}

# -----------------------------------------------------------------------------

beginLog <- function(Id)
{
  StdOutFileName <- paste("clusterStdOutFile", Id, ".txt", sep="")

  sink(file=StdOutFileName, append=FALSE, type=c("output", "message"), split=TRUE)
}

# -----------------------------------------------------------------------------

endLog <- function(Id)
{
  sink()
}

# -----------------------------------------------------------------------------

printLog <- function(Id)
{
  clusterMonitor <- get("clusterMonitor",   envir=globalenv())

  if (exists("cluster", envir=clusterMonitor))
  {
    StdOutFileName <- paste("clusterStdOutFile", Id, ".txt", sep="")

    if (file.exists(StdOutFileName))
    {
      con <- file(StdOutFileName, "rt")

      writeLines(readLines(con))
      close(con)
      unlink(StdOutFileName)
    }
  }
}

# -----------------------------------------------------------------------------

runMonitor <- function(nCores, TimeOutTime=5, MaxLines=40)
{
  library(parallel)

  SleepBy <- 0.01

  monitorLoop <- function(nCores)
  {
    if (nCores > 0)
    {
      cons   <- list()
      isOpen <- list()

      print(paste("Waiting for", nCores, "connections..."))

      cn    <- 1
      nOpen <- 0

      while (cn <= nCores)
      {
        portNum <- 6011 + cn
        con     <- try(socketConnection(host="localhost", port = portNum, blocking=FALSE, server=FALSE, open="rt+", timeout=30), silent=FALSE)

        if (class(con)[1] != "try-error")
        {
          print(paste("Connected", cn, "to port", portNum))

          isOpen[[cn]] <- TRUE
          cons[[cn]]   <- con
          cn           <- cn + 1
        }
        else
        {
          Sys.sleep(SleepBy)
        }
      }

      nOpen         <- cn - 1
      TimeOutCount  <- as.integer(TimeOutTime / SleepBy)
      Buffer        <- array("", nCores)
      TimeOut       <- array(TimeOutCount, nCores)

      cat("Logging cores...\n")

      while (nOpen > 0)
      {
        for (cn in 1:nCores)
        {
          if (isOpen[[cn]])
          {
            con     <- cons[[cn]]
            Lines   <- readLines(con=con, warn=FALSE)
            Output  <- Buffer[cn]

            if (length(Lines) > 0)
            {
              Output <- paste(Buffer[cn], paste(paste(Lines, collapse="\n"), "\n", sep=""))

              if (regexpr("--CLOSE--", Output) > 0)
              {
                # Signal closure
                writeLines("C", con=con)

                close(con)

                isOpen[[cn]] <- FALSE
                nOpen        <- nOpen - 1

                cat(paste("\n\n--------- Closing worker ", cn, " ---------\n", Output,"\n", sep=""))
              }
            }

            Lines <- unlist(strsplit(Output, "\n"))
            Count <- length(Lines)

            TimeOut[cn] <- TimeOut[cn] - 1
            PrintBuffer <- (TimeOut[cn] <= 0) || (Count > MaxLines)

            if (TimeOut[cn] <= 0)
            {
              TimeOut[cn] <- TimeOutCount
            }

            if (PrintBuffer && (Count > 1))
            {
              cat(paste("\n\n--------- Worker ", cn, " ---------\n", paste(Lines[1:(Count-1)], collapse="\n"), "\n", sep=""))

              Output <- Lines[Count]
            }

            Buffer[cn] <- Output
          }
        }

        Sys.sleep(SleepBy)
      }
    }
  }

  error <- try(monitorLoop(nCores))

  if (class(error) == "try-error")
  {
    print(error)

    # If we have an error in the monitor code we want to see it. Therefore wait
    # for a very long time (1 week).
    TimeOutCount <- as.integer(604800 / SleepBy)

    while (TimeOutCount > 0)
    {
      Sys.sleep(SleepBy)

      TimeOutCount <- TimeOutCount - 1
    }
  }
}
