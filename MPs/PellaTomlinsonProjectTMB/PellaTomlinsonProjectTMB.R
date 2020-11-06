require(BuildSys)
require(TMB)


# Assumed that working folder is root of niMSE-IO-BET-YFT and that this code 
# resides in the "MPs/PellaTomlinsonProjectTMB" sub folder.
Project <- new("BSysProject", WorkingFolder="./MPs/PellaTomlinsonProjectTMB")

# -----------------------------------------------------------------------------

PTproj.TMB.25 <- function(pset)
{
  save(pset, file="Objects/pset.RDA")

  return(PellaTomlinsonProjectionTMB(pset, BMSY_Prop=pset$tune, Gain=0.25, debug=FALSE))
}

class(PTproj.TMB.25)               <- "IO_MP_tune"
attr(PTproj.TMB.25, "BSysProject") <- Project 


# -----------------------------------------------------------------------------
# Fit Pella Tomlinson Production model and use it to determine a TAC to drive
# yield to the desired target without crashing the stock.
# -----------------------------------------------------------------------------
PellaTomlinsonProjectionTMB <- function(pset, BMSY_Prop=1.0, Gain=0.15, MinCatchProp=0.15, debug=FALSE, deltaTACLimUp=0.9, deltaTACLimDown=0.9)
{
  # -----------------------------------------------------------------------------
  # Projection model
  # -----------------------------------------------------------------------------
  PT.project <- function(model, C, B0)
  {
    Y     <- length(C)
    r     <- model$r
    K     <- model$K
    p     <- model$p
    q     <- model$q
    B     <- array(NA, dim=Y)
    B[1]  <- B0

    for(y in 2:Y)
    {
      B[y] <- B[y-1] + r * B[y-1] - r * B[y-1] * ((B[y-1] / K) ^ p) - C[y-1]
    
      # impose an asymptotic limit to 10000. We use an asymptotic limit rather
      # than a hard limit (piecewise linear etc.) because it makes for a more
      # minimizer friendly objective function.
      limit <- 100.0

      if (B[y] < 3 * limit)
      {
        B[y] <- limit * (1.0 + (4.0 / (1.0 + exp(-((B[y] / limit) - 3)))))
      }
    }

    return (B)
  }

  # -----------------------------------------------------------------------------

  require(TMB)

  LibName <- "PTmodel"

  C_hist <- pset$Cobs
  I_hist <- pset$Iobs

  C_hist[which(C_hist == 0)] <- NA
  I_hist[which(I_hist == 0)] <- NA

  # filter leading NA's from the catch and cpue data
  ix                <- min(which(!is.na(C_hist))):length(C_hist)
  C_hist            <- C_hist[ix]
  I_hist            <- I_hist[ix]
  Nyear             <- length(C_hist)
  valid             <- which(!is.na(C_hist * I_hist))
  Eps_t_map         <- rep(NA, times=Nyear)
  Eps_t_map[valid]  <- valid
  Eps_t_map         <- as.factor(Eps_t_map)
  t_s               <- 1:Nyear

  #check for first run
  if (is.null(pset$env$report))
  {
    # estimate sigma I
    I_dev_est   <- log(I_hist[valid] / lowess(t_s[valid], I_hist[valid])$y)
    sigmaI_est  <- ((mean((I_dev_est - mean(I_dev_est)) ^ 2)) ^ 0.5)

    # estimate MSY initial parameter
    MSY_guess   <- mean(sort(C_hist, decreasing=T)[1:5]) 

    pset$env$estimated_p         <- c()
    pset$env$estimated_logMSY    <- c()
    pset$env$estimated_logK      <- c()
    pset$env$estimated_logq      <- c()
    pset$env$estimated_logSigmaI <- c()

    # do a retrospective fitting over 5 cases to estimate priors
    for (ix in 4:0)
    {
      # initial fit parameter set
      Parameters <- list(log_KonMSY     = log(50), 
                         log_MSY        = log(MSY_guess), 
                         log_p_n        = 0.0, 
                         log_n          = log(0.2), 
                         log_sigmaI     = log(sigmaI_est), 
                         log_sigmaRonI  = log(0.1),
                         Eps_t          = rep(0, Nyear - ix))

      # For first fitting fit without random effects to get into the right
      # ballpark and then use random effects with priors derrived from this
      # fit.
      data <- list(I_t=I_hist[1:(Nyear - ix)], 
                   C_t=C_hist[1:(Nyear - ix)],
                   prior_p_mean=0.5, 
                   prior_p_sigma=0.5,
                   prior_logMSY_mean=log(MSY_guess),
                   prior_logMSY_sigma=0.2,
                   prior_logK_mean=NA,
                   prior_logK_sigma=NA,
                   prior_logq_mean=NA,
                   prior_logq_sigma=NA,
                   prior_logSigmaI_mean=NA,
                   prior_logSigmaI_sigma=NA,
                   sigmaC=0.05,
                   sigmaEonI=0.05)

      # Don't fit sigmaRonI or Eps_t and no random effects
      # we use this fit as a baseline for priors of K and MSY
      Map               <- list()
      Map$Eps_t         <- factor(rep(NA, Nyear - ix))
      Map$log_sigmaRonI <- factor(NA)

      model <- MakeADFun(data=data, DLL=LibName, parameters=Parameters, map=Map, silent=TRUE)
      Opt   <- nlminb(start=model$par, objective=model$fn, gradient=model$gr, control=list("eval.max"=1e3, "iter.max"=1e3))
      rpt   <- model$report()

      pset$env$estimated_p         <- c(pset$env$estimated_p,         rpt$p)
      pset$env$estimated_logMSY    <- c(pset$env$estimated_logMSY,    log(rpt$MSY))
      pset$env$estimated_logK      <- c(pset$env$estimated_logK,      log(rpt$K))
      pset$env$estimated_logq      <- c(pset$env$estimated_logq,      log(rpt$q))
      pset$env$estimated_logSigmaI <- c(pset$env$estimated_logSigmaI, log(rpt$sigmaI))
    }

    if (debug)
    {
      par(mfrow=c(2,2))

      # diagnostic plots
      t_s <- 1:Nyear
      
      # catch fit
      plot(t_s, C_hist, type = "l")
      points(t_s, rpt$Cpred_t)

      # CPUE fit
      plot(t_s, I_hist, type = "l")
      points(t_s, rpt$Ipred_t)

      # effort fit
      plot(t_s, rpt$E_t, type = "l")
      points(t_s, rpt$q * C_hist / I_hist)

      # production model process noise
      plot(t_s, rpt$Eps_t, type = "l")

      browser()
    }

    pset$env$save     <- debug
    pset$env$report   <- rpt
    pset$env$BMSY     <- c()
    pset$env$MSY      <- c()
    pset$env$r        <- c()
    pset$env$K        <- c()
    pset$env$q        <- c()
    pset$env$p        <- c()
    pset$env$B0       <- c()
    pset$env$obj      <- c()
    pset$env$message  <- c()
    pset$env$y        <- c(pset$y)
    Eps_t             <- rep(0.0, Nyear)
  }
  else
  {
    rpt               <- pset$env$report
    Nprev_year        <- length(rpt$Eps_t)
    Eps_t             <- c(rpt$Eps_t, rep(0.0, Nyear - Nprev_year))

    # Init parameters for fit with random effects and priors
    Parameters <- list(log_KonMSY     = rpt$log_KonMSY, 
                       log_MSY        = rpt$log_MSY, 
                       log_p_n        = rpt$log_p_n, 
                       log_n          = rpt$log_n, 
                       log_sigmaI     = rpt$log_sigmaI,
                       log_sigmaRonI  = rpt$log_sigmaRonI,
                       Eps_t          = rep(0, Nyear))

    # For first fitting fit without random effects to get into the right
    # ballpark and then use random effects with priors derrived from this
    # fit.
    data <- list(I_t=I_hist, 
                 C_t=C_hist,
                 prior_p_mean=pset$env$prior_p_mean, 
                 prior_p_sigma=pset$env$prior_p_sigma,
                 prior_logMSY_mean=pset$env$prior_logMSY_mean,
                 prior_logMSY_sigma=0.2,
                 prior_logK_mean=pset$env$prior_logK_mean,
                 prior_logK_sigma=0.2,
                 prior_logq_mean=pset$env$prior_logq_mean,
                 prior_logq_sigma=0.2,
                 prior_logSigmaI_mean=NA,
                 prior_logSigmaI_sigma=NA,
                 sigmaC=0.05,
                 sigmaEonI=0.05)

    # Don't fit sigmaRonI or Eps_t and no random effects
    # we use this fit as a baseline for priors of K and MSY
    Map               <- list()
    Map$Eps_t         <- factor(rep(NA, Nyear))
    Map$log_sigmaRonI <- factor(NA)

    model <- MakeADFun(data=data, DLL=LibName, parameters=Parameters, map=Map, silent=TRUE)
    Opt   <- nlminb(start=model$par, objective=model$fn, gradient=model$gr, control=list("eval.max"=1e3, "iter.max"=1e3))
    rpt   <- model$report()

    pset$env$estimated_p         <- c(pset$env$estimated_p,         rpt$p)
    pset$env$estimated_logMSY    <- c(pset$env$estimated_logMSY,    log(rpt$MSY))
    pset$env$estimated_logK      <- c(pset$env$estimated_logK,      log(rpt$K))
    pset$env$estimated_logq      <- c(pset$env$estimated_logq,      log(rpt$q))
    pset$env$estimated_logSigmaI <- c(pset$env$estimated_logSigmaI, log(rpt$sigmaI))
  }

  # update prior estimates
  pset$env$prior_p_mean        <- mean(pset$env$estimated_p)
  pset$env$prior_p_sigma       <- var(pset$env$estimated_p) ^ 0.5
  pset$env$prior_logMSY_mean   <- mean(pset$env$estimated_logMSY)
  pset$env$prior_logMSY_sigma  <- var(pset$env$estimated_logMSY) ^ 0.5
  pset$env$prior_logK_mean     <- mean(pset$env$estimated_logK)
  pset$env$prior_logK_sigma    <- var(pset$env$estimated_logK) ^ 0.5
  pset$env$prior_logq_mean     <- mean(pset$env$estimated_logq)
  pset$env$prior_logq_sigma    <- var(pset$env$estimated_logq) ^ 0.5

  # Init parameters for fit with random effects and priors
  Parameters <- list(log_KonMSY     = rpt$log_KonMSY, 
                     log_MSY        = rpt$log_MSY, 
                     log_p_n        = rpt$log_p_n, 
                     log_n          = rpt$log_n, 
                     log_sigmaI     = rpt$log_sigmaI,
                     log_sigmaRonI  = rpt$log_sigmaRonI,
                     Eps_t          = rep(0, Nyear))

  # Do random effects fit
  data <- list(I_t=I_hist, 
               C_t=C_hist,
               prior_p_mean=rpt$p, 
               prior_p_sigma=0.1,
               prior_logMSY_mean=rpt$log_MSY,
               prior_logMSY_sigma=0.2,
               prior_logK_mean=log(rpt$K),
               prior_logK_sigma=0.2,
               prior_logq_mean=log(rpt$q),
               prior_logq_sigma=0.2,
               prior_logSigmaI_mean=rpt$log_sigmaI,
               prior_logSigmaI_sigma=0.05,
               sigmaC=0.05,
               sigmaEonI=0.05)

  Map       <- list()
  Map$Eps_t <- Eps_t_map
  Random    <- c("Eps_t")

  model <- MakeADFun(data=data, DLL=LibName, parameters=Parameters, map=Map, random=Random, silent=TRUE)
  Opt   <- nlminb(start=model$par, objective=model$fn, gradient=model$gr, control=list("eval.max"=1e3, "iter.max"=1e3))

  Parameters  <- Opt$par
  Fit         <- model$report()

  # save starting point for next time
  pset$env$report   <- Fit

  MinCatch          <- MinCatchProp * Fit$MSY
 
  pset$env$BMSY     <- c(pset$env$BMSY,    Fit$BMSY)
  pset$env$MSY      <- c(pset$env$MSY,     Fit$MSY)
  pset$env$r        <- c(pset$env$r,       Fit$r)
  pset$env$K        <- c(pset$env$K,       Fit$K)
  pset$env$q        <- c(pset$env$q,       Fit$q)
  pset$env$p        <- c(pset$env$p,       Fit$p)
  pset$env$B0       <- c(pset$env$B0,      Fit$B0)
  pset$env$obj      <- c(pset$env$obj,     Opt$objective)
  pset$env$message  <- c(pset$env$message, Opt$message)

  if (debug)
  {
    # diagnostic plots
    t_s <- 1:Nyear
    
    # catch fit
    plot(t_s, C_hist, type = "l")
    points(t_s, Fit$Cpred_t)

    # CPUE fit
    plot(t_s, I_hist, type = "l")
    points(t_s, Fit$Ipred_t)

    # effort fit
    plot(t_s, Fit$E_t, type = "l")
    points(t_s, Fit$q * C_hist / I_hist)

    # production model process noise
    plot(t_s, Fit$Eps_t, type = "l")

    browser()
  }

  lastTAC <- pset$prevTACE$TAC

  if (debug && !is.null(pset$complete) && pset$complete)
  {
    par(mfrow=c(2,2))

    plot(pset$env$BMSY, type="l", col="red")
    plot(pset$env$MSY,  type="l", col="red")
    plot(pset$env$r,    type="l", col="red")
    plot(pset$env$K,    type="l", col="red")

    browser()

    plot(pset$env$q,    type="l", col="red")
    plot(pset$env$p,    type="l", col="red")
    plot(pset$env$B0,   type="l", col="red")
    plot(pset$env$obj,  type="l", col="red")
    print(pset$env$message)

    browser()
  }

  # R some value less than 1.0 that controls the speed which the MP aims for
  # the target stock depletion
  R       <- Gain
  endTime <- pset$interval + 1

  # Need to account for depletion from last "interval" years of operating
  # under lastTAC because reporting of TAC is "interval" years behind
  # the present and the fishery has been operating for "interval" years at a
  # TAC of lastTAC
  BStart  <- PT.project(Fit, rep(lastTAC, times=endTime), Fit$B_t[Nyear])[endTime]
  Btarget <- BStart + ((Fit$BMSY * BMSY_Prop - BStart) * R)

  projection.objective <- function(TAC)
  {
     obj <- (Btarget - (PT.project(Fit, rep(TAC, times=endTime), BStart)[endTime])) ^ 2

    return (obj)
  }

  # if F is unrealistically high the catch limiting can cause a local maximum
  # at high F so we make sure we have a low F as as a starting point
  TAC_opt <- nlminb(start=lastTAC / 50, objective=projection.objective, lower=0.0, upper=Inf)
  newTAC  <- TAC_opt$par

  if (debug)
  {
    print(paste("r=", Fit$r, "K=", Fit$K, "p=", Fit$p, "q=", Fit$q, "n=", Fit$n, "MSY=", Fit$MSY, "BMSY=",Fit$BMSY, "BStart=", BStart, "BTarget=", Btarget, "TAC=", newTAC, "lastTAC=", lastTAC))
  }

  if (newTAC < MinCatch)
  {
    newTAC <- MinCatch
  }

  #End of pipe TAC change constraint - might interact poorly with constraint above
  deltaTAC <- newTAC/lastTAC - 1

  if (deltaTAC > deltaTACLimUp)
  {
    deltaTAC = deltaTACLimUp
  }

  if (deltaTAC < -deltaTACLimDown)
  {
    deltaTAC = -deltaTACLimDown
  }

  newTAC <- lastTAC * (1 + deltaTAC)

  if (newTAC < 9)
  {
    newTAC <- 9 #shut the fishery down, except collect some data
  }

  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF, TAC=newTAC))
}
