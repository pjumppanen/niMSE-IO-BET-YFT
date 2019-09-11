CMCsum <- 406003.8

#save(data, file="test.data.rda")
load(file="test.data3.rda")
load(file="test.data2.rda")
load(file="test.data.rda")

Tau    <- 25.0
C2Init <- 1 / (CMCsum * 20)
params <- log(c(0.1, C2Init, 1, 0.9))
maxp   <- c(log(1), log(C2Init * 20), log(1.5), log(1.0))
minp   <- c(log(1e-6), log(C2Init / 20), log(0.2), log(1e-2))
weight <- exp((-(length(data$C_hist) - 1):0) / Tau)

prefit <- function(data, params, minp, maxp)
{
  window <- 25
  nreps  <- max(length(data$C_hist) - window, 1)
  p      <- c()
  r      <- c()
  K      <- c()

  for (cn in 1:nreps)
  {
    ix  <- cn:(cn + window - 1)
    opt <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, lower=minp, upper=maxp)
    Fit <- PT.model(opt$par, data$C_hist[ix], data$I_hist[ix], Type=1)

    if (opt$convergence == 0)
    {
      p <- c(p, Fit$p)
      r <- c(r, Fit$r)
      K <- c(K, Fit$K)
    }
  }

  # K is the most volatile parameter with some fitted values off the charts.
  # use a histogram approach to filter out outliers.
  powers  <- floor(log10(K))
  density <- array(0, dim=max(powers))

  sapply(powers, function(x) {density[x] <<- density[x] + 1})

  mostcommon <- which(density == max(density))
  max_limit  <- (10 ^ max(mostcommon)) * 10
  min_limit  <- (10 ^ min(mostcommon))

  ix    <- which((K > min_limit) & (K < max_limit))
  p_est <- exp(mean(log(p[ix])))
  r_est <- exp(mean(log(r[ix])))
  K_est <- exp(mean(log(K[ix])))

  return (list(p=p_est, r=r_est, K=K_est))
}

pf<-prefit(data, params, minp, maxp)

opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist, I_hist=data$I_hist, Type=0, lower=minp, upper=maxp)
Fit    <- PT.model(opt$par, data$C_hist[ix], data$I_hist[ix], Type=1)

params <- log(c(0.9))
maxp   <- c(log(1.0))
minp   <- c(log(1e-6))

ix <- 1:50
ix     <- max(length(data$C_hist) - 25, 1):length(data$C_hist)

PT.model.fit.n(params, data$C_hist, data$I_hist, Type=1, K=pf$K, r=pf$r, p=pf$p)

opt    <- nlminb(start=params, objective=PT.model.fit.n, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, K=pf$K, r=pf$r, p=pf$p)
opt    <- nlminb(start=params, objective=PT.model.fit.n, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, K=pf$K, r=pf$r, p=pf$p, lower=minp, upper=maxp)
opt    <- nlminb(start=params, objective=PT.model.fit.n, C_hist=data$C_hist, I_hist=data$I_hist, Type=0, K=pf$K, r=pf$r, p=pf$p, lower=minp, upper=maxp)
Fit    <- PT.model.fit.n(opt$par, data$C_hist[ix], data$I_hist[ix], Type=1, K=pf$K, r=pf$r, p=pf$p)
Fit    <- PT.model.fit.n(opt$par, data$C_hist, data$I_hist, Type=1, K=pf$K, r=pf$r, p=pf$p)
Fit    <- PT.model.fixed.K(c(log(2),0, 0), data$C_hist[ix], data$I_hist[ix], Type=1, K=pf$K, r=pf$r, p=pf$p)
Fit    <- PT.model.fixed.K(c(0 ,0, 0, 0), data$C_hist[ix], data$I_hist[ix], Type=1, K=pf$K, r=pf$r, p=pf$p)
opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, lower=minp, upper=maxp)


params <- log(c((pf$r/pf$p), 1.0 / (pf$K^pf$p), pf$p, 1.0))
Fit    <- PT.model(par, data$C_hist[ix], data$I_hist[ix], Type=1)

plot(Fit$LI_hist)
lines(Fit$LB, col=2)

plot(Fit$B, col=2)
lines(data$I_hist[ix] / Fit$q)
lines(data$I_hist / Fit$q)

plot(data$I_hist[ix] / Fit$q, col=2)
lines(Fit$B)

plot(data$I_hist[ix])

opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0)

opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, lower=minp, upper=maxp)
Fit    <- PT.model(opt$par, data$C_hist[ix], data$I_hist[ix], Type=1)
#opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist, I_hist=data$I_hist, weight=weight, Type=0, lower=minp, upper=maxp)
#Fit    <- PT.model(opt$par, data$C_hist, data$I_hist, weight=weight, Type=1)
#opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist, I_hist=data$I_hist, Type=0, lower=minp, upper=maxp)
#Fit    <- PT.model(opt$par, data$C_hist, data$I_hist, Type=1)

plot(Fit$LI_hist)
lines(Fit$LB, col=2)

plot(data$I_hist / Fit$q)
lines(Fit$B, col=2)


ix     <- (length(data$C_hist) - 35):length(data$C_hist)
opt    <- nlminb(start=params, objective=PT.model, C_hist=data$C_hist[ix], I_hist=data$I_hist[ix], Type=0, lower=minp, upper=maxp)
Fit    <- PT.model(opt$par, data$C_hist[ix], data$I_hist[ix], weight=weight, Type=1)

plot(Fit$LI_hist)
lines(Fit$LB, col=2)

plot(data$I_hist / Fit$q)
lines(Fit$B, col=2)


PT.model.fixed.K <- function(params, C_hist, I_hist, weight=NULL, Type, K, r, p)
{
  # C1 = (r/p)
  # C2 = 1.0 / (K^p))
#  par <- c(params[1] + log(r/p), params[2] - p * log(K), params[3] + log(p), params[4])
  par <- c(params[1] + log(r/p), -p * log(K), log(p), params[2])

  return (PT.model(par, C_hist, I_hist, weight, Type))
}


# -----------------------------------------------------------------------------
# Pella-Tomlinson Model function - fixed n
# -----------------------------------------------------------------------------
PT.model.fit.n <- function(params, C_hist, I_hist, weight=NULL, Type, r, K, p)
{
  # Model formulation is,
  # B(t+1)=B(t) + (r/p)B(t)(1-(B(t)/K)^p) - C(t)
  #
  Y     <- length(C_hist)
  n     <- exp(params[1])
  B     <- array(NA,dim=Y)
  B[1]  <- n * K

  for(y in 2:Y)
  {
    Bsurvive <- B[y-1] + (r / p) * B[y-1]
    Bdie     <- (r / p) * B[y-1] * ((B[y-1] / K) ^ p) + C_hist[y-1]
    dlim     <- 2.0 / (1 + exp((Bdie / Bsurvive)^5)) # saturation mechanism that forces B to always be positive
    B[y]     <- Bsurvive - dlim * Bdie

    if (is.na(B[y]))
    {
      browser()
    }
  }

  if (Type == 0)
  {
    # B and I_hist differ only by the scaling factor q. Taking the log, the
    # scaling factor comes out as an additive constant and the effect of
    # subtracting the mean will be to remove that point of difference between
    # the two series. Building the cost function this way ensures that the model
    # fits the shape shape in log space for both, even though they differ by a
    # scaling factor which can be determine in a secondary process and is q.
    sB      <- sign(B)
    LB      <- sB * log(sB * B)
    LB      <- LB - mean(LB)
    LI_hist <- log(I_hist)
    LI_hist <- LI_hist - mean(LI_hist)

    if (is.null(weight))
    {
      cost  <- sum((LI_hist - LB) ^ 2)
    }
    else
    {
      cost  <- sum((weight * (LI_hist - LB)) ^ 2)
    }

    return (cost)
  }
  else if (Type == 1)
  {
    sB      <- sign(B)
    LB      <- sB * log(sB * B)
    LI_hist <- log(I_hist)
    q       <- exp(mean(LI_hist) - mean(LB))
    LB      <- LB - mean(LB)
    LI_hist <- LI_hist - mean(LI_hist)

    B0       <- B[1]
    BY       <- B[Y]
    MSY      <- r * K / ((p + 1.0) ^ (1.0 + (1.0 / p)))
    E_MSY    <- r / (q * (p + 1.0))
    CPUE_MSY <- MSY / E_MSY

    Fit <- list(LB=LB, LI_hist=LI_hist, p=p, q=q, r=r, K=K, n=n, MSY=MSY, BMSY=CPUE_MSY / q, CPUE_MSY=CPUE_MSY, B0=B0, BY=BY, B=B)

    return (Fit)
  }
}


# -----------------------------------------------------------------------------
# Pella-Tomlinson Model function
# -----------------------------------------------------------------------------
PT.model <- function(params, C_hist, I_hist, Type, weight=NULL)
{
  # Model formulation is,
  # B(t+1)=B(t) + (r/p)B(t)(1-(B(t)/K)^p) - C(t)
  #
  # To help the optimizer fit the model we re-arrange things into a decoupled
  # form,
  #
  # B(t+1)=(C1 + 1) * B(t) - C1 * C2 * (B(t) ^ (p + 1)) - C(t)
  #
  # Where,
  #
  # C1 = (r/p)
  # C2 = 1.0 / (K^p))
  #
  # Translated back we have,
  #
  # K = (1.0 / C2) ^ (1.0 / p)
  # r = C1 * p
  #
  Y     <- length(C_hist)
  C1    <- exp(params[1])
  C2    <- exp(params[2])
  p     <- exp(params[3])
  n     <- exp(params[4])
  B     <- array(NA,dim=Y)
  K     <- (1.0 / C2) ^ (1.0 / p)
  B[1]  <- n * K

  for(y in 2:Y)
  {

    Bsurvive <- (C1 + 1.0) * B[y-1]
    Bdie     <- C1 * C2 * (B[y-1] ^ (p + 1)) + C_hist[y-1]
    dlim     <- 2.0 / (1 + exp((Bdie / Bsurvive)^5)) # saturation mechanism that forces B to always be positive
    B[y]     <- Bsurvive - dlim * Bdie

    if (is.na(B[y]) || B[y] <= 0.0)
    {
      browser()
    }
  }

  if (Type == 0)
  {
    # B and I_hist differ only by the scaling factor q. Taking the log, the
    # scaling factor comes out as an additive constant and the effect of
    # subtracting the mean will be to remove that point of difference between
    # the two series. Building the cost function this way ensures that the model
    # fits the shape shape in log space for both, even though they differ by a
    # scaling factor which can be determine in a secondary process and is q.
    sB      <- sign(B)
    LB      <- sB * log(sB * B)
    LI_hist <- log(I_hist)
    devs    <- LI_hist - LB
    devs    <- devs - mean(devs)

    if (is.null(weight))
    {
      cost  <- sum(devs ^ 2)
    }
    else
    {
      cost  <- sum((weight * devs) ^ 2)
    }

    return (cost)
  }
  else if (Type == 1)
  {
    sB      <- sign(B)
    LB      <- sB * log(sB * B)
    LI_hist <- log(I_hist)
    q       <- exp(mean(LI_hist - LB))
    LB      <- LB - mean(LB)
    LI_hist <- LI_hist - mean(LI_hist)

    K   <- (1.0 / C2) ^ (1.0 / p)
    B0  <- B[1]
    BY  <- B[Y]
    r   <- C1 * p

    MSY      <- r * K / ((p + 1.0) ^ (1.0 + (1.0 / p)))
    E_MSY    <- r / (q * (p + 1.0))
    CPUE_MSY <- MSY / E_MSY

    Fit <- list(LB=LB, LI_hist=LI_hist, p=p, q=q, r=r, K=K, n=n, MSY=MSY, BMSY=CPUE_MSY / q, CPUE_MSY=CPUE_MSY, B0=B0, BY=BY, B=B)

    return (Fit)
  }
}

