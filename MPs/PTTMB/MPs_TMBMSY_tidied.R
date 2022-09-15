#------------------------------------------------------------------------------
# Management Procedures for Indian Ocean MSE
#
# exploratory version - initially containing only those based on TMB
#------------------------------------------------------------------------------
# MP must provide an aggregate TAC and disaggregated TAE by fishery
# (if TAE>0, that fishery is given 0 TAC) seasonal (and fishery in the case of TAC)
# disaggregations are currently based on the "recent" historical means the
# R-based projection code assumes that TAC catch proportions remain constant
# among seasons within years the Cpp-based projections assume that the TAC
# Effort proportions remain constant among seasons within years
#
# leading parameter MSY instead of r
#------------------------------------------------------------------------------

library(TMB)
library(BuildSys) # xxx part of PJ MP format change
library(ggplot2)

# Assumed that working folder is root of niMSE-IO-BET-YFT.
ProjectPTBoB0Targ <- new("BSysProject", WorkingFolder="./MPs/PTTMB/PTBoB0Targ", Debug=FALSE, CXXFLAGS=c("-std=gnu++11", "-fno-aggressive-loop-optimizations"))
ProjectPT41F      <- new("BSysProject", WorkingFolder="./MPs/PTTMB/PT41F", Debug=FALSE, CXXFLAGS=c("-std=gnu++11", "-fno-aggressive-loop-optimizations"))

#------------------------------------------------------------------------------
# Pella-Tomlinson type MP with internal projection to calculate the TAC required 
# to hit BTarget - intially in terms of depletion in nProj years
# t15 = 15% change constraint; B34 = 34% depletion initial tuning target
#------------------------------------------------------------------------------
PTBoB0Targ.t15<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t15) <- "IO_MP_tune"
attr(PTBoB0Targ.t15, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

# t10 = 10% change constraint; B34 = 34% depletion initial tuning target
PTBoB0Targ.t10<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.1, deltaTACLimDown=0.1,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t10) <- "IO_MP_tune"
attr(PTBoB0Targ.t10, "BSysProject") <- ProjectPTBoB0Targ 

#------------------------------------------------------------------------------

PTBoB0Targ.t25<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.25, deltaTACLimDown=0.25,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t25)<-"IO_MP_tune"
attr(PTBoB0Targ.t25, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

#------------------------------------------------------------------------------

PTBoB0Targ.t35<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.35, deltaTACLimDown=0.35,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t35)<-"IO_MP_tune"
attr(PTBoB0Targ.t35, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

#------------------------------------------------------------------------------

PTBoB0Targ.t50<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.5, deltaTACLimDown=0.5,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t50)<-"IO_MP_tune"
attr(PTBoB0Targ.t50, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

#------------------------------------------------------------------------------

PTBoB0Targ.t75<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.75, deltaTACLimDown=0.75,
                         nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t75)<-"IO_MP_tune"
attr(PTBoB0Targ.t75, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

#------------------------------------------------------------------------------
# t15 = 15% change constraint; B34 = 34% depletion initial tuning target
# projection taregt years if above and below dep target = 10, 10
#------------------------------------------------------------------------------

PTBoB0Targ.t15.pr10<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
                         nProjAbove = 10, nProjBelow = 10, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t15.pr10) <- "IO_MP_tune"
attr(PTBoB0Targ.t15.pr10, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change


# projection taregt years if above and below dep target = 15, 15
PTBoB0Targ.t15.pr15<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
                              nProjAbove = 15, nProjBelow = 15, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t15.pr15) <- "IO_MP_tune"
attr(PTBoB0Targ.t15.pr15, "BSysProject") <- ProjectPTBoB0Targ # xxx part of PJ MP format change

# projection taregt years if above and below dep target = 5, 5
PTBoB0Targ.t15.pr5<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
                              nProjAbove = 5, nProjBelow = 5, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t15.pr5) <- "IO_MP_tune"
attr(PTBoB0Targ.t15.pr5, "BSysProject") <- ProjectPTBoB0Targ 



#------------------------------------------------------------------------------
# projection taregt years if above and below dep target = 25, 25
PTBoB0Targ.t15.pr25<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
                             nProjAbove = 25, nProjBelow = 25, DepErrLow = 0.1)
{
  return (PTBoB0Targ.base(pset, 
                          BLower, 
                          BUpper, 
                          BoB0Targ, 
                          useF, 
                          deltaTACLimUp, 
                          deltaTACLimDown,
                          nProjAbove, 
                          nProjBelow, 
                          DepErrLow))
}

class(PTBoB0Targ.t15.pr25) <- "IO_MP_tune"
attr(PTBoB0Targ.t15.pr25, "BSysProject") <- ProjectPTBoB0Targ 



#------------------------------------------------------------------------------

PTBoB0Targ.base<-function(pset, BLower=0.001,BUpper=0.004,BoB0Targ=0.28, useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15,
       #nProjAbove = 15, nProjBelow = 5, DepErr = 0.5){
       nProjAbove = 30, nProjBelow = 3, DepErrLow = 0.1){
    # define priors
  initDepMode <- 1. # (initial depletion = B/K)
  #set MSY prior Mode (and initial value) to 80% of mean of 5 highest catches
  MSYMode    <- 0.8*mean(sort(pset$Cobs, decreasing=T)[1:5]) 
  #set k prior mode relative to MSY
  kMode      <- 20*MSYMode
  #logit transform bounds for k ... could not get perfect convergence except by tightening up k Bounds
  kBounds <- c(MSYMode*5, MSYMode*50)
  sigmaPMode <- 0.15 #sigma productivity
  sigmaIMode <- 0.15 #sigma Index
  shapeMode  <- -0.16 #shape #-0.16 # do not expect this to be estimable
  # MSY parameterization
  #C_hist <- pset$Cobs
  #I_hist <- pset$Iobs
  #CMCsum <- pset$CMCsum  # "recent" annual catch in mass
  #lastTAC <- pset$prevTACE$TAC
  #MSYballpark <- mean(C_hist[40:50]) # assumes recent catch near MSY
  #Kballpark   <- 20.*MSYballpark # assumes recent harvest rate ~5%
  #rballpark   <- MSYballpark/(Kballpark/((p+1)^(1/p))) # solves for r given K and MSY estimates
  
  Data = list("I_t"=pset$Iobs, "c_t"=pset$Cobs,  
              "priorMode" =c(initDepMode,MSYMode, kMode, shapeMode, sigmaIMode, sigmaPMode), 
              #tested ok  "priorLogCV"=c(0.05,       .25,    5.,      0.05,      0.1,        5.) )
              "priorLogCV"=c(0.05,       .25,    5.,      0.05,      0.1,        0.1), 
              "log_kBounds"=log(kBounds)) 
  
  Binit <- rep(kMode,pset$y)
  #browser()  
  
  newTACPrior <- 1000. # start small
  #Params = list("log_r"=log(rMode), "log_k"=log(kMode), "shape"=shapeMode, "log_q"=log(1), "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  #Params = list("log_r"=log(rMode), "log_k"=log(kMode), "shape"=shapeMode, "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  Params = list("log_MSY"=log(MSYMode), "log_k"=log(kMode), "shape"=shapeMode, "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit),
                "log_newTAC" = log(newTACPrior)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  
  Random = c("log_B_t")
  
  Map = list() # fix parameters at correct simulation values
  #Map[["log_MSY"]] = factor(NA)   # fix this parameter value
  #Map[["log_r"]] = factor(NA)   # fix this parameter value
  #Map[["log_k"]] = factor(NA)   # fix this parameter value
  #Map[["shape"]] = factor(NA)
  Map[["log_sigmaI"]] = factor(NA)
  
  #Map[["log_sigmaP"]] = factor(NA)
  tmbList <- list(Data, Params, Random, Map) 
  names(tmbList) <- c('Data', 'Params', 'Random', 'Map')
  return(PTBoB0Targ(pset, BLower=BLower,BUpper=BUpper,BoB0Targ=pset$tune * BoB0Targ, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=1,
        gridSearch = 0, tmbList = tmbList, nProjAbove = nProjAbove, nProjBelow = nProjBelow, DepErrLow = DepErrLow))
}

#------------------------------------------------------------------------------




#------------------------------------------------------------------------------
#Pella-Tomlinson 40:10-type MPs (details implemented below)
# highest level function sets options including priors and TAC change constraints
# mid-level function does TMB interface, minimization (and grid-search if req'd)
# low level function is written in TMB / C++
#------------------------------------------------------------------------------
# F-based hockey-stick 15% change constraint

PT41F.t15.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t15.tmb)<-"IO_MP_tune"
attr(PT41F.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change


#------------------------------------------------------------------------------
# F-based continuous VB-style hockey-stick 15% change constraint
# BLower same interpretation but BUpper is the analogue of the VB k parameter, with values ~5-20 probably sensible
PTvb1.5.t15.tmb<-function(pset, BLower=0.1,BUpper=5,CMaxProp=1., useF=-1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PTvb1.5.t15.tmb)<-"IO_MP_tune"
attr(PTvb1.5.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------
# F-based continuous VB-style hockey-stick 15% change constraint
# BLower same interpretation but BUpper is the analogue of the VB k parameter, with values ~5-20 probably sensible
PTvb1.20.t15.tmb<-function(pset, BLower=0.1,BUpper=20,CMaxProp=1., useF=-1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PTvb1.20.t15.tmb)<-"IO_MP_tune"
attr(PTvb1.20.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change


#------------------------------------------------------------------------------
# F-based continuous VB-style hockey-stick 15% change constraint
# BLower same interpretation but BUpper is the analogue of the VB k parameter, with values ~5-20 probably sensible
PTvb05.20.t15.tmb<-function(pset, BLower=0.05,BUpper=20,CMaxProp=1., useF=-1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PTvb05.20.t15.tmb)<-"IO_MP_tune"
attr(PTvb05.20.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------
# F-based continuous VB-style hockey-stick 15% change constraint
# BLower same interpretation but BUpper is the analogue of the VB k parameter, with values ~5-20 probably sensible
PTvb0.10.t15.tmb<-function(pset, BLower=0.0,BUpper=10,CMaxProp=1., useF=-1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PTvb0.10.t15.tmb)<-"IO_MP_tune"
attr(PTvb0.10.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change


#------------------------------------------------------------------------------
# F-based hockey-stick 10% change constraint
PT41F.t10.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.1, deltaTACLimDown=0.1)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t10.tmb)<-"IO_MP_tune"
attr(PT41F.t10.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

# F-based hockey-stick 25% change constraint
PT41F.t25.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.25, deltaTACLimDown=0.25)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t25.tmb)<-"IO_MP_tune"
attr(PT41F.t25.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

# F-based hockey-stick 35% change constraint
PT41F.t35.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.35, deltaTACLimDown=0.35)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t35.tmb)<-"IO_MP_tune"
attr(PT41F.t35.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

# F-based hockey-stick 50% change constraint
PT41F.t50.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.5, deltaTACLimDown=0.5)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t50.tmb)<-"IO_MP_tune"
attr(PT41F.t50.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

# F-based hockey-stick 75% change constraint
PT41F.t75.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.75, deltaTACLimDown=0.75)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown))
}

class(PT41F.t75.tmb)<-"IO_MP_tune"
attr(PT41F.t75.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

PT41F.base.tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=1, deltaTACLimUp=0.25, deltaTACLimDown=0.25, MShape=TRUE){
  # define priors
  initDepMode <- 1. # (initial depletion = B/K)
  #set MSY prior Mode (and initial value) to 80% of mean of 5 highest catches
  MSYMode    <- 0.8*mean(sort(pset$Cobs, decreasing=T)[1:5]) 
  #set k prior mode relative to MSY
  kMode      <- 20*MSYMode
  #logit transform bounds for k
  kBounds <- c(MSYMode*5, MSYMode*40)
    
  sigmaPMode <- 0.15 #sigma productivity
  sigmaIMode <- 0.15 #sigma Index
  shapeMode  <- -0.16 #shape #-0.16 # do not expect this to be estimable
   # MSY parameterization

  #C_hist <- pset$Cobs
  #I_hist <- pset$Iobs
  #CMCsum <- pset$CMCsum  # "recent" annual catch in mass
  #lastTAC <- pset$prevTACE$TAC
  #MSYballpark <- mean(C_hist[40:50]) # assumes recent catch near MSY
  #Kballpark   <- 20.*MSYballpark # assumes recent harvest rate ~5%
  #rballpark   <- MSYballpark/(Kballpark/((p+1)^(1/p))) # solves for r given K and MSY estimates

  Data = list("I_t"=pset$Iobs, "c_t"=pset$Cobs,  
              "priorMode" =c(initDepMode,MSYMode, kMode, shapeMode, sigmaIMode, sigmaPMode), 
  #tested ok  "priorLogCV"=c(0.05,       .25,    5.,      0.05,      0.1,        5.) )
              "priorLogCV"=c(0.05,       .25,    5.,      0.05,      0.1,        0.2),
              "log_kBounds"=log(kBounds))

  Binit <- rep(kMode,pset$y)
#browser()  
  
  
  #Params = list("log_r"=log(rMode), "log_k"=log(kMode), "shape"=shapeMode, "log_q"=log(1), "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  #Params = list("log_r"=log(rMode), "log_k"=log(kMode), "shape"=shapeMode, "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  Params = list("log_MSY"=log(MSYMode), "log_k"=log(kMode), "shape"=shapeMode, "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  
  Random = c("log_B_t")
  
  Map = list() # fix parameters at correct simulation values
  #Map[["log_MSY"]] = factor(NA)   # fix this parameter value
  #Map[["log_r"]] = factor(NA)   # fix this parameter value
  #Map[["log_k"]] = factor(NA)   # fix this parameter value
  if (MShape)
  {
    Map[["shape"]] = factor(NA)
  }

  Map[["log_sigmaI"]] = factor(NA)
  #Map[["log_sigmaP"]] = factor(NA)
  tmbList <- list(Data, Params, Random, Map) 
  names(tmbList) <- c('Data', 'Params', 'Random', 'Map')
    return(PT4010tmb(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=1,
                   gridSearch = 0, tmbList = tmbList))
}

#------------------------------------------------------------------------------

# F-based hockey-stick 15% change constraint - model-specific hockey stick shape
PT41FM.t15.tmb<-function(pset, BLower=0.001,BUpper=0.004,CMaxProp=1., useF=1, deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown,
                         MShape=FALSE))
}

class(PT41FM.t15.tmb)<-"IO_MP_tune"
attr(PT41FM.t15.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

# F-based hockey-stick 15% change constraint - model-specific hockey stick shape
PT41FM.t10.tmb<-function(pset, BLower=0.001,BUpper=0.004,CMaxProp=1., useF=1, deltaTACLimUp=0.1, deltaTACLimDown=0.1)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown,
                         MShape=FALSE))
}

class(PT41FM.t10.tmb)<-"IO_MP_tune"
attr(PT41FM.t10.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change


#------------------------------------------------------------------------------

# F-based hockey-stick 25% change constraint - model-specific hockey stick shape
PT41FM.t25.tmb<-function(pset, BLower=0.001,BUpper=0.004,CMaxProp=1., useF=1, deltaTACLimUp=0.25, deltaTACLimDown=0.25)
{
  return (PT41F.base.tmb(pset,
                         BLower,
                         BUpper,
                         CMaxProp,
                         useF,
                         deltaTACLimUp,
                         deltaTACLimDown,
                         MShape=FALSE))
}

class(PT41FM.t25.tmb)<-"IO_MP_tune"
attr(PT41FM.t25.tmb, "BSysProject") <- ProjectPT41F # xxx part of PJ MP format change

#------------------------------------------------------------------------------

shouldLogPerformance <- function(pset)
{
  return ((!is.null(pset$MP_environment)                  & 
           exists("TAC",       envir=pset$MP_environment) &
           exists("B",         envir=pset$MP_environment) &
           exists("Depletion", envir=pset$MP_environment) &
           exists("q",         envir=pset$MP_environment)))
}
   
#------------------------------------------------------------------------------
# Function for logging MP performance data
#------------------------------------------------------------------------------
logPerformance <- function(pset, Report, TAC, plots=NA)
{
  if (!is.null(pset$MP_environment)                  & 
      exists("TAC",       envir=pset$MP_environment) &
      exists("B",         envir=pset$MP_environment) &
      exists("Depletion", envir=pset$MP_environment) &
      exists("q",         envir=pset$MP_environment))
  {
    pset$MP_environment$TAC        <- c(pset$MP_environment$TAC, as.double(TAC))
    pset$MP_environment$B          <- c(pset$MP_environment$B, as.double(Report$B_t[length(Report$B_t)]))
    pset$MP_environment$Depletion  <- c(pset$MP_environment$Depletion, as.double(Report$Depletion_t[length(Report$Depletion_t)]))
    pset$MP_environment$q          <- c(pset$MP_environment$q, as.double(Report$q[length(Report$q)]))
    pset$MP_environment$plots      <- plots
  }
}


#------------------------------------------------------------------------------
# Pella Tomlinson Production model with generic 40-10 type rule - MPs are defined with tuning parameters above
# useF option uses the 40:10 rule for F rather than C, in which case FMax = FMSY*CMaxProp
# positive gridSearch value is preferred at this time
# tmb indicates joint process and observation error model implemented with TMB
#------------------------------------------------------------------------------

PT4010tmb<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, shockAbsorber=1, useF=1, gridSearch=0, tmbList =list){

  diagnose <- 0.01 #0.05 # 0 = skip, >0 is T and rnd pproportion to plot
  
  # bypass PT fitting if CPUE depletion indicates collapse
  I_hist <- pset$Iobs
  Iy <- I_hist[!is.na(I_hist)]
  Idepletion <- mean(Iy[(length(Iy)-1):length(Iy)])/mean(Iy[1:5])
  print("CPUE % depletion: " %&% floor(Idepletion*100))
  if (Idepletion < 0.1){
    print("skip PT fitting - stock clearly needs rebuilding")
    newTAC <- 1.    #i.e. shutdown fishery as fast as possible
    #browser()
  } else {
    
  
  # translate MSE outputs to TMB  

  # define priors
  #initDepMode <- 1. # (initial depletion = B/K)
  #rMode <- 0.50 #r   # prior mode
  #kMode <- 1000 #k  # prior mode
  #sigmaPMode <- 0.15 #sigma productivity
  #sigmaIMode <- 0.15 #sigma Index
  #shapeMode <- -0.16 #shape #-0.16 # do not expect this to be estimable
  #Data = list("y_t"=Bobs_t, "c_t"=catch_t, "ysd_t"=rep(sigmaI,n_years), 
  #            "priorMode" =c(initDepMode,rMode, kMode, shapeMode, sigmaIMode, sigmaPMode), 
  #            #"priorLogCV"=c(0.01,       0.05,   0.05,   0.05,      0.05,        0.05) )
  #            "priorLogCV"=c(0.05,       0.5,    0.5,   0.05,      0.1,        0.1) )
  #"priorLogCV"=c(0.1,        10.,    10.,   0.1,      0.2,        0.2) )
  #"priorLogCV"=c(1000.,        1000.,    1000.,   1000.,      1000.,        1000.) )
  #Params = list("log_r"=log(r), "log_k"=log(k), "shape"=shape, "log_q"=log(1), "log_sigmaP"=log(sigmaP), "log_sigmaI"=log(sigmaI),"log_x_t"=log(Bobs_t)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  
  # The Random declaration appears irrelevant to result, maybe affects reporting?
  #Random = c("log_x_t", "logit_exploit_t") # this version does not use logit trick
  #Random = c("log_x_t")
  
  #Map = list() # fix these parameters if req'd (probably in this order) 
  #Map[["shape"]] = factor(NA)
  #Map[["log_sigmaI"]] = factor(NA)
  #Map[["log_sigmap"]] = factor(NA)

  # Compile
  #dyn.unload( dynlib("source/PTtmbMSYR") )
  #compile( "source/PTtmbMSYR.cpp" )
  #dyn.load( dynlib("source/PTtmbMSYR") )
  #dlls <- getLoadedDLLs()
  #if(!("PTtmbMSYR" %in% dlls$PTtmbMSYR[[1]])) dyn.load( dynlib("source/PTtmbMSYR") )
  
  obj = MakeADFun( data=tmbList$Data, parameters=tmbList$Params, random=tmbList$Random, map=tmbList$Map, 
                  DLL="PTtmbMSYR") #, silent=T)
  #suppress output
  obj$env$tracemgc <- FALSE
  obj$env$inner.control$trace <- FALSE
  obj$env$silent <- TRUE  
  
  
#  browser()
  
  #dyn.unload( dynlib("source/PTtmbMSY") )
  #dlls <- getLoadedDLLs()
  #getDLLRegisteredRoutines(dlls[["PTtmb"]])
  
  # Optimize - no bounds
  Opt = nlminb(start=obj$par, objective=obj$fn, gradient=obj$gr, control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4))
  # Optimize - kbounds
  #Opt = nlminb(start=obj$par, objective=obj$fn, gradient=obj$gr, 
  #             control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4),
  #             upper = c(Inf, MSYMode*30, Inf, Inf, Inf, Inf))
  #Params = list("log_MSY"=log(MSYMode), "log_k"=log(kMode), "shape"=shapeMode, "log_sigmaP"=log(sigmaPMode), "log_sigmaI"=log(sigmaIMode),"log_B_t"=log(Binit)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
  
    
  Opt[["diagnostics"]] = data.frame( "Est"=Opt$par, "final_gradient"=obj$gr(Opt$par) )
  Report = obj$report()
  SD = try(sdreport( obj ))

  if(!all(is.finite(Report$nll_comp))) browser()   
  if(!all(is.finite(Opt$objective))) browser()   

  if(sum(Report$B_t<0)>0){  #negative biomass
    plot(Report$B_t)
    print("")
    print("Negative biomass PT fitting failure")
    #browser()
  }
  
  # visualize and save results
  if( all(abs(Opt$diagnostics$final_gradient)<0.01) ){
    if( !inherits(SD,"try-error")) {
      #if(runif(1)<diagnose) Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY = pset$MSY)
      #if(iRep<5) readline("pause for graph check")
      #print("")
      print("convergence okay")
      #browser()
      
    } else {
      print("converged, but some other error likely...")
      Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
      if(diagnose) browser()
    }
    #print(SD)
    #print("")
  } else {
    print("")
    print("gradient convergence failure")
    Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
    #if(diagnose) browser()
  }  
    msy <- exp(Report$log_MSY)
    k   <- Report$k
    Y   <- length(Report$B_t)
    SD1 <- summary(SD)
    
    # Current biomass point estimate
    #BY  <- Report$B_t[Y]  
    # Current biomass probably simplistic lower confidence bound
    BCI <- 0.4   # 0.25 = lower 25th assuming normal
    BY  <- SD1[rownames(SD1)=="B_t",][Y,1] + qnorm(BCI)* SD1[rownames(SD1)=="B_t",][Y,2]  
    #browser()
    
    #Apply the 40:10 rule to F ...
    if(useF){
      FMSY = -log(1-msy/k)
      FMult = CMaxProp # maximum F relative to FMSY
      if(BY / k <= BLower) TACF <- 0.0001    #i.e. (almost) shutdown fishery
      if(BY / k  >  BLower & BY / k  <= BUpper) TACF <- FMult*FMSY*((BY / k )/(BUpper-BLower) + ( 1 - (BUpper/(BUpper-BLower))))^shockAbsorber
      if(BY / k  >  BUpper) TACF <- FMult*FMSY
      newTAC <-  BY*(1-exp(-TACF))
    }

  #print(Report$nll_comp)
  names(newTAC) <- "TAC"
  } # fit the PT model
  
      
  lastTAC         <- pset$prevTACE$TAC
  # apply TAC change constraints  
  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)
  if(newTAC<9) newTAC <- 9 #shut the fishery down, except collect some data
  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  if (min(TAEbyF) < 0)
  {
    print("MP TAEbyF < 0")
    browser()
  }

  if (min(newTAC) < 0)
  {
    print("MP TAC < 0")
    browser()
  }
  #test 
  #newTAC <- pset$prevTACE$TAC
  #newTAC <- 100000*CMaxProp

  #dyn.unload( dynlib("source/PTtmbMSY") )
#if(CMaxProp > 0.5 & CMaxProp < 5)  browser()
#  browser()
  if (shouldLogPerformance(pset))
  {
    plots <- reportPlots(report=Report, sdsummary=summary(SD), tmbList = tmbList)

    logPerformance(pset, Report, newTAC, plots)
  }

  rm(SD, Report, obj)

  return (list(TAEbyF=pset$prevTACE$TAEbyF,TAC=newTAC))
}

#------------------------------------------------------------------------------

#uses model-based hockeystick HCR
PTB<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, shockAbsorber=1, useF=F, gridSearch=0, tmbList =list){
  
  diagnose <- 1. # 0 = skip, >0 is T and rnd pproportion to plot
  
  # bypass PT fitting if CPUE depletion indicates collapse
  I_hist <- pset$Iobs
  Iy <- I_hist[!is.na(I_hist)]
  Idepletion <- mean(Iy[(length(Iy)-1):length(Iy)])/mean(Iy[1:5])
  print("CPUE % depletion: " %&% floor(Idepletion*100))
  if (Idepletion < 0.1){
    print("skip PT fitting - stock clearly needs rebuilding")
    newTAC <- 1.    #i.e. shutdown fishery as fast as possible
    #browser()
  } else {
    
    
    # translate MSE outputs to TMB  
    
    # define priors
    #initDepMode <- 1. # (initial depletion = B/K)
    #rMode <- 0.50 #r   # prior mode
    #kMode <- 1000 #k  # prior mode
    #sigmaPMode <- 0.15 #sigma productivity
    #sigmaIMode <- 0.15 #sigma Index
    #shapeMode <- -0.16 #shape #-0.16 # do not expect this to be estimable
    #Data = list("y_t"=Bobs_t, "c_t"=catch_t, "ysd_t"=rep(sigmaI,n_years), 
    #            "priorMode" =c(initDepMode,rMode, kMode, shapeMode, sigmaIMode, sigmaPMode), 
    #            #"priorLogCV"=c(0.01,       0.05,   0.05,   0.05,      0.05,        0.05) )
    #            "priorLogCV"=c(0.05,       0.5,    0.5,   0.05,      0.1,        0.1) )
    #"priorLogCV"=c(0.1,        10.,    10.,   0.1,      0.2,        0.2) )
    #"priorLogCV"=c(1000.,        1000.,    1000.,   1000.,      1000.,        1000.) )
    #Params = list("log_r"=log(r), "log_k"=log(k), "shape"=shape, "log_q"=log(1), "log_sigmaP"=log(sigmaP), "log_sigmaI"=log(sigmaI),"log_x_t"=log(Bobs_t)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
    
    # The Random declaration appears irrelevant to result, maybe affects reporting?
    #Random = c("log_x_t", "logit_exploit_t") # this version does not use logit trick
    #Random = c("log_x_t")
    
    #Map = list() # fix these parameters if req'd (probably in this order) 
    #Map[["shape"]] = factor(NA)
    #Map[["log_sigmaI"]] = factor(NA)
    #Map[["log_sigmap"]] = factor(NA)
    
    # Compile
    #dyn.unload( dynlib("source/PTtmbMSY") )
    #compile( "source/PTtmbMSY.cpp" )
    #dyn.load( dynlib("source/PTtmbMSY") )
    #dlls <- getLoadedDLLs()
    #if(!("PTtmbMSY" %in% dlls$PTtmbMSY[[1]])) dyn.load( dynlib("source/PTtmbMSY") )

    obj = MakeADFun( data=tmbList$Data, parameters=tmbList$Params, random=tmbList$Random, map=tmbList$Map, 
                     DLL="PTtmbMSYR") #, silent=T)
    #suppress output
    obj$env$tracemgc <- FALSE
    obj$env$inner.control$trace <- FALSE
    obj$env$silent <- TRUE  
    
    
    #dyn.unload( dynlib("source/PTtmbMSY") )
    #dlls <- getLoadedDLLs()
    #getDLLRegisteredRoutines(dlls[["PTtmb"]])
converged <- 0
convergeAttempt <- 0
bestNLL <- 9E+99
while (converged<1 & convergeAttempt<10){    
  convergeAttempt <- convergeAttempt + 1
#browser()  
    # sytematic grid for log_MSY initial, increasing jitter for other pars
    msyPar <- obj$par[1] * (1+(-1)^convergeAttempt*(convergeAttempt-1)/30)
    kPar   <- obj$par[3] * (1+(-1)^convergeAttempt*(convergeAttempt-1)/30)
    initPar <- obj$par
    #initPar <- obj$par*exp(rnorm(length(obj$par))*0.01*as.numeric(convergeAttempt-1))
    initPar[1] <- msyPar 
    initPar[3] <- kPar 
print(obj$par)
print(initPar)
    Opt = nlminb(start = initPar, 
                 objective=obj$fn, gradient=obj$gr, control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4))

print(Opt)

# Repeat minimization - act on best
# not fully implemented just testing if it likely matters    
#if(bestNLL - Opt$objective > 0.001 & bestNLL < 9E99){
if(convergeAttempt > 8 ){
  print("c(convergeAttempt, bestNLL, Opt$objective)")
  print(c(convergeAttempt, bestNLL, Opt$objective))
  bestNLL <- Opt$objective 
  bestOpt <- Opt
  bestInitPar <- initPar
  browser()
}    
    # Optimize with (k) bounds
    #Opt = nlminb(start=obj$par, objective=obj$fn, gradient=obj$gr, 
    #             control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4),
    #             upper = c(Inf, log(exp(obj$par[1])*30), Inf, Inf, Inf, Inf))
    Opt[["diagnostics"]] = data.frame( "Est"=Opt$par, "final_gradient"=obj$gr(Opt$par) )
    Report = obj$report()
    SD = try(sdreport( obj ))
    
    if(!all(is.finite(Report$nll_comp))) browser()   
    if(!all(is.finite(Opt$objective))) browser()   
    
    if(sum(Report$B_t<0)>0){  #negative biomass
      plot(Report$B_t)
      print("")
      print("Negative biomass PT fitting failure")
      #browser()
    }
    
    # visualize and save results
    if( all(abs(Opt$diagnostics$final_gradient)<0.01) ){
      if( !inherits(SD,"try-error")) {
        
        if(runif(1)<diagnose) Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
        #if(iRep<5) readline("pause for graph check")
        #print("")
        converged <- converged+1
        print("convergence okay")
        #browser()
        
      } else {
        print("converged, but some other error likely...")
        Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
        if(diagnose) browser()
      }
      #print(SD)
      #print("")
    } else {
      print("")
      print("gradient convergence failure")
      Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
      #if(diagnose) browser()
    }
} #repeat convergence loop
if(convergeAttempt>1){
  print(convergeAttempt %&% " attempts at convergence")
  #browser()
}
if(convergeAttempt==10){
  print("10 failed attempts at convergence")
  browser()
}
msy <- exp(Report$log_MSY)
    k   <- Report$k
    Y   <- length(Report$B_t)
    SD1 <- summary(SD)
    
    # Current biomass point estimate
    BY  <- Report$B_t[Y]  
    # Current biomass probably simplistic lower confidence bound
    # might be associated with rare 1 in 500 error ...
    # BCI <- 0.5   # 0.25 = lower 25th assuming normal
    # BY  <- SD1[rownames(SD1)=="B_t",][Y,1] + qnorm(BCI)* SD1[rownames(SD1)=="B_t",][Y,2]  

    BTmp    <- seq(1, k, k/100)
    prodTmp <- ((Report$shape+1)/Report$shape)*Report$r*BTmp*(1-(BTmp/k)^Report$shape) 
    BTmpMSY <- BTmp[prodTmp==max(prodTmp)]
    BUpper  <- BTmpMSY/k
    
    
    #Apply the model-specific double linear Hockey-stick rule to F ...
    if(useF == 1){
      FMSY = -log(1-msy/k)
      FMult = CMaxProp # maximum F relative to FMSY
      if(BY / k <= BLower) TACF <- 0.0001    #i.e. (almost) shutdown fishery
      if(BY / k  >  BLower & BY / k  <= BUpper) TACF <- FMult*FMSY*((BY / k )/(BUpper-BLower) + ( 1 - (BUpper/(BUpper-BLower))))^shockAbsorber
      if(BY / k  >  BUpper) TACF <- FMult*FMSY
      newTAC <-  BY*(1-exp(-TACF))
    }
 
    #Apply the VB curve Hockey-stick rule 
    if(useF == -1){  # sloppy conditional over-rides useF
      FMSY  = -log(1-msy/k)
      FMult = CMaxProp # maximum F relative to FMSY
      rate  = BUpper   #rate of approaching asymptote, i.e. VB k
      if(BY / k <= BLower) TACF <- 0.0001    #i.e. (almost) shutdown fishery
      if(BY / k  >  BLower){ #calculate the continuous VB hockeystick value
        TACF <- FMult*(1-exp(-rate*(BY/k - BLower))) / (1-exp(-rate*(1 - BLower)))
      }
      newTAC <-  BY*(1-exp(-TACF))
    }


    
    #print(Report$nll_comp)
    names(newTAC) <- "TAC"
  } # fit the PT model
  if(is.na(newTAC)){browser()}
  
  lastTAC         <- pset$prevTACE$TAC
  # apply TAC change constraints  
  deltaTAC <- newTAC/lastTAC - 1
  
  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)
  if(newTAC<9) newTAC <- 9 #shut the fishery down, except collect some data
  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery
  
  if (min(TAEbyF) < 0)
  {
    print("MP TAEbyF < 0")
    browser()
  }
  
  if (min(newTAC) < 0)
  {
    print("MP TAC < 0")
    browser()
  }
  #test 
  #newTAC <- pset$prevTACE$TAC
  #newTAC <- 100000*CMaxProp
  
  #dyn.unload( dynlib("source/PTtmbMSY") )
  #if(CMaxProp > 0.5 & CMaxProp < 5)  browser()
  #  browser()
  if (shouldLogPerformance(pset))
  {
    plots <- reportPlots(report=Report, sdsummary=summary(SD), tmbList = tmbList)
    
    logPerformance(pset, Report, newTAC, plots)
  }

  rm(SD, report, obj)

  if(is.na(newTAC)){browser()}
  return (list(TAEbyF=pset$prevTACE$TAEbyF,TAC=newTAC))
}

#------------------------------------------------------------------------------

# fits PT model and then finds the constant TAC
# in a deterministic projection 
# that hits the depletion target tuning parameter
PTBoB0Targ<-function(pset, BLower=0.1,BUpper=0.4,BoB0Targ=0.34, deltaTACLimUp=0.9, deltaTACLimDown=0.9, shockAbsorber=1, useF=F, gridSearch=0, tmbList =list, nProjAbove = 15, nProjBelow = 5, DepErrLow = 0.5){
  
  diagnose <- 0.01 # 0 = skip, >0 is T and rnd proportion to plot
  # bypass PT fitting if CPUE depletion indicates collapse
  I_hist <- pset$Iobs
  Iy <- I_hist[!is.na(I_hist)]
  Idepletion <- mean(Iy[(length(Iy)-1):length(Iy)])/mean(Iy[1:5])
  print("CPUE % depletion: " %&% floor(Idepletion*100))
  if (Idepletion < 0.05){ #0.1 seems effective and appropriate
    print("skip PT fitting - stock clearly needs rebuilding")
    newTAC <- 1.    #i.e. shutdown fishery as fast as possible
    #browser()
  } else {
    # Compile
    #dyn.unload( dynlib("source/PTtmbMSYRBoB0Targ") )
    #compile( "source/PTtmbMSYRBoB0Targ.cpp" )
    #dyn.load( dynlib("source/PTtmbMSYRBoB0Targ") )
    #dlls <- getLoadedDLLs()
    #if(!("PTtmbMSYR" %in% getLoadedDLLs()$PTtmbMSYRBoB0Targ[[1]])) dyn.load( dynlib("source/PTtmbMSYRBoB0Targ") )

    # Do three phase minimization with staggered search over MSY in first phase
    # 1) fix MSY and find k and B_t only
    # 2) find full set of pop parms
    # 3) fix pop parms and find newTAC to hit target
    # ignore projection influence in first minimization
    converged <- 0
    convergeAttempt <- 0
    msyGrid <- 0
    bestNLL <- 9E+99

    initlog_MSYPar <- tmbList$Params$log_MSY  
    
    while (converged<1 & convergeAttempt<12){    
      convergeAttempt <- convergeAttempt + 1
      msyGrid <- msyGrid + 1
      if(msyGrid==4){
        msyGrid <- 1
        #gradually cinch up bound for rare failures
        tmbList$Data$log_kBounds[2] <- log(exp(tmbList$Data$log_kBounds[2]*0.9)) 
      }
            
      tmbList$Data$newTACSwitch <- c(0.,0.) 
      tmbList$Data$BoB0Targ     <- BoB0Targ 
      # placeholders: [1] num proj years [2] initial depletion (uncertainty-dependent)
      tmbList$Data$Depletion_Y       <- 1. # unused placeholder til defined in proj phase 
      tmbList$Data$nProjYears        <- 10 # unused placeholder til defined in proj phase 
      
      
      # phase 1
      tmbList$Map[["log_MSY"]] = factor(NA)
      tmbList$Map[["shape"]] = factor(NA)
      tmbList$Map[["log_sigmaP"]] = factor(NA)
      tmbList$Map[["log_newTAC"]] = factor(NA)

      #tmbList$Params$log_MSY <- initlog_MSYPar  + ((-1)^convergeAttempt*(convergeAttempt-1)/30)
      tmbList$Params$log_MSY <- initlog_MSYPar + (msyGrid-3)/2

      obj1 = MakeADFun( data=tmbList$Data, parameters=tmbList$Params, random=tmbList$Random, map=tmbList$Map, 
                     DLL="PTtmbMSYRBoB0Targ") #, silent=T)

      #suppress output
      obj1$env$tracemgc <- FALSE
      obj1$env$inner.control$trace <- FALSE
      obj1$env$silent <- TRUE  
    
      #phase 2
    #dyn.unload( dynlib("source/PTtmbMSY") )
    #dlls <- getLoadedDLLs()
    #getDLLRegisteredRoutines(dlls[["PTtmb"]])
      # sytematic grid for log_MSY initial, increasing jitter for other pars
      #msyPar <- obj1$par[1] * (1+(-1)^convergeAttempt*(convergeAttempt-1)/30)
      #Par   <- obj$par[3] * (1+(-1)^convergeAttempt*(convergeAttempt-1)/30)
      initPar <- obj1$par
      #initPar <- obj$par*exp(rnorm(length(obj$par))*0.01*as.numeric(convergeAttempt-1))
      #initPar[1] <- msyPar 
      #initPar[3] <- kPar 
      print(initPar)

      Opt1 = nlminb(start = initPar, 
                   objective=obj1$fn, gradient=obj1$gr, control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4))
      Report1 = obj1$report()

      # phase 2
      tmbList$Map[["log_MSY"]] = NULL
      #projMap[["log_k"]] = factor(NA)   
      tmbList$Map[["shape"]] = NULL
      #tmbList$Map[["shape"]] = factor(NA)
      tmbList$Map[["log_sigmaP"]] = NULL
      #tmbList$Map[["log_sigmaP"]] = factor(NA)

      #projMap[["log_sigmaI"]] = factor(NA)
      tmbList$Map[["log_newTAC"]] = factor(NA)

      tmbList$Params[["log_k"]]=log(Report1$k)
      tmbList$Params[["log_B_t"]]=Report1$log_B_t 

      obj2 = MakeADFun( data=tmbList$Data, parameters=tmbList$Params, random=tmbList$Random, map=tmbList$Map, 
                       DLL="PTtmbMSYRBoB0Targ") #, silent=T)
      initPar2 <- obj2$par

      Opt2 = nlminb(start = initPar2, 
                   objective=obj2$fn, gradient=obj2$gr, control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4))

      print(Opt2)
#browser()      
      # Repeat minimization - act on best
      # not fully implemented just testing if it likely matters    
      #if(bestNLL - Opt$objective > 0.001 & bestNLL < 9E99){
      if(convergeAttempt > 11 ){
        print("c(convergeAttempt, bestNLL, Opt$objective)")
        print(c(convergeAttempt, bestNLL, Opt2$objective))
        bestNLL <- Opt2$objective 
        bestOpt <- Opt2
        bestInitPar <- initPar
        Plot_Fn( report=Report2, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
        print(initPar)
        #browser()
      }    
      # Optimize with (k) bounds
      #Opt = nlminb(start=obj$par, objective=obj$fn, gradient=obj$gr, 
      #             control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4),
      #             upper = c(Inf, log(exp(obj$par[1])*30), Inf, Inf, Inf, Inf))
      Opt2[["diagnostics"]] = data.frame( "Est"=Opt2$par, "final_gradient"=obj2$gr(Opt2$par) )
      Report2 = obj2$report()
      SD = try(sdreport( obj2 ))
      
      if(!all(is.finite(Report2$nll_comp))) browser()   
      if(!all(is.finite(Opt2$objective))) browser()   
      
      if(sum(Report2$B_t<0)>0){  #negative biomass
        plot(Report2$B_t)
        print("")
        print("Negative biomass PT fitting failure")
        #browser()
      }
      
      # visualize and save results
      if( all(abs(Opt2$diagnostics$final_gradient)<0.01) ){
        if( !inherits(SD,"try-error")) {
           #if(pset$y == 67) Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
          #if(runif(1)<diagnose) Plot_Fn( report=Report, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
          #if(iRep<5) readline("pause for graph check")
          #print("")
          converged <- converged+1
          print("convergence okay")
          #browser()
          
        } else {
          print("converged, but some other error likely...")
          Plot_Fn( report=Report2, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
          if(diagnose) browser()
        }
        #print(SD)
        #print("")
      } else {
        print("")
        print("gradient convergence failure")
        Plot_Fn( report=Report2, sdsummary=summary(SD), tmbList = tmbList, OMMSY=pset$MSY)
        #if(diagnose) browser()
      }
    } #repeat convergence loop
    if(convergeAttempt>1){
      print(convergeAttempt %&% " attempts at convergence")
      #browser()
    }
    if(convergeAttempt==12){
      print(convergeAttempt %&% " failed attempts at convergence")
      browser()
    }
    msy <- exp(Report2$log_MSY)
    k   <- Report2$k
    Y   <- length(Report2$B_t)

    
    BTmp    <- seq(1, k, k/100)
    prodTmp <- ((Report2$shape+1)/Report2$shape)*Report2$r*BTmp*(1-(BTmp/k)^Report2$shape) 
    BTmpMSY <- BTmp[prodTmp==max(prodTmp)]
    BUpper  <- BTmpMSY/k

    ##################################################################################
    # phase 3 - fix parms except  the TAC that hits the projection target    
    tmbList$Data$newTACSwitch <- c(1.,1.)
    projMap <- list()
    projMap[["log_MSY"]] = factor(NA)   # fix this parameter value
    projMap[["log_k"]] = factor(NA)   
    projMap[["shape"]] = factor(NA)
    projMap[["log_sigmaP"]] = factor(NA)
    projMap[["log_sigmaI"]] = factor(NA)
    projMap[["log_B_t"]] = factor(rep(NA, length(Report2$log_B_t)))  
    
    projParams <-list("log_MSY"=Report2$log_MSY, "log_k"=log(Report2$k), 
                  "shape"=Report2$shape, "log_sigmaP"=Report2$log_sigmaP, 
                  "log_sigmaI"=Report2$log_sigmaI, "log_B_t"=Report2$log_B_t,
                  "log_newTAC" = log(1000.)) #,  "log_sigmac"=log(0.01), "logit_exploit_t"=rep(0,n_years-1) )
    
    #obj = MakeADFun( data=tmbList$Data, parameters=tmbList$Params, random=tmbList$Random, map=tmbList$Map, 
    #                 DLL="PTtmbMSYRBoB0Targ") #, silent=T)

    
    # Current biomass point estimate... use depletion instead - far more precise
    Depletion_YBest  <- Report2$Depletion_t[Y]  
    
    #tmbList$Data$nProjAbove <- 10
    #tmbList$Data$nProjBelow <- 10
    #tmbList$Data$DepErr     <- 0.5

    #rebuilding period - coud be conditonal on current status relative to target    
    # probably trying to be too clever
    # logistic function for defining rebuilding period  
    minTime     <- nProjBelow
    maxTime     <- nProjAbove
    curveMid    <- 1   #depletion / Depletion Target
   curveSpread  <- 10 #10
#Depletion_YBest <- c(1:20)/10
    nProjYears  <- round(minTime + (maxTime-minTime)/(1+exp(-curveSpread*(Depletion_YBest/BoB0Targ-curveMid))))
    tmbList$Data$nProjYears <- nProjYears

#logistic function for defining projection initial depletion %ile from S.E.  
    minPtile    <- DepErrLow # 0.1
    maxPtile    <- 0.1
    curveMid    <- 1   #depletion / Depletion Target
    curveSpread <- 10
    #Depletion_Y <- c(1:20)/10
    Ptile    <- minPtile + (maxPtile-minPtile)/(1+exp(-curveSpread*(Depletion_YBest/BoB0Targ-curveMid)))

    
    # Current depletion constant (probably simplistic) lower confidence bound
    #BCI <- 0.25   # 0.25 = lower 25th assuming normal
    SDsum <- summary(SD)
    #Depletion_Y  <- SDsum[rownames(SDsum)=="Depletion_t",][Y,1] + qnorm(BCI)* SDsum[rownames(SDsum)=="Depletion_t",][Y,2]  
    
    # Current depletion using sliding uncertainty bound f(depletion point estimate)
    Depletion_Y  <- SDsum[rownames(SDsum)=="Depletion_t",][Y,1] + qnorm(Ptile)* SDsum[rownames(SDsum)=="Depletion_t",][Y,2]  

    tmbList$Data$Depletion_Y <- Depletion_Y
    
    objProj = MakeADFun( data=tmbList$Data, parameters=projParams, map=projMap, 
    #objProj = MakeADFun( data=tmbList$Data, parameters=projParams, random=tmbList$Random, map=projMap, 
              DLL="PTtmbMSYRBoB0Targ") #, silent=T)
    initProjPar <- list("log_newTAC" = log(1000.))
    #initProjPar <- initPar[1:2]
    #initProjPar$"log_newTAC" = log(1000.)
    OptProj = nlminb(start = initProjPar, # 
                 objective=objProj$fn, gradient=objProj$gr, control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4))
    
    ReportProj = objProj$report()
    print(ReportProj$newTAC)
    SDProj = try(sdreport( objProj ))
    if(runif(1)<diagnose) Plot_FnProj(report=Report2, reportProj=ReportProj, sdsummary=summary(SD), sdsummaryProj = summary(SDProj), tmbList = tmbList, OMMSY=pset$MSY)
    
    newTAC <- ReportProj$newTAC
    print(c("newTAC  ",newTAC))
    if(is.na(newTAC)){browser()}

    print("PT projection tuning error  ")
    print(sqrt(ReportProj$nll_comp[6]/1000))
    #browser()
    
    #projection tuning not always possible - 
    # i.e. if cannot rebuild fast enough 
    # no problem as long as TAC cut by the max
    if(sqrt(ReportProj$nll_comp[6]/10000) > 0.02 & ReportProj$newTAC > 1000){ 
      print(sqrt(ReportProj$nll_comp[6]/10000))
      print("PT projection tuning error check")
      Plot_FnProj(report=Report2, reportProj=ReportProj, sdsummary=summary(SD), sdsummaryProj = summary(SDProj), tmbList = tmbList, OMMSY=pset$MSY)
      #browser()
    }
  } # fit the PT model
  
  
  # this MP does not require any further calculations except TAC constraints
  # newTAC calculation happens within TMB - 
  # note potential numerical issue if rebuilding not possible even with shutdown 
  # e.g. will projection influence K etc?
  
  lastTAC         <- pset$prevTACE$TAC
  # apply TAC change constraints  
  deltaTAC <- newTAC/lastTAC - 1
  
  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)
  if(newTAC<9) newTAC <- 9 #shut the fishery down, except collect some data
  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery
  
  if (min(TAEbyF) < 0)
  {
    print("MP TAEbyF < 0")
    browser()
  }
  
  if (min(newTAC) < 0)
  {
    print("MP TAC < 0")
    browser()
  }
  #test 
  #newTAC <- pset$prevTACE$TAC
  #newTAC <- 100000*CMaxProp
  
  #dyn.unload( dynlib("source/PTtmbMSY") )
  #if(CMaxProp > 0.5 & CMaxProp < 5)  browser()
  #  browser()
  
  #print(c("newTAC 2",newTAC))
  if (shouldLogPerformance(pset))
  {
    plots <- reportPlots(report=Report2, sdsummary=summary(SD), tmbList = tmbList)
    
    logPerformance(pset, ReportProj, newTAC, plots)
  }

  rm(SD, obj1, obj2, SDProj, ReportProj, objProj)

  if(is.na(newTAC)){browser()}
  return (list(TAEbyF=pset$prevTACE$TAEbyF,TAC=newTAC))
} # PTBoB0Targ

#------------------------------------------------------------------------------

# MP plot func for testing
Plot_Fn <- function( report, sdsummary, tmbList, OMMSY){
  par( mfrow=c(2,2), mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02)
  Y <- length(report$B_t)
  data <- tmbList$Data
  # Biomass
  #matplot( cbind(report$B_t/1000,report$Bpred_t/1000), type="l", 
  #         ylim=c(0,report$k*1.5/1000), ylab="", xlab="year", main="Biomass")
  plot(c(report$B_t/1000), type="l", 
           ylim=c(0,report$k*1.5/1000), ylab="", xlab="year", main="Biomass & CPUE")
  Mat = cbind( report$B_t, sdsummary[which(rownames(sdsummary)=="B_t"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat[,1]+Mat[,2],rev(Mat[,1]-Mat[,2]))/1000, col=rgb(1,0,0,0.2), border=NA)
  points(data$I_t/report$q/1000, col=1, pch=15)

  # Depletion
  #matplot( cbind(report$B_t/report$k,report$Depletion_t), type="l", ylim=c(-.4,1.5), ylab="", xlab="year", main="B/K + Prod devs")
  plot(report$Depletion_t, type="l", ylim=c(-.4,1.5), ylab="", xlab="year", main="B/K & Prod devs")
  Mat = cbind( report$Depletion_t, sdsummary[which(rownames(sdsummary)=="Depletion_t"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat[,1]+Mat[,2],rev(Mat[,1]-Mat[,2])), col=rgb(1,0,0,0.2), border=NA)

  Mat2 = cbind(report$recDev, sdsummary[which(rownames(sdsummary)=="recDev"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat2[,1]+Mat2[,2],rev(Mat2[,1]-Mat2[,2])), col=rgb(0,0,1,0.2), border=NA)
  
  lines(report$recDev)
  abline(h=0., lty=2)
  abline(h=1., lty=2)
  
  # Catch
  #matplot( cbind(catch_t,report$cpred_t[-n_years]), type="l", log="y", ylab="", xlab="year", main="Catch")
  plot(1:Y, data$c_t/1000, type="l", ylab="", xlab="year", main="Catch")
  # Exploitation rate
  #matplot( cbind(exploit_t[-n_years],report$exploit_t), type="l", log="y", ylab="", xlab="year", main="Exploitation fraction")
  #if(filePlot) dev.off()
  
  #production function
  B   <- seq(0.01, report$k, report$k/100)
  RG1 <- ((report$shape+1)/report$shape)*report$r*B*(1-(abs(B/report$k))^report$shape) 
  BMSYoK <- floor(100*B[RG1==max(RG1)]/report$k)
  msy <- floor(exp(report$log_MSY)/1000)
  k   <- floor(report$k/1000)
  msyok  <- floor(1000*msy/k)/10
  OMMSY  <- floor(OMMSY/1000)
  plot(B/1000,RG1/1000, type='l', cex.main=0.7,
       main = "MSY: " %&% msy %&% "    k: " %&% k %&% "  OM-MSY: " %&% OMMSY
           %&% "\nBMSY/K: " %&% BMSYoK %&% "%  MSY/k: " %&% msyok %&% "%")
           

  #check weird q issue...
  #par(mfrow=c(1,1))
  #plot(report$B_t, type='l')
  #points(data$I_t/report$q, col=1)
  #It <- data$I_t[!is.na(data$I_t)]  
  #Bt <- report$B_t[!is.na(data$I_t)]  
  #q2 <- exp(1/sum(It)*sum(log(It/Bt)))
  #points(data$I_t/report$q, col=2, pch=15, cex=0.5)
  #print(report$q)
  #print(q2)
  #q3 <- sum(It)/sum(Bt)
  #points(data$I_t/q3, col=3, cex=0.5)
  
  
  #print("")
  #print(Y)
  #browser()
}

#------------------------------------------------------------------------------

Plot_FnProj = function( report, reportProj, sdsummary, sdsummaryProj, tmbList, OMMSY){

  par( mfrow=c(2,2), mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02)
  Y <- length(report$B_t)
  data <- tmbList$Data
  # Biomass
  #matplot( cbind(report$B_t/1000,report$Bpred_t/1000), type="l", 
  #         ylim=c(0,report$k*1.5/1000), ylab="", xlab="year", main="Biomass")
  plot(c(reportProj$B_t/1000,reportProj$BProj_t/1000), type="l", 
       ylim=c(0,report$k*1.5/1000), ylab="", xlab="year", main="Biomass & CPUE")
  Mat = cbind( report$B_t, sdsummary[which(rownames(sdsummary)=="B_t"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat[,1]+Mat[,2],rev(Mat[,1]-Mat[,2]))/1000, col=rgb(1,0,0,0.2), border=NA)
  points(data$I_t/report$q/1000, col=1, pch=15)
  
  # Depletion
  #matplot( cbind(report$B_t/report$k,report$Depletion_t), type="l", ylim=c(-.4,1.5), ylab="", xlab="year", main="B/K + Prod devs")
  plot(c(report$Depletion_t, reportProj$BProj_t/report$k), type="l", ylim=c(-.4,1.5), ylab="", xlab="year", main="B/K & Prod devs")
  Mat = cbind( report$Depletion_t, sdsummary[which(rownames(sdsummary)=="Depletion_t"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat[,1]+Mat[,2],rev(Mat[,1]-Mat[,2])), col=rgb(1,0,0,0.2), border=NA)
  
  Mat2 = cbind(report$recDev, sdsummary[which(rownames(sdsummary)=="recDev"),"Std. Error"])
  polygon( x=c(1:Y,Y:1), y=c(Mat2[,1]+Mat2[,2],rev(Mat2[,1]-Mat2[,2])), col=rgb(0,0,1,0.2), border=NA)
  
  lines(report$recDev)
  abline(h=0., lty=2)
  abline(h=1., lty=2)
  
  # Catch
  #matplot( cbind(catch_t,report$cpred_t[-n_years]), type="l", log="y", ylab="", xlab="year", main="Catch")
  plot(c(data$c_t/1000, rep(reportProj$newTAC/1000,length(reportProj$BProj_t))), type="l", ylab="", xlab="year", main="Catch", col=2)
  lines(data$c_t/1000)
  # Exploitation rate
  #matplot( cbind(exploit_t[-n_years],report$exploit_t), type="l", log="y", ylab="", xlab="year", main="Exploitation fraction")
  #if(filePlot) dev.off()
  
  #production function
  B   <- seq(0.01, report$k, report$k/100)
  RG1 <- ((report$shape+1)/report$shape)*report$r*B*(1-(abs(B/report$k))^report$shape) 
  BMSYoK <- floor(100*B[RG1==max(RG1)]/report$k)
  msy <- floor(exp(report$log_MSY)/1000)
  k   <- floor(report$k/1000)
  msyok  <- floor(1000*msy/k)/10
  OMMSY  <- floor(OMMSY/1000)
  plot(B/1000,RG1/1000, type='l', cex.main=0.7,
       main = "MSY: " %&% msy %&% "    k: " %&% k %&% "  OM-MSY: " %&% OMMSY
       %&% "\nBMSY/K: " %&% BMSYoK %&% "%  MSY/k: " %&% msyok %&% "%")
  
  
  #check weird q issue...
  #par(mfrow=c(1,1))
  #plot(report$B_t, type='l')
  #points(data$I_t/report$q, col=1)
  #It <- data$I_t[!is.na(data$I_t)]  
  #Bt <- report$B_t[!is.na(data$I_t)]  
  #q2 <- exp(1/sum(It)*sum(log(It/Bt)))
  #points(data$I_t/report$q, col=2, pch=15, cex=0.5)
  #print(report$q)
  #print(q2)
  #q3 <- sum(It)/sum(Bt)
  #points(data$I_t/q3, col=3, cex=0.5)
  
  
  #print("")
  #print(Y)
  #browser()
}

#------------------------------------------------------------------------------

reportPlots <- function(report, sdsummary, tmbList)
{
  Y       <- length(report$B_t)
  data    <- tmbList$Data
  colors  <- c("Catch"="#001D34", "TAC"="#00A9CE")

  # CPUE
  cpue_serr  <- as.double(sdsummary[which(rownames(sdsummary)=="B_t"), "Std. Error"]) * report$q
  cpue       <- as.double(report$B_t * report$q)
  cpue_lower <- cpue - cpue_serr
  cpue_upper <- cpue + cpue_serr
  cpue_data  <- data.frame(t=1:Y, cpue_t=cpue, lower=cpue_lower, upper=cpue_upper, cpue=data$I_t)
  cpue_plot  <- ggplot(data=cpue_data, aes(x=t, y=cpue_t)) + 
                       geom_line(colour=colors[1], size=2, alpha=0.5) +
                       geom_point(color="black", shape=1, size=5, mapping=aes(x=t, y=cpue)) + 
                       geom_ribbon(data=cpue_data, aes(x=t, ymin=lower, ymax=upper), alpha=0.07) + 
                       ggtitle("CPUE") + 
                       theme_bw()

  # Biomass
  biomass_serr  <- as.double(sdsummary[which(rownames(sdsummary)=="B_t"), "Std. Error"]) / 1000
  biomass       <- as.double(report$B_t) / 1000
  biomass_lower <- biomass - biomass_serr
  biomass_upper <- biomass + biomass_serr
  biomass_cpue  <- as.double(data$I_t / report$q) / 1000
  biomass_data  <- data.frame(t=1:Y, B_t=biomass, lower=biomass_lower, upper=biomass_upper, B_cpue=biomass_cpue)
  biomass_plot  <- ggplot(data=biomass_data, aes(x=t, y=B_t)) + 
                     geom_line(colour=colors[1], size=2, alpha=0.5) +
                     geom_point(color="black", shape=1, size=5, mapping=aes(x=t, y=B_cpue)) + 
                     geom_ribbon(data=biomass_data, aes(x=t, ymin=lower, ymax=upper), alpha=0.07) + 
                     ggtitle("Biomass") + 
                     theme_bw()

  # Depletion
  depletion       <- as.double(report$Depletion_t)
  depletion_serr  <- as.double(sdsummary[which(rownames(sdsummary)=="Depletion_t"), "Std. Error"])
  depletion_lower <- depletion - depletion_serr
  depletion_upper <- depletion + depletion_serr
  depletion_data  <- data.frame(t=1:Y, depletion_t=depletion, lower=depletion_lower, upper=depletion_upper)
  depletion_plot  <- ggplot(data=depletion_data, aes(x=t, y=depletion_t)) + 
                       geom_line(colour=colors[1], size=2, alpha=0.5) +
                       geom_ribbon(data=depletion_data, aes(x=t, ymin=lower, ymax=upper), alpha=0.07) + 
                       ggtitle("Depletion") + 
                       theme_bw()

  # Recruitment deviations
  recDev       <- as.double(report$recDev)
  recDev_serr  <- as.double(sdsummary[which(rownames(sdsummary)=="recDev"), "Std. Error"])
  recDev_lower <- recDev - recDev_serr
  recDev_upper <- recDev + recDev_serr
  recDev_data  <- data.frame(t=1:Y, recDev_t=recDev, lower=recDev_lower, upper=recDev_upper)
  recDev_plot  <- ggplot(data=recDev_data, aes(x=t, y=recDev_t)) + 
                    geom_line(colour=colors[1], size=2, alpha=0.5) +
                    geom_ribbon(data=recDev_data, aes(x=t, ymin=lower, ymax=upper), alpha=0.07) + 
                    ggtitle("Recuitment Deviations") + 
                    theme_bw()

  # Production function
  B         <- seq(0.01, as.double(report$k), as.double(report$k) / 100)
  RG1       <- as.double((report$shape + 1)/ report$shape) * report$r * B * (1.0 - (abs(B / report$k)) ^ report$shape)
  BMSYoK    <- as.double(floor(100 * B[RG1==max(RG1)] / report$k))
  msy       <- as.double(floor(exp(report$log_MSY) / 1000))
  k         <- as.double(floor(report$k / 1000))
  msyok     <- as.double(floor(1000 * msy / k) / 10)
  Title     <- paste("MSY:", msy, " k:", k, "BMSY/K: ", BMSYoK, "%  MSY/k:", msyok, "%")
  prod_data <- data.frame(B=B / 1000, RG1=RG1 / 1000)

  prod_plot <- ggplot(data=prod_data, aes(x=B, y=RG1)) + 
                 geom_line(colour=colors[1], size=2, alpha=0.5) +
                 ggtitle(Title) + 
                 theme_bw()

  return (list(cpue_plot=cpue_plot, biomass_plot=biomass_plot, depletion_plot=depletion_plot, recDev_plot=recDev_plot, prod_plot=prod_plot))
}

