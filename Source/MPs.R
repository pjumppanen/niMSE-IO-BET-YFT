
# ===============================================================================================================
# === Management Procedures for Indian Ocean MSE==========================================================================
#
# MP must provide an aggregate TAC and disaggregated TAE by fishery (if TAE>0, that fishery is given 0 TAC)
# seasonal (and fishery in the case of TAC) disaggregations are currently based on the "recent" historical means
# the R-based projection code assumes that TAC catch proportions remain constant among seasons within years
# the Cpp-based projections assume that the TAC Effort proportions remain constant among seasons within years
# ===============================================================================================================


MP_FunctionExports <- c()


#Pella-Tomlinson 40:10-type MPs (details implemented below)
#===============================================================================

#tuning
PT41.tune.9<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp))
}
class(PT41.tune.9)<-"IO_MP_tune"

PT41.tune.15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.tune.15)<-"IO_MP_tune"

PT41.t15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t15)<-"IO_MP_tune"

PT41.t25<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.25, deltaTACLimDown=0.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t25)<-"IO_MP_tune"

PT41.t50<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t50)<-"IO_MP_tune"

PT41.t10<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t10)<-"IO_MP_tune"

PT80.t15<-function(pset, BLower=0.0,BUpper=0.8,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT80.t15)<-"IO_MP_tune"


PT41.tune.05<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.05, deltaTACLimDown=0.05){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.tune.05)<-"IO_MP_tune"


PT61.tune.15 <-function(pset, BLower=0.1,BUpper=0.6,CMaxProp=1.0, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT61.tune.15)<-"IO_MP_tune"

PT40.tune.15 <-function(pset, BLower=0.,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT40.tune.15)<-"IO_MP_tune"

PT30.tune.15 <-function(pset, BLower=0.,BUpper=0.3,CMaxProp=1.0, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT30.tune.15)<-"IO_MP_tune"

#tuned to OM-ref, 216 reps, tuning level3
PT41.15.216.t3<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.4395871){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp))
}
class(PT41.15.216.t3)<-"IO_MP"


# the fishing mortality equivalent of PT41.tune.15
PT41F.tune.15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41F.tune.15)<-"IO_MP_tune"





# non-tuning

PT41.100.9<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT41.100.9)<-"IO_MP"

PT41.100.5<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.100.5)<-"IO_MP"

PT41.100.2<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.2, deltaTACLimDown=0.2){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.100.2)<-"IO_MP"

PT41.100.1<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.100.1)<-"IO_MP"

PT42.125.2<-function(pset, BLower=0.2,BUpper=0.4,CMaxProp=1.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT42.125.2)<-"IO_MP"

PT42.125.5<-function(pset, BLower=0.2,BUpper=0.4,CMaxProp=1.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT42.125.5)<-"IO_MP"

PT61.75.2<-function(pset, BLower=0.1,BUpper=0.6,CMaxProp=0.75){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT61.75.2)<-"IO_MP"

PT61.75.5<-function(pset, BLower=0.1,BUpper=0.6,CMaxProp=0.75){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT61.75.5)<-"IO_MP"

PT62.125.2<-function(pset, BLower=0.2,BUpper=0.6,CMaxProp=1.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT62.125.2)<-"IO_MP"

PT62.125.5<-function(pset, BLower=0.2,BUpper=0.6,CMaxProp=1.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT62.125.5)<-"IO_MP"

PT82.150.2<-function(pset, BLower=0.2,BUpper=0.8,CMaxProp=1.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT82.150.2)<-"IO_MP"

PT82.150.5<-function(pset, BLower=0.2,BUpper=0.8,CMaxProp=1.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT82.150.5)<-"IO_MP"

PT82.100.2<-function(pset, BLower=0.2,BUpper=0.8,CMaxProp=1.0){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT82.100.2)<-"IO_MP"

PT82.100.5<-function(pset, BLower=0.2,BUpper=0.8,CMaxProp=1.0){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=CMaxProp))
}
class(PT82.100.5)<-"IO_MP"

# Aim fot CPUE target MPs
#===============================================================================

#tuning
IT5.tune.15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.tune.15) <- "IO_MP_tune"

IT5.t15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15) <- "IO_MP_tune"

IT5.t15.l1 <- function(pset,yrsmth=5,lambda=0.1,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15.l1) <- "IO_MP_tune"

IT5.t10 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t10) <- "IO_MP_tune"



IT5.t25 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.25, deltaTACLimDown=0.25){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t25) <- "IO_MP_tune"

IT5.t50 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50) <- "IO_MP_tune"



IT10.t15 <- function(pset,yrsmth=10,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT10.t15) <- "IO_MP_tune"


IT5.tune.05 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.05, deltaTACLimDown=0.05){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.tune.05) <- "IO_MP_tune"


#tuned to OM-ref, 216 reps, tuning level3
IT5.15.216.t3 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(3.1349248,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.15.216.t3) <- "IO_MP"





#non-tuning
IT1.00 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(1.0,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT1.00)<-"IO_MP"

IT1.50 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(1.5,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT1.50)<-"IO_MP"

IT1.50.9 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(1.5,0.9,0.9,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT1.50.9)<-"IO_MP"

IT2.00 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(2.0,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT2.00)<-"IO_MP"

IT2.50 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(2.5,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT2.50)<-"IO_MP"

IT2.50.9 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(2.5,0.9,0.9,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT2.50.9)<-"IO_MP"

IT3.00 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(3.0,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT3.00)<-"IO_MP"

IT3.50 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2){
  return(CPUETarget(pset, ITargPars=c(3.5,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT3.50)<-"IO_MP"



MP_FunctionExports <- c(MP_FunctionExports, "PellaTomlinson4010")

# Pella Tomlinson Production model with generic 40-10 type rule - MPs are defined with tuning parameters above
# useF option uses the 40:10 rule for F rather than C, in which case FMax = FMSY*CMaxProp
PellaTomlinson4010<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, useF=F){
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass

  #Initial Model parameters
  rInit <- 0.1
  KInit <- 20.*CMCsum
  p     <- -0.16  # don't bother trying to estimate p; for p = -0.16, BMSY/K ~0.33

  params<-log(c(rInit,KInit))
#print("PT4010 1")
#browser()
  #par(mfrow=c(3,3))
  opt<-optim(par=params,fn=PT.f, returnOpt=1,
             C_hist=C_hist,I_hist=I_hist, CMCsum=CMCsum, p=p,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=F, doPlot=F)

  #get the biomass/BMSY estimate
  d <- PT.f(params=opt$par, returnOpt=2, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=F)
  lastTAC <- pset$prevTACE$TAC

  #Apply something like a 40-10 rule for catch relative to MSY
  if(useF==F){
    if(d["BY"]/d["K"] <= BLower)                        newTAC <- 1.    #i.e. shutdown fishery
    if(d["BY"]/d["K"] >  BLower & d["BY"]/d["K"] <= BUpper) newTAC <- CMaxProp*d["MSY"]*(d["BY"]/d["K"])/(BUpper-BLower) + CMaxProp*d["MSY"]*( 1 - (BUpper/(BUpper-BLower)))
    if(d["BY"]/d["K"] >  BUpper)                         newTAC <- CMaxProp*d["MSY"]
  }
  #Apply the 40:10 rule to F rather than catch...
  if(useF){
    FMSY = -log(1-d["MSY"]/d["K"])
    FMult = CMaxProp # maximum F relative to FMSY
    if(d["BY"]/d["K"] <= BLower)   TACF <- 0.0001    #i.e. shutdown fishery
    if(d["BY"]/d["K"] >  BLower & d["BY"]/d["K"] <= BUpper) TACF <- FMult*FMSY*(d["BY"]/d["K"])/(BUpper-BLower) + FMult*FMSY*( 1 - (BUpper/(BUpper-BLower)))
    if(d["BY"]/d["K"] >  BUpper)   TACF <- FMult*FMSY
    newTAC <-  d["BY"]*(1-exp(-TACF))
  }




  names(newTAC) <- "TAC"

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

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}


MP_FunctionExports <- c(MP_FunctionExports, "PT.f")

#Pella-Tomlinson Model function
PT.f <- function(params, C_hist,I_hist, CMCsum, p, doPlot=F, returnOpt=1){

  #Model parameters
  #B(t+1)=B(t) + (r/p)B(1-(B/K)^p) - C    ; MSY = rK/4 ?
  Y <- length(C_hist)
  r <- exp(params[1])
  K <- exp(params[2])

  B <- array(NA,dim=Y)

  B[1] <- K
  for(y in 2:Y){
    B[y] <- B[y-1] + ((p+1)/p)*r*B[y-1]*(1-(B[y-1]/K)^p) - C_hist[y-1]
    if(B[y]<1e-5) B[y] <- 1e-5
  }
  q <- sum(I_hist[!is.na(I_hist)])/sum(B[!is.na(I_hist)])
  LLH <- sum((q*B[!is.na(I_hist)]-I_hist[!is.na(I_hist)])^2)

  MSY <- r*K/((p+1)^(1/p))

  # quick and dirty plausibility constraints - no claim that this is a good approach for constraining the PT model
  if(MSY < 0.2*CMCsum) LLH <- LLH + (MSY - 0.2*CMCsum)^2
  if(MSY > 5.0*CMCsum)  LLH <- LLH + (MSY - 5.0*CMCsum)^2

#print(c("PT.f",r,K,B[Y],MSY, B[Y]/K))
#browser()

  if(doPlot){
    plot(I_hist/q, main="MSY: " %&% MSY %&% " C(Y)/MSY: " %&% as.character(CMCsum/MSY), ylim=c(0,max(I_hist/q, na.rm=T)))
    lines(B)
    lines(C_hist,col=2)
  }

  if(returnOpt == 1){ # return objective function
    return(LLH)
  } else {         # return MSY-related stuff
    BMSY <- K/((p+1)^(1/p))
    outList <- c(B[Y], K, MSY, BMSY)
    names(outList) <- c("BY","K","MSY","BMSY")
    return(outList)
  }
}
#for(C in c(1:100)/100){
#  print(C)
#  PT.f(params=log(c(.1,100)), C_hist=rep(C,10000), I_hist=rep(1,10000), CMCsum=1,p=-0.16,doPlot=F)
#}
for(p in c(-100:200)/100){
  print(c("p, BMSY/K: ", p,1/((p+1)^(1/p))) )    #p=-0.16   BMSY/K~0.33
}



MP_FunctionExports <- c(MP_FunctionExports, "CPUETarget")

#MP resembling first level of ETBF Harvest Strategy (CPUE slope to target)
#Raise or lower TAC proportional to (recent weighted average) Index difference from a target
MP_FunctionExports <- c(MP_FunctionExports, "CPUETarget")

#------------------------------------------------------------------------------
# MP resembling first level of ETBF Harvest Strategy (CPUE slope to target)
# Raise or lower TAC proportional to (recent weighted average) Index
# difference from a target
#------------------------------------------------------------------------------
CPUETarget <- function(pset, ITargPars=c(2.5,0.2,0.2,0.1,0.1,0.1,0.1), yrsmth=5,lambda=0.4,xx=0.2)
{
  ny     <- length(pset$Cobs) #number years of data
  ind    <- (ny - (yrsmth - 1)):ny
  I_hist <- pset$Iobs[ind] #historical Abundance index
  yind   <- 1:yrsmth
  slppar <- summary(lm(I_hist ~ yind))$coefficients[2,1:2] #slope of recent index
  Islp   <-slppar[1]

  #MP Control Parameters
  ITarg <- ITargPars[1] # arbitrary CPUE Target, (conceptually at least) should correspond to time when stock perceived to be okay

  deltaTACLimUp   <- ITargPars[2]    #max TAC increase
  deltaTACLimDown <- ITargPars[3]    #max TAC decrease

  #Responsivenes (gain) parameters
  negPar1 <- ITargPars[4]             # decrease rate when I < target
  negPar2 <- ITargPars[5]             # decrease rate when I increasing
  posPar1 <- ITargPars[6]              # increase rate when I > target
  posPar2 <- ITargPars[7]              # increase rate when I increasing

  #HCR
  lastTAC  <- pset$prevTACE$TAC
  ICurrent <- (3 / 6) * pset$Iobs[ny] + (2 / 6) * pset$Iobs[ny - 1] + (1 / 6) * pset$Iobs[ny - 2]

  if (ICurrent <  ITarg)
  {
    # probably drop TAC
    deltaTAC <- -(ITarg - ICurrent) * negPar1 + Islp * negPar2

  } else
  {
    #probably increase TAC
    deltaTAC <- +(ICurrent - ITarg) * posPar1 + Islp * posPar2
  }

  if (deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if (deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown

  newTAC <- lastTAC * (1 + deltaTAC)

  if (newTAC < 9) newTAC <- 9 #shut the fishery down, except collect some data

  TAEbyF <- 0.*pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}



# constant catch projections
#===============================================================================
#for tuning
CCt <- function(pset,Cinit=400000)
{
  TAC     <- Cinit * pset$tune #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CCt) <- "IO_MP_tune"

#------------------------------------------------------------------------------

# tuned to OM-ref, 216 reps, tuning level1
CCt.216.t1 <- function(pset)
{
  TAC     <- 400000 * 1.4251754 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CCt.216.t1) <- "IO_MP"

#------------------------------------------------------------------------------

# tuned to OM-ref, 216 reps, tuning level3
CCt.216.t3 <- function(pset)
{
  TAC     <- 400000 * 0.8792413 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CCt.216.t3) <- "IO_MP"

#------------------------------------------------------------------------------


# non-tuning
CC001 <- function(pset)
{
  TAC     <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC001) <- "IO_MP"

#------------------------------------------------------------------------------

CC050 <- function(pset)
{
  TAC     <- 50000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC050) <- "IO_MP"

#------------------------------------------------------------------------------

CC087 <- function(pset)
{
  TAC     <- 87000.0 #aggregate TAC (annual) by fishery (BET reported in 2016)
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC087) <- "IO_MP"

#------------------------------------------------------------------------------

CC100 <- function(pset)
{
  TAC     <- 100000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0*pset$prevTACE$TAEbyF   #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC100) <- "IO_MP"

#------------------------------------------------------------------------------

CC150 <- function(pset)
{
  TAC     <- 150000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC150) <- "IO_MP"

#------------------------------------------------------------------------------

CC200 <- function(pset)
{
  TAC     <- 200000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC200) <- "IO_MP"

#------------------------------------------------------------------------------

CC250 <- function(pset)
{
  TAC     <- 250000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC250) <- "IO_MP"

#------------------------------------------------------------------------------

CC300 <- function(pset)
{
  TAC     <- 300000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC300) <- "IO_MP"

#------------------------------------------------------------------------------

CC350 <- function(pset)
{
  TAC     <- 350000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC350) <- "IO_MP"

#------------------------------------------------------------------------------

CC400 <- function(pset)
{
  TAC     <- 400000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC400) <- "IO_MP"

#------------------------------------------------------------------------------

CC500 <- function(pset)
{
  TAC     <- 500000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC500) <- "IO_MP"

#------------------------------------------------------------------------------

CC413 <- function(pset)
{
  TAC     <- 413000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC413) <- "IO_MP"

#------------------------------------------------------------------------------

CC330 <- function(pset)
{
  TAC     <- 330000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC330) <- "IO_MP"

#------------------------------------------------------------------------------

CC450 <- function(pset)
{
  TAC     <- 450000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC450) <- "IO_MP"


# constant effort projections
#===============================================================================
CE1.0 <- function(pset)
{
  # Current effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 1.0+ 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE1.0) <- "IO_MP"

#------------------------------------------------------------------------------

CE0.01 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.01 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE0.01) <- "IO_MP"

#------------------------------------------------------------------------------

CE0.1 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.1 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE0.1) <- "IO_MP"

#------------------------------------------------------------------------------

CE0.25 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.25 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE0.25) <- "IO_MP"

#------------------------------------------------------------------------------

CE0.5 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.5 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE0.5) <- "IO_MP"

#------------------------------------------------------------------------------

CE0.75 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.75 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE0.75) <- "IO_MP"

#------------------------------------------------------------------------------

CE1.5 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 1.5 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE1.5) <- "IO_MP"

#------------------------------------------------------------------------------

CE2.0 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 2.0 + 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE2.0) <- "IO_MP"

#------------------------------------------------------------------------------

CE5.0 <- function(pset)
{
  # Constant effort projection
  TAC     <- 0.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 5.0+ 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CE5.0) <- "IO_MP"


#------------------------------------------------------------------------------
# mix of constant catch and effort
#------------------------------------------------------------------------------
CC100CE1.0yft <- function(pset)
{
  # mix of constant catch and effort for YFT
  TAC     <- 100000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  TAEbyF[c(1,4,12,14)] <- 1.0  #IO YFT gillnets and others

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC100CE1.0yft) <- "IO_MP"

#------------------------------------------------------------------------------

CC50CE1.0bet <- function(pset)
{
  # mix of constant catch and effort for BET
  TAC     <- 50000.0 #aggregate TAC (annual) by fishery
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  TAEbyF[c(10,11,12)] <- 1.0  #IO BET mixed gears and others

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}

class(CC50CE1.0bet) <- "IO_MP"





