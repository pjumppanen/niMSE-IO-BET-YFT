#------------------------------------------------------------------------------
# Management Procedures for Indian Ocean MSE
#------------------------------------------------------------------------------
# MP must provide an aggregate TAC and disaggregated TAE by fishery
# (if TAE>0, that fishery is given 0 TAC) seasonal (and fishery in the case of TAC)
# disaggregations are currently based on the "recent" historical means the
# R-based projection code assumes that TAC catch proportions remain constant
# among seasons within years the Cpp-based projections assume that the TAC
# Effort proportions remain constant among seasons within years
#------------------------------------------------------------------------------


MP_FunctionExports <- c()


#Pella-Tomlinson 40:10-type MPs (details implemented below)
#------------------------------------------------------------------------------

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

#delayed TAC change implemntation until 2024
PT41.td15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  if(pset$y<73){ #for BET 67 = 2021
    deltaTACLimUp <- 0.01
    deltaTACLimDown <- 0.01
  }
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.td15)<-"IO_MP_tune"

# relaxed change constraint for first 2 MP applications only
PT41.x60t25<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.25, deltaTACLimDown=0.25){
  if(pset$y<73){ # for YFT 69 = 2021
    # PT return (list(TAEbyF=TAEbyF,TAC=newTAC))
    #x[[2]] <- pset$prevTACE$TAC*0.4
    deltaTACLimDown <- 0.6
  }
  #print(c("WTF",pset$y,deltaTACLimUp, deltaTACLimDown))
  x <- PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown)
  return(x)
}
class(PT41.x60t25)<-"IO_MP_tune"







PT41.t25<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.25, deltaTACLimDown=0.25){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t25)<-"IO_MP_tune"

PT41.t50<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t50)<-"IO_MP_tune"

PT41.t90<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.9, deltaTACLimDown=0.9){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t90)<-"IO_MP_tune"

PT41.t10<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT41.t10)<-"IO_MP_tune"

PT80.t15<-function(pset, BLower=0.0,BUpper=0.8,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT80.t15)<-"IO_MP_tune"

PT80.t50<-function(pset, BLower=0.0,BUpper=0.8,CMaxProp=1., deltaTACLimUp=0.50, deltaTACLimDown=0.50){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}
class(PT80.t50)<-"IO_MP_tune"


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
#------------------------------------------------------------------------------

#tuning
IT5.tune.15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.tune.15) <- "IO_MP_tune"

IT5.t15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15) <- "IO_MP_tune"

# delay TAC change for initial stability
IT5.td15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  if(pset$y<73){ #for BET 67 = 2021
      deltaTACLimUp <- 0.01
    deltaTACLimDown <- 0.01
  }
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.td15) <- "IO_MP_tune"


# relaxed TAC change constraint for first 2 applications only
IT5.x60t15 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  if(pset$y<73){ # for YFT 69 = 2021
    deltaTACLimDown <- 0.6
  }
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.x60t15) <- "IO_MP_tune"

# relaxed TAC change constraint for first 2 applications only
IT5.x60t25 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.25, deltaTACLimDown=0.25){
  if(pset$y<73){ # for YFT 69 = 2021
    deltaTACLimDown <- 0.6
  }
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.x60t25) <- "IO_MP_tune"



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

# all gain parameters = .1
IT5.t50g1 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.1,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g1) <- "IO_MP_tune"

# all gain parameters = .2
IT5.t50g2 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.2,0.2,0.2,0.2), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g2) <- "IO_MP_tune"

# all gain parameters = .3
IT5.t50g3 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.3,0.3,0.3,0.3,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g3) <- "IO_MP_tune"

# gain parameters = .3 and .1
IT5.t50g3311 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.3,0.3,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g3311) <- "IO_MP_tune"

# gain parameters = .1 and .3
IT5.t50g1133 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.1,0.1,0.3,0.3,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g1133) <- "IO_MP_tune"

# gain parameters = .1 and .3
IT5.t50g1313 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.1,0.3,0.1,0.3,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g1313) <- "IO_MP_tune"

# gain parameters = .1 and .3
IT5.t50g3131 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.3,0.1,0.3,0.1,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t50g3131) <- "IO_MP_tune"

# gain parameters = .1 and .3
IT5.t15g1313 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.1,0.3,0.1,0.3,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15g1313) <- "IO_MP_tune"

# gain parameters = .1 and .3
IT5.t15g3131 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.3,0.1,0.3,0.1,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15g3131) <- "IO_MP_tune"



IT3.t50 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT3.t50) <- "IO_MP_tune"

IT5.t90 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.9, deltaTACLimDown=0.9){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t90) <- "IO_MP_tune"



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


#------------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PellaTomlinson4010")

# -----------------------------------------------------------------------------
# Pella Tomlinson Production model with generic 40-10 type rule - MPs are
# defined with tuning parameters above useF option uses the 40:10 rule for F
# rather than C, in which case FMax = FMSY*CMaxProp
# -----------------------------------------------------------------------------
PellaTomlinson4010 <- function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, useF=FALSE)
{
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass

  #Initial Model parameters
  rInit  <- 0.1
  KInit  <- 20.0 * CMCsum
  p      <- 1.0

  params <-log(c(rInit, KInit, p))

  ix     <- which(!is.na(I_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  ix     <- which(!is.na(C_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  opt    <- optim(par=params, fn=PT.model, gr=PT.model.gradient, returnOpt=1, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, method="BFGS", hessian=F, doPlot=F)

  # get the biomass/BMSY estimate
  d       <- PT.model(params=opt$par, returnOpt=2, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, doPlot=FALSE)
  lastTAC <- pset$prevTACE$TAC

  if (useF)
  {
    #Apply the 40:10 rule to F rather than catch...
    FMSY = -log(1-d["MSY"]/d["K"])
    FMult = CMaxProp # maximum F relative to FMSY
    if ( d["BY"] / d["K"] <= BLower)                                  TACF <- 0.0001    #i.e. shutdown fishery
    if ((d["BY"] / d["K"] >  BLower) && (d["BY"] / d["K"] <= BUpper)) TACF <- FMult * FMSY * (d["BY"] / d["K"]) / (BUpper - BLower) + FMult * FMSY * (1 - (BUpper / (BUpper - BLower)))
    if ( d["BY"] / d["K"] >  BUpper)                                  TACF <- FMult * FMSY

    newTAC <- d["BY"] * (1.0 - exp(-TACF))
  }
  else
  {
    #Apply something like a 40-10 rule for catch relative to MSY
    if ( d["BY"] / d["K"] <= BLower)                                  newTAC <- 1.0    #i.e. shutdown fishery
    if ((d["BY"] / d["K"] >  BLower) && (d["BY"] / d["K"] <= BUpper)) newTAC <- CMaxProp * d["MSY"] * (d["BY"] / d["K"]) / (BUpper - BLower) + CMaxProp * d["MSY"] * (1 - (BUpper / (BUpper - BLower)))
    if ( d["BY"] / d["K"] >  BUpper)                                  newTAC <- CMaxProp * d["MSY"]
  }

  names(newTAC) <- "TAC"
  deltaTAC      <- newTAC / lastTAC - 1

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

  return (list(TAEbyF=TAEbyF, TAC=newTAC))
}


MP_FunctionExports <- c(MP_FunctionExports, "PT.model")

# -----------------------------------------------------------------------------
# Pella-Tomlinson Model function
# -----------------------------------------------------------------------------
PT.model <- function(params, C_hist, I_hist, CMCsum, returnOpt=1, doPlot=FALSE)
{
  # Model parameters
  # B(t+1)=B(t) + (r/p)B(1-(B/K)^p) - C    ; MSY = rK/((p+1)^(1+(1/p)))
  #
  Y    <- length(C_hist)
  r    <- exp(params[1])
  K    <- exp(params[2])
  p    <- exp(params[3])
  B    <- array(NA,dim=Y)
  B[1] <- K

  for(y in 2:Y)
  {
    # Note that we square B / K to force a positive number so that then
    # raising to the power of p / 2 exists. When B > 0 it is the same as
    # (B / K) ^ p but still exists when B is negative. This is just an
    # aid for fitting purposes.
    B[y] <- B[y-1] + (r * B[y-1] / p) * (1.0 - ((B[y-1] / K) ^ 2) ^ (p/2)) - C_hist[y-1]
  }

  q   <- sum(I_hist) / sum(B)
  LLH <- sum((q * B - I_hist) ^ 2)
  MSY <- r * K / ((p + 1.0) ^ (1.0 + 1.0 / p))

  if (doPlot)
  {
    plot(I_hist / q, main="MSY: " %&% MSY %&% " C(Y)/MSY: " %&% as.character(CMCsum / MSY), ylim=c(0,max(I_hist / q, na.rm=T)))
    lines(B)
    lines(C_hist, col=2)
  }

  if (returnOpt == 1)
  {
    # return objective function
    return(LLH)
  }
  else
  {
    # return MSY-related stuff
    BMSY           <- MSY / r
    outList        <- c(B[Y], K, MSY, BMSY)
    names(outList) <- c("BY","K","MSY","BMSY")

    return(outList)
  }
}


MP_FunctionExports <- c(MP_FunctionExports, "PT.model.gradient")

# -----------------------------------------------------------------------------
# AD adjoint of PT-model obtained by Tapenade AD of code,
#
# MODULE COMMON
#
#  INTEGER, PARAMETER :: dim_stack
#
#  INTEGER Y
#  REAL C_hist(Y), I_hist(Y)
#
# END
#
#
# REAL FUNCTION PT(params)
#
#   USE COMMON
#
#   REAL, INTENT (IN) :: params(3), C_hist(Y), I_hist(Y), Y
#
#   REAL B(Y), q, r, K, p
#   INTEGER yi
#
#   r = EXP(params(1))
#   K = EXP(params(2))
#   p = EXP(params(3))
#
#   B(1) = K
#
#   DO yi=2,Y
#     ! Note that we square B/K to force a positive number so that then
#     ! raising to the power of p/2 exists. When B > 0 it is the same as
#     ! (B/K)**p but still exists when B is negative. This is just an
#     ! aid for fitting purposes.
#     B(yi) = B(yi-1) + (r * B(yi-1) / p) * (1.0 - ((B(yi-1) / K)**2)**(p/2)) - C_hist(yi-1)
#   END DO
#
#   q  = sum(I_hist) / sum(B)
#   PT = sum((q * B - I_hist)**2)
#
#   RETURN
#
# END
#
# to obtain the code,
#
# SUBROUTINE PT_B(params, paramsb, ptb)
#   USE COMMON_B
#   IMPLICIT NONE
#   REAL, INTENT(IN) :: params(3), c_hist(y), i_hist(y), y
#   REAL :: paramsb(3)
#   REAL :: b(y), q, r, k, p
#   REAL :: bb(y), qb, rb, kb, pb
#   INTEGER :: yi
#   INTRINSIC EXP
#   INTRINSIC SUM
#   REAL :: ptb
#   REAL :: pt
#   REAL :: temp3
#   REAL :: temp2
#   REAL :: temp1
#   REAL :: temp0
#   REAL :: tempb2(y)
#   REAL :: tempb1
#   REAL :: tempb0
#   REAL :: tempb
#   REAL :: temp
#   REAL :: temp4
#   r = EXP(params(1))
#   k = EXP(params(2))
#   p = EXP(params(3))
#   b(1) = k
#   DO yi=2,y
# ! Note that we square B/K to force a positive number so that then
# ! raising to the power of p/2 exists. When B > 0 it is the same as
# ! (B/K)**p but still exists when B is negative. This is just an
# ! aid for fitting purposes.
#     CALL PUSHREAL4(b(yi))
#     b(yi) = b(yi-1) + r*b(yi-1)/p*(1.0-((b(yi-1)/k)**2)**(p/2)) - c_hist(yi-1)
#   END DO
#   q = SUM(i_hist)/SUM(b)
#   temp4 = SUM(b)
#   bb = 0.0
#   tempb2 = 2*(q*b-i_hist)*ptb
#   qb = SUM(b*tempb2)
#   bb = q*tempb2 - SUM(i_hist)*qb/temp4**2
#   kb = 0.0
#   pb = 0.0
#   rb = 0.0
#   DO yi=y,2,-1
#     CALL POPREAL4(b(yi))
#     temp0 = r/p
#     tempb = b(yi-1)*temp0*bb(yi)
#     temp = b(yi-1)/k
#     temp3 = temp**2
#     temp2 = p/2
#     IF (temp3 .LE. 0.0 .AND. (temp2 .EQ. 0.0 .OR. temp2 .NE. INT(temp2))) THEN
#       tempb0 = 0.0
#     ELSE
#       tempb0 = -(2*temp*temp2*temp3**(temp2-1)*tempb/k)
#     END IF
#     temp1 = temp3**temp2
#     tempb1 = b(yi-1)*(1.0-temp1)*bb(yi)/p
#     bb(yi-1) = bb(yi-1) + tempb0 + ((1.0-temp1)*temp0+1.0)*bb(yi)
#     kb = kb - temp*tempb0
#     IF (temp3 .LE. 0.0) THEN
#       pb = pb - temp0*tempb1
#     ELSE
#       pb = pb - temp0*tempb1 - temp1*LOG(temp3)*tempb/2
#     END IF
#     rb = rb + tempb1
#     bb(yi) = 0.0
#   END DO
#   kb = kb + bb(1)
#   paramsb(3) = paramsb(3) + EXP(params(3))*pb
#   paramsb(2) = paramsb(2) + EXP(params(2))*kb
#   paramsb(1) = paramsb(1) + EXP(params(1))*rb
# END SUBROUTINE PT_B
#
# and translated back to R code below. paramsb is set to zero and ptb set to one
#
# -----------------------------------------------------------------------------
PT.model.gradient <- function(params, C_hist, I_hist, CMCsum, returnOpt=1, doPlot=FALSE)
{
  Y       <- length(C_hist)
  paramsb <- array(as.double(NA), dim=c(3))
  b       <- array(as.double(NA), dim=c(Y))
  stackr  <- array(as.double(NA), dim=c(Y))
  sr      <- 1
  r       <- exp(params[1])
  k       <- exp(params[2])
  p       <- exp(params[3])
  b[1]    <- k

  for (yi in 2:Y)
  {
    stackr[sr] <- b[yi]
    sr         <- sr + 1
    b[yi]      <- b[yi - 1] + (r * b[yi - 1] / p) * (1.0 - ((b[yi - 1] / k) ** 2) ** (p / 2)) - C_hist[yi - 1]
  }

  q       <- sum(I_hist) / sum(b)
  temp4   <- sum(b)
  bb      <- 0.0
  tempb2  <- 2.0 * (q * b - I_hist)
  qb      <- sum(b * tempb2)
  bb      <- q * tempb2 - sum(I_hist) * qb / temp4 ^ 2
  kb      <- 0.0
  pb      <- 0.0
  rb      <- 0.0

  for (yi in Y:2)
  {
    sr    <- sr - 1
    b[yi] <- stackr[sr]
    temp0 <- r / p
    tempb <- b[yi - 1] * temp0 * bb[yi]
    temp  <- b[yi - 1] / k
    temp3 <- temp ^ 2
    temp2 <- p / 2

    if ((temp3 <= 0.0) && ((temp2 == 0.0) || (temp2 != trunc(temp2))))
    {
      tempb0 <- 0.0
    }
    else
    {
      tempb0 <- -(2 * temp * temp2 * temp3 ^ (temp2 - 1.0) * tempb / k)
    }

    temp1      <- temp3 ^ temp2
    tempb1     <- b[yi - 1] * (1.0 - temp1) * bb[yi] / p
    bb[yi - 1] <- bb[yi - 1] + tempb0 + ((1.0 - temp1) * temp0 + 1.0) * bb[yi]
    kb         <- kb - temp * tempb0

    if (temp3 <= 0.0)
    {
      pb <- pb - temp0 * tempb1
    }
    else
    {
      pb <- pb - temp0 * tempb1 - temp1 * log(temp3) * tempb / 2
    }

    rb      <- rb + tempb1
    bb[yi]  <- 0.0
  }

  kb         <- kb + bb[1]
  paramsb[3] <- exp(params[3]) * pb
  paramsb[2] <- exp(params[2]) * kb
  paramsb[1] <- exp(params[1]) * rb

  return (paramsb)
}


MP_FunctionExports <- c(MP_FunctionExports, "CPUETarget")

#------------------------------------------------------------------------------
# MP resembling first level of ETBF Harvest Strategy (CPUE slope to target)
# Raise or lower TAC proportional to (recent weighted average) Index
# difference from a target
#------------------------------------------------------------------------------
CPUETarget <- function(pset, ITargPars=c(2.5, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1), yrsmth=5,lambda=0.4,xx=0.2)
{
  ny     <- length(pset$Cobs) #number years of data
  ind    <- (ny - (yrsmth - 1)):ny
  I_hist <- pset$Iobs[ind] #historical Abundance index
  yind   <- 1:yrsmth
  slppar <- summary(lm(I_hist ~ yind))$coefficients[2,1:2] #slope of recent index
  Islp   <- slppar[1]

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

  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF, TAC=newTAC))
}



# constant catch projections
#------------------------------------------------------------------------------
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



#------------------------------------------------------------------------------
MP_FunctionExports <- c(MP_FunctionExports, "CPUE_setpoint_control")

#------------------------------------------------------------------------------
# MP using CPUE setpoint to control TAC along with simple model estimation
# of MSY and CPUE at MSY
#------------------------------------------------------------------------------
CPUE_setpoint_control <- function(pset, target_CPUE_scale=1.25, resume_MSY_scale=0.5, close_CPUE_scale = 0.3, open_CPUE_scale = 0.3, TAC_gain=0.05)
{
  require(splines)

  # check for completion of MP
  if (!is.null(pset$complete) && (pset$complete ==TRUE))
  {
    # Use this as a oportunity to plot diagnostic stuff
  }

  catch_model <- function(a, b, effort)
  {
    C <- a * effort * (b - effort)

    return (C)
  }

  CPUE_model <- function(a, b, effort)
  {
    CPUE <- a * (b - effort)

    return (CPUE)
  }

  fitCatchEffortModel <- function(catch_series, CPUE_series)
  {
    objective <- function(par)
    {
      a <- par[1]
      b <- par[2]

      squareError <- sum(((catch_model(a, b, effort_series) - catch_series) ^ 2), na.rm=TRUE)

      return (squareError)
    }

    # filter out point with large changes in CPUE as these will bias estimate because the
    # data is running far from steady state conditions
    relative_change <- c(1.0, CPUE_series[2:length(CPUE_series)] / CPUE_series[1:(length(CPUE_series) - 1)])
    ix              <- which(!((relative_change < 0.707) | (relative_change > 2)))
    catch_series    <- catch_series[ix]
    CPUE_series     <- CPUE_series[ix]

    effort_series <- catch_series / CPUE_series
    start         <- c(4 * max(catch_series, na.rm=TRUE) / (max(effort_series, na.rm=TRUE) ^ 2), max(effort_series, na.rm=TRUE))
    fit           <- nlminb(start, objective)
    a             <- fit$par[1]
    b             <- fit$par[2]

#    par(mfrow=c(1,1))
#    plot(effort_series, catch_series)
#    points(effort_series, catch_model(a, b, effort_series), col="red")
#
#    readline(prompt="Press [enter] to continue")
#
#    plot(effort_series, CPUE_series)
#    points(effort_series, CPUE_model(a, b, effort_series), col="red")
#
#    readline(prompt="Press [enter] to continue")

    results <- list(MSY      = (a * b * b / 4),
                    CPUE_MSY = (a * b / 2),
                    a        = a,
                    b        = b)

    return (results)
  }

  #check for first run
  if (is.null(pset$env$closed))
  {
    pset$env$closed <- FALSE
  }

  # years between MP evaluation
  count <- pset$interval
  ny    <- length(pset$Cobs) #number years of data
  model <- fitCatchEffortModel(pset$Cobs, pset$Iobs)

  if (count > ny)
  {
    count <- ny
  }

  years   <- ny:(ny - count + 1)
  Catches <- pset$Cobs[years]
  lastTAC <- pset$prevTACE$TAC
  CPUE    <- pset$Iobs[years]
  Ey      <- Catches / CPUE
  Catch   <- (1 / 2) * Catches[1] + (1 / 3) * Catches[2] + (1 / 6) * Catches[3]
  E       <- (1 / 2) * Ey[1] + (1 / 3) * Ey[2] + (1 / 6) * Ey[3]
  CPUEest <- (Catch / E)

  measuredCPUE <- CPUE[1]
#  measuredCPUE <- CPUEest

  predicted_catch <- catch_model(model$a, model$b, E)
  predicted_CPUE  <- CPUE_model(model$a, model$b, E)
  refCPUE         <- target_CPUE_scale * model$CPUE_MSY
  closeCPUE       <- close_CPUE_scale * refCPUE
  openCPUE        <- open_CPUE_scale * refCPUE
  prediction      <- spline(years,CPUE, xmax=years[1] + 3)
  newCPUE         <- prediction$y[length(prediction$y)]

  if (pset$env$closed)
  {
    if (newCPUE > openCPUE)
    {
      newTAC          <- resume_MSY_scale * model$MSY
      pset$env$closed <- FALSE
    }
    else
    {
      newTAC          <- 9
    }
  }
  else
  {
    if (measuredCPUE < closeCPUE)
    {
      newTAC          <- 9
      pset$env$closed <- TRUE
    }
    else
    {
      if (lastTAC == 9)
      {
        error         <- newCPUE - refCPUE
      }
      else
      {
        error         <- measuredCPUE - refCPUE
      }

      gain            <- TAC_gain * model$b
      deltaC          <- error * gain
      newTAC          <- lastTAC + deltaC
    }
  }

#  print("----------")
#  print(paste("last TAC : ", lastTAC))
#  print(paste("new TAC : ", newTAC))
#  print(paste("CPUE : ", CPUE))
#  print(paste("ref CPUE : ", refCPUE))
#  print(paste("model MSY : ", model$MSY))

  if (newTAC < 9) newTAC <- 9 #shut the fishery down, except collect some data

  TAEbyF <- 0.*pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}


ISP_0.4_0.3_0.3_0.05 <- function(pset)
{
  return (CPUE_setpoint_control(pset, target_CPUE_scale=pset$tune * 1.0, resume_MSY_scale=0.5, close_CPUE_scale = 0.3, open_CPUE_scale = 0.3, TAC_gain=0.05))
}

class(ISP_0.4_0.3_0.3_0.05) <- "IO_MP_tune"
