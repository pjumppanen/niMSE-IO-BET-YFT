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


PTproj.15 <- function(pset)
{
  return(PellaTomlinsonProjection(pset, BMSY_Prop=pset$tune, Gain=0.15))
}

class(PTproj.15)<-"IO_MP_tune"


PTproj.25 <- function(pset)
{
  return(PellaTomlinsonProjection(pset, BMSY_Prop=pset$tune, Gain=0.25))
}

class(PTproj.25)<-"IO_MP_tune"


# imposes TAC change constraint on top of original MP PTproj.1.35bmsy.25
PTproj.1.35bmsy.25.tc15 <- function(pset)
{
  return(PellaTomlinsonProjection(pset, BMSY_Prop=1.35, Gain=0.25, MinCatchProp=0.20 * pset$tune, deltaTACLimUp=0.15, deltaTACLimDown=0.15))
}
class(PTproj.1.35bmsy.25.tc15)<-"IO_MP_tune"


PTproj.0.85catchprop.25.tc15 <- function(pset)
{
  return(PellaTomlinsonProjection(pset, BMSY_Prop=1.0 * pset$tune, Gain=0.25, MinCatchProp=0.85, deltaTACLimUp=0.15, deltaTACLimDown=0.15))
}
class(PTproj.0.85catchprop.25.tc15)<-"IO_MP_tune"


PTproj.1.35bmsy.25 <- function(pset)
{
  return(PellaTomlinsonProjection(pset, BMSY_Prop=1.35, Gain=0.25, MinCatchProp=0.20 * pset$tune))
}
class(PTproj.1.35bmsy.25)<-"IO_MP_tune"


PT41AL.t15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return(PellaTomlinsonAbsoluteLimits(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41AL.t15)<-"IO_MP_tune"

PT41AL.t25<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.25, deltaTACLimDown=0.25)
{
  return(PellaTomlinsonAbsoluteLimits(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41AL.t25)<-"IO_MP_tune"

PT41AL.t50<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.5, deltaTACLimDown=0.5)
{
  return(PellaTomlinsonAbsoluteLimits(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41AL.t50)<-"IO_MP_tune"

PT41AL.t90<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.9, deltaTACLimDown=0.9)
{
  return(PellaTomlinsonAbsoluteLimits(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41AL.t90)<-"IO_MP_tune"



PT41A.t15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.15, deltaTACLimDown=0.15)
{
  return(PellaTomlinsonAlternative(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41A.t15)<-"IO_MP_tune"

PT41A.t25<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.25, deltaTACLimDown=0.25)
{
  return(PellaTomlinsonAlternative(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41A.t25)<-"IO_MP_tune"

PT41A.t50<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.5, deltaTACLimDown=0.5)
{
  return(PellaTomlinsonAlternative(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41A.t50)<-"IO_MP_tune"

PT41A.t90<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., deltaTACLimUp=0.9, deltaTACLimDown=0.9)
{
  return(PellaTomlinsonAlternative(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown))
}

class(PT41A.t90)<-"IO_MP_tune"



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


# the fishing mortality equivalent of PT41.t15
PT41F.t15<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT41F.t15)<-"IO_MP_tune"

# the fishing mortality equivalent of PT41.t15
PT60F.t15<-function(pset, BLower=0.,BUpper=0.6,CMaxProp=1., useF=T, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT60F.t15)<-"IO_MP_tune"

# the fishing mortality equivalent of PT41.t15
PT62F.t15<-function(pset, BLower=0.2,BUpper=0.6,CMaxProp=1., useF=T, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT62F.t15)<-"IO_MP_tune"


# the fishing mortality equivalent of PT41.x
PT41F.t10<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT41F.t10)<-"IO_MP_tune"

# the fishing mortality equivalent of PT41.t50
PT41F.t50<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.5, deltaTACLimDown=0.5){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT41F.t50)<-"IO_MP_tune"

# the fishing mortality equivalent of PT41.t50
PT41F.t75<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.75, deltaTACLimDown=0.75){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, useF=T))
}
class(PT41F.t75)<-"IO_MP_tune"

# a fishing mortality version of PT4010, with a shape exponent
PT41F.t50.s30 <-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1., useF=T, deltaTACLimUp=0.5, deltaTACLimDown=0.5, shockAbsorber=0.3){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, shockAbsorber=shockAbsorber, useF=T))
}
class(PT41F.t50.s30)<-"IO_MP_tune"

# a fishing mortality version of PT4010, with a shape exponent
PT30F.t50.s30 <-function(pset, BLower=0.,BUpper=0.3,CMaxProp=1., useF=T, deltaTACLimUp=0.5, deltaTACLimDown=0.5, shockAbsorber=0.3){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, shockAbsorber=shockAbsorber, useF=T))
}
class(PT30F.t50.s30)<-"IO_MP_tune"

# a fishing mortality version of PT4010, with a shape exponent
PT80F.t50.s30 <-function(pset, BLower=0.,BUpper=0.8,CMaxProp=1., useF=T, deltaTACLimUp=0.5, deltaTACLimDown=0.5, shockAbsorber=0.3){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, shockAbsorber=shockAbsorber, useF=T))
}
class(PT80F.t50.s30)<-"IO_MP_tune"

# a fishing mortality version of PT4010, with a shape exponent
PT80F.t75.s30 <-function(pset, BLower=0.,BUpper=0.8,CMaxProp=1., useF=T, deltaTACLimUp=0.75, deltaTACLimDown=0.75, shockAbsorber=0.3){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, shockAbsorber=shockAbsorber, useF=T))
}
class(PT80F.t75.s30)<-"IO_MP_tune"

PT30F.t75.s20 <-function(pset, BLower=0.,BUpper=0.3,CMaxProp=1., useF=T, deltaTACLimUp=0.75, deltaTACLimDown=0.75, shockAbsorber=0.2){
  return(PellaTomlinson4010(pset, BLower=BLower,BUpper=BUpper,CMaxProp=pset$tune * CMaxProp, deltaTACLimUp=deltaTACLimUp, deltaTACLimDown=deltaTACLimDown, shockAbsorber=shockAbsorber, useF=T))
}
class(PT30F.t75.s20)<-"IO_MP_tune"


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

IT5.t75 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.75, deltaTACLimDown=0.75){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.1,0.1,0.1,0.1), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t75) <- "IO_MP_tune"

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

# all gain parameters = .2
IT5.t15g2 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.2,0.2,0.2,0.2), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15g2) <- "IO_MP_tune"

# all gain parameters = .15
IT5.t15g1.5 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.15,0.15,0.15,0.15,0.15), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t15g1.5) <- "IO_MP_tune"

# all gain parameters = .3
IT5.t10g3 <- function(pset,yrsmth=5,lambda=0.4,xx=0.2, deltaTACLimUp=0.1, deltaTACLimDown=0.1){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.3,0.3,0.3,0.3,0.3), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT5.t10g3) <- "IO_MP_tune"

# all gain parameters = .3
IT7.t15g2 <- function(pset,yrsmth=7,lambda=0.4,xx=0.2, deltaTACLimUp=0.15, deltaTACLimDown=0.15){
  return(CPUETarget(pset, ITargPars=c(pset$tune *1.0,deltaTACLimUp,deltaTACLimDown,0.2,0.2,0.2,0.2,0.2), yrsmth=yrsmth,lambda=lambda,xx=xx))
}
class(IT7.t15g2) <- "IO_MP_tune"


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

#------------------------------------------------------------------------------

# Pella Tomlinson Production model with generic 40-10 type rule - MPs are defined with tuning parameters above
# useF option uses the 40:10 rule for F rather than C, in which case FMax = FMSY*CMaxProp
# positive gridSearch value is preferred at this time
PellaTomlinson4010<-function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, shockAbsorber=1, useF=F, gridSearch=3){

  p <- -0.16  # don't bother trying to estimate p; for p = -0.16, BMSY/K ~0.33
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass
  lastTAC <- pset$prevTACE$TAC
  MSYballpark <- mean(C_hist[40:50]) # assumes recent catch near MSY
  Kballpark   <- 20.*MSYballpark # assumes recent harvest rate ~5%
  rballpark   <- MSYballpark/(Kballpark/((p+1)^(1/p))) # solves for r given K and MSY estimates

  par(mfrow=c(2,2))

  #print(c("I sum", sum(I_hist[29:50])))  #confirm that all MPs are using the same CPUE series

  # for speed and efficiency with minimization sensitivity, don't bother fitting if the population has essentially crashed
  Iy <- I_hist[!is.na(I_hist)]

  if (mean(Iy[(length(Iy)-1):length(Iy)]) <  0.1*mean(Iy[1:5])){
    print("skip PT fitting - stock clearly needs rebuilding")
    newTAC <- 1.    #i.e. (almost) shutdown fishery
  } else {
    #Initial Model parameters
    #rInit <- 0.1
    #KInit <- 20.*CMCsum #initial K defined relative to recent catches
    rBest       <- rballpark
    KBest       <- Kballpark
    params      <- log(c(rBest,KBest))
    lowerBounds <- log(exp(params)/20)
    upperBounds <- log(exp(params)*20)

    #  if(gridSearch >= 0){ #not required if minimal grid search used
    bestOpt <- optim(par=params,fn=PT.f, returnOpt=1,
             C_hist=C_hist,I_hist=I_hist, CMCsum=CMCsum, p=p,
             method="L-BFGS-B",
             lower=lowerBounds,upper=upperBounds,
             hessian=F, doPlot=F)
    # This is only for before and after gridSearch comparisons...otherwise get rid of it
    #PT.f(par=bestOpt$par, returnOpt=1, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=T)
    rBest <- exp(bestOpt$par[1])
    KBest <- exp(bestOpt$par[2])
    #  }

    #if using gridSearch, repeat the minimization for a grid of fixed K values, and find the corresponding r for each; then optimize both
    if(gridSearch>0){
      KgridMin <- 0.5
      KgridMax  <- 2.5
      repeat {
        if (bestOpt$value < 10 | gridSearch > 200) break
        #rInit    <- 0.1
        for(iGrid in 1:gridSearch){
          Kgrid   <- Kballpark*(KgridMin + KgridMax*((iGrid-1)/gridSearch))
          params  <- log(c(rBest))
          #find best r for fixed K from grid
          rOpt <- optim(par=params,fn=PT.f, returnOpt=1,        Kgrid=log(Kgrid),
               C_hist=C_hist,I_hist=I_hist, CMCsum=CMCsum, p=p,
               method="L-BFGS-B",
               lower=lowerBounds[1],upper=upperBounds[1],
               hessian=F, doPlot=F)
          #retain best solution for multi-variate minimization
          if(rOpt$value < bestOpt$value){
            PT.f(params=c(rOpt$par, log(Kgrid)), returnOpt=1, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=F)
            bestOpt <- rOpt
            rBest   <- exp(rOpt$par)
            KBest   <- Kgrid
          }
          #print(c("iGrid,gridSearch,bestOpt$value"))
          #print(c(iGrid,gridSearch,rOpt$value, bestOpt$value))
        }
        print("                                                                                  bestOpt$value  " %&% bestOpt$value)
        if(bestOpt$value > 5){
          #increase grid density
          KgridMin <- KgridMin*0.95
          KgridMax <- KgridMax*1.05
          gridSearch <- floor(gridSearch*1.5)

          #re-bracket best value
          #KgridMin <- KBest*0.8
          #KgridMax <- KBest*1.2

          print(c("                                                                                  gridSearch  " %&% gridSearch))
          #readline("expand gridSearch")
        }
        #if(gridSearch>100) readline("max gridsearch reached")
      } # repeat loop

      # Do a final minimization with r and K active from best starting point
      params<-log(c(rBest,KBest))

      finalOpt<-optim(par=params,fn=PT.f, returnOpt=1,
               C_hist=C_hist,I_hist=I_hist, CMCsum=CMCsum, p=p,
               method="L-BFGS-B",
               lower=lowerBounds,upper=upperBounds,
               hessian=F, doPlot=F)
      #readline("final Opt complete")
      if(finalOpt$value < bestOpt$value){
        PT.f(par=c(finalOpt$par), returnOpt=1, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=F)
        #readline("final Opt better than previous best")
        bestOpt <- finalOpt
        rBest   <- exp(finalOpt$par[1])
        KBest   <- exp(finalOpt$par[2])
      }
      #PT.f(par=params, returnOpt=2, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=T)
      #if(finalOpt$value>10){
      #   readline("final calc ")
      #}
  }






  #speedy gridSearch only does a single func evaluation for grid of R and K then optimizes both from best point
  #not as robust as above
    if(gridSearch<0){
      NgridK  <- abs(gridSearch)
      #Kgrid   <- 20.*CMCsum*(4*((1:NgridK)/NgridK))
      #rGrid   <- c(0.075, 0.1, 0.125)

      MSYballpark <- mean(C_hist[40:50]) #CMCsum # assumes current catch near MSY
      Kballpark   <- 20.*MSYballpark # assumes surrent harvest rate ~5%
      #Kgrid       <- Kballpark*(0.5*1.5^c(1:NgridK)) #mostly okay for BET NgridK=4
      Kgrid       <- Kballpark*(0.75*1.15^c(1:NgridK))  #mostly okay for BET NgridK=8
      rGrid       <- c(0.75, 1., 1.25)*MSYballpark/(Kballpark/((p+1)^(1/p))) # solves for r given K and MSY estimates

      bestLLH <- 9E+99
      for(iGridK in 1:NgridK){
        for(iGridr in 1:length(rGrid)){
          params  <- log(c(rGrid))
          LLH <- PT.f(params=c(log(rGrid[iGridr]), log(Kgrid[iGridK])), returnOpt=1, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=F)
          #retain best solution for multi-variate minimization
          if(LLH < bestLLH){
            bestLLH <- LLH
            KBest   <- Kgrid[iGridK]
            rBest   <- rGrid[iGridr]
          }
        } #rGrid
      } #Kgrid
      # Do a final minimization with r and K active from best starting point
      params<-log(c(rBest,KBest))
      finalOpt<-optim(par=params,fn=PT.f, returnOpt=1,
                   C_hist=C_hist,I_hist=I_hist, CMCsum=CMCsum, p=p,
                   method="L-BFGS-B",
                   lower=lowerBounds,upper=upperBounds,
                   hessian=F, doPlot=F)


      rBest   <- exp(finalOpt$par[1])
      KBest   <- exp(finalOpt$par[2])
    }


    #get the biomass/BMSY estimate - view final model fit if in doubt
    params <- log(c(rBest,KBest))
    d <- PT.f(par=params, returnOpt=2, C_hist=C_hist, I_hist=I_hist, CMCsum=CMCsum, p=p, doPlot=F)
    #print("TAC applied to this result")
    #readline("extra calc check")

    #Apply something like a 40-10 rule for catch relative to MSY
    if(useF==F){
      if(d["BY"]/d["K"] <= BLower)                            newTAC <- 1.    #i.e. (almost) shutdown fishery
      if(d["BY"]/d["K"] >  BLower & d["BY"]/d["K"] <= BUpper) newTAC <- CMaxProp*(d["MSY"]*(d["BY"]/d["K"])/(BUpper-BLower) + d["MSY"]*( 1 - (BUpper/(BUpper-BLower))))^shockAbsorber
      if(d["BY"]/d["K"] >  BUpper)                            newTAC <- CMaxProp*d["MSY"]
    }
    #Apply the 40:10 rule to F rather than catch...
    if(useF){
      FMSY = -log(1-d["MSY"]/d["K"])
      FMult = CMaxProp # maximum F relative to FMSY
      if(d["BY"]/d["K"] <= BLower)                            TACF <- 0.0001    #i.e. (almost) shutdown fishery
      if(d["BY"]/d["K"] >  BLower & d["BY"]/d["K"] <= BUpper) TACF <- FMult*FMSY*((d["BY"]/d["K"])/(BUpper-BLower) + ( 1 - (BUpper/(BUpper-BLower))))^shockAbsorber
      if(d["BY"]/d["K"] >  BUpper)                            TACF <- FMult*FMSY
      newTAC <-  d["BY"]*(1-exp(-TACF))
    }

    names(newTAC) <- "TAC"

  }# bypass model fitting if stock already crashed...

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


# -----------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PT.f")

# -----------------------------------------------------------------------------
#Pella-Tomlinson Model function
#unstable as C approaches B
#
PT.f <- function(params, C_hist, I_hist, CMCsum, p, doPlot=F, returnOpt=1, Kgrid=F){

  #Model parameters
  #B(t+1)=B(t) + (r/p)B(1-(B/K)^p) - C

  # not sure which of the following  options are most helpful or whether they interact, but conceptually seem like they might be
  # to improve numerical stability, tried adding a "foraging arena" type refuge - part of the population that is subject to harvesting
  # but not vulnerable to LL CPUE = IrefugeMin + IrefugeProp*B/K
  minBI        <- 0.01 # 0.02 # if CPUE biomass decreases below this level, set CPUE and B LLH components to this level
                       # whether its a bit "wrong" is irrelevant if the model is in the space where fishery shut down is  required
  IrefugeMin   <- 0.1  # 0.1 # minimum proportion of biomass not vulnerable to LL CPUE
  IrefugeProp  <- 0.5  # 0.5 # additional proportion not vulnerable as a function (proportional) of (1-B(y)/K)

  midIWt       <- 0.01 #weighting for majrrity of CPUE series

  initIWtYrs   <- 10    #number of initial years for different CPUE wt
  initIWt      <- 1    #initial CPUEwt

  lastIWtYrs   <- 5    #number of final years for different CPUE wt
  lastIWt      <- 5    #final CPUE wt

  # mpLag defined for testing Paavo's projection idea to try and account for most recent removals
  # implementation below only okay if mpLag < mpPeriod (i.e. usually 3)
  mpLag <- 2 # should be an argument if usefu: 0 is the original

  Y  <- length(C_hist) + 1 # i.e. can calculate biomass one year ahead of last catch
  BI <- array(NA,dim=Y+mpLag)
  B  <- array(NA,dim=Y+mpLag)
  r  <- exp(params[1])

  if(mpLag>0){
    C_histProj <- c(C_hist,rep(C_hist[Y-1], mpLag))
  }

  if(Kgrid){
    #if(returnOpt==2) browser()
    K <- exp(Kgrid)
  } else {
    K <- exp(params[2])
  }
  CPen <- 0

  B[1] <- K
  #Umax <- 0.9
  for(y in 2:(Y+mpLag)){
    B[y]  <- B[y-1] + ((p+1)/p)*r*B[y-1]*(1-(B[y-1]/K)^p) - C_histProj[y-1]
    BI[y] <- B[y]*(1 - (IrefugeMin+IrefugeProp*(1-B[y]/K)))

    if(is.na(B[y])){
      print("                              Undefined Biomass Error")
      browser()
    }

    # options for avoiding negative biomass: some may be used in combination

    # option 1 - set maximum HR
    #if (C_hist[y-1]/B[y] > 0.7){
    #  CPen <- CPen + 0.1*(C_hist[y-1]/B[y] - 0.9)^2
    #}

    # option 2 - penalize negative biomass (option 3 should take precedence if CPUErefuge > 0 )
    if (B[y]<1e-5){
      CPen <- CPen + 0.1*((B[y]/K)^2)
      B[y] <- 1e-5
    }
    # option 3 - foraging arena - CPUE refuge i.e. min B in reserve that is not indexed by CPUE?
    if (BI[y]<1e-5){
      CPen <- CPen + 0.1*((BI[y]/K)^2)
      BI[y] <- 1e-5
    }

  }

  # required since PJ changed NAs to zeroes at some point
  I_hist[I_hist == 0] <- NA
  # This should probably be in log-space (though this does make collapse situations more difficult)
  # q <- sum(I_hist[!is.na(I_hist)])/sum(B[!is.na(I_hist)])
  # LLH <- sum((q*B[!is.na(I_hist)]-I_hist[!is.na(I_hist)])^2)
  #Haddon's analytical soln for log-space q
  q <- exp(1/sum(!is.na(I_hist))*sum(log(I_hist[!is.na(I_hist)] / BI[!is.na(I_hist)])))

  Iy <- I_hist[!is.na(I_hist)]
  BIy <- BI[!is.na(I_hist)]

  #log-space causes problems as B <- 0 set low values to 2%, i.e. anything below this should be the same for th MP anyway
  Iy[Iy<minBI*mean(Iy[1:5])] <- minBI*mean(Iy[1:5])
  BIy[BIy<minBI*mean(BIy[1:5])] <- minBI*mean(BIy[1:5])

  #vector of CPUE weightings
  Iwt <- rep(midIWt, length(Iy))
  Iwt[1:initIWtYrs] <- initIWt
  Iwt[(length(Iy)-(lastIWtYrs-1)):length(Iy)] <- lastIWt

  #all obs weighted equally
  LLH <- sum(Iwt*log(q*BIy/Iy)^2)

  LLH <- LLH + CPen

  MSY <- r*K/((p+1)^(1/p))

  # quick and dirty plausibility constraints - no claim that this is a good approach for constraining the PT model
  # This should be removed; perhaps repleaced with grid of starting points
  #if(MSY < 0.2*CMCsum) LLH <- LLH + (MSY - 0.2*CMCsum)^2
  #if(MSY > 5.0*CMCsum)  LLH <- LLH + (MSY - 5.0*CMCsum)^2


  if(doPlot){
    depPercent <- round(1000*B[Y]/B[1])/10
    obj        <- round(10*LLH)/10
    msy        <- round(MSY/1000)

    plot(BI, type='l', col=5, main="BT/B0 %: " %&% depPercent %&% "\nMSY " %&% msy %&% "    Obj " %&% obj, ylim=c(0,max(I_hist/q, na.rm=T)))
    points(I_hist/q,col=1)
    lines(B,col=3)
    lines(10*C_hist,col=2)
    print("")
    print(c("PT.f  OBJfn:         ", LLH))
    print(c("K:  ", K))
    print(c("r:  ", r))
    print(c("MSY:  ", MSY))
    print(c("BT/B0:", B[Y]/B[1]))
    print(c("BT+lag/B0:", B[Y+mpLag]/B[1]))
  }

  #check for poor model fit
  #if(parent.frame(6)$MseDef@OMList[parent.frame(3)$sim] == "h70_M10_t10_q0_iH_iR1_CLRW_SL"){
  if(returnOpt==2 & LLH > 10 & mean(B[(Y-3):Y])/B[1] > 0.1){
    print("temporary (and not sufficient) check for PT.f minimization failure")
#    print(parent.frame(6)$MseDef@OMList[parent.frame(3)$sim]) #problem model
#    browser()
  }
  #}



  if(returnOpt == 1){ # return objective function
    return(LLH)
  } else {         # return MSY-related stuff
    BMSY <- K/((p+1)^(1/p))
    outList <- c(B[Y+mpLag], K, MSY, BMSY)
    names(outList) <- c("BY","K","MSY","BMSY")
    return(outList)
  }
}
#for(C in c(1:100)/100){
#  print(C)
#  PT.f(params=log(c(.1,100)), C_hist=rep(C,10000), I_hist=rep(1,10000), CMCsum=1,p=-0.16,doPlot=F)
#}
#for(p in c(-100:200)/100){
#  print(c("p, BMSY/K: ", p,1/((p+1)^(1/p))) )    #p=-0.16   BMSY/K~0.33
#}


# -----------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PellaTomlinsonAlternative")

# -----------------------------------------------------------------------------
# Pella Tomlinson Production model with generic 40-10 type rule - MPs are
# defined with tuning parameters above useF option uses the 40:10 rule for F
# rather than C, in which case FMax = FMSY*CMaxProp
# -----------------------------------------------------------------------------
PellaTomlinsonAlternative <- function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, useF=FALSE)
{
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass

  #Initial Model parameters
  C2Init <- (CMCsum * 1000)
  maxp   <- c(log(CMCsum * 100000), log(CMCsum * 1000), log(1.0))
  minp   <- c(log(CMCsum / 10), log(CMCsum / 1000), log(0.1))

  #check for first run
  if (is.null(pset$env$params))
  {
    params  <- log(c(C2Init, CMCsum, 0.9))

    pset$env$params <- params
  }
  else
  {
    params  <- pset$env$params
    maxp[1] <- params[1] + log(50)
    minp[1] <- params[1] - log(50)
    maxp[2] <- params[2] + log(50)
    minp[2] <- params[2] - log(50)
  }

  ix     <- which(!is.na(I_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  ix     <- which(!is.na(C_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  # get the biomass/BMSY estimate
#  opt    <- nlminb(start=params, objective=PT.model.fixed.p, gradient=PT.model.fixed.p.gradient, C_hist=C_hist, I_hist=I_hist, Type=0, weight=NULL, p=0.42, lower=minp, upper=maxp)
  opt    <- nlminb(start=params, objective=PT.model.fixed.p, C_hist=C_hist, I_hist=I_hist, Type=0, weight=NULL, p=0.42, lower=minp, upper=maxp)
  d      <- PT.model.fixed.p(opt$par, C_hist, I_hist, Type=1, p=0.42)

  pset$env$params <- opt$par
  lastTAC         <- pset$prevTACE$TAC

  if (useF)
  {
    #Apply the 40:10 rule to F rather than catch...
    FMSY = -log(1-d$MSY / d$K)
    FMult = CMaxProp # maximum F relative to FMSY
    if ( d$BY / d$K <= BLower)                            TACF <- 0.0001    #i.e. shutdown fishery
    if ((d$BY / d$K >  BLower) && (d$BY / d$K <= BUpper)) TACF <- FMult * FMSY * (d$BY / d$K) / (BUpper - BLower) + FMult * FMSY * (1 - (BUpper / (BUpper - BLower)))
    if ( d$BY / d$K >  BUpper)                            TACF <- FMult * FMSY

    newTAC <- d$BY * (1.0 - exp(-TACF))
  }
  else
  {
    #Apply something like a 40-10 rule for catch relative to MSY
    if ( d$BY / d$K <= BLower)                            newTAC <- 1.0    #i.e. shutdown fishery
    if ((d$BY / d$K >  BLower) && (d$BY / d$K <= BUpper)) newTAC <- CMaxProp * d$MSY * (d$BY / d$K) / (BUpper - BLower) + CMaxProp * d$MSY * (1 - (BUpper / (BUpper - BLower)))
    if ( d$BY / d$K >  BUpper)                            newTAC <- CMaxProp * d$MSY
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


#------------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PellaTomlinsonAbsoluteLimits")

# -----------------------------------------------------------------------------
# Pella Tomlinson Production model based MP with TAC chnage limits based on
# modelled MSY. This MP also respects fishery closure and does not attempt to
# limit the TAC change because of such action.
# -----------------------------------------------------------------------------
PellaTomlinsonAbsoluteLimits <- function(pset, BLower=0.1,BUpper=0.4,CMaxProp=1.0, deltaTACLimUp=0.9, deltaTACLimDown=0.9, useF=FALSE)
{
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass

  #Initial Model parameters
  C2Init <- (CMCsum * 1000)
  maxp   <- c(log(CMCsum * 100000), log(CMCsum * 1000), log(1.0))
  minp   <- c(log(CMCsum / 10), log(CMCsum / 1000), log(0.1))

  #check for first run
  if (is.null(pset$env$params))
  {
    params  <- log(c(C2Init, CMCsum, 0.9))

    pset$env$params <- params
  }
  else
  {
    params  <- pset$env$params
    maxp[1] <- params[1] + log(50)
    minp[1] <- params[1] - log(50)
    maxp[2] <- params[2] + log(50)
    minp[2] <- params[2] - log(50)
  }

  ix     <- which(!is.na(I_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  ix     <- which(!is.na(C_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  # get the biomass/BMSY estimate
#  opt    <- nlminb(start=params, objective=PT.model.fixed.p, gradient=PT.model.fixed.p.gradient, C_hist=C_hist, I_hist=I_hist, Type=0, weight=NULL, p=0.42, lower=minp, upper=maxp)
  opt    <- nlminb(start=params, objective=PT.model.fixed.p, C_hist=C_hist, I_hist=I_hist, Type=0, weight=NULL, p=0.42, lower=minp, upper=maxp)
  d      <- PT.model.fixed.p(opt$par, C_hist, I_hist, Type=1, p=0.42)

  pset$env$params <- opt$par

  lastTAC    <- pset$prevTACE$TAC
  closureTAC <- CMaxProp * d$MSY * 0.15

  if (useF)
  {
    #Apply the 40:10 rule to F rather than catch...
    FMSY  <- -log(1.0 - d$MSY / d$K)
    FMult <- CMaxProp # maximum F relative to FMSY

    if (d$BY / d$K <= BLower)
    {
      TACF <- -log(1.0 - (closureTAC / d$BY))
    }
    else if ((d$BY / d$K >  BLower) && (d$BY / d$K <= BUpper))
    {
      TACF <- FMult * FMSY * (d$BY / d$K) / (BUpper - BLower) + FMult * FMSY * (1 - (BUpper / (BUpper - BLower)))
    }
    else if (d$BY / d$K >  BUpper)
    {
      TACF <- FMult * FMSY
    }

    newTAC <- d$BY * (1.0 - exp(-TACF))
  }
  else
  {
    #Apply something like a 40-10 rule for catch relative to MSY
    if (d$BY / d$K <= BLower)
    {
      newTAC <- closureTAC
    }
    else if ((d$BY / d$K >  BLower) && (d$BY / d$K <= BUpper))
    {
      newTAC <- CMaxProp * d$MSY * (d$BY / d$K) / (BUpper - BLower) + CMaxProp * d$MSY * (1 - (BUpper / (BUpper - BLower)))
    }
    else if (d$BY / d$K >  BUpper)
    {
      newTAC <- CMaxProp * d$MSY
    }
  }

  names(newTAC) <- "TAC"

  deltaTAC  <- newTAC - lastTAC
  upLimit   <- deltaTACLimUp * d$MSY
  downLimit <- -deltaTACLimDown * d$MSY

  if (deltaTAC > upLimit)
  {
    deltaTAC <- upLimit
  }
  else if (deltaTAC < downLimit)
  {
    deltaTAC <- downLimit
  }

  newTAC <- lastTAC + deltaTAC

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

# -----------------------------------------------------------------------------


MP_FunctionExports <- c(MP_FunctionExports, "PellaTomlinsonProjection")

# -----------------------------------------------------------------------------
# Fit Pella Tomlinson Production model and use it to determine a TAC to drive
# yield to the desired target without crashing the stock.
# -----------------------------------------------------------------------------
PellaTomlinsonProjection <- function(pset, BMSY_Prop=1.0, Gain=0.15, MinCatchProp=0.15, debug=FALSE, deltaTACLimUp=0.9, deltaTACLimDown=0.9)
{
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass
  C2Init <- (CMCsum * 1000)
  maxp <- c(log(CMCsum * 100000), log(CMCsum * 1000), log(1.0))
  minp <- c(log(CMCsum / 10), log(CMCsum / 1000), log(0.1))

  #check for first run
  if (is.null(pset$env$params))
  {
    #Initial Model parameters
    params <- log(c(C2Init, CMCsum, 0.9))

    pset$env$save     <- debug
    pset$env$params   <- params
    pset$env$MSY      <- c()
    pset$env$BMSY     <- c()
    pset$env$r        <- c()
    pset$env$K        <- c()
    pset$env$q        <- c()
    pset$env$p        <- c()
    pset$env$B0       <- c()
    pset$env$obj      <- c()
    pset$env$message  <- c()
    pset$env$y        <- c(pset$y)
  }
  else
  {
    pset$env$y <- c(pset$env$y, pset$y)
    params     <- pset$env$params
    maxp[1]    <- params[1] + log(50)
    minp[1]    <- params[1] - log(50)
    maxp[2]    <- params[2] + log(50)
    minp[2]    <- params[2] - log(50)
  }

  # filter leading NA's from the catch and cpue data
  ix     <- which(!is.na(I_hist))
  first  <- min(ix)
  last   <- max(ix)
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  ix     <- which(!is.na(C_hist))
  first  <- max(c(first, min(ix)))
  last   <- min(c(last, max(ix)))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]
  weight <- rep(1, times=length(C_hist))

  start_ix <- min(20, length(C_hist))
  end_ix   <- max(length(C_hist) - 20, 1)

  if (start_ix < end_ix)
  {
    weight[start_ix:end_ix] <- 0.5
  }

#  opt    <- nlminb(start=params, objective=PT.model.fixed.p, gradient=PT.model.fixed.p.gradient, C_hist=C_hist, I_hist=I_hist, Type=0, weight=weight, p=0.42, lower=minp, upper=maxp)
  opt    <- nlminb(start=params, objective=PT.model.fixed.p, C_hist=C_hist, I_hist=I_hist, Type=0, weight=weight, p=0.42, lower=minp, upper=maxp)
  Fit    <- PT.model.fixed.p(opt$par, C_hist, I_hist, Type=1, p=0.42)
  params <- opt$par

  if (length(pset$env$K) > 0)
  {
    span <- Fit$K / mean(pset$env$K)

    if ((span > 10) || (span < 0.1))
    {
      # poor fit so discard and use previous good fit
      Fit               <- pset$env$lastFit
    }
    else
    {
      # save starting point for next time
      pset$env$lastFit  <- Fit
      pset$env$params   <- params
    }
  }
  else
  {
    # save starting point for next time
    pset$env$lastFit  <- Fit
    pset$env$params   <- params
  }

  MinCatch          <- MinCatchProp * Fit$MSY

  pset$env$MSY      <- c(pset$env$MSY,     Fit$MSY)
  pset$env$BMSY     <- c(pset$env$BMSY,    Fit$BMSY)
  pset$env$r        <- c(pset$env$r,       Fit$r)
  pset$env$K        <- c(pset$env$K,       Fit$K)
  pset$env$q        <- c(pset$env$q,       Fit$q)
  pset$env$p        <- c(pset$env$p,       Fit$p)
  pset$env$B0       <- c(pset$env$B0,      Fit$B0)
  pset$env$obj      <- c(pset$env$obj,     opt$objective)
  pset$env$message  <- c(pset$env$message, opt$message)

  if (debug)
  {
    # diagnostic plot
    plot(I_hist / Fit$q, ylim=c(0,max(max(I_hist / Fit$q, na.rm=T), max(pset$B[first:last], na.rm=T))))
    lines(Fit$B, col=2)
    lines(pset$B[first:last], col=3)
    browser()
  }

  lastTAC <- pset$prevTACE$TAC

  if (debug && !is.null(pset$complete) && pset$complete)
  {
    plot(pset$env$BMSY, type="l", col="red")
    plot(pset$env$MSY,  type="l", col="red")
    plot(pset$env$r,    type="l", col="red")
    plot(pset$env$K,    type="l", col="red")
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
  BStart  <- PT.project(Fit, rep(lastTAC, times=endTime), Fit$BY)[endTime]
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

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)
  if(newTAC<9) newTAC <- 9 #shut the fishery down, except collect some data

  TAEbyF <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF, TAC=newTAC))
}

# -----------------------------------------------------------------------------


MP_FunctionExports <- c(MP_FunctionExports, "PT.project")

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
    B[y] <- B[y-1] + (r / p) * B[y-1] - (r / p) * B[y-1] * ((B[y-1] / K) ^ p) - C[y-1]

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

MP_FunctionExports <- c(MP_FunctionExports, "PT.model.fixed.p")


# -----------------------------------------------------------------------------
# Pella-Tomlinson Model function
# -----------------------------------------------------------------------------
PT.model.fixed.p <- function(params, C_hist, I_hist, Type, p, weight=NULL)
{
  return (PT.model(c(params[1], params[2], log(p), params[3]), C_hist, I_hist, Type, weight))
}


# -----------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PT.model.fixed.p.gradient")


# -----------------------------------------------------------------------------
# Pella-Tomlinson Model gradient function
# -----------------------------------------------------------------------------
PT.model.fixed.p.gradient <- function(params, C_hist, I_hist, Type, p, weight=NULL)
{
  gradient <- PT.model.gradient(c(params[1], params[2], log(p), params[3]), C_hist, I_hist, Type, weight)

  return (c(gradient[1], gradient[2], gradient[4]))
}


# -----------------------------------------------------------------------------

MP_FunctionExports <- c(MP_FunctionExports, "PT.model")


# -----------------------------------------------------------------------------
# Pella-Tomlinson Model function
# -----------------------------------------------------------------------------
PT.model <- function(params, C_hist, I_hist, Type, weight=NULL)
{
  # Model formulation is,
  # B(t+1)=B(t) + (r/p)B(t)(1-(B(t)/K)^p) - C(t)
  #
  # In an attempt to help the optimizer fit the model we re-arrange things into
  # the form,
  #
  # B(t+1)=((r / p) + 1) * B(t) - (B(t) * (r / p) * ((B(t) * r / L) ^ p) - C(t)
  #
  # Where,
  #
  # L = K * r
  #
  # Translated back we have,
  #
  # r = L / K
  #
  # Does this help at all? I don't know exactly.
  #
  Y     <- length(C_hist)
  K     <- exp(params[1])
  MSY   <- exp(params[2])
  p     <- exp(params[3])
  n     <- exp(params[4])
  B     <- array(NA,dim=Y)
  L     <- MSY * ((p + 1.0) ^ (1.0 + (1.0 / p)))
  r     <- L / K
  B[1]  <- n * K

  for(y in 2:Y)
  {
    # Despite this changing the PT model structure it seems critical to the
    # useful functioning of the MP that relies on it. This is because although the
    # PT model fit is no doubt biased, it still provides useful model parameter
    # estimates under high fishing pressure. In the traditional model under
    # high pressure such that B is driven to zero or close too zero, the model
    # estimates start varying wildly and depart radically from the "true"
    # model parameters. Why this is so I'm not entirely sure but it appears that
    # the collapse of the stock alters the objective function surface such that
    # it has many local minima to lock on to whereas this alteration seems to
    # blur the minima into one.
    Bsurvive <- B[y-1] + (r / p) * B[y-1]
    Bdie     <- (r / p) * B[y-1] * ((B[y-1] / K) ^ p) + C_hist[y-1]
    dlim     <- 2.0 / (1 + exp((Bdie / Bsurvive)^5)) # saturation mechanism that forces B to always be positive
    B[y]     <- Bsurvive - dlim * Bdie

    # sanity check. This should never be true
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
    LB      <- log(B)
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
    LB       <- log(B)
    LI_hist  <- log(I_hist)
    q        <- exp(mean(LI_hist - LB))
    LB       <- LB - mean(LB)
    LI_hist  <- LI_hist - mean(LI_hist)
    B0       <- B[1]
    BY       <- B[Y]
    E_MSY    <- r / (q * (p + 1.0))
    CPUE_MSY <- MSY / E_MSY

    Fit <- list(LB=LB, LI_hist=LI_hist, p=p, q=q, r=r, K=K, n=n, MSY=MSY, BMSY=CPUE_MSY / q, CPUE_MSY=CPUE_MSY, B0=B0, BY=BY, B=B)

    return (Fit)
  }
}

# -----------------------------------------------------------------------------


MP_FunctionExports <- c(MP_FunctionExports, "PT.model.gradient")

# -----------------------------------------------------------------------------
# AD adjoint of PT-model obtained by Tapenade AD of code,
#
# MODULE COMMON
#
#  INTEGER, PARAMETER :: dim_stack
#
#  INTEGER Y
#  REAL C_hist(Y), I_hist(Y), weight(Y)
#
# END
#
#
# REAL FUNCTION PT(params)
#
#   USE COMMON
#
#   INTEGER, INTENT (IN) :: Y
#   REAL, INTENT (IN) :: params(4), C_hist(Y), I_hist(Y), weight(Y)
#
#   REAL B(Y), q, r, K, p, lim, n, MSY, L, Bsurvive, Bdie, dlim, LB, LI_hist, devs
#   INTEGER yi
#
#   K     = EXP(params(1))
#   MSY   = EXP(params(2))
#   p     = EXP(params(3))
#   n     = EXP(params(4))
#   L     = MSY * ((p + 1.0) ** (1.0 + (1.0 / p)))
#   r     = L / K
#   B(1)  = n * K
#
#   DO yi=2,Y
#     Bsurvive = B(yi-1) + (r / p) * B(yi-1)
#     Bdie     = (r / p) * B(yi-1) * ((B(yi-1) / K) ** p) + C_hist(yi-1)
#     dlim     = 2.0 / (1 + exp((Bdie / Bsurvive) ** 5))
#     B(yi)    = Bsurvive - dlim * Bdie
#   END DO
#
#   LB      = log(B)
#   LI_hist = log(I_hist)
#   devs    = LI_hist - LB
#   devs    = devs - (sum(devs) / Y)
#
#   PT = sum((weight * devs) ** 2)
#
#   RETURN
#
# END
#
# to obtain the code,
#
#
# !        Generated by TAPENADE     (INRIA, Ecuador team)
# !  Tapenade 3.14 (r7259) - 18 Jan 2019 09:35
# !
# !  Differentiation of pt in reverse (adjoint) mode (with options i4 dr8 r4):
# !   gradient     of useful results: result
# !   with respect to varying inputs: result params
# !   RW status of diff variables: result:in-zero params:out
# SUBROUTINE PT_B(params, paramsb, result, resultb)
#   USE COMMON
#   IMPLICIT NONE
#   REAL, INTENT(IN) :: params(4)
#   REAL :: paramsb(4)
#   REAL :: result
#   REAL :: resultb
#   REAL :: b(y), q, r, k, p, lim, n, msy, l, bsurvive, bdie, dlim, devs(y)
#   REAL :: bb(y), rb, kb, pb, nb, msyb, lb, bsurviveb, bdieb, dlimb, devsb(y)
#   INTEGER :: yi
#   INTRINSIC EXP
#   INTRINSIC LOG
#   INTRINSIC SUM
#   REAL :: temp
#   REAL :: temp0
#   REAL :: temp1
#   REAL :: temp2
#   REAL :: temp3
#   REAL :: temp4
#   REAL :: temp5
#   REAL :: temp6
#   REAL :: tempb
#   REAL :: tempb0
#   REAL :: tempb1
#   REAL :: tempb2
#   REAL :: tempb3
#   k = EXP(params(1))
#   msy = EXP(params(2))
#   p = EXP(params(3))
#   n = EXP(params(4))
#   l = msy*(p+1.0)**(1.0+1.0/p)
#   r = l/k
#   b(1) = n*k
#   DO yi=2,y
#     CALL PUSHREAL4(bsurvive)
#     bsurvive = b(yi-1) + r/p*b(yi-1)
#     CALL PUSHREAL4(bdie)
#     bdie = r/p*b(yi-1)*(b(yi-1)/k)**p + c_hist(yi-1)
#     CALL PUSHREAL4(dlim)
#     dlim = 2.0/(1+EXP((bdie/bsurvive)**5))
#     CALL PUSHREAL4(b(yi))
#     b(yi) = bsurvive - dlim*bdie
#   END DO
#   devs = LOG(i_hist) - LOG(b)
#   devs = devs - SUM(devs)/y
#   devsb = 0.0
#   devsb = weight**2*2*devs*resultb
#   devsb = devsb - SUM(devsb)/y
#   bb = 0.0
#   bb = -(devsb/b)
#   kb = 0.0
#   pb = 0.0
#   rb = 0.0
#   DO yi=y,2,-1
#     dlimb = -(bdie*bb(yi))
#     temp4 = bdie/bsurvive
#     temp5 = temp4**5
#     temp6 = EXP(temp5) + 1
#     tempb = -(5*temp4**4*EXP(temp5)*2.0*dlimb/(temp6**2*bsurvive))
#     CALL POPREAL4(b(yi))
#     bsurviveb = bb(yi) - temp4*tempb
#     bdieb = tempb - dlim*bb(yi)
#     bb(yi) = 0.0
#     CALL POPREAL4(dlim)
#     CALL POPREAL4(bdie)
#     temp1 = r/p
#     tempb0 = b(yi-1)*temp1*bdieb
#     temp3 = b(yi-1)/k
#     IF (temp3 .LE. 0.0 .AND. (p .EQ. 0.0 .OR. p .NE. INT(p))) THEN
#       tempb1 = 0.0
#     ELSE
#       tempb1 = p*temp3**(p-1)*tempb0/k
#     END IF
#     temp2 = temp3**p
#     tempb2 = b(yi-1)*temp2*bdieb/p
#     bb(yi-1) = bb(yi-1) + (r/p+1.0)*bsurviveb + temp2*temp1*bdieb + tempb1
#     kb = kb - temp3*tempb1
#     CALL POPREAL4(bsurvive)
#     tempb3 = b(yi-1)*bsurviveb/p
#     IF (temp3 .LE. 0.0) THEN
#       pb = pb - r*tempb3/p - temp1*tempb2
#     ELSE
#       pb = pb + temp2*LOG(temp3)*tempb0 - temp1*tempb2 - r*tempb3/p
#     END IF
#     rb = rb + tempb3 + tempb2
#   END DO
#   nb = k*bb(1)
#   kb = kb + n*bb(1) - l*rb/k**2
#   lb = rb/k
#   temp = 1.0/p
#   temp0 = (p+1.0)**(temp+1.0)
#   msyb = temp0*lb
#   IF (.NOT.(p + 1.0 .LE. 0.0 .AND. (temp + 1.0 .EQ. 0.0 .OR. temp + 1.0 .NE. INT(temp + 1.0))))
#     pb = pb + (msy*(temp+1.0)*(p+1.0)**temp-temp0*LOG(p+1.0)*msy*temp/p)*lb
#   paramsb = 0.0
#   paramsb(4) = paramsb(4) + EXP(params(4))*nb
#   paramsb(3) = paramsb(3) + EXP(params(3))*pb
#   paramsb(2) = paramsb(2) + EXP(params(2))*msyb
#   paramsb(1) = paramsb(1) + EXP(params(1))*kb
#   resultb = 0.0
# END SUBROUTINE PT_B
#
# and translated back to R code below. paramsb is set to zero and ptb set to one
#
# -----------------------------------------------------------------------------
PT.model.gradient <- function(params, C_hist, I_hist, Type, weight=NULL)
{
  Y         <- length(C_hist)
  paramsb   <- array(as.double(NA), dim=c(4))
  b         <- array(as.double(NA), dim=c(Y))
  bb        <- array(as.double(NA), dim=c(Y))
  stackr    <- array(as.double(NA), dim=c(4 * Y))
  sr        <- 1
  k         <- exp(params[1])
  msy       <- exp(params[2])
  p         <- exp(params[3])
  n         <- exp(params[4])
  l         <- msy * (p + 1.0) ^ (1.0 + (1.0 / p))
  r         <- l / k
  b[1]      <- n * k
  bsurvive  <- 0.0
  bdie      <- 0.0
  dlim      <- 0.0

  for (yi in 2:Y)
  {
    stackr[sr] <- bsurvive
    sr         <- sr + 1
    bsurvive   <- b[yi-1] + (r / p) * b[yi-1]
    stackr[sr] <- bdie
    sr         <- sr + 1
    bdie       <- (r / p) * b[yi-1] * ((b[yi-1] / k) ^ p) + C_hist[yi-1]
    stackr[sr] <- dlim
    sr         <- sr + 1
    dlim       <- 2.0 / (1 + exp((bdie / bsurvive) ^ 5))
    stackr[sr] <- b[yi]
    sr         <- sr + 1
    b[yi]      <- bsurvive - dlim * bdie
  }

  devs  <- log(I_hist) - log(b)
  devs  <- devs - sum(devs) / Y

  if (is.null(weight))
  {
    devsb <- 2 * devs
  }
  else
  {
    devsb <- weight ^ 2 * 2 * devs
  }

  devsb <- devsb - sum(devsb) / Y
  bb    <- -(devsb / b)
  kb    <- 0.0
  pb    <- 0.0
  rb    <- 0.0

  for (yi in Y:2)
  {
    dlimb     <- -(bdie * bb[yi])
    temp4     <- bdie / bsurvive
    temp5     <- temp4 ^ 5
    temp6     <- exp(temp5) + 1
    tempb     <- -(5 * (temp4 ^ 4) * exp(temp5) * 2.0 * dlimb / ((temp6 ^ 2) * bsurvive))
    sr        <- sr - 1
    b[yi]     <- stackr[sr]
    bsurviveb <- bb[yi] - temp4 * tempb
    bdieb     <- tempb - dlim * bb[yi]
    bb[yi]    <- 0.0
    sr        <- sr - 1
    dlim      <- stackr[sr]
    sr        <- sr - 1
    bdie      <- stackr[sr]
    temp1     <- r / p
    tempb0    <- b[yi-1] * temp1 * bdieb
    temp3     <- b[yi-1] / k

    if ((temp3 <= 0.0) && ((p == 0.0) || (p != as.integer(p))))
    {
      tempb1 <- 0.0
    }
    else
    {
      tempb1 <- p * temp3 ^ (p-1) * tempb0 / k
    }

    temp2     <- temp3 ^ p
    tempb2    <- b[yi-1] * temp2 * bdieb / p
    bb[yi-1]  <- bb[yi-1] + ((r / p) + 1.0) * bsurviveb + temp2 * temp1 * bdieb + tempb1
    kb        <- kb - temp3 * tempb1
    sr        <- sr - 1
    bsurvive  <- stackr[sr]
    tempb3    <- b[yi-1] * bsurviveb / p

    if (temp3 <= 0.0)
    {
      pb <- pb - r * tempb3 / p - temp1 * tempb2
    }
    else
    {
      pb <- pb + temp2 * log(temp3) * tempb0 - temp1 * tempb2 - r * tempb3 / p
    }

    rb <- rb + tempb3 + tempb2
  }

  nb    <- k * bb[1]
  kb    <- kb + n * bb[1] - l * rb / k ^ 2
  lb0   <- rb / k
  temp  <- 1.0 / p
  temp0 <- (p + 1.0) ^ (temp + 1.0)
  msyb  <- temp0 * lb0

  if (!((p + 1.0 <= 0.0) && ((temp + 1.0 == 0.0) || (temp + 1.0 != as.integer(temp + 1.0)))))
  {
    pb <- pb + (msy * (temp + 1.0) * (p + 1.0) ^ temp - temp0 * log(p + 1.0) * msy * temp / p) * lb0
  }

  paramsb[4] <- exp(params[4]) * nb
  paramsb[3] <- exp(params[3]) * pb
  paramsb[2] <- exp(params[2]) * msyb
  paramsb[1] <- exp(params[1]) * kb

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

# non-tuning constant catch with TAC change constraints to find minimum rebuilding times
CC001.t15 <- function(pset, deltaTACLimUp = 0.15, deltaTACLimDown = 0.15)
{

  lastTAC  <- pset$prevTACE$TAC
  newTAC   <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF   <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}

class(CC001.t15) <- "IO_MP"

#------------------------------------------------------------------------------
# non-tuning constant catch with TAC change constraints to find minimum rebuilding times
CC001.t25 <- function(pset, deltaTACLimUp = 0.25, deltaTACLimDown = 0.25)
{

  lastTAC  <- pset$prevTACE$TAC
  newTAC   <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF   <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}

class(CC001.t25) <- "IO_MP"

#------------------------------------------------------------------------------
# non-tuning constant catch with TAC change constraints to find minimum rebuilding times
CC001.t35 <- function(pset, deltaTACLimUp = 0.35, deltaTACLimDown = 0.35)
{

  lastTAC  <- pset$prevTACE$TAC
  newTAC   <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF   <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}

class(CC001.t35) <- "IO_MP"

#------------------------------------------------------------------------------
# non-tuning constant catch with TAC change constraints to find minimum rebuilding times
CC001.t45 <- function(pset, deltaTACLimUp = 0.45, deltaTACLimDown = 0.45)
{

  lastTAC  <- pset$prevTACE$TAC
  newTAC   <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF   <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}

class(CC001.t45) <- "IO_MP"

#------------------------------------------------------------------------------
# non-tuning constant catch with TAC change constraints to find minimum rebuilding times
CC001.t65 <- function(pset, deltaTACLimUp = 0.65, deltaTACLimDown = 0.65)
{

  lastTAC  <- pset$prevTACE$TAC
  newTAC   <- 1000.0 #aggregate TAC (annual) by fishery
  TAEbyF   <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  deltaTAC <- newTAC/lastTAC - 1

  #print(deltaTAC)
  if(deltaTAC >  deltaTACLimUp)   deltaTAC =  deltaTACLimUp
  if(deltaTAC < -deltaTACLimDown) deltaTAC = -deltaTACLimDown
  newTAC <- lastTAC*(1+deltaTAC)

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}

class(CC001.t65) <- "IO_MP"
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

CC091 <- function(pset)
{
  TAC     <- 91000.0 #aggregate TAC (annual) by fishery (BET reported in 2017)
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}
class(CC091) <- "IO_MP"

#------------------------------------------------------------------------------

CC081 <- function(pset)
{
  TAC     <- 81000.0 #aggregate TAC (annual) by fishery (BET reported in 2017)
  TAEbyF  <- 0.0 * pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=TAC))
}
class(CC081) <- "IO_MP"

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
# MP using CPUE setpoint to control TAC along with Pella-Tomlinson model to
# estimate the CPUE setpoint for MSY
#------------------------------------------------------------------------------
CPUE_setpoint_control <- function(pset, target_CPUE_scale=1.25, resume_MSY_scale=0.5, close_CPUE_scale = 0.2, open_CPUE_scale = 0.3, TAC_gain=0.05)
{
  C_hist <- pset$Cobs
  I_hist <- pset$Iobs
  CMCsum <- pset$CMCsum  # "recent" annual catch in mass

  #Initial Model parameters
  C2Init <- (CMCsum * 1000)
  maxp   <- c(log(CMCsum * 100000), log(CMCsum * 1000), log(4.0), log(1.0))
  minp   <- c(log(CMCsum / 10), log(CMCsum / 1000), log(0.25), log(0.1))
  params <- log(c(C2Init, CMCsum, 1, 0.9))

  ix     <- which(!is.na(I_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  ix     <- which(!is.na(C_hist))
  C_hist <- C_hist[ix]
  I_hist <- I_hist[ix]

  opt    <- nlminb(start=params, objective=PT.model, Type=0, C_hist=C_hist, I_hist=I_hist, lower=minp, upper=maxp)
#  opt    <- nlminb(start=params, objective=PT.model, gradient=PT.model.gradient, Type=0, C_hist=C_hist, I_hist=I_hist, lower=minp, upper=maxp)

  # get the biomass/BMSY estimate
  model  <- PT.model(params=opt$par, Type=1, C_hist=C_hist, I_hist=I_hist)

  #check for first run
  if (is.null(pset$env$closed))
  {
    pset$env$closed    <- FALSE
    pset$env$wasClosed <- FALSE
  }

  # years between MP evaluation
  count <- pset$interval
  ny    <- length(pset$Cobs) #number years of data

  if (count > ny)
  {
    count <- ny
  }

  years   <- ny:(ny - count + 1)
  Catches <- pset$Cobs[years]
  lastTAC <- pset$prevTACE$TAC
  CPUE    <- pset$Iobs[years]

  measuredCPUE <- CPUE[1]

  refCPUE         <- target_CPUE_scale * model$CPUE_MSY
  closeCPUE       <- close_CPUE_scale * refCPUE
  openCPUE        <- open_CPUE_scale * refCPUE
  prediction      <- spline(years,CPUE, xmax=years[1] + 3)
  newCPUE         <- prediction$y[length(prediction$y)]
  minTAC          <- 0.15 * model$MSY

  if (pset$env$closed)
  {
    if (newCPUE > openCPUE)
    {
      newTAC             <- resume_MSY_scale * model$MSY
      pset$env$closed    <- FALSE
      pset$env$wasClosed <- TRUE
    }
    else
    {
      newTAC <- minTAC
    }
  }
  else
  {
    if (measuredCPUE < closeCPUE)
    {
      newTAC          <- minTAC
      pset$env$closed <- TRUE
    }
    else
    {
      if (pset$env$wasClosed)
      {
        error              <- newCPUE - refCPUE
        pset$env$wasClosed <- FALSE
      }
      else
      {
        error <- measuredCPUE - refCPUE
      }

      gain   <- TAC_gain * (model$r / (model$p * (model$q ^ model$r)))
      deltaC <- error * gain
      newTAC <- lastTAC + deltaC
    }
  }

  if (is.na(newTAC))
  {
    browser()
  }

  if (newTAC < minTAC)
  {
    newTAC <- minTAC
  }

  TAEbyF <- 0.*pset$prevTACE$TAEbyF #TAE by fishery

  return (list(TAEbyF=TAEbyF,TAC=newTAC))
}


ISP_0.4_0.3_0.3_0.05 <- function(pset)
{
  return (CPUE_setpoint_control(pset, target_CPUE_scale=pset$tune * 1.0, resume_MSY_scale=0.5, close_CPUE_scale = 0.3, open_CPUE_scale = 0.3, TAC_gain=0.05))
}

class(ISP_0.4_0.3_0.3_0.05) <- "IO_MP_tune"
