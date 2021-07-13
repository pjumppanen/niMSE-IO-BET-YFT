# -----------------------------------------------------------------------------
# evaluate and compare a suite of YFT grids en route to the 2021 TCMP OM ensemble
#   i.e. to address some spatial configuration questions:
#   1) Does the difference between 2 and 4 area OM configurations matter for the MP evaluations?
#   2) Are the MP evaluations sensitive to the rec distn par time blocking?
# -----------------------------------------------------------------------------

rm(list=ls(all=TRUE))


# local backup
gitMirrorDir <- "C:\\Users\\kol018\\OneDrive - CSIRO\\IOTC-postCovid\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase3\\niMSE-IO-BET-YFT\\"
# local not backed up
batchDir <- "C:\\niMSE-IO-BET-YFT-tmp\\OMconditioning\\Yellowfin\\"
#HPC storage
clusterDir <- "X:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\"


#setwd("H:\\C-offline\\MSE-IO-BET-YFT\\gitMirror\\phase3\\niMSE-IO-BET-YFT")
setwd(gitMirrorDir)



source("Source/MseMain.R")
# source("OMconditioning/RStuff/importGrid2.f.R")


#test delete me top
load(file = "Objects/OMgridY21.1.500.RDA")

# xxx attempt to add TMB MP using  PJ's latest push
# (as opposed to old approach of adding TMB R script into mseMain)
OMgridY21.1.500 <- upgradeObject(OMgridY21.1.500) # required for PJ's new approach to MP organization
OMgridY21.1.500 <- addMP_SourceCode(OMgridY21.1.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")

load(file="Objects/tunedMP.Y3.MPp50.RDA")
#tunedMP.Y3.MPp50 <- upgradeObject(tunedMP.Y3.MPp50)

# re-run OMs with tuned MP from 21.5
print(system.time(mseY21.1.Y3.MPp50 <- runMse(OMgridY21.1.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
#print(system.time(mseY21.1.Y3.MPp50 <- runMse(OMgridY21.1.500, MPs=list("PTBoB0Targ.t50"), CppMethod=0, interval=3, Report=F, UseCluster=0)))

# use cpp with EffffortCeiling2
print(system.time(mseY21.1.Y3.MPp50cppEC2 <- runMse(OMgridY21.1.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=2.0)))
save(mseY21.1.Y3.MPp50cppEC2, file="Objects/mseY21.1.Y3.MPp50cppEC2.RDA")

# xxx test delete me bottom





#mainRootDir <- "H:\\C-offline\\MSE-IO-BET-YFT\\"  #modify for local path
mainRootDir <- gitMirrorDir  #modify for local path

# Tuning objective 2018 TCMP YFT2 - TCMP.Y18.2 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT3 - TCMP.Y18.3 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)





#load(file = mainRootDir %&% "gitMirror\\phase3\\niMSE-IO-BET-YFT\\Objects\\"  %&% "OMgridY20.1.List.Converged.RDA")
load(file = gitMirrorDir %&% "\\Objects\\"  %&% "OMgridY21.1.List.Converged.RDA")

source("Rscripts/Build OM-gridY21.1.converged.500.R")

print(system.time(OMgridY21.1.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.1.500, file="Objects/OMgridY21.1.500.RDA")
#load(file = "Objects/OMgridY21.1.500.RDA")


# test OM
print(system.time(test <- runMse(OMgridY21.1.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))
print(system.time(test <- runMse(OMgridY21.1.500, MPs="PT41.100.9", CppMethod=0, interval=3, Report=F, UseCluster=0)))
TCMP.Y18.2@tuningLogDomain         = c(-4,4)
MPL <- list("IT5.t75")
print(system.time(mseOMgridY21.1.500.TY18.2.1  <- runMse(OMgridY21.1.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=0, interval=3, Report=F, UseCluster=0)))

# run with tuned MP from 21.5
print(system.time(mseY21.1.Y2.MPp75 <- runMse(OMgridY21.1.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.1.Y2.MPp75, file="Objects/mseY21.1.Y2.MPp75.RDA")

print(system.time(mseY21.1.Y3.MPp75 <- runMse(OMgridY21.1.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.1.Y3.MPp75, file="Objects/mseY21.1.Y3.MPp75.RDA")


OMgridY21.1.500 <- upgradeObject(OMgridY21.1.500) # required for PJ's new approach to MP organization
OMgridY21.1.500 <- addMP_SourceCode(OMgridY21.1.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.1.Y3.MPp50 <- runMse(OMgridY21.1.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.1.Y3.MPp50, file="Objects/mseY21.1.Y3.MPp50.RDA")
load("Objects/mseY21.1.Y3.MPp50.RDA")




##########################################################################
#Y21.3 - 2 rec distn time blocks
load(file = mainRootDir %&% "Objects\\"  %&% "OMgridY21.3.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.3.converged.500.R")
print(system.time(OMgridY21.3.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.3.500, file="Objects/OMgridY21.3.500.RDA")
#load(file = "Objects/OMgridY21.3.500.RDA")


# run with tuned MP from 21.5
print(system.time(mseY21.3.Y2.MPp75 <- runMse(OMgridY21.3.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.3.Y2.MPp75, file="Objects/mseY21.3.Y2.MPp75.RDA")

print(system.time(mseY21.3.Y3.MPp75 <- runMse(OMgridY21.3.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.3.Y3.MPp75, file="Objects/mseY21.3.Y3.MPp75.RDA")

OMgridY21.3.500 <- upgradeObject(OMgridY21.3.500) # required for PJ's new approach to MP organization
OMgridY21.3.500 <- addMP_SourceCode(OMgridY21.3.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.3.Y3.MPp50 <- runMse(OMgridY21.3.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.3.Y3.MPp50, file="Objects/mseY21.3.Y3.MPp50.RDA")




##########################################################################
#Y21.5 -  Y21.1 structure except no tags and with grid from Y21.2 
# Use as the baseline MP tuning to compare issues related to 
# recruitment sensitivity, 2 vs 4 area structure, and additional Catch years 

load(file = mainRootDir %&% "Objects\\"  %&% "OMgridY21.5.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.5.converged.500.R")
print(system.time(OMgridY21.5.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.5.500, file="Objects/OMgridY21.5.500.RDA")
#load(file = "Objects/OMgridY21.5.500.RDA")


TCMP.Y18.2@tuningLogDomain         = c(-1,0.5)
TCMP.Y18.2@tuningTolerance          <- 0.05
MPList0 <- list("CCt")

print(system.time(test21.5.0 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.2, MPs=MPList0, CppMethod=0, interval=3, Report=F, UseCluster=1)))
MPList1 <- list("PTBoB0Targ.t75") #,"CCt")
# Y2 tuning, projection MP
print(system.time(mseY21.5.Y2.MPp75 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.2, MPs=MPList1, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y2.MPp75, file="Objects/mseY21.5.Y2.MPp75.RDA")
load(file="Objects/mseY21.5.Y2.MPp75.RDA")
MPs <- getMPs(mseY21.5.Y2.MPp75)
tunedMP.Y2.test1 <- MPs[[1]]
save(tunedMP.Y2.test1, file="Objects/tunedMP.Y2.test1.RDA")
load(file="Objects/tunedMP.Y2.test1.RDA")



#Y3 tuning, projection MP 25% change constraint - tuning fails
MPL <- list("PTBoB0Targ.t25") #,"CCt")
print(system.time(mseY21.5.Y3.MPp25 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPp25, file="Objects/mseY21.5.Y3.MPp25.RDA")
load(file="Objects/mseY21.5.Y3.MPp25.RDA")
MPs <- getMPs(mseY21.5.Y3.MPp25)
tunedMP.Y3.MPp25 <- MPs[[1]]
save(tunedMP.Y3.MPp25, file="Objects/tunedMP.Y3.MPp25.RDA")
load(file="Objects/tunedMP.Y3.MPp25.RDA")

#Y3 tuning, projection MP 35% change constraint - tuning fails
MPL <- list("PTBoB0Targ.t35") #,"CCt")
print(system.time(mseY21.5.Y3.MPp35 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPp35, file="Objects/mseY21.5.Y3.MPp35.RDA")
load(file="Objects/mseY21.5.Y3.MPp35.RDA")
MPs <- getMPs(mseY21.5.Y3.MPp35)
tunedMP.Y3.MPp35 <- MPs[[1]]
save(tunedMP.Y3.MPp35, file="Objects/tunedMP.Y3.MPp35.RDA")
load(file="Objects/tunedMP.Y3.MPp35.RDA")



#Y3 tuning, projection MP 50% change constraint
MPL <- list("PTBoB0Targ.t50") #,"CCt")
print(system.time(mseY21.5.Y3.MPp50 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPp50, file="Objects/mseY21.5.Y3.MPp50.RDA")
load(file="Objects/mseY21.5.Y3.MPp50.RDA")
MPs <- getMPs(mseY21.5.Y3.MPp50)
tunedMP.Y3.MPp50 <- MPs[[1]]
save(tunedMP.Y3.MPp50, file="Objects/tunedMP.Y3.MPp50.RDA")
load(file="Objects/tunedMP.Y3.MPp50.RDA")

#Y3 tuning, projection MP 75% change constraint
print(system.time(mseY21.5.Y3.MPp75 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPList1, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPp75, file="Objects/mseY21.5.Y3.MPp75.RDA")
load(file="Objects/mseY21.5.Y3.MPp75.RDA")
MPs <- getMPs(mseY21.5.Y3.MPp75)
tunedMP.Y3.test1 <- MPs[[1]]
save(tunedMP.Y3.test1, file="Objects/tunedMP.Y3.test1.RDA")
load(file="Objects/tunedMP.Y3.test1.RDA")

#Y3 tuning, hockeystick MP 75% change constraint
MPList2 <- list("PT41F.t75.tmb")
print(system.time(mseY21.5.Y3.MPhs75 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPList2, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPhs75, file="Objects/mseY21.5.Y3.MPhs75.RDA")
load(file="Objects/mseY21.5.Y3.MPhs75.RDA")
MPs <- getMPs(mseY21.5.Y3.MPhs75)
tunedMP.Y3.test2 <- MPs[[1]]
save(tunedMP.Y3.test2, file="Objects/tunedMP.Y3.test2.RDA")
load(file="Objects/tunedMP.Y3.test2.RDA")

#Y3 tuning, hockeystick MP - 50% change constraint
MPList3 <- list("PT41F.t50.tmb")
print(system.time(mseY21.5.Y3.MPhs50 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPList3, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPhs50, file="Objects/mseY21.5.Y3.MPhs50.RDA")
load(file="Objects/mseY21.5.Y3.MPhs50.RDA")
MPs <- getMPs(mseY21.5.Y3.MPhs50)
tunedMP.Y3.test2 <- MPs[[1]]
save(tunedMP.Y3.test2, file="Objects/tunedMP.Y3.test2.RDA")
load(file="Objects/tunedMP.Y3.test2.RDA")

#Y3 tuning, hockeystick MP - 35% change constraint -failed to tune 
MPList3 <- list("PT41F.t35.tmb")
print(system.time(mseY21.5.Y3.MPhs35 <- runMse(OMgridY21.5.500, TuningPars=TCMP.Y18.3, MPs=MPList3, CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPhs35, file="Objects/mseY21.5.Y3.MPhs35.RDA")
load(file="Objects/mseY21.5.Y3.MPhs35.RDA")
MPs <- getMPs(mseY21.5.Y3.MPhs35)
tunedMP.Y3.test2 <- MPs[[1]]
save(tunedMP.Y3.test2, file="Objects/tunedMP.Y3.hs35.RDA")
load(file="Objects/tunedMP.Y3.hs35.RDA")

# compare R and cpp F implementations for 21.5
OMgridY21.5.500 <- upgradeObject(OMgridY21.5.500) # required for PJ's new approach to MP organization
OMgridY21.5.500 <- addMP_SourceCode(OMgridY21.5.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
rm(mseY21.5.Y3.MPp50cpp)
print(system.time(mseY21.5.Y3.MPp50cpp <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseY21.5.Y3.MPp50cpp, file="Objects/mseY21.5.Y3.MPp50cpp.RDA")
load(file="Objects/tunedMP.Y3.hs35.RDA")

# check if Effort Ceiling 20 is th elimiting issue? 40
print(system.time(mseY21.5.Y3.MPp50cppEC40 <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=40)))
save(mseY21.5.Y3.MPp50cppEC40, file="Objects/mseY21.5.Y3.MPp50cppEC40.RDA")

# confirm default Effort Ceiling 20 is in fact applied? 20
print(system.time(mseY21.5.Y3.MPp50cppEC20 <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=20)))
save(mseY21.5.Y3.MPp50cppEC20, file="Objects/mseY21.5.Y3.MPp50cppEC20.RDA")



# check if Effort Ceiling 20 is th elimiting issue? 5
# 5 seems identical to 40, but different to default of 20?!
print(system.time(mseY21.5.Y3.MPp50cppEC5 <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=5)))
save(mseY21.5.Y3.MPp50cppEC5, file="Objects/mseY21.5.Y3.MPp50cppEC5.RDA")

# check if Effort Ceiling 20 is th elimiting issue? 0.5
print(system.time(mseY21.5.Y3.MPp50cppEC0.5 <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=0.5)))
save(mseY21.5.Y3.MPp50cppEC0.5, file="Objects/mseY21.5.Y3.MPp50cppEC0.5.RDA")

# check if Effort Ceiling 20 is th elimiting issue? 2
print(system.time(mseY21.5.Y3.MPp50cppEC2 <- runMse(OMgridY21.5.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=2.0)))
save(mseY21.5.Y3.MPp50cppEC2, file="Objects/mseY21.5.Y3.MPp50cppEC2.RDA")
load(file="Objects/mseY21.5.Y3.MPp50cppEC2.RDA")



# inflate all movement rates for 21.5
OMgridY21.5.500 <- upgradeObject(OMgridY21.5.500) # required for PJ's new approach to MP organization
OMgridY21.5.500 <- addMP_SourceCode(OMgridY21.5.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")

OMgridY21.5MU.500 <- OMgridY21.5.500
OMgridY21.5MU.500 <- forAllModelDataDo(OMgridY21.5MU.500, function(ModelData, RefVars, ix)
  {
    #inline function for uniform normalized movement rates among all regions for all ages
    ModelData@mov <- ModelData@mov*0 + sum(ModelData@mov)/prod(dim(ModelData@mov))
    return(ModelData)
  })
save(OMgridY21.5MU.500, file="Objects/OMgridY21.5MU.500.RDA")
load("Objects/OMgridY21.5MU.500.RDA")

print(system.time(mseY21.5MU.Y3.MPp50cpp <- runMse(OMgridY21.5MU.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseY21.5MU.Y3.MPp50cpp, file="Objects/mseY21.5MU.Y3.MPp50cpp.RDA")
load(file="Objects/mseY21.5MU.Y3.MPp50cpp.RDA")

print(system.time(mseY21.5MU.Y3.MPp50cppEC2 <- runMse(OMgridY21.5MU.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=2)))
save(mseY21.5MU.Y3.MPp50cppEC2, file="Objects/mseY21.5MU.Y3.MPp50cppEC2.RDA")
load(file="Objects/mseY21.5MU.Y3.MPp50cppEC2.RDA")

print(system.time(mseY21.5MU.Y3.MPp50 <- runMse(OMgridY21.5MU.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.5MU.Y3.MPp50, file="Objects/mseY21.5MU.Y3.MPp50.RDA")
load(file="Objects/mseY21.5MU.Y3.MPp50.RDA")

print(system.time(mseY21.5MU.CC400 <- runMse(OMgridY21.5MU.500, MPs=list("CC400"), CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseY21.5MU.CC400, file="Objects/mseY21.5MU.CC400.RDA")



##########################################################################
#Y21.6 -  Y21.1 structure with 2 extra years of catch data 
load(file = gitMirrorDir %&% "\\Objects\\"  %&% "OMgridY21.6.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.6.converged.500.R")
print(system.time(OMgridY21.6.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.6.500, file="Objects/OMgridY21.6.500.RDA")
#load(file = "Objects/OMgridY21.6.500.RDA")

# test OM
print(system.time(test21.6 <- runMse(OMgridY21.6.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))
lastHistYr <- 2019
firstMPYr  <- 2022

# run with tuned MP from 21.5
print(system.time(mseY21.6.Y2.MPp75 <- runMse(OMgridY21.6.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.6.Y2.MPp75, file="Objects/mseY21.6.Y2.MPp75.RDA")

# run with tuned MP from 21.5
print(system.time(mseY21.6.Y3.MPp75 <- runMse(OMgridY21.6.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.6.Y3.MPp75, file="Objects/mseY21.6.Y3.MPp75.RDA")

OMgridY21.6.500 <- upgradeObject(OMgridY21.6.500) # required for PJ's new approach to MP organization
OMgridY21.6.500 <- addMP_SourceCode(OMgridY21.6.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.6.Y3.MPp50 <- runMse(OMgridY21.6.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.6.Y3.MPp50, file="Objects/mseY21.6.Y3.MPp50.RDA")





##########################################################################
#Y21.7 -  Y21.1 structure with 4 extra years of catch data 
load(file = gitMirrorDir %&% "\\Objects\\"  %&% "OMgridY21.7.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.7.converged.500.R")
print(system.time(OMgridY21.7.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.6.500, file="Objects/OMgridY21.7.500.RDA")
#load(file = "Objects/OMrefY21.7.500.RDA")

# test OM
print(system.time(test21.7 <- runMse(OMgridY21.7.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))
lastHistYr <- 2021
firstMPYr  <- 2022



##########################################################################
#Y21.2 - 2 areas 1 rec distn time block
load(file = mainRootDir %&% "Objects\\"  %&% "OMgridY21.2.List.RDA")
source("Rscripts/Build OM-gridY21.2.converged.500.R")
print(system.time(OMgridY21.2.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.2.500, file="Objects/OMgridY21.2.500.RDA")
#load(file = "Objects/OMgridY21.2.500.RDA")

# test OM
print(system.time(test <- runMse(OMgridY21.2.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))

#TCMP.Y18.2@tuningLogDomain         = c(-.25,-.75)
#TCMP.Y18.2@tuningTolerance          <- 0.05
#MPList0 <- c("CCt")
#print(system.time(test21.2 <- runMse(OMgridY21.2.500, TuningPars=TCMP.Y18.2, MPs=MPList0, CppMethod=0, interval=3, Report=F, UseCluster=1)))
#getMPs(test21.2)

# run with tuned MP from 21.5
print(system.time(mseY21.2.Y2.MPp75 <- runMse(OMgridY21.2.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.2.Y2.MPp75, file="Objects/mseY21.2.Y2.MPp75.RDA")

# run with tuned MP from 21.5
print(system.time(mseY21.2.Y3.MPp75 <- runMse(OMgridY21.2.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.2.Y3.MPp75, file="Objects/mseY21.2.Y3.MPp75.RDA")

OMgridY21.2.500 <- upgradeObject(OMgridY21.2.500) # required for PJ's new approach to MP organization
OMgridY21.2.500 <- addMP_SourceCode(OMgridY21.2.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.2.Y3.MPp50 <- runMse(OMgridY21.2.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.2.Y3.MPp50, file="Objects/mseY21.2.Y3.MPp50.RDA")

# use cpp with EffortCeiling2
print(system.time(mseY21.2.Y3.MPp50cppEC2 <- runMse(OMgridY21.2.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=1, interval=3, Report=F, UseCluster=1, EffortCeiling=2.0)))
save(mseY21.2.Y3.MPp50cppEC2, file="Objects/mseY21.2.Y3.MPp50cppEC2.RDA")






##########################################################################
#Y21.4 - 2 areas 2 rec distn time blocks
load(file = mainRootDir %&% "Objects\\"  %&% "OMgridY21.4.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.4.converged.500.R")
print(system.time(OMgridY21.4.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.4.500, file="Objects/OMgridY21.4.500.RDA")
#load(file = "Objects/OMgridY21.4.500.RDA")

# test OM
print(system.time(test <- runMse(OMgridY21.4.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))

# run with tuned MP from 21.5
print(system.time(mseY21.4.Y2.MPp75 <- runMse(OMgridY21.4.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.4.Y2.MPp75, file="Objects/mseY21.4.Y2.MPp75.RDA")

# run with tuned MP from 21.5
print(system.time(mseY21.4.Y3.MPp75 <- runMse(OMgridY21.4.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.4.Y3.MPp75, file="Objects/mseY21.4.Y3.MPp75.RDA")

OMgridY21.4.500 <- upgradeObject(OMgridY21.4.500) # required for PJ's new approach to MP organization
OMgridY21.4.500 <- addMP_SourceCode(OMgridY21.4.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.4.Y3.MPp50 <- runMse(OMgridY21.4.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.4.Y3.MPp50, file="Objects/mseY21.4.Y3.MPp50.RDA")




##########################################################################
#Y21.8 - 2 areas 1 rec distn time block catch extended 2017 for 2018-19
load(file = mainRootDir %&% "Objects\\"  %&% "OMgridY21.8.List.Converged.RDA")
source("Rscripts/Build OM-gridY21.8.converged.500.R")
print(system.time(OMgridY21.8.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
#save(OMgridY21.8.500, file="Objects/OMgridY21.8.500.RDA")
#load(file = "Objects/OMgridY21.8.500.RDA")

# test OM
print(system.time(test <- runMse(OMgridY21.8.500, MPs="CC001", CppMethod=0, interval=3, Report=F, UseCluster=0)))

# run with tuned MP from 21.5
print(system.time(mseY21.8.Y2.MPp75 <- runMse(OMgridY21.8.500, MPs=list(tunedMP.Y2.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.8.Y2.MPp75, file="Objects/mseY21.8.Y2.MPp75.RDA")

# run with tuned MP from 21.5
print(system.time(mseY21.8.Y3.MPp75 <- runMse(OMgridY21.8.500, MPs=list(tunedMP.Y3.test1), CppMethod=0, interval=3, Report=F, UseCluster=1)))
save(mseY21.8.Y3.MPp75, file="Objects/mseY21.8.Y3.MPp75.RDA")

OMgridY21.8.500 <- upgradeObject(OMgridY21.8.500) # required for PJ's new approach to MP organization
OMgridY21.8.500 <- addMP_SourceCode(OMgridY21.8.500, gitMirrorDir %&% "MPs\\TMB_MPs\\PTtmbMSYRBoB0Targ.R")
print(system.time(mseY21.8.Y3.MPp50 <- runMse(OMgridY21.8.500, MPs=list(tunedMP.Y3.MPp50), CppMethod=0, interval=3, Report=F, UseCluster=0)))
save(mseY21.8.Y3.MPp50, file="Objects/mseY21.8.Y3.MPp50.RDA")


lastHistYr <- 2019
firstMPYr  <- 2022






########################################################################
# plot single set of MP results
# not sure merging appropriate if structures (including MP start yr) differ

test <- mseY21.5MU.CC400
test <- mseY21.5MU.CC001
test <- mseY21.5MU.Y3.MPp50
test <- mseY21.6.Y3.MPp50
test <- mseY21.1.Y3.MPp50

# 2 vs 4 area
test <- mseY21.5.Y3.MPp50    # baseline tuned cpp
test <- mseY21.1.Y3.MPp50    # WPTT 4 area
test <- mseY21.2.Y3.MPp50    # WPTT 2 area

# rec dist time-blocking (in 2 and 4 areas)
test <- mseY21.1.Y3.MPp50    # WPTT 4 area
test <- mseY21.3.Y3.MPp50    # WPTT 4 area X 2 blocks
test <- mseY21.2.Y3.MPp50    # WPTT 2 area
test <- mseY21.4.Y3.MPp50    # WPTT 2 area X 2 blocks

# extend catch 2 years
test <- mseY21.6.Y3.MPp50    # two new catch years
lastHistYr <- 2019
firstMPYr  <- 2022

# miscellaneous F explorations
test <- mseY21.5.Y3.MPp50    # baseline tuned R Pope F
test <- mseY21.5.Y3.MPp50cpp # baseline tuned cpp Baranov F
#test <- mseY21.5.Y3.MPp50cppEC20 # should be identical to previous if effortCeiling argument applied as assumed (20) 
test <- mseY21.5MU.Y3.MPp50  # baseline tuned movement uniform R Pope F
test <- mseY21.5MU.Y3.MPp50cpp  # baseline tuned movement uniform cpp Baranov F
#test <- mseY21.5.Y3.MPp50cppEC40 # baseline tuned movement uniform cpp Baranov F, with effort ceiling raised from 20 to 40
#test <- mseY21.5.Y3.MPp50cppEC5 # baseline tuned movement uniform cpp Baranov F, with effort ceiling raised from 20 to 5
test <- mseY21.5.Y3.MPp50cppEC2 # baseline tuned movement uniform cpp Baranov F, with effort ceiling raised from 20 to 5
#test <- mseY21.5.Y3.MPp50cppEC0.5 # baseline tuned movement uniform cpp Baranov F, with effort ceiling raised from 20 to 0.5
test <- mseY21.5MU.Y3.MPp50cppEC2  # baseline tuned movement uniform cpp Baranov F

test <- mseY21.1.Y3.MPp50cppEC2 #ref set 4 area with effort ceiling 2
test <- mseY21.2.Y3.MPp50cppEC2 #ref set 2 area with effort ceiling 2


lastHistYr <- 2017
firstMPYr  <- 2022

FLim   <- test@MseDef@Flim
SBLim  <- test@MseDef@SBlim

FTarg  <- 1.
SBTarg <- 1.

Cref <- 440. # 440=2019 YFT catch
YFTTargs <- c(SBTarg, FTarg)
YFTLims  <- c(SBLim, FLim)

histd <- msevizHistoricTimeSeriesData(test)
projd <- msevizProjectedTimeSeriesData(test)
projd <- projd[projd$year <= 2040,]

names(YFTTargs) <- c("S3", "S4")
names(YFTLims) <- c("S3", "S4")
names(Cref) <- "S10"

# TCMP report plot - projections tuncated before first MP application
projd2 <- projd[projd$year <= 2020,]
tcmpCatchPlot(histd, projd2, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, 
              combined=F, doWorms=F)



par(mfrow=c(2,2))
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch/TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch/TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

# 
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch/TAC", OrList=c("q0"), lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch/TAC", OrList=c("q1"), lastHistYr = lastHistYr, firstMPYr = firstMPYr)

summary(projd)

projdlastC <- projd[projd$year == 2019 & projd$qname == "C/TAC","data"]
hist(as.numeric(unlist(projdlastC)), breaks=c(0:21)/20, xlim=c(0,1.1))

Fcrit <- 0.95 #criterion for declaring C/TAC success
#calculate proportion of model options that pass the catch 2018 test
tmp1 <- projd[projd$year == 2018 & projd$qname == "C/TAC","data"]
sum(tmp1[as.numeric(tmp1$data)>Fcrit])/500

#calculate proportion of model options that pass the catch 2019 test
tmp2 <- projd[projd$year == 2019 & projd$qname == "C/TAC","data"]
sum(tmp2[as.numeric(tmp2$data)>Fcrit])/500

#calculate proportion of model options that pass the last bridging catch year test
tmp3 <- projd[projd$year == 2021 & projd$qname == "C/TAC","data"]
sum(tmp3[as.numeric(tmp3$data)>Fcrit])/500

#calculate the distribution of model options that pass the catch 2019 test
mList.1    <- unlist(projd[projd$year == 2019 & projd$qname == "C/TAC","model"])
mList.1    <- unlist(lapply(mList.1, as.character))
mList.2 <- NULL
for (im in 1:length(mList.1)){
  mList.2     <- c(mList.2, unlist(strsplit(as.character(mList.1[im]), split="_")) )
}
table(mList.2)

mListC19.1   <- projd[projd$year == 2019 & projd$qname == "C/TAC",]
mListC19.1   <- mListC19.1[as.numeric(mListC19.1$data)>Fcrit,"model"]
mListC19.1   <- unlist(lapply(mListC19.1, as.character))
mListC19.2 <- NULL
for (im in 1:length(mListC19.1)){
  mListC19.2     <- c(mListC19.2, unlist(strsplit(mListC19.1[im], split="_")) )
}
table(mListC19.2)

#plot(table(mListC19.2)/table(mList.2), xlab="OM model option", ylab="Proportion > 95% C2019 extracted")
plot(table(mListC19.2)/length(mListC19.1), xlab="OM model option", ylab="Proportion > 95% C2019 extracted")

# extract the runs that pass the Catch 2019 test and plot them 
summary(projd)
iterSuccess <- projd$year == 2019 & projd$qname == "C/TAC" & projd$data > Fcrit 
iterSuccessList <- as.integer(unlist(projd[iterSuccess,"iter"]))
projdFiltered <- projd[projd$iter %in% iterSuccessList]

plotOMruns2(histd, projdFiltered, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projdFiltered, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projdFiltered, "C/TAC", ylab= "Catch/TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)





########################################################################
# plot some merged MP results
# not sure merging appropriate if structures (including MP start yr) differ



# spatial effect fig
YearsAveraged <- 15  #9, 14, 20
perfd <- msevizPerformanceData(mseY21.5.Y3.MPp50, YearsAveraged)
perfd$mp <- "OM21.5.MP-AY3"  

perfd2 <- msevizPerformanceData(mseY21.2.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.2.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.1.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.1.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

# rec time block fig 
YearsAveraged <- 15  #9, 14, 20
perfd <- msevizPerformanceData(mseY21.1.Y3.MPp50, YearsAveraged)
perfd$mp <- "OM21.1.MP-AY3"  

perfd2 <- msevizPerformanceData(mseY21.2.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.2.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.3.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.3.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.4.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.4.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)









#perfd2 <- msevizPerformanceData(mseY21.5.Y3.MPp50cpp, YearsAveraged)
#perfd2$mp <- "OM21.5.cpp.MP-AY3"  
#perfd <- merge(perfd, perfd2, all=TRUE)

# perfd2 <- msevizPerformanceData(mseY21.5MU.Y3.MPp50, YearsAveraged)
# perfd2$mp <- "OM21.5.MU"  
# perfd <- merge(perfd, perfd2, all=TRUE)



perfd2 <- msevizPerformanceData(mseY21.3.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.3.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.6.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.6.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.4.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.4.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.8.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.8.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.2.Y3.MPp50, YearsAveraged)
perfd2$mp <- "OM21.2.MP-AY3"  
perfd <- merge(perfd, perfd2, all=TRUE)



plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)


# compare a range  effort constraint  assumptions and fish mixing rates
# for OM withs with identical production dynamics
YearsAveraged <- 15  #9, 14, 20
perfd <- msevizPerformanceData(mseY21.5.Y3.MPp50, YearsAveraged)
perfd$mp <- "OM21.5.Pope"  

perfd2 <- msevizPerformanceData(mseY21.5.Y3.MPp50cpp, YearsAveraged)
perfd2$mp <- "OM21.5.cppEC20"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.5.Y3.MPp50cppEC2, YearsAveraged)
perfd2$mp <- "OM21.5.cppEC2"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.5MU.Y3.MPp50cpp, YearsAveraged)
perfd2$mp <- "OM21.5MU.cppEC20"  
perfd <- merge(perfd, perfd2, all=TRUE)

perfd2 <- msevizPerformanceData(mseY21.5MU.Y3.MPp50cppEC2, YearsAveraged)
perfd2$mp <- "OM21.5MU.cppEC2"  
perfd <- merge(perfd, perfd2, all=TRUE)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)






#####################################################################
# plot some spatial F summary stuff
library(grid)

proj <- msevizProjectedTimeSeriesData(mseY21.5.Y3.MPp50cppEC2)

# Paavo's heatmap:
plotConTAC_timeDistributionByF(proj)
# DK plots:
plotConTAC_proptionsByF(proj, 2020, plotEachF=F, thresholds=c(0.75, 0.95))
plotConTAC_proptionsByF(proj, 2020, thresholds=c(0.75, 0.95))



xxx outdated below here...








#one-off object fix - updates some CPUE slots without recreating OM
#OMrefY20.1.500 <- upgradeObject(OMrefY20.1.500)


TCMP.Y18.2@tuningLogDomain         = c(-4,4)
MPL <- list("IT5.t75")
print(system.time(mseOMgridYsave(mseY21.2.Y3.MPp50, file="Objects/mseY21.1.Y3.MPp50.RDA")
save(mseY21.1.Y3.MPp50, file="Objects/mseY21.1.Y3.MPp50.RDA")
save(mseY21.1.Y3.MPp50, file="Objects/mseY21.1.Y3.MPp50.RDA")
save(mseY21.1.Y3.MPp50, file="Objects/mseY21.1.Y3.MPp50.RDA")
.500.TY18.2.1  <- runMse(OMgridY21.1.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=0, interval=3, Report=F, UseCluster=0)))
#save(mseOMrefY20.1.500.TY18.2.1, file="Objects/mseOMrefY20.1.500.TY18.2.1.RDA")





# grid <- importGrid.f(gridList=names(OMrefY19.3List), gridDir="Z:\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\", convergedNum=3)

# Create gridY19.4.500 model definition - the new ref case
# load("objects/OMrefY19.4List.RDA")       #the weighted list of models
# table(unlist(strsplit(names(OMrefY19.4List), split="_")))/length(OMrefY19.4List)
# source("Rscripts/Build OM Model-OMrefY19.4.500.R")
# print(system.time(OMrefY19.4.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4.500 <- initCPUE_SeriesFrom(OMrefY19.4.500, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4.500, file="Objects/OMrefY19.4.500.RDA")

# Create gridY19.4.420 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4.420.R")
# print(system.time(OMrefY19.4.420 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4.420 <- initCPUE_SeriesFrom(OMrefY19.4.420, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4.420, file="Objects/OMrefY19.4.420.RDA")

# Create gridY19.4BF.368 model definition - uniform sample of 1 per model with the worst fit of the repeated minimization results
# load("objects/OMrefY19.4BFList.RDA")       #the weighted list of models
# source("Rscripts/Build OM Model-OMrefY19.4BF.368.R")
# print(system.time(OMrefY19.4BF.368 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# OMrefY19.4BF.368 <- initCPUE_SeriesFrom(OMrefY19.4BF.368, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
# save(OMrefY19.4BF.368, file="Objects/OMrefY19.4BF.368.RDA")


# Create OMrefY19.3b.500 model - fixes minor errors in original presentedt to TCMP2019 and adds one model with preferred CPUE series for MP
load("Objects/OMrefY19.3List.RDA")
newNameList <- c(names(OMrefY19.3List), "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
OMrefY19.3bList <- rep(1/length(newNameList), length(newNameList))
names(OMrefY19.3bList) <- newNameList
save(OMrefY19.3bList, file="Objects/OMrefY19.3bList.RDA")
source("Rscripts/Build OM Model-OMrefY19.3b.500.R")
print(system.time(OMrefY19.3b.500 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
OMrefY19.3b.500 <- initCPUE_SeriesFrom(OMrefY19.3b.500, "h80_M10_t0001_q0_iH_i3_iR1_gr2_CL75_SL_x4")
save(OMrefY19.3b.500, file="Objects/OMrefY19.3b.500.RDA")

# the old and erroneous OMrefY19.3
# grid <- importGrid.f(gridList=names(OMrefY19.3List), gridDir="Z:\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\gridY19.3\\", convergedNum=3)
# note that due to confounded design and model failure perhaps, CPUE combination q0, iH, iR1 is not available for base observed CPUE  
#load("Objects/OMrefY19.3List.RDA")
#load("Objects/OMrefY19.3.500.error.RDA")
#OMrefY19.3.500 <- initCPUE_SeriesFrom(OMrefY19.3.500, "h70_M06_t10_q0_iH_i3_iR2_gr2_ess5_SD_x8")
#getParameters(OMrefY19.3.500)$IACin
#getParameters(OMrefY19.3.500)$Icv
#OMrefY19.3.500 <- setParameters(OMrefY19.3.500, parameters=list(IACin=0.5, Icv=c(0.20000,0.20001)))
#save(OMrefY19.3.500, file="Objects/OMrefY19.3.500.RDA")






load(file="Objects/OMrefY19.4.500.RDA")

# set up some plot related variables
FLim   <- OMrefY19.4.500@MseDef@Flim
SBLim  <- OMrefY19.4.500@MseDef@SBlim

FTarg  <- 1.
SBTarg <- 1.

Cref <- 409. # 409=2017 YFT catch
YFTTargs <- c(SBTarg, FTarg)
YFTLims  <- c(SBLim, FLim)

names(YFTTargs) <- c("S3", "S4")
names(YFTLims) <- c("S3", "S4")
names(Cref) <- "S10"

###################################################################################
# set the tuning objectives
# Tuning objective 2018 TCMP YFT1 - TCMP.Y18.1 (TY5 retained from TCMP01)
#TCMP.Y18.1 <- new("TuningParameters")
#TCMP.Y18.1@performanceMeasure       <- "SBoSBMSY"
#TCMP.Y18.1@performanceMeasureClass  <- "0.5"
#TCMP.Y18.1@performanceMeasureYears  <- c(2024,2024)
#TCMP.Y18.1@tuningTarget             <- 1.0
#TCMP.Y18.1@tuningTolerance          <- 0.01
#TCMP.Y18.1@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT2 - TCMP.Y18.2 (TY5 retained from TCMP01)
TCMP.Y18.2 <- new("TuningParameters")
TCMP.Y18.2@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.2@performanceMeasureClass  <- "0.5"
TCMP.Y18.2@performanceMeasureYears  <- c(2029,2029)
TCMP.Y18.2@tuningTarget             <- 1.0
TCMP.Y18.2@tuningTolerance          <- 0.01
TCMP.Y18.2@tuningLogDomain         = c(-4,4)

# Tuning objective 2018 TCMP YFT3 - TCMP.Y18.3 (TY5 retained from TCMP01)
TCMP.Y18.3 <- new("TuningParameters")
TCMP.Y18.3@performanceMeasure       <- "SBoSBMSY"
TCMP.Y18.3@performanceMeasureClass  <- "0.5"
TCMP.Y18.3@performanceMeasureYears  <- c(2034,2034)
TCMP.Y18.3@tuningTarget             <- 1.0
TCMP.Y18.3@tuningTolerance          <- 0.01
TCMP.Y18.3@tuningLogDomain         = c(-4,4)

# standard constant catch demo plots
MPListC <- c("CC001","CC413")
print(system.time(mseOMrefY19.4.500.CC  <- runMse(OMrefY19.4.500, MPs=MPListC, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.CC)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.CC)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)


# suite of models that tune well for 18.2 with default settings
#MPL18.2 <- list("PT41F.t50", "PT41F.t75", "IT5.t50", "IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20") 
#print(system.time(mseOMrefY19.4.500.TY18.2.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

#rerun with corrected OM these for presentations
#MPL18.2 <- list("IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20") 
#print(system.time(mseOMrefY19.4.500.TY18.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL18.2, CppMethod=1, interval=3, Report=F, UseCluster=1)))

TCMP.Y18.2@tuningLogDomain         = c(-4,4)
MPL <- list("IT5.t75")
print(system.time(mseOMrefY19.4.500.TY18.2.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.1, file="Objects/mseOMrefY19.4.500.TY18.2.1.RDA")


TCMP.Y18.2@tuningLogDomain         = c(0.,1.)
MPL <- list("PTproj.1.35bmsy.25")
print(system.time(mseOMrefY19.4.500.TY18.2.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.2, file="Objects/mseOMrefY19.4.500.TY18.2.2.RDA")


TCMP.Y18.2@tuningLogDomain         = c(0.,1.5)
MPL <- list("PT30F.t75.s20")
print(system.time(mseOMrefY19.4.500.TY18.2.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.2.3, file="Objects/mseOMrefY19.4.500.TY18.2.3.RDA")






# need more nuanced search for models to tune for 18.3 because multiple solutions may be possible, and/or tuning might fail, due to oscillation frequency

MPL <- list("IT5.t50")  #
#TCMP.Y18.3@tuningTolerance          <- 0.05
TCMP.Y18.3@tuningLogDomain         = c(0,3)
print(system.time(mseOMrefY19.4.500.TY18.3.1  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.1, file="Objects/mseOMrefY19.4.500.TY18.3.1.RDA")
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01


# PJ's projection MP okay
TCMP.Y18.3@tuningLogDomain         = c(0.,1)
MPL <- list("PTproj.1.35bmsy.25")
print(system.time(mseOMrefY19.4.500.TY18.3.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.2, file="Objects/mseOMrefY19.4.500.TY18.3.2.RDA")

getMPs(mseOMrefY19.4.500.TY18.3.2)

#MPL <- list("PT41F.t75")  #tunes on 18.3 but not stable
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.2  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT41F.t50")  
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT41F.t50.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.4  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01



#MPL <- list("PT80F.t75.s30")
#MPL <- list("PT30F.t75.s20")
#MPL <- list("PTproj.1.35bmsy.25") #tunes on 18.3 fine
#MPL <- list("PT30F.t75.s20")      #18.3 tuning problems - small oscillations can prevent successful tuning even though it should be possible
#MPL <- list("PT41F.t50.s30")      # should be possible

MPL <- list("PT80F.t50.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
print(system.time(mseOMrefY19.4.500.TY18.3.3  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
save(mseOMrefY19.4.500.TY18.3.3, file="Objects/mseOMrefY19.4.500.TY18.3.3.RDA")
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01

#MPL <- list("PT80F.t75.s30")
#TCMP.Y18.3@tuningTolerance          <- 0.05
#TCMP.Y18.3@tuningLogDomain         = c(0,1)
#print(system.time(mseOMrefY19.4.500.TY18.3.7  <- runMse(OMrefY19.4.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
#TCMP.Y18.3@tuningLogDomain         = c(-4,4)
#TCMP.Y18.3@tuningTolerance          <- 0.01




# plots -  only need one histd
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.1)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.2), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.2.3), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.1), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.2), all=TRUE)
histd <- merge(histd, msevizHistoricTimeSeriesData(mseOMrefY19.4.500.TY18.3.3), all=TRUE)

# select Y18.2 tunings to plot and rename
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.1)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.2), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.2.3), all=TRUE)
#projd <- projd[as.character(projd$mp) %in% c("IT5.t75", "PTproj.1.35bmsy.25", "PT30F.t75.s20"),]  
projd$mp <- "Y2." %&%  as.character(projd$mp) 

# select Y18.3 tunings to plot and rename
projd.3 <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.1)
projd.3 <- merge(projd.3, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.2), all=TRUE)
projd.3 <- merge(projd.3, msevizProjectedTimeSeriesData(mseOMrefY19.4.500.TY18.3.3), all=TRUE)
projd.3$mp <- "Y3." %&%  as.character(projd.3$mp) 

projd <- merge(projd, projd.3, all=TRUE)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)


YearsAveraged <- 20  #9, 14, 20
perfd <- msevizPerformanceData(mseOMrefY19.4.500.TY18.2.1, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(mseOMrefY19.4.500.TY18.2.2, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(mseOMrefY19.4.500.TY18.2.3, YearsAveraged), all=TRUE)
perfd$mp <- "Y2." %&% as.character(perfd$mp)  

perfd.3 <- msevizPerformanceData(mseOMrefY19.4.500.TY18.3.1, YearsAveraged)
perfd.3 <- merge(perfd.3, msevizPerformanceData(mseOMrefY19.4.500.TY18.3.2, YearsAveraged), all=TRUE)
perfd.3 <- merge(perfd.3, msevizPerformanceData(mseOMrefY19.4.500.TY18.3.3, YearsAveraged), all=TRUE)
perfd.3$mp <- "Y3." %&% as.character(perfd.3$mp)  

# not sure why this fails and rbind works in this case...?
#perfd <- merge(perfd, perfd.3, all=TRUE)
perfd <- rbind(perfd, perfd.3)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



resultsY2.1 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.1, list("IT5.t75" =           "D.Y2"))
resultsY2.2 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.2, list("PTproj.1.35bmsy.25"= "Mr.Y2"))
resultsY2.3 <- changeMP_Names(mseOMrefY19.4.500.TY18.2.3, list("PT30F.t75.s20"=      "M.Y2"))
resultsY3.1 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.1, list("IT5.t50" =           "D.Y3"))
resultsY3.2 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.2, list("PTproj.1.35bmsy.25"= "Mr.Y3"))
resultsY3.3 <- changeMP_Names(mseOMrefY19.4.500.TY18.3.3, list("PT80F.t50.s30" =     "M.Y3"))  

getMPs(mseOMrefY19.4.500.TY18.3.1)

results <- list(resultsY2.1, resultsY2.2, resultsY2.3, resultsY3.1, resultsY3.2, resultsY3.3)
MP_names <-    c("D.Y2", "Mr.Y2", "M.Y2", "D.Y3", "Mr.Y3", "M.Y3")
MP_newnames <- MP_names

createTable1(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.20")
createTable1(9, results, MPs=MP_names, MPs_short=MP_newnames,  prefix="OMrefY19.4.500.9")
createTable1(14, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.14")


# not updated
createTable2(1,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(5,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(10, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")
createTable2(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.4.500.")



############################################################################################################################
# evaluate alternate OMs with same MPs - first set is alternate sampling of reference grid
# in terms of multinomial, uniform, best and worst convergence, main effeccts vs all 2 way interactions

# Create gridY19.4.420 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4.420.R")
# print(system.time(OMrefY19.4.420 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
load("objects/OMrefY19.4.420.RDA")       #the weighted list of models

D.Y2 <- getMPs(mseOMrefY19.4.500.TY18.2.1)
M.Y3  <- getMPs(mseOMrefY19.4.500.TY18.3.3)
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrefY19.4.420  <- runMse(OMrefY19.4.420, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.420)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.420)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)





# load("objects/OMrefY19.4BFList.RDA")       #the weighted list of models
# Create gridY19.4BF.368 model definition - uniform sample of 1 per model
# source("Rscripts/Build OM Model-OMrefY19.4BF.368.R")
# print(system.time(OMrefY19.4BF.368 <- createMseFramework(MseDef, UseCluster=0)))  #parallel processing fails with this function on DK laptop for some reason 
# save(OMrefY19.4BF.368, file="Objects/OMrefY19.4BF.368.RDA")

load("objects/OMrefY19.4BF.368.RDA")       #the weighted list of models
print(system.time(mseOMrefY19.4BF.368<- runMse(OMrefY19.4BF.368, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4BF.368)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4BF.368)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)




load("objects/OMrefY19.3b.500.RDA")       #the weighted list of models
print(system.time(mseOMrefY19.3b.500  <- runMse(OMrefY19.3b.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

histd <- msevizHistoricTimeSeriesData(mseOMrefY19.3b.500)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.3b.500)
projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)




# rename the selected MPs and plot together in the standard plots 

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrefY19.4.500.TY18.3.3, YearsAveraged)
perfd$mp   <- "Y3.M.4.500" 
perfdtmp    <- msevizPerformanceData(mseOMrefY19.4.500.TY18.2.1, YearsAveraged)
perfdtmp$mp <- "Y2.D.4.500"   

perfd <- rbind(perfd, perfdtmp)

perfd4.420 <- msevizPerformanceData(mseOMrefY19.4.420, YearsAveraged)
perfd4.420$mp <- as.character(perfd4.420$mp)
perfd4.420$mp[perfd4.420$mp == "IT5.t75"] <- "Y2.D.4.420"   
perfd4.420$mp[perfd4.420$mp == "PT80F.t50.s30"] <- "Y3.M.4.420"   
plotBPs2(perfd4.420, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd4.420)

perfd4BF.368 <- msevizPerformanceData(mseOMrefY19.4BF.368, YearsAveraged)
perfd4BF.368$mp <- as.character(perfd4BF.368$mp)
perfd4BF.368$mp[perfd4BF.368$mp == "IT5.t75"] <- "Y2.D.4BF.368"   
perfd4BF.368$mp[perfd4BF.368$mp == "PT80F.t50.s30"] <- "Y3.M.4BF.368"   
plotBPs2(perfd4BF.368, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd4BF.368)

perfd3b.500 <- msevizPerformanceData(mseOMrefY19.3b.500, YearsAveraged)
perfd3b.500$mp <- as.character(perfd3b.500$mp)
perfd3b.500$mp[perfd3b.500$mp == "IT5.t75"] <- "Y2.D.3b.500"   
perfd3b.500$mp[perfd3b.500$mp == "PT80F.t50.s30"] <- "Y3.M.3b.500"   
plotBPs2(perfd3b.500, limit=YFTLims, target=YFTTargs, blackRef=Cref)

perfd <- rbind(perfd, perfd3b.500)



plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



################################################################################################################
# find minimum rebuilding times

#MseDef@proyears      <- as.integer(26)  

#OMrefY19.4.500.longTime <- setParameters(OMrefY19.4.500, parameters=list(proyears=46)) #not required
MPL <- c("CC001.t15", "CC001.t25", "CC001.t35", "CC001.t45",  "CC001.t65", "CC001")
print(system.time(mseOMrefY19.4.500.CCtest  <- runMse(OMrefY19.4.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))


# plots -  only need one histd
histd <- msevizHistoricTimeSeriesData(mseOMrefY19.4.500.CCtest)
projd <- msevizProjectedTimeSeriesData(mseOMrefY19.4.500.CCtest)

#projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)


####################################################################################################################
# robustness tests
####################################################################################################################


# Case 1:   OMrobY19.4.ICV30 
#   CPUE CV = 0.3, auto-correlation = 0.5
# -----------------------------------------
load(file="Objects/OMrefY19.4.500.RDA")

getParameters(OMrefY19.4.500)$IACin
getParameters(OMrefY19.4.500)$Icv
OMrobY19.4.ICV30 <- setParameters(OMrefY19.4.500, parameters=list(IACin=0.5, Icv=c(0.30000,0.30001)))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.ICV30  <- runMse(OMrobY19.4.ICV30, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))


YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.ICV30, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.ICV30)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.ICV30)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 2:  OMrobY19.4.10overRep 
#   10% reported over catch
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears

# set 10% reported over catch implemented as 1.1 times TAC
OMrobY19.4.10overRep <- setParameters(OMrefY19.4.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears))))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.10overRep  <- runMse(OMrobY19.4.10overRep, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.10overRep, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(OMrobY19.4.10overRep)
projd <- msevizProjectedTimeSeriesData(OMrobY19.4.10overRep)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 3:  OMrobY19.4.10overIUU
#   10% unreported over catch
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears

# set 10% unreported under catch implemented as 1.1 times TAC and catch bias 1/1.1
OMrobY19.4.10overIUU <- setParameters(OMrefY19.4.500, parameters=list(ImplErrBias=as.karray(rep(1.1, times=proyears)), Cbmean=1.0 / 1.1))
MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.10overIUU  <- runMse(OMrobY19.4.10overIUU, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.10overIUU, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.10overIUU)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.10overIUU)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)





# -----------------------------------------
# Case 4: OMrobY19.4.qTrend2 
#   CPUE catchability trend of 2% per annum
# -----------------------------------------

proyears <- OMrefY19.4.500@MseDef@proyears
# set CPUE catchability trend of 3% per annum
OMrobY19.4.qTrend2 <- setParameters(OMrefY19.4.500, parameters=list(ITrendin=2.0))

print(system.time(mseOMrobY19.4.qTrend2 <- runMse(OMrobY19.4.qTrend2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.qTrend2, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.qTrend2)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.qTrend2)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)



# -----------------------------------------
# Case 5: OMrobY19.4.recShock 
#   recruitment failure of 50% for 8 quarters starting near beginning of projections
# -----------------------------------------
proyears <- OMrefY19.4.500@MseDef@proyears

OMrobY19.4.recShock <- setParameters(OMrefY19.4.500, parameters=list(RecScale = as.karray(c(rep(1,times=4),rep(0.55,times=2),rep(1,times=proyears-6)))))

MPL <- c(M.Y3,D.Y2) 
print(system.time(mseOMrobY19.4.recShock  <- runMse(OMrobY19.4.recShock, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseOMrobY19.4.recShock, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)


histd <- msevizHistoricTimeSeriesData(mseOMrobY19.4.recShock)
projd <- msevizProjectedTimeSeriesData(mseOMrobY19.4.recShock)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)



# -----------------------------------------
# Case 0: just rerun the refernce set to make comparable plots with the robustness sets 
# -----------------------------------------

MPL <- c(M.Y3,D.Y2) 
print(system.time(mseTmp  <- runMse(OMrefY19.4.500, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))

YearsAveraged <- 20  
perfd    <-   msevizPerformanceData(mseTmp, YearsAveraged)
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



histd <- msevizHistoricTimeSeriesData(mseTmp)
projd <- msevizProjectedTimeSeriesData(mseTmp)
projd <- projd[projd$year <= 2040,]
lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr, Cref=409000)






gc()











zzz old below here



perfd <- perfd[perfd$mp %in% c("M.Y18.1", "D.Y18.1", "M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

rename <- list(M.Y18.1 ="MY1", D.Y18.1 ="DY1",
               M.Y18.2 ="MY2", D.Y18.2 ="DY2",
               M.Y18.3 ="MY3", D.Y18.3 ="DY3") 

if (!is.na(rename))
{
  substitute <- function(names)
  {
    return (sapply(as.vector(names), FUN=function(name){rename[[name]]}))
  }
  
  perfd[,"mp"] <- factor(perfd[,substitute(mp)], levels=rename)
}
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

require(devEMF)

#as emf files
emf(file="Report/OMrefY19.3.500.TY18.BPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.TOs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.KobeMPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()







MPL <- list(MY2a ="PT41.t50",  MY2b = "PT41.x60t25", MY2c = "PT41.t90",  MY2d = "PT80.t50",
            DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )




###################################################################################
# tune 3 MPs X 3 tuning objectives

# fails with TAC limit 0f 50%, okay with 90%
MPL <- list(M.Y18.1 = MPList0[4], D.Y18.1 = MPList0[5], C.Y18.1 = MPList0[3])
#OMrefY19.3.500.TY18.1.test <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL[1], CppMethod=1, interval=3, Report=F, UseCluster=0)
print(system.time(OMrefY19.3.500.TY18.1  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.1, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.1.MPs <- getMPs(OMrefY19.3.500.TY18.1)
TY18.1.MPs[[1]]@tuneError
TY18.1.MPs[[2]]@tuneError
TY18.1.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.1,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.1.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.1.RDA")

# fails with TAC limit 0f 25%, ok with 50%
MPL <- list(M.Y18.2 = MPList0[6], D.Y18.2 = MPList0[7], C.Y18.2 = MPList0[3])
print(system.time(OMrefY19.3.500.TY18.2  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2.MPs <- getMPs(OMrefY19.3.500.TY18.2)
TY18.2.MPs[[1]]@tuneError
TY18.2.MPs[[2]]@tuneError
TY18.2.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.2,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.2.RDA")

# fails with TAC limit 0f 15%, ok with 25%
MPL <- list(M.Y18.3 = MPList0[1], D.Y18.3 = MPList0[2], C.Y18.3 = MPList0[3])
print(system.time(OMrefY19.3.500.TY18.3  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.3.MPs <- getMPs(OMrefY19.3.500.TY18.3)
TY18.3.MPs[[1]]@tuneError
TY18.3.MPs[[2]]@tuneError
TY18.3.MPs[[3]]@tuneError
save(OMrefY19.3.500.TY18.3,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.3.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.3.RDA")

# This fails to converge
# # try 15% limits
# MPL <- list(M.Y18.3 = MPList1[1], D.Y18.3 = MPList1[2], C.Y18.3 = MPList1[3])
# print(system.time(OMrefY19.3.500.TY18.3t  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.3, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
# TY18.3.MPs <- getMPs(OMrefY19.3.500.TY18.3t)
# TY18.3.MPs[[1]]@tuneError
# TY18.3.MPs[[2]]@tuneError
# TY18.3.MPs[[3]]@tuneError
# save(OMrefY19.3.500.TY18.3t,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.3t.RDA",sep=""))
# load(file="Objects/OMrefY19.3.500.TY18.3t.RDA")


###################################################################################
# plot the 3 MPs X 3 tuning objectives aggregate plots

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.1, YearsAveraged)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.3, YearsAveraged), all=TRUE)

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4)

#select and rename perfd MPs
perfd <- perfd[perfd$mp %in% c("M.Y18.1", "D.Y18.1", "M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

rename <- list(M.Y18.1 ="MY1", D.Y18.1 ="DY1",
               M.Y18.2 ="MY2", D.Y18.2 ="DY2",
               M.Y18.3 ="MY3", D.Y18.3 ="DY3") 

if (!is.na(rename))
{
  substitute <- function(names)
  {
    return (sapply(as.vector(names), FUN=function(name){rename[[name]]}))
  }
  
  perfd[,"mp"] <- factor(perfd[,substitute(mp)], levels=rename)
}
plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)

require(devEMF)

#as emf files
emf(file="Report/OMrefY19.3.500.TY18.BPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.TOs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.KobeMPs.emf", width=7, height=7)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()


#as png files
png(file="Report/OMrefY19.3.500.TY18.BPs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

png(file="Report/OMrefY19.3.500.TY18.TOs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()

png(file="Report/OMrefY19.3.500.TY18.KobeMPs.png", width=588, height=588)
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=3,ymax=4), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::popViewport()
dev.off()


###################################################################################
# plot time series of 2 feedback-based MPs X 3 tuning objectives time series plots
histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.1)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)
histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.3), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.1)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)
projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.3), all=TRUE)

projd <- projd[projd$year <= 2040,]

#select and rename projd MPs
projd <- projd[projd$mp %in% c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3"),]

rename <- list(M.Y18.1 ="MY1", D.Y18.1 ="DY1",
               M.Y18.2 ="MY2", D.Y18.2 ="DY2",
               M.Y18.3 ="MY3", D.Y18.3 ="DY3") 

if (!is.na(rename))
{
  substitute <- function(names)
  {
    return (sapply(as.vector(names), FUN=function(name){rename[[name]]}))
  }
  
  projd[,"mp"] <- factor(projd[,substitute(mp)], levels=rename)
}


lastHistYr <- 2017
firstMPYr  <- 2021

plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

# as metafiles
emf(file="Report/OMrefY19.3.500.TY18.KobeCols.emf", width=7, height=7)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.Recruitment.emf", width=7, height=7)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.SSB_SSBMSY.emf", width=7, height=7)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.F_FMSY.emf", width=7, height=7)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

emf(file="Report/OMrefY19.3.500.TY18.C.emf", width=7, height=7)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", Cref=Cref*1000, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

# as png files
png(file="Report/OMrefY19.3.500.TY18.KobeCols.png", width=588, height=588)
plotKobeCols(om=histd, runs=projd, lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.Recruitment.png", width=588, height=588)
plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.SSB_SSBMSY.png", width=588, height=588)
plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.F_FMSY.png", width=588, height=588)
plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

png(file="Report/OMrefY19.3.500.TY18.C.png", width=588, height=588)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
dev.off()

MP_names    <- c("M.Y18.1","D.Y18.1","M.Y18.2","D.Y18.2","M.Y18.3","D.Y18.3")
MP_newnames <- c("MY1",    "DY1",    "MY2",    "DY2"    ,"MY3",   "DY3")


results <- list(OMrefY19.3.500.TY18.1, OMrefY19.3.500.TY18.2, OMrefY19.3.500.TY18.3)
createTable1(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(1,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(5,  results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(10, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")
createTable2(20, results, MPs=MP_names, MPs_short=MP_newnames, prefix="OMrefY19.3.500.TY18.")



##################################################################################################
# Contrasting MPs for tuning level 2 only

#MPListC <- c("CC001","CC413")
#MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t50","IT5.t50")
#MPList0 <- c("PT41.t25","IT5.t25", "CCt", "PT41.t90", "IT5.t90", "PT41.t50","IT5.t50")
#MPList1 <- c("PT41.t15","IT5.t15","CCt", "PT41.t10","PT80.t15", "IT5.t10", "IT10.t15", "IT5.t15.l1")
#MPList2 <- c("PT41F.t15","PT41F.t10","PT80F.t15")


MPL <- list(MY2a ="PT41.t50",  MY2b = "PT41.x60t25", MY2c = "PT41.t90",  MY2d = "PT80.t50",
            DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )


#MPL <- list(MY2b = MPList0[6])

print(system.time(OMrefY19.3.500.TY18.2xC  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2xC.MPs <- getMPs(OMrefY19.3.500.TY18.2xC)
for(i in 1:length(MPL)){
  print(c(i,MPL[[i]],TY18.2xC.MPs[[1]]@tuneError))
}
save(OMrefY19.3.500.TY18.2xC,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2xC.RDA",sep=""))
load(file="Objects/OMrefY19.3.500.TY18.2xC.RDA")



histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2xC)
#histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2xC)
#projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- projd[projd$year <= 2040,]

#6 MPs with highest contrast
#projd <- projd[projd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]
projd <- projd[projd$mp %in% c("DY2a","DY2b","DY2c","DY2d","DY2e","DY2f","DY2g" ),]


lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CPUE(aggregate)", ylab= "CPUE", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "CoTAC", ylab= "C / TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.2xC, YearsAveraged)
#perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- perfd[perfd$mp %in% c("MY2a","MY2b","MY2d","DY2a","DY2b","DY2f" ),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)




##################################################################################################
# additional Contrasting MPs for tuning level 2 only

MPL2 <- list(MY2d = "PT80.t50", MY2e = "PT120.t50", MY2f = "PT80.t90", MY2g = "PT80.t35", MY2h = "PT80.t39")
MPL3 <- list(MY2i = "PT120.t50.25", MY2j = "PT120.t75.15", MY2k = "PT120.t50.15")

# DY2a = "IT5.t50" , DY2b = "IT5.x60t15",  DY2c = "IT5.x60t25", DY2d = "IT3.t50", DY2e = "IT5.t50g3", DY2f = "IT5.t50g1313", DY2g = "IT5.t50g3131" )
# MPL4 <- list(DY2b = "IT5.t50g20", DY2c = "IT5.t50g60", DY2d = "IT5.t50g20L2", DY2e = "IT5.t50g60L2")
#MPL4 <- list(DY2g = "IT5.t50g20x.5") #probsably best IT series so far , DY2f = "IT5.t50g20x2")
#MPL4 <- list(IT5.t50g60x.1 = "IT5.t50g60x.1", IT5.t50g60x.5 = "IT5.t50g60x.5", IT5.t50g60x2 = "IT5.t50g60x2") first two fail to plot, third okay
MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2", IT5.t50g60x.1 = "IT5.t50g60x.1", IT5.t50g60x.5 = "IT5.t50g60x.5", IT5.t50g60x2 = "IT5.t50g60x2")
MPL4 <- list(IT5.t50g60x.1 = "IT5.t50g60x.1") #tune failure
MPL4 <- list(IT5.t50g60x2 = "IT5.t50g60x2") 
MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2", IT5.t50g20x.5 = "IT5.t50g20x.5", IT5.t50g60x2 = "IT5.t50g60x2", IT5.t50g20 = "IT5.t50g20") 
MPL4 <- list(IT5.t60.25g20 = "IT5.t60.25g20", IT5.t60.25g20x2 = "IT5.t60.25g20x2") #nothing special here
MPL4 <- list(IT5.t50.25g20 = "IT5.t50.25g20x.5", IT5.t60.25g20x2 = "IT5.t50.25g20x2") 


MPL4 <- list(IT5.t50g20x.2 = "IT5.t50g20x.2")
MPL4 <- list(IT5.t50.CAT = "IT5.t50.CAT", IT5.t50 = "IT5.t50")
MPL4 <- list(IT5.t50.g51.CAT = "IT5.t50.g51.CAT",IT5.t50.g15.CAT = "IT5.t50.g15.CAT", IT5.t50.g1m5.CAT = "IT5.t50.g1m5.CAT",
             IT5.t50.g51 = "IT5.t50.g51",IT5.t50.g15 = "IT5.t50.g15", IT5.t50.g1m5 = "IT5.t50.g1m5")
MPL4 <- list(IT5.t50.g55 = "IT5.t50.g55", IT5.t50.g55L10 = "IT5.t50.g55L10",  IT5.t50.g5m5 = "IT5.t50.g5m5",   IT5.t50.g5m5L10 = "IT5.t50.g5m5L10")
MPL4 <- list(IT5.t50.g5m1 = "IT5.t50.g5m1",   IT5.t50.g5m1L10 = "IT5.t50.g5m1L10")
MPL4 <- list(IT5.t50.g5m2 = "IT5.t50.g5m2", IT5.t50.g5m1 = "IT5.t50.g5m1")
MPL4 <- list(IT5.t50.g52 = "IT5.t50.g55", IT5.t50.g51 = "IT5.t50.g51")
MPL4 <- list(IT5.t50b = "IT5.t50d", IT5.t50c = "IT5.t50e")


TCMP.Y18.2@tuningLogDomain         =  c(-4,4)      # c(.3,1.1) 
TCMP.Y18.2@tuningTolerance          <- 0.05 # 0.05

print(system.time(OMrefY19.3.500.TY18.2xD  <- runMse(OMrefY19.3.500, TuningPars=TCMP.Y18.2, MPs=MPL4, CppMethod=1, interval=3, Report=F, UseCluster=1)))
TY18.2xD.MPs <- getMPs(OMrefY19.3.500.TY18.2xD)
for(i in 1:length(MPL4)){
  print(c(i,MPL4[[i]],TY18.2xD.MPs[[1]]@tune))
  print(c(i,MPL4[[i]],TY18.2xD.MPs[[1]]@tuneError))
}
#save(OMrefY19.3.500.TY18.2xD,file=paste(getwd(),"/Objects/OMrefY19.3.500.TY18.2xD.RDA",sep=""))
#load(file="Objects/OMrefY19.3.500.TY18.2xD.RDA")

#Use tuned value
print(system.time(testMSE  <- runMse(OMrefY19.3.500, MPs=TY18.2xD.MPs, CppMethod=1, interval=3, Report=F, UseCluster=0)))







histd <- msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2xD)
#histd <- merge(histd,msevizHistoricTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2xD)
#projd <- merge(projd, msevizProjectedTimeSeriesData(OMrefY19.3.500.TY18.2), all=TRUE)

projd <- projd[projd$year <= 2040,]

#6 MPs with highest contrast
#projd <- projd[projd$mp %in% c('IT5.t50g60x.1', 'IT5.t50g60x.5', 'IT5.t50g60x2')]
#projd <- projd[projd$mp %in% c('IT5.t50g60x.1', 'IT5.t50g60x.5')]
#projd <- projd[projd$mp %in% c('IT5.t50g60x2')]


lastHistYr <- 2017
firstMPYr  <- 2021

plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C", ylab= "Catch (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
plotOMruns2(histd, projd, "C/TAC", ylab= "Catch / TAC", lastHistYr = lastHistYr, firstMPYr = firstMPYr)
#plotOMruns2(histd, projd, "TAC", ylab= "TAC (1000t)", lastHistYr = lastHistYr, firstMPYr = firstMPYr)







plotCbyTAC(projd, firstMPYr = 2021)

totalCAT   <- as.numeric(unlist(projd[projd$qname=="C","data"]))
TAC  <- as.numeric(unlist(projd[projd$qname=="TAC","data"]))
plot(x=TAC,y=totalCAT)
lines(TAC,TAC, col=2)

Ctmp     <- projd[projd$qname=="C",]
TACtmp   <- projd[projd$qname=="TAC",]
Ctmp2    <- tmp[tmp$data>600000,]
TACtmp2  <- tmp[tmp$data>600000,]
plot(as.numeric(TACtmp2$data),as.numeric(Ctmp2$data))

TACtmp <- projd[projd$model=="h90_M08_t0001_q1_iH_i1_iR2_gr2_CL75_SD_x4",]

YearsAveraged <- 20

perfd <- msevizPerformanceData(OMrefY19.3.500.TY18.2xD, YearsAveraged)
#perfd <- merge(perfd, msevizPerformanceData(OMrefY19.3.500.TY18.2, YearsAveraged), all=TRUE)
perfd <- perfd[perfd$mp %in% c("MY2e"),]

plotBPs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
plotTOs2(perfd, limit=YFTLims, target=YFTTargs, blackRef=Cref)
kobeMPs2(perfd, xlim=SBLim, ylim=FLim, xmax=2,ymax=2)



