####################################################
# run standalone MP given catch and LL CPUE series #
####################################################
# P. Jumppanen & R. Hillary CSIRO 2022 #############
####################################################

# new file containing the testing code

source("./Source/AssessMP.R")

# number of years between MP runs

MP_Interval <- 1

# source .csv file with y, Cobs and Iobs data

CatchAndCPUEcsv <- "./Source/mpdata.csv"

# path to MP code source

MP_SourcePath   <- "./MPs/PTTMB/MPs_TMBMSY_tidied.R"

# name of MP being tested

#MP_Name <- "PT41F.t15.tmb"
MP_Name <- "PTBoB0Targ.t15"

# tuning objective 

#tunobj <- "B2"
tunobj <- "B3"

# get tuning parameter

tpar <- read.csv("./Source/tuning_params.csv",header=T)
theta <- subset(tpar,MP == MP_Name & tuning == tunobj)$par

# call to test it

results <- assessMP(MP_Name, MP_SourcePath, CatchAndCPUEcsv, MP_Interval, theta, Build=TRUE)

# results in the environment results.

print(results$TAC)
print(results$B)
print(results$Depletion)
print(results$q)
