#------------------------------------------------------------------------------
# Function to calculate catch by fleet for a number of MseFramework results
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15', 'TB2.IT5.t15',
#                  'TB3.PT41.t15', 'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# CbyF_1 <- findCatchByFleet(1, results=results, MPs=finalMPList)
# CbyF_5 <- findCatchByFleet(5, results=results, MPs=finalMPList)
# CbyF_10 <- findCatchByFleet(10, results=results, MPs=finalMPList)
# CbyF_20 <- findCatchByFleet(20, results=results, MPs=finalMPList)
#------------------------------------------------------------------------------
findCatchByFleet <- function(years, percentiles=c(0.1,0.25,0.5,0.75,0.8,0.9), results, MPs)
{
  last <- NULL

  for (result in results)
  {
    stats <- performanceStatistics(result, c("Yf"), years, percentiles=percentiles, appendTo=last)
    last  <- stats
  }

  cnames  <- t(stats[MP %in% MPs, j=c("MP")])
  nfleets <- (ncol(stats) - 1) / (1 + length(percentiles))
  rnames  <- paste("fleet", 1:nfleets)
  results <- array(as.double(NA), dim=c(nfleets, length(cnames)))
  rownames(results) <- rnames
  colnames(results) <- cnames

  for (cn in 1:nfleets)
  {
    col <- paste("Yf", cn, ".mean", sep="")
    results[cn,] <- t(stats[MP %in% MPs, get(col)])
  }

  return (results)
}


#------------------------------------------------------------------------------
# Function to calculate CPUE by area for a number of MseFramework results
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15', 'TB2.IT5.t15',
#                  'TB3.PT41.t15', 'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# CPUE_byA_1 <- findCPUE_ByArea(1, results=results, MPs=finalMPList)
# CPUE_byA_5 <- findCPUE_ByArea(5, results=results, MPs=finalMPList)
# CPUE_byA_10 <- findCPUE_ByArea(10, results=results, MPs=finalMPList)
# CPUE_byA_20 <- findCPUE_ByArea(20, results=results, MPs=finalMPList)
#------------------------------------------------------------------------------
findCPUE_ByArea <- function(years, percentiles=c(0.1,0.25,0.5,0.75,0.8,0.9), results, MPs)
{
  last <- NULL

  for (result in results)
  {
    stats <- performanceStatistics(result, c("CPUEr"), years, percentiles=percentiles, appendTo=last)
    last  <- stats
  }

  cnames  <- t(stats[MP %in% MPs, j=c("MP")])
  nareas  <- (ncol(stats) - 1) / (1 + length(percentiles))
  rnames  <- paste("area", 1:nareas)
  results <- array(as.double(NA), dim=c(nareas, length(cnames)))
  rownames(results) <- rnames
  colnames(results) <- cnames

  for (cn in 1:nareas)
  {
    col <- paste("CPUEr", cn, ".mean", sep="")
    results[cn,] <- t(stats[MP %in% MPs, get(col)])
  }

  return (results)
}


#------------------------------------------------------------------------------
# Function to table 1 results for IOTC report
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15', 'TB2.IT5.t15',
#                  'TB3.PT41.t15', 'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# Table1 <- findTable1_Data(20, results, MPs=finalMPList)
#------------------------------------------------------------------------------
findTable1_Data <- function(years, results, MPs)
{
  last       <- NULL
  statistics <- c("SBoSBMSY",
                 "GK",
                 "PrSBgtSBlim",
                 "Y",
                 "AAVY")
  percentiles <- c(0.25,0.5,0.75)

  for (result in results)
  {
    stats <- performanceStatistics(result, statistics, years, percentiles=percentiles, appendTo=last)
    last  <- stats
  }

  cnames  <- c("SBoSBMSY0.5",
               "GK0.5",
               "PrSBgtSBlim0.5",
               "Y0.5",
               "AAVY0.5",
               "SBoSBMSY0.25",
               "SBoSBMSY0.75",
               "Y0.25",
               "Y0.75")

  return (stats[MP %in% MPs, mget(c("MP",cnames))])
}


#------------------------------------------------------------------------------
# Function to table 2 results for IOTC report
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15', 'TB2.IT5.t15',
#                  'TB3.PT41.t15', 'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# Table2_20 <- findTable2_Data(20, results, MPs=finalMPList)
#------------------------------------------------------------------------------
findTable2_Data <- function(years, results, MPs)
{
  last       <- NULL
  statistics <- c("SBoSB0",
                  "minSBoSB0",
                  "SBoSBMSY",
                  "FoFMSY",
                  "FoFtarg",
                  "GK",
                  "RK",
                  "PrSBgt0.2SB0",
                  "PrSBgtSBlim",
                  "Y",
                  "YoMSY",
                  "relCPUE",
                  "AAVY",
                  "YcvPct",
                  "PrYlt0.1MSY")

  percentiles <- c(0.5)

  for (result in results)
  {
    stats <- performanceStatistics(result, statistics, years, percentiles=percentiles, appendTo=last)
    last  <- stats
  }

  rnames  <- c("SBoSB00.5",
               "minSBoSB00.5",
               "SBoSBMSY0.5",
               "FoFMSY0.5",
               "FoFtargmean",
               "GKmean",
               "RKmean",
               "PrSBgt0.2SB0mean",
               "PrSBgtSBlimmean",
               "Ymean",
               "YoMSYmean",
               "relCPUEmean",
               "AAVYmean",
               "YcvPctmean",
               "PrYlt0.1MSYmean")

  stats <- stats[MP %in% MPs, mget(c("MP",rnames))]

  cnames  <- t(stats[, j=c("MP")])
  results <- array(as.double(NA), dim=c(length(rnames), length(cnames)))
  rownames(results) <- rnames
  colnames(results) <- cnames

  for (cn in 1:length(rnames))
  {
    results[cn,] <- t(stats[, get(rnames[cn])])
  }

  return (results)
}

