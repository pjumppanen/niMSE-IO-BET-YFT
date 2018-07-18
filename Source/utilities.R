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
               "GKmean",
               "PrSBgtSBlimmean",
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
               "FoFtarg0.5",
               "GKmean",
               "RKmean",
               "PrSBgt0.2SB0mean",
               "PrSBgtSBlimmean",
               "Y0.5",
               "YoMSY0.5",
               "relCPUE0.5",
               "AAVY0.5",
               "YcvPct0.5",
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


#------------------------------------------------------------------------------
# Function to create table 1 results for IOTC report in word XML document
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15',  'TB2.IT5.t15',
#                  'TB3.PT41.t15',  'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# finalMPList_short <- c('TB1.M', 'TB1.D',
#                        'TB2.M', 'TB2.D',
#                        'TB3.M', 'TB3.D',
#                        'TB4.M', 'TB4.D')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# createTable1(20, results, MPs=finalMPList, MPs_short=finalMPList_short)
#------------------------------------------------------------------------------
createTable1 <- function(years, results, MPs, MPs_short)
{
  if (length(MPs) > 8)
  {
    print("ERROR: maximum number of MPs is 8")
    stop()
  }

  Table1 <- findTable1_Data(years, results, MPs)

  MPs                 <- Table1[,j=MP]
  SBs                 <- Table1[,j=SBoSBMSY0.5]
  SB25s               <- Table1[,j=SBoSBMSY0.25]
  SB75s               <- Table1[,j=SBoSBMSY0.75]
  GKs                 <- Table1[,j=GKmean]
  PrSBgtLims          <- Table1[,j=PrSBgtSBlimmean]
  Catches             <- Table1[,j=Y0.5]
  Catch25s            <- Table1[,j=Y0.25]
  Catch75s            <- Table1[,j=Y0.75]
  Catch_variabilities <- Table1[,j=AAVY0.5]

  DarkShade   <- 166
  LightShade  <- 255

  GreyShade <- function(x) {return (paste(x,x,x, sep=""))}

  SB_shade                  <- GreyShade(as.hexmode(DarkShade + floor((max(SBs) - SBs) / (max(SBs) - min(SBs)) * (LightShade - DarkShade))))
  GK_shade                  <- GreyShade(as.hexmode(DarkShade + floor((max(GKs) - GKs) / (max(GKs) - min(GKs)) * (LightShade - DarkShade))))
  PrSBgtLims_shade          <- GreyShade(as.hexmode(DarkShade + floor((max(PrSBgtLims) - PrSBgtLims) / (max(PrSBgtLims) - min(PrSBgtLims)) * (LightShade - DarkShade))))
  Catches_shade             <- GreyShade(as.hexmode(DarkShade + floor((max(Catches) - Catches) / (max(Catches) - min(Catches)) * (LightShade - DarkShade))))
  Catch_variabilities_shade <- GreyShade(as.hexmode(DarkShade + floor((Catch_variabilities - min(Catch_variabilities)) / (max(Catch_variabilities) - min(Catch_variabilities)) * (LightShade - DarkShade))))

  xml <- paste(readLines("./templates/table_1_template.xml.txt"), collapse="\n")

  for (idx in 1:length(MPs))
  {
    MP_tag                        <- paste("\\{MP", idx, "\\}", sep="")
    SB_tag                        <- paste("\\{SB", idx, "\\}", sep="")
    SB25_tag                      <- paste("\\{SB", idx, "a\\}", sep="")
    SB75_tag                      <- paste("\\{SB", idx, "b\\}", sep="")
    SB_colour_tag                 <- paste("\\{SB", idx, "col\\}", sep="")
    KobeGreen_tag                 <- paste("\\{KG", idx, "\\}", sep="")
    KobeGreen_colour_tag          <- paste("\\{KG", idx, "col\\}", sep="")
    PrSBgtLim_tag                 <- paste("\\{PSB", idx, "\\}", sep="")
    PrSBgtLim_colour_tag          <- paste("\\{PSB", idx, "col\\}", sep="")
    Catch_tag                     <- paste("\\{Y", idx, "\\}", sep="")
    Catch25_tag                   <- paste("\\{Y", idx, "a\\}", sep="")
    Catch75_tag                   <- paste("\\{Y", idx, "b\\}", sep="")
    Catch_colour_tag              <- paste("\\{Y", idx, "col\\}", sep="")
    Catch_variability_tag         <- paste("\\{CV", idx, "\\}", sep="")
    Catch_variability_colour_tag  <- paste("\\{CV", idx, "col\\}", sep="")

    xml <- gsub(MP_tag,        MPs_short[idx],               xml)
    xml <- gsub(SB_tag,        format(SBs[idx],   digits=2), xml)
    xml <- gsub(SB25_tag,      format(SB25s[idx], digits=2), xml)
    xml <- gsub(SB75_tag,      format(SB75s[idx], digits=2), xml)
    xml <- gsub(SB_colour_tag, SB_shade[idx],                xml)

    xml <- gsub(KobeGreen_tag,        format(GKs[idx], digits=2), xml)
    xml <- gsub(KobeGreen_colour_tag, GK_shade[idx],              xml)

    xml <- gsub(PrSBgtLim_tag,        format(PrSBgtLims[idx], digits=2), xml)
    xml <- gsub(PrSBgtLim_colour_tag, PrSBgtLims_shade[idx],             xml)

    xml <- gsub(Catch_tag,        format(Catches[idx], digits=2),  xml)
    xml <- gsub(Catch25_tag,      format(Catch25s[idx], digits=2), xml)
    xml <- gsub(Catch75_tag,      format(Catch75s[idx], digits=2), xml)
    xml <- gsub(Catch_colour_tag, Catches_shade[idx],              xml)

    xml <- gsub(Catch_variability_tag,        format(Catch_variabilities[idx], digits=2), xml)
    xml <- gsub(Catch_variability_colour_tag, Catch_variabilities_shade[idx],             xml)
  }

  for (idx in length(MPs):8)
  {
    MP_tag                        <- paste("\\{MP", idx, "\\}", sep="")
    SB_tag                        <- paste("\\{SB", idx, "\\}", sep="")
    SB25_tag                      <- paste("\\{SB", idx, "a\\}", sep="")
    SB75_tag                      <- paste("\\{SB", idx, "b\\}", sep="")
    SB_colour_tag                 <- paste("\\{SB", idx, "col\\}", sep="")
    KobeGreen_tag                 <- paste("\\{KG", idx, "\\}", sep="")
    KobeGreen_colour_tag          <- paste("\\{KG", idx, "col\\}", sep="")
    PrSBgtLim_tag                 <- paste("\\{PSB", idx, "\\}", sep="")
    PrSBgtLim_colour_tag          <- paste("\\{PSB", idx, "col\\}", sep="")
    Catch_tag                     <- paste("\\{Y", idx, "\\}", sep="")
    Catch25_tag                   <- paste("\\{Y", idx, "a\\}", sep="")
    Catch75_tag                   <- paste("\\{Y", idx, "b\\}", sep="")
    Catch_colour_tag              <- paste("\\{Y", idx, "col\\}", sep="")
    Catch_variability_tag         <- paste("\\{CV", idx, "\\}", sep="")
    Catch_variability_colour_tag  <- paste("\\{CV", idx, "col\\}", sep="")

    xml <- gsub(MP_tag,        "",       xml)
    xml <- gsub(SB_tag,        "",       xml)
    xml <- gsub(SB25_tag,      "",       xml)
    xml <- gsub(SB75_tag,      "",       xml)
    xml <- gsub(SB_colour_tag, "ffffff", xml)

    xml <- gsub(KobeGreen_tag,        "",       xml)
    xml <- gsub(KobeGreen_colour_tag, "ffffff", xml)

    xml <- gsub(PrSBgtLim_tag,        "",       xml)
    xml <- gsub(PrSBgtLim_colour_tag, "ffffff", xml)

    xml <- gsub(Catch_tag,        "",       xml)
    xml <- gsub(Catch25_tag,      "",       xml)
    xml <- gsub(Catch75_tag,      "",       xml)
    xml <- gsub(Catch_colour_tag, "ffffff", xml)

    xml <- gsub(Catch_variability_tag,        "",       xml)
    xml <- gsub(Catch_variability_colour_tag, "ffffff", xml)
  }

  Xmlfile <- file(paste("./Report/Table_1_", years, "yr.xml", sep=""), "wt")
  writeLines(xml, con=Xmlfile)
  close(Xmlfile)
}


#------------------------------------------------------------------------------
# Function to create table 2 results for IOTC report in word XML document
#------------------------------------------------------------------------------
# Typical usage:
#
# finalMPList <- c('TB1.1PT41.t25', 'TB1.1IT5.t25',
#                  'TB2.PT41.t15',  'TB2.IT5.t15',
#                  'TB3.PT41.t15',  'TB3.IT5.t15',
#                  'TB4.PT41.t15', 'TB4.IT5.t15')
#
# finalMPList_short <- c('TB1.M', 'TB1.D',
#                        'TB2.M', 'TB2.D',
#                        'TB3.M', 'TB3.D',
#                        'TB4.M', 'TB4.D')
#
# results <- list(mseOMrefB18.2.304.TB1.1,
#                 mseOMrefB18.2.304.TB2,
#                 mseOMrefB18.2.304.TB3,
#                 mseOMrefB18.2.304.TB4)
#
# createTable2(20, results, MPs=finalMPList, MPs_short=finalMPList_short)
#------------------------------------------------------------------------------
createTable2 <- function(years, results, MPs, MPs_short)
{
  if (length(MPs) > 8)
  {
    print("ERROR: maximum number of MPs is 8")
    stop()
  }

  Table2 <- findTable2_Data(years, results, MPs)
  rowMap <- list(SBoSB00.5=1,
                 minSBoSB00.5=2,
                 SBoSBMSY0.5=3,
                 FoFMSY0.5=4,
                 FoFtarg0.5=5,
                 GKmean=6,
                 RKmean=7,
                 PrSBgt0.2SB0mean=8,
                 PrSBgtSBlimmean=9,
                 Y0.5=10,
                 YoMSY0.5=12,
                 relCPUE0.5=11,
                 AAVY0.5=13,
                 YcvPct0.5=14,
                 PrYlt0.1MSYmean=15)

  MPs   <- colnames(Table2)
  stats <- rownames(Table2)

  xml <- paste(readLines("./templates/table_2_template.xml.txt"), collapse="\n")
  xml <- gsub("\\{PERIOD\\}", paste(years, "year average"), xml)

  for (cn in 1:length(MPs))
  {
    tag <- paste("\\{MP", cn, "\\}", sep="")
    xml <- gsub(tag, finalMPList_short[cn], xml)
  }

  for (cn in 1:length(stats))
  {
    nrow <- rowMap[[stats[cn]]]

    for (ncol in 1:length(MPs))
    {
      tag   <- paste("\\{", nrow, "\\.", ncol, "\\}", sep="")
      value <- format(Table2[nrow, ncol], digits=2)
      xml   <- gsub(tag, value, xml)
    }

    for (cm in length(MPs):8)
    {
      tag   <- paste("\\{", nrow, "\\.", ncol, "\\}", sep="")
      value <- ""
      xml   <- gsub(tag, value, xml)
    }
  }

  Xmlfile <- file(paste("./Report/Table_2_", years, "yr.xml", sep=""), "wt")
  writeLines(xml, con=Xmlfile)
  close(Xmlfile)
}
