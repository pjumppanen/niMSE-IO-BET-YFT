# -----------------------------------------------------------------------------
# Joint CPUE - 4 regions
# -----------------------------------------------------------------------------
# Adapted from CPUE.BET.input.R to cater for a changed input file format.
# -----------------------------------------------------------------------------
# Modification of Dan Fu's original YFT CPUE procesing file (2018?).
# Selects the CPUE series from Hoyle output files, applies the scaling factor 
# and outputs a text file for the makeGrid code to use. makeGrid selects the 
# weighted series, adds the CV and catchability trend. Adding a new regional 
# weighting factor option requires this file to be edited and re-run.
#
# Extra step required in 2019 - splice together 1979+ and 1979- series with 1979 
# as the calibration year. Assesment used 4 seasonal series with independent 
# q (check selectivity). For temperate region, OM normalizes seasonal series 
# and merges (by non-NA common observations) - should reconfirm again that 
# this does not matter for MSE.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# DK append string operator
# -----------------------------------------------------------------------------
"%&%" <- function(string1, string2){return(paste(string1,string2,sep=""))} 


# -----------------------------------------------------------------------------
# DK fn convert decimal year to SS quarters as years format
# -----------------------------------------------------------------------------
decYrToSeasAsYr <- function(decYr,
                            endSeasAsYr=356,
                            numSeas=4,
                            endYr=2015,
                            endSeas=4)
{
  seasAsYr <- endSeasAsYr - numSeas * ((endYr + endSeas / numSeas - 1 / (2 * numSeas)) - decYr)
  
  return(seasAsYr)
}


# -----------------------------------------------------------------------------
# assessment CPUE
# -----------------------------------------------------------------------------
assessmentCPUE <- function(wdCPUE, exportFileName, MPDatFile, IsSeasonal=FALSE)
{
  # from Hoyle 2018 (not updated in 2019)
  # Fu 2019 document reports region 1(SWeq)=0.8, 2(Eeq)=1.0, 3+4(SW+SE)=0.63, 5(NWeq)=0.63
  # Sent email to check if this is a typo or if area 3+4 interpretation is more complicated 
  betwts <- list('7594 m8' = c(0.732, 1.0, 0.376+0.51,  0.629),
                 '7994 m8' = c(0.799, 1.0, 0.373+0.486, 0.626), # SH scaling region 1(SWeq)=s16, 2(Eeq)=s17, 3+4(SW+SE)=s18, 5(NWeq)=s19
                 '8000 m8' = c(0.687, 1.0, 0.489+0.601, 0.531))

  #YrQtr,R1N,R1S,R2,R3
  #1979,1.620150426,2.078522336,1.61262325,1.082704996
  CPUE_Data <- read.csv(file = wdCPUE %&% "IOTC_BET_JointCPUE_submitted.csv", header=T,as.is=T)
  CPUE_Data <- cbind(CPUE_Data, yr=as.integer(floor(CPUE_Data$YrQtr)), q=(CPUE_Data$YrQtr - floor(CPUE_Data$YrQtr)))

  if (IsSeasonal == FALSE)
  {
    #re-normalize temperate cpue by season and use single series (unlike assessment) 
    R34 <- cbind(subset(CPUE_Data, 
                        !is.na(R3),
                        select=c('YrQtr','R3','yr','q')),
                 AssessmentAreaName='3')

    names(R34)[2] <- "pr"

    #identify years with values for all seasons 
    allSeasYrs  <- as.numeric(names(table(R34$yr)[table(R34$yr) == 4]))
    R34renorm   <- R34

    for (q in c(0.0, 0.25, 0.5, 0.75))
    {
      R34renorm[R34$q == q, 'pr'] <- R34[R34$q == q, 'pr'] / mean(R34[R34$q == q & R34$yr %in% allSeasYrs, 'pr'])
    }

    par(mfrow=c(1,1))
    plot(R34$pr, type='l', ylab="Temperate CPUE", xlab="Time (quarter)")
    lines(R34renorm$pr, type='l', col=2)

    data <- rbind(# f16 = R1S = cpue region 1
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R3, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('1', length(CPUE_Data$YrQtr))),

                  # f17 = R2 = cpue region 2
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R2, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('2', length(CPUE_Data$YrQtr))),

                  # AssessmentAreaName 3 = f18, CPUE R3+R4
                  R34renorm,

                  # f19 = R1N = cpue region 5
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R1N, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('4', length(CPUE_Data$YrQtr))))
  }
  else
  {
    data <- rbind(# f16 = R1S = cpue region 1
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R1S, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('1', length(CPUE_Data$YrQtr))),

                  # f17 = R2  = cpue region 2
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R2, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('2', length(CPUE_Data$YrQtr))),

                  #AssessmentAreaName 3 = f18-21, CPUE R3+R4
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R3, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('3', length(CPUE_Data$YrQtr))),

                  # f22 = R1N = cpue region 5 
                  data.frame(YrQtr=CPUE_Data$YrQtr, pr=CPUE_Data$R1N, yr=CPUE_Data$yr, q=CPUE_Data$q, AssessmentAreaName=rep('4', length(CPUE_Data$YrQtr))))
  }

  data            <- data[!is.na(data$pr), ]
  data$pr_7594_m8 <- data$pr
  data$pr_7994_m8 <- data$pr
  data$pr_8000_m8 <- data$pr
  data$ssYr       <- decYrToSeasAsYr(data$YrQtr + 0.125)
  data$target     <- "Htrop"

  # Note that Dan used a custom function "Mean()" which might differ from mean() - (seemed to produce the same result when tested)
  index           <- data$AssessmentAreaName == '1'
  work            <- data[index,]
  work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][1]
  work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][1]
  work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][1]
  data[index,]    <- work

  index           <- data$AssessmentAreaName == '2'
  work            <- data[index,]
  work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][2]
  work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][2]
  work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][2]
  data[index,]    <- work

  index           <- data$AssessmentAreaName == '3'
  work            <- data[index,]
  work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][3]
  work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][3]
  work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][3]
  data[index,]    <- work

  index           <- data$AssessmentAreaName == '4'
  work            <- data[index,]
  work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][4]
  work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][4]
  work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][4]
  data[index,]    <- work

  #annualized, aggregated data for MP to use 
  MPdat           <- as.data.frame(cbind(data$yr, data$pr_7994_m8))
  colnames(MPdat) <- c("yr", "cpue")
  keepYrs         <- table(MPdat$yr) == 16 #only retain cpue observations with no non-mssing values (4 areas X 4 seasons) 
  keepYrs         <- names(keepYrs)[keepYrs]
  MPdat           <- MPdat[MPdat$yr %in% keepYrs,]
  MPdat           <- aggregate(MPdat$cpue, FUN=sum, by=list(MPdat$yr))
  colnames(MPdat) <- c("yr","cpue")
  MPdat$cpue      <- MPdat$cpue / mean(MPdat$cpue)

  plot(MPdat$yr, MPdat$cpue, type='b')

  write.table(MPdat, file=MPDatFile, row.names=F)

  #add 1% q trend
  data$pr_7594_m8_q1 <- data$pr_7594_m8 / (0.9975 ^ (max(data$ssYr) - (data$ssYr)))
  data$pr_7994_m8_q1 <- data$pr_7994_m8 / (0.9975 ^ (max(data$ssYr) - (data$ssYr)))
  data$pr_8000_m8_q1 <- data$pr_8000_m8 / (0.9975 ^ (max(data$ssYr) - (data$ssYr)))

  # tropical cluster not recommended for 2019 (because CPUE group did not run the analysis, and the issue of MSE CPUE uncertainty to be reviewed in new CPUE group ToR)
  # these values use the tropical cluster, but did not do the discards, and are included only for curiousity
  # repeat with tropical cluster CPUE
  #data2 <- rbind(cbind(subset(read.csv(file = wdCPUE %&% 'std_xTW/outputs\\' %&% "Joint_regB2_R1_dellog_boat_allyrs.csv", header=T,as.is=T), 
  #                            !is.na(pr),
  #                            select=c('yq','pr','yr','qtr')),
  #                     AssessmentAreaName='1'),
  #			          cbind(subset(read.csv(file = wdCPUE %&% 'std_xTW/outputs\\' %&% "Joint_regB2_R2_dellog_boat_allyrs.csv", header=T,as.is=T), 
  #                            !is.na(pr),
  #                            select=c('yq','pr','yr','qtr')),
  #                     AssessmentAreaName='2'),
  #			          cbind(subset(read.csv(file = wdCPUE %&% 'std_xTW/outputs\\' %&% "Joint_regB2_R3_dellog_boat_allyrs.csv", header=T,as.is=T), 
  #                            !is.na(pr),
  #                            select=c('yq','pr','yr','qtr')),
  #                     AssessmentAreaName='3'),
  #			          cbind(subset(read.csv(file = wdCPUE %&% 'std_xTW/outputs\\' %&% "Joint_regB2_R1_dellog_boat_allyrs.csv", header=T,as.is=T), 
  #                            !is.na(pr),select=c('yq','pr','yr','qtr')),
  #                     AssessmentAreaName='4')) #intentionally same as first
  #
  #data2$pr_7594_m8  <- data2$pr
  #data2$pr_7994_m8  <- data2$pr
  #data2$pr_8000_m8  <- data2$pr
  #data2$ssYr        <- decYrToSeasAsYr(data2$yq)
  #data2$target      <- "Ctrop"
  #
  #
  #index           <- data2$AssessmentAreaName == '1'
  #work            <- data2[index,]
  #work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][1]
  #work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][1]
  #work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][1]
  #data2[index,]   <- work
  #
  #index           <- data2$AssessmentAreaName == '2'
  #work            <- data2[index,]
  #work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][2]
  #work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][2]
  #work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][2]
  #data2[index,]   <- work
  #
  #index           <- data2$AssessmentAreaName == '3'
  #work            <- data2[index,]
  #work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][3]
  #work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][3]
  #work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][3]
  #data2[index,]   <- work
  #
  #index           <- data2$AssessmentAreaName == '4'
  #work            <- data2[index,]
  #work$pr_7594_m8 <- work$pr_7594_m8 / mean(work[work$yr >= 1975 & work$yr <= 1994, 'pr_7594_m8']) * betwts[['7594 m8']][4]
  #work$pr_7994_m8 <- work$pr_7994_m8 / mean(work[work$yr >= 1979 & work$yr <= 1994, 'pr_7994_m8']) * betwts[['7994 m8']][4]
  #work$pr_8000_m8 <- work$pr_8000_m8 / mean(work[work$yr >= 1980 & work$yr <= 2000, 'pr_8000_m8']) * betwts[['8000 m8']][4]
  #data2[index,]   <- work
  #
  #add 1% q trend
  #data2$pr_7594_m8_q1 <- data2$pr_7594_m8 / (0.9975 ^ (max(data2$ssYr) - (data2$ssYr)))
  #data2$pr_7994_m8_q1 <- data2$pr_7994_m8 / (0.9975 ^ (max(data2$ssYr) - (data2$ssYr)))
  #data2$pr_8000_m8_q1 <- data2$pr_8000_m8 / (0.9975 ^ (max(data2$ssYr) - (data2$ssYr)))


  regionLab=c("1S","2","3","1N")

  par(mfrow=c(3,2))

  #compare standardization method
  for (ia in c(4,1,2,3))
  {
    plotDat1 <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_7994_m8[data$AssessmentAreaName      == ia & data$yr > 1972])
    #  plotDat2 <- cbind(data2$YrQtr[data2$AssessmentAreaName == ia & data2$yr > 1972], data2$pr_7994_m8[data2$AssessmentAreaName == ia & data2$yr > 1972])
    
    if (ia == "1") 
    {
      plot(1, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="", main="", sub="")
    }
    
    plot(plotDat1, type='l', main="Region " %&% regionLab[ia], col=2, ylab="area-weighted CPUE", xlab="", ylim=c(0,2), yaxs='i')
    #  lines(plotDat2, col=1)
  }

  par(mfrow=c(3,2))

  #compare q trend assumption
  for (ia in c(4,1,2,3))
  {
    plotDat1 <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_7994_m8[data$AssessmentAreaName    == ia & data$yr > 1972])
    plotDat3 <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_7994_m8_q1[data$AssessmentAreaName == ia & data$yr > 1972])
    
    if (ia == "1")
    {
      plot(1, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="", main="", sub="")
    }
    
    plot(plotDat1, type='l',main="Region " %&% regionLab[ia], col=1, ylab="area-weighted CPUE", xlab="", ylim=c(0,2), yaxs='i')
    lines(plotDat3, col=2, lty=2)
  }

  par(mfrow=c(3,2))

  #compare area weighting assumptions
  for (ia in c(4,1,2,3))
  {
    plotDat1     <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_7994_m8[data$AssessmentAreaName == ia & data$yr > 1972])
    plotDat1[,2] <- plotDat1[,2] / max(data$pr_7994_m8[data$AssessmentAreaName == "2" & data$yr > 1972])

    plotDat2     <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_7594_m8[data$AssessmentAreaName == ia & data$yr > 1972])
    plotDat2[,2] <- plotDat2[,2] / max(data$pr_7594_m8[data$AssessmentAreaName == "2" & data$yr > 1972])

    plotDat3     <- cbind(data$YrQtr[data$AssessmentAreaName == ia & data$yr > 1972], data$pr_8000_m8[data$AssessmentAreaName == ia & data$yr > 1972])
    plotDat3[,2] <- plotDat3[,2] / max(data$pr_8000_m8[data$AssessmentAreaName == "2" & data$yr > 1972])
    
    if (ia=="1")
    {
      plot(1, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="", main="", sub="")
    }
    
    plot(plotDat3, type='l', main="Region " %&% regionLab[ia], col=3, ylab="area-weighted CPUE", xlab="", ylim=c(0,1.1), yaxs='i', lwd=3)
    lines(plotDat2, col=2, lwd=2)
    lines(plotDat1, col=1)
  }

  #output data file for OMgrid
  write.table(data, file=exportFileName)
}


# Assumes in R we have change directory to the folder containing this script file
wdCPUE          <- "./IOTC_InputData/BET/2022/"

# the disaggregated CPUE data files for the OM grid
exportFileName          <- "./BETOMcpue2022.csv"
seasonalExportFileName  <- "./BETOMcpue2022seasonal.csv"

# annualized aggregate data for the MP to use
MPDatFile               <- "./BETMPcpue2022.csv"
seasonalMPDatFile       <- "./BETMPcpue2022seasonal.csv"

assessmentCPUE(wdCPUE, exportFileName, MPDatFile, IsSeasonal=FALSE)
assessmentCPUE(wdCPUE, seasonalExportFileName, seasonalMPDatFile, IsSeasonal=TRUE)

