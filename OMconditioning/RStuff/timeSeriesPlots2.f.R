# Do some time series plots

timeSeriesPlots2.f <- function (mList = gridY1List, SSYr=0.25, yrSeasY=284,
    seasAsYr=101, endSeasAsYr=284, numSeas=4, endYr=2017, endSeas=4, xlims=c(1950,2017), mListLabels=mList)
{

    for (im in 1:length(mList)){  
      m <- get(mList[im])
      SPBSeries <- m$derived_quants[substring(m$derived_quants$LABEL, 1, 3) == "SPB", ]
      SPBSeries <- SPBSeries[3:nrow(SPBSeries), ]
      numTime <- nrow(SPBSeries)

      SPBoSPBMSYSeries <- SPBSeries[, 2]/as.numeric(subset(m$derived_quants, m$derived_quants$LABEL == "SSB_MSY")$Value)

      firstQ <- m$timeseries$Seas[1]==1
      yearLab <- seasAsYrToDecYr.f(as.numeric(substring(SPBSeries$LABEL, 5)),endSeasAsYr=endSeasAsYr, numSeas=numSeas, endYr=endYr, endSeas=endSeas)

      if(im==1){
          plot(yearLab, SPBoSPBMSYSeries, ylim = c(0, 4), main = "SSB/SSBMSY", xlim=xlims, xaxs = 'i', yaxs = 'i',
           ylab = "SSB/SSBMSY", xlab = "", type = "l", col = 1,
           lwd = 1)
      abline(h=1, col=1,lty=2, lwd=2)  
      } else {
        lines(yearLab, SPBoSPBMSYSeries, col = im, lwd = 1 + floor(im/7))
      }
    }  
  legend(x=1951, y=2, legend=mListLabels, col=c(1:length(mList)), lty=1)
  
  for (im in 1:length(mList)){  
    m <- get(mList[im])
    SPBSeries <- m$derived_quants[substring(m$derived_quants$LABEL, 1, 3) == "SPB", ]
    SPBSeries <- SPBSeries[3:nrow(SPBSeries), ]
    numTime <- nrow(SPBSeries)
    
    SPBoSPB0Series   <- SPBSeries[, 2]/as.numeric(subset(m$derived_quants, m$derived_quants$LABEL == "SPB_Virgin")$Value)
    
    firstQ <- m$timeseries$Seas[1]==1
    yearLab <- seasAsYrToDecYr.f(as.numeric(substring(SPBSeries$LABEL, 5)),endSeasAsYr=endSeasAsYr, numSeas=numSeas, endYr=endYr, endSeas=endSeas)
    
    if(im==1){
      plot(yearLab, SPBoSPB0Series, ylim = c(0, 1.1), main = "SB/SB0", xlim=xlims,
           ylab = "SB/SB0", xlab = "", type = "l", col = 1, xaxs = 'i', yaxs = 'i',
           lwd = 1)
    } else {
      lines(yearLab, SPBoSPB0Series, col = im, lwd = 1 + floor(im/7))
    }
    legend(x=1951, y=0.7, legend=mListLabels, col=c(1:length(mList)), lty=1, 
           lwd = c(rep(1,8), rep(2,8)))
  }  
}

