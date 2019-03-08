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
#
# IMPORTANT
# ---------
# If the model projections in results were already run with short MP name
# substitutions then the MPs argument should contain the short names for the
# MPs. If this isn't the case then the call to findTable1_Data() will return
# no data and no table will be created.
#------------------------------------------------------------------------------
createTable1 <- function(years, results, MPs, MPs_short, prefix="")
{
  if (length(MPs) > 30)
  {
    print("ERROR: maximum number of MPs is 30")
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

  xml <- paste(readLines("./templates/table_1_base_template.xml.txt"), collapse="\n")

  data_rows_xml <- ""

  for (idx in 1:length(MPs))
  {
    data_xml <- paste(readLines("./templates/table_1_data_template.xml.txt"), collapse="\n")

    data_xml <- gsub("\\{MP\\}",     MPs_short[idx],                            data_xml)
    data_xml <- gsub("\\{SB\\}",     formatC(SBs[idx],   format="f", digits=2), data_xml)
    data_xml <- gsub("\\{SBa\\}",    formatC(SB25s[idx], format="f", digits=2), data_xml)
    data_xml <- gsub("\\{SBb\\}",    formatC(SB75s[idx], format="f", digits=2), data_xml)
    data_xml <- gsub("\\{SBcol\\}",  SB_shade[idx],                             data_xml)

    data_xml <- gsub("\\{KG\\}",     formatC(GKs[idx], format="f", digits=2), data_xml)
    data_xml <- gsub("\\{KGcol\\}",  GK_shade[idx],                           data_xml)

    data_xml <- gsub("\\{PSB\\}",    formatC(PrSBgtLims[idx], format="f", digits=2), data_xml)
    data_xml <- gsub("\\{PSBcol\\}", PrSBgtLims_shade[idx],                          data_xml)

    data_xml <- gsub("\\{Y\\}",      formatC(Catches[idx], format="f", digits=1),  data_xml)
    data_xml <- gsub("\\{Ya\\}",     formatC(Catch25s[idx], format="f", digits=1), data_xml)
    data_xml <- gsub("\\{Yb\\}",     formatC(Catch75s[idx], format="f", digits=1), data_xml)
    data_xml <- gsub("\\{Ycol\\}",   Catches_shade[idx],                           data_xml)

    data_xml <- gsub("\\{CV\\}",     formatC(Catch_variabilities[idx], format="f", digits=2), data_xml)
    data_xml <- gsub("\\{CVcol\\}",  Catch_variabilities_shade[idx],                          data_xml)

    data_rows_xml <- paste(data_rows_xml, data_xml, sep="")
  }

  xml <- gsub("\\{Data\\}", data_rows_xml, xml)

  Xmlfile <- file(paste("./Report/", prefix, "Table_1_", years, "yr.xml", sep=""), "wt")
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
#
# IMPORTANT
# ---------
# If the model projections in results were already run with short MP name
# substitutions then the MPs argument should contain the short names for the
# MPs. If this isn't the case then the call to findTable2_Data() will return
# no data and no table will be created.
#------------------------------------------------------------------------------
createTable2 <- function(years, results, MPs, MPs_short, prefix="")
{
  PageWidth       <- "11906"
  PageLength      <- "16838"
  PageOrientation <- "portrait"

  if (length(MPs) > 14)
  {
    print("ERROR: maximum number of MPs is 14")
    stop()
  }
  else if (length(MPs) > 8)
  {
    PageWidth       <- "16838"
    PageLength      <- "11906"
    PageOrientation <- "landscape"
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
  nMPs  <- length(MPs)

  xml <- paste(readLines("./templates/table_2_base_template.xml.txt"), collapse="\n")
  xml <- gsub("\\{PERIOD\\}", paste(years, "year average"), xml)
  xml <- gsub("\\{MP_col_span\\}", paste(nMPs), xml)
  xml <- gsub("\\{MP_col_span_p2\\}", paste(nMPs + 2), xml)
  xml <- gsub("\\{Page_Width\\}", PageWidth, xml)
  xml <- gsub("\\{Page_Height\\}", PageLength, xml)
  xml <- gsub("\\{Page_Orientation\\}", PageOrientation, xml)

  title_cols_xml <- ""

  for (cn in 1:nMPs)
  {
    title_xml <- paste(readLines("./templates/table_2_MP_title_template.xml.txt"), collapse="\n")

    title_cols_xml <- paste(title_cols_xml, gsub("\\{MP\\}", MPs_short[cn], title_xml), sep="")
  }

  xml <- gsub("\\{MP_title_columns\\}", title_cols_xml, xml)

  for (cn in 1:length(stats))
  {
    nrow <- rowMap[[stats[cn]]]

    data_cols_xml <- ""

    for (ncol in 1:nMPs)
    {
      data_xml <- paste(readLines("./templates/table_2_MP_data_template.xml.txt"), collapse="\n")
      value    <- formatC(Table2[nrow, ncol], format="f", digits=2)

      data_cols_xml <- paste(data_cols_xml, gsub("\\{Data\\}", value, data_xml), sep="")
    }

    tag <- paste("\\{MP_data_", nrow, "_columns\\}", sep="")
    xml <- gsub(tag, data_cols_xml, xml)
  }

  Xmlfile <- file(paste("./Report/", prefix, "Table_2_", years, "yr.xml", sep=""), "wt")
  writeLines(xml, con=Xmlfile)
  close(Xmlfile)
}


#------------------------------------------------------------------------------
# Helper function for standard BET plots
#------------------------------------------------------------------------------
betPlots.f <- function(mseObj,
                       SBLim,
                       SBTarg,
                       FLim,
                       FTarg,
                       Cref=missing,
                       YearsAveraged=20,
                       outputPath=NA,
                       prefix="")
{
  require(devEMF)

  if (!missing(Cref))
  {
    names(Cref) <- "S10"
  }

  BETTargs <- c(SBTarg, FTarg)
  BETLims  <- c(SBLim, FLim)

  names(BETTargs) <- c("S3", "S4")
  names(BETLims)  <- c("S3", "S4")

  beginDraw <- function(filename, width=6, height=6, outputPath=NA)
  {
    if (is.na(outputPath))
    {
      windows()
    }
    else
    {
      emf(file=outputPath %&% filename %&% ".emf", width=width, height=height)
    }
  }

  endDraw <- function(outputPath=NA)
  {
    if (!is.na(outputPath))
    {
      dev.off()
    }
  }

  histd <- msevizHistoricTimeSeriesData(mseObj)
  projd <- msevizProjectedTimeSeriesData(mseObj)

  beginDraw(prefix %&% "Recruitment", width=6, height=6, outputPath=outputPath)
  plotOMruns2(histd, projd, "Recruitment", ylab= "Recruitment")
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "SSB_SSBMSY", width=6, height=6, outputPath=outputPath)
  plotOMruns2(histd, projd, "SSB/SSBMSY", limit=SBLim, target=SBTarg, ylab= "SSB/SSBMSY")
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "F_FMSY", width=6, height=6, outputPath=outputPath)
  plotOMruns2(histd, projd, "F/FMSY", limit=FLim, target=FTarg, ylab= "F/FMSY")
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "C", width=6, height=6, outputPath=outputPath)
  plotOMruns2(histd, projd, "C", Cref=Cref, ylab= "Catch (1000t)")
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "CPUE", width=6, height=6, outputPath=outputPath)
  plotOMruns2(histd, projd, "CPUE(aggregate)", Cref=Cref, ylab= "CPUE(aggregate)")
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "KobeCols", width=6, height=6, outputPath=outputPath)
  plotKobeCols(om=histd[histd$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),], runs=projd[projd$qname%in% c("PrGreen","PrOrange","PrYellow","PrRed"),])
  endDraw(outputPath=outputPath)

  perfd <- msevizPerformanceData(mseObj, YearsAveraged)

  beginDraw(prefix %&% "BPs", width=6, height=3.375, outputPath=outputPath)
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
  print(plotBPs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S3", "S9", "S6", "S10", "S14")), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::popViewport()
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "TOs", width=6, height=3.375, outputPath=outputPath)
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
  print(plotTOs2(perfd, limit=BETLims, target=BETTargs, indicators = c("S9", "S3", "S6", "S14")), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::popViewport()
  endDraw(outputPath=outputPath)

  beginDraw(prefix %&% "KobeMP", width=6, height=3.375, outputPath=outputPath)
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1)))
  print(kobeMPs2(perfd, xlim=SBLim, ylim=FLim, ymax=3), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::popViewport()
  endDraw(outputPath=outputPath)
}


#------------------------------------------------------------------------------
# Helper function for box plotting indicator parameter sensitivty
#------------------------------------------------------------------------------
# This function creates box plot sensitivities broken down by model parameter.
# It requires as input, the data.table extracted from the mse object by call
# to the msevizPerformanceData() function. For example,
#
# dt <- msevizPerformanceData(mseObj, 20)
# indicatorSensitivityBPs(dt, colourPalette=rainbow(10, s=0.6))
#
# Specifying an output path will force the creation of .emf windows metafile
# output instead of outputting to the screen. The print.* arguments allow some
# control over how that output will appear.
#
# Keeping the data.table creation outside of this function adds flexibility as
# the data.table output can be pre-filtered using data.table functionality.
#------------------------------------------------------------------------------
indicatorSensitivityBPs <- function(dt,
                                    colourPalette=missing,
                                    outputPath=NA,
                                    prefix="",
                                    print.width=6.25,
                                    print.height=2.25,
                                    print.fontscale=1)
{
  require(devEMF)

  no_params <- length(unlist(strsplit(as.character(dt$model[1]), "[_]")))

  if (!missing(colourPalette))
  {
    group_colours <- colourPalette
  }
  else
  {
    group_colours <- terrain.colors(n=no_params + 1, alpha = 1)
  }

  indicator_names <- levels(dt$name)

  for (indicator_name in indicator_names)
  {
    dt_indicator  <- dt[name==indicator_name][,.(data,model)]
    dt_boxplot    <- NULL
    param_count   <- 1
    param_colours <- list("0000"=group_colours[1])
    param_map     <- list(All="0000")
    display_names <- c("All")

    for (param in 1:no_params)
    {
      param_names <- unlist(lapply(strsplit(as.character(dt_indicator$model),"_"), FUN=function(x){x[param]}))

      for (param_name in levels(factor(param_names)))
      {
        filtered_param_name                  <- sprintf("%04d", param_count)
        param_colours[[filtered_param_name]] <- group_colours[param + 1]
        param_map[[param_name]]              <- filtered_param_name
        display_names                        <- c(display_names, param_name)
        param_count                          <- param_count + 1
      }

      filtered_param_names <- sapply(param_names, FUN=function(x){param_map[[x]]})

      if (is.null(dt_boxplot))
      {
        dt_boxplot <- data.table(param=filtered_param_names, data=dt_indicator$data, stringsAsFactors=TRUE)
      }
      else
      {
        dt_boxplot <- data.table(param=c(as.character(dt_boxplot$param), filtered_param_names), data=c(dt_boxplot$data, dt_indicator$data), stringsAsFactors=TRUE)
      }
    }

    dt_boxplot <- data.table(param=c(as.character(dt_boxplot$param), rep("0000", times=length(dt_boxplot$param))), data=c(dt_boxplot$data, dt_boxplot$data), stringsAsFactors=TRUE)

    colours <- sapply(levels(dt_boxplot$param), FUN=function(x){param_colours[[x]]})
    pars    <- list(boxwex=0.8, staplewex=0.5, outwex=0.5, las=2)

    if (!is.na(outputPath))
    {
      emf(file=outputPath %&% prefix %&% "BPs_" %&% gsub("[^0-9A-Za-z.]", "_", indicator_name) %&% ".emf", width=print.width, height=print.height, pointsize = print.fontscale * 7)
      pars <- c(pars, outlwd=0)
    }

    boxplot(data ~ param, data=dt_boxplot, col=colours, names=display_names, pars=pars)
    title(indicator_name)

    if (!is.na(outputPath))
    {
      dev.off()
    }
  }
}
