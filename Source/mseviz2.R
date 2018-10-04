# mseviz2.R - DK modifications and additions to Iago's mse viz package for IOTC TCMP

#DK changes:  xmax and ymax args - otherwise if CIs exceed scale limts the CIs are not plotted
kobeMPs2 <- function (data, x = "S3", y = "S4", xlim = 0.4, ylim = 1.4, probs = c(0.1,
    0.5, 0.9), size = 0.75, alpha = 1, xmax=2, ymax=2)
{
    data <- data[, as.list(quantile(data, probs = probs, na.rm = TRUE)),
        keyby = list(indicator, name, year, mp)]
    daty <- data[indicator %in% y, ]
    setnames(daty, seq(5, 4 + length(probs)), paste0("y", paste0(probs *
        100, "%")))
    datx <- data[indicator %in% x, ]
    setnames(datx, seq(5, 4 + length(probs)), paste0("x", paste0(probs *
        100, "%")))

    datx$'x90%'[datx$'x90%' > xmax]  <- xmax
    daty$'y90%'[daty$'y90%' > ymax]  <- ymax

    data <- cbind(daty, datx[, -(1:4)])
    p <- ggplot(data, aes(x = `x50%`, y = `y50%`)) + geom_rect(aes(xmin = 1,
        xmax = Inf, ymin = 0, ymax = 1), colour = "green", fill = "green") +
        geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            colour = "yellow", fill = "yellow") + geom_rect(aes(xmin = 1,
        xmax = Inf, ymin = 1, ymax = Inf), colour = "orange",
        fill = "orange") + geom_rect(aes(xmin = 0, xmax = 1,
        ymin = 1, ymax = Inf), colour = "red", fill = "red") +
        geom_hline(aes(yintercept = 1)) + geom_vline(aes(xintercept = 1)) +
        scale_x_continuous(expand = c(0, 0), limits = c(0, xmax)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank()) +
        geom_linerange(aes(ymin = `y10%`, ymax = `y90%`), size = size, alpha = alpha) +

        #alpha = alpha) + geom_linerangeh(aes(xmin = `x10%`, xmax = `x90%`), size = size, alpha = alpha)
        geom_linerangeh(aes(xmin = `x10%`, xmax = `x90%`), size = size, alpha = alpha) +

        geom_point(aes(fill = mp),
        shape = 21, size = 4) + scale_shape(solid = FALSE) +
        theme(legend.title = element_blank()) +
        #geom_segment(aes(x = xlim, xend = xmax, y = ylim, yend = ylim), colour="black", size=1.5) +
        #geom_segment(aes(x = xlim, xend = xmax, y = ylim, yend = ylim), colour="red", size=1) +
        #geom_segment(aes(x = xlim, xend = xlim, y = 0, yend = ylim), colour="black", size=1.5) +
        #geom_segment(aes(x = xlim, xend = xlim, y = 0, yend = ylim), colour="red", size=1) +
        geom_segment(aes(x = 0, xend = xmax, y = ylim, yend = ylim), colour="black", size=1.5) +
        geom_segment(aes(x = 0, xend = xmax, y = ylim, yend = ylim), colour="white", size=1) +
        geom_segment(aes(x = xlim, xend = xlim, y = 0, yend = Inf), colour="black", size=1.5) +
        geom_segment(aes(x = xlim, xend = xlim, y = 0, yend = Inf), colour="white", size=1) +
        labs(x = expression(SB/SB[MSY]),
        y = expression(F/F[MSY])) + annotate("text", x = 0.35,
        y = 0.1, label = "SB[lim]", parse = TRUE) + annotate("text",
        x = 0.85, y = 0.1, label = "SB[targ]", parse = TRUE) +
        annotate("text", x = 0.15, y = 1.4, label = "F[lim]",
            parse = TRUE) + annotate("text", x = 0.15, y = 1.1,
        label = "F[targ]", parse = TRUE)
    return(p)
}


#DK changes:  indicator list, ref line colours reversed, backgournd color white
plotBPs2 <- function (data, indicators =  c("S3", "S9", "S6", "S10", "S14"),
    target = missing, limit = missing, blackRef = missing)
{   par(mfrow=c(3,2))
    data <- data[indicator %in% indicators, ]

    #would like axis reversal conditional on (AAV), but can't see an easy way to do this
    #p <- p + scale_y_reverse()
    # can easily negate AAV, but this is probably more confusing
    #data$indicator <- data$indicator*-1

    cols <- unique(data[, c("indicator", "name")])
    data[, `:=`(name, factor(name, levels = cols$name[match(cols$indicator,
        indicators)], ordered = TRUE))]

    p <- ggplot(data, aes(x = mp, y = data, colour = mp))  + theme_bw() +
         xlab("") + ylab("") +
        theme(axis.text.x = element_blank(), legend.position = c(0.85,
             0.15), legend.title = element_blank())

    p <- p + geom_boxplot(data=data, outlier.shape = NA,
        aes(fill = mp), colour = "black") + facet_wrap(~name, scales = "free_y", labeller = "label_parsed")

    if (!missing(target)) {
        dat <- data[indicator %in% names(target), ]
        dat[, `:=`(target, unlist(target)[match(indicator, names(target))])]
        p <- p + geom_rect(data=dat, aes(xmin = 0, xmax = Inf, ymin = target, ymax = Inf),
            colour = rgb(0.7,1,0.7), fill=rgb(0.7,1,0.7))
        p <- p + geom_hline(data = dat, aes(yintercept = target),
            colour = "green", linetype = "longdash", size = 1)
    }
    if (!missing(limit)) {
        dat <- data[indicator %in% names(limit), ]
        dat[, `:=`(limit, unlist(limit)[match(indicator, names(limit))])]
        p <- p + geom_rect(data=dat, aes(xmin = 0, xmax = Inf, ymin = 0, ymax = limit),
            colour = rgb(1,0.7,.7), fill=rgb(1,0.7,0.7))
        p <- p + geom_hline(data = dat, aes(yintercept = limit),
            colour = "red", linetype = "longdash", size = 1)
    }
    if (!missing(blackRef)) {
        dat <- data[indicator %in% names(blackRef), ]
        dat[, `:=`(blackRef, unlist(blackRef)[match(indicator, names(blackRef))])]
        p <- p + geom_hline(data = dat, aes(yintercept = blackRef),
            colour = "black", linetype = "longdash", size = 1)
    }

    p <- p + geom_boxplot(data=data, outlier.shape = NA,
        aes(fill = mp), colour = "black") + facet_wrap(~name, scales = "free_y", labeller = "label_parsed")



    return(p)
}


#DK changes: added SSB ref lines, independent Y axes, backgournd color white
plotTOs2 <- function (data, x = "S10", indicators = c("S3", "S6", "S9", "S14"), probs = c(0.1,
    0.5, 0.9), size = 0.5, alpha = 0.75, target = missing, limit = missing, blackRef = missing)
{

    data <- data[, as.list(quantile(data, probs = probs, na.rm = TRUE)),
        keyby = list(indicator, name, year, mp)]
    daty <- data[indicator %in% indicators, ]
    setnames(daty, seq(5, 4 + length(probs)), paste0("y", paste0(probs *
        100, "%")))
    datx <- data[indicator %in% x, ]
    setnames(datx, seq(5, 4 + length(probs)), paste0("x", paste0(probs *
        100, "%")))
    data <- cbind(daty, datx[, -(1:4)])
    p <- ggplot(data, aes(x = `x50%`, y = `y50%`))  + theme_bw() + xlab(unique(datx$name)) +
        ylab("") + geom_linerange(aes(ymin = `y10%`, ymax = `y90%`),
        size = size, alpha = alpha) + geom_linerangeh(aes(xmin = `x10%`,
        xmax = `x90%`), size = size, alpha = alpha) + geom_point(aes(fill = mp),
        shape = 21, size = 4) + facet_wrap(~name, scales = "free_y") + scale_shape(solid = FALSE) +
        theme(legend.title = element_blank())

    if (!missing(target)) {
        dat <- data[indicator %in% names(target), ]
        dat[, `:=`(target, unlist(target)[match(indicator, names(target))])]
        p <- p + geom_rect(data=dat, aes(xmin = 0, xmax = Inf, ymin = target, ymax = Inf),
            colour = rgb(0.7,1,0.7), fill=rgb(0.7,1,0.7))
        p <- p + geom_hline(data = dat, aes(yintercept = target),
            colour = "green", linetype = "longdash", size = 0.5)
    }
    if (!missing(limit)) {
        dat <- data[indicator %in% names(limit), ]
        dat[, `:=`(limit, unlist(limit)[match(indicator, names(limit))])]
        p <- p + geom_rect(data=dat, aes(xmin = 0, xmax = Inf, ymin = 0, ymax = limit),
            colour = rgb(1,0.7,.7), fill=rgb(1,0.7,0.7))
        p <- p + geom_hline(data = dat, aes(yintercept = limit),
            colour = "red", linetype = "longdash", size = 0.5)
    }
    if (!missing(blackRef)) {
        p <- p + geom_vline( aes(xintercept = blackRef),
            colour = "black", linetype = "longdash", size = 0.5)
    }

    p <- p + geom_linerange(aes(ymin = `y10%`, ymax = `y90%`),
        size = size, alpha = alpha) + geom_linerangeh(aes(xmin = `x10%`,
        xmax = `x90%`), size = size, alpha = alpha) + geom_point(aes(fill = mp),
        shape = 21, size = 4) + facet_wrap(~name, scales = "free_y") + scale_shape(solid = FALSE) +
        theme(legend.title = element_blank())


    return(p)
}


# Symantics of this function is now changed. Typical usage is:
#
#   histd <- msevizHistoricTimeSeriesData(mseFramework)
#   projd <- msevizProjectedTimeSeriesData(mseFramework)
#   plotOMruns2(histd, projd, "SSB/SSBMSY")
#
# for worms to work the projd data table must have "SSB/SSBMSY" results.
#
# DK changes: ribbon colour arg (I think Toshi requested something other than red), y axis always includes 0
#             y label argument, background predominantly white (for printing), historical variability
#             3 worms are plotted if wormNums defined (3 simulation numbers are expected)
#             in this case chosen by the SSB/SSBMSY(2019) 25th, 50, 75th percentiles (of first MP, which should not matter)
#              catch ref line
plotOMruns2 <- function(om.dt,
                        runs.dt,
                        indicator,
                        limit = missing,
                        target = missing,
                        Cref=missing,
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
                        ylab = "",
                        ribCol = "grey",
                        lastHistYr = 2015,
                        firstMPYr = 2019,
                        doWorms = TRUE,
                        CScale=0.001,
                        title = "",
                        combined = FALSE)
{
  om.dt$data[om.dt$qname == "F/FMSY" & om.dt$data > 3] <- 3
  runs.dt$data[runs.dt$qname == "F/FMSY" & runs.dt$data > 3] <- 3

  #scale catch
  om.dt$data[om.dt$qname == "C"] <-  om.dt$data[om.dt$qname == "C"]*CScale
  runs.dt$data[runs.dt$qname == "C"] <-  runs.dt$data[runs.dt$qname == "C"]*CScale

  om            <- om.dt[qname==indicator,]
  runs          <- runs.dt[qname==indicator,]
  worms         <- NULL
  wormsHistoric <- NULL

  if (doWorms)
  {
    MP  <- levels(factor(runs.dt$mp))[1]
    tmp <- runs.dt[mp == MP & year == firstMPYr & qname == "SSB/SSBMSY",]

    if (nrow(tmp) > 0)
    {
      quants   <- quantile(tmp$data, probs = c(0.25,0.5,0.75), type=1)
      wormNums <- as.integer(unlist(tmp[tmp$data %in% quants, 'iter']))

      worm1    <- runs[iter == wormNums[1]]
      worm1    <- worm1[,c(1,5,2)]

      colnames(worm1)[3] <- "worm.1"

      wormHistoric1 <- om[iter == wormNums[1]]
      wormHistoric1 <- wormHistoric1[,c(1,2)]

      colnames(wormHistoric1)[2] <- "worm.1"

      worm2   <- runs[iter == wormNums[2]]
      worm2   <- worm2[,c(1,5,2)]

      colnames(worm2)[3] <- "worm.2"

      wormHistoric2 <- om[iter == wormNums[2]]
      wormHistoric2 <- wormHistoric2[,c(1,2)]

      colnames(wormHistoric2)[2] <- "worm.2"

      worm3   <- runs[iter == wormNums[3]]
      worm3   <- worm3[,c(1,5,2)]

      colnames(worm3)[3] <- "worm.3"

      wormHistoric3 <- om[iter == wormNums[3]]
      wormHistoric3 <- wormHistoric3[,c(1,2)]

      colnames(wormHistoric3)[2] <- "worm.3"

      worms <- worm1[worm2[worm3, on=.(year,mp)],on=.(year,mp)]

      wormsHistoric <- wormHistoric1[wormHistoric2[wormHistoric3, on=.(year)],on=.(year)]
    }
  }

  if (om$qname[1] == "F/FMSY")
  {
    runs$data[runs$data>3] <- 3
  }

  if (combined && (max(om$iter) != max(runs$iter)))
  {
    stop("ERROR: om.dt and runs.dt must have the same iter numbers for a combined plot")
  }

  if ("iter" %in% colnames(om) && length(unique(om$iter)) > 1)
  {
    om <- om[, as.list(quantile(data, probs = probs, na.rm = TRUE)), keyby = list(year)]

  } else
  {
    om[, `:=`(`50%`, data)]
  }

  runs <- runs[, as.list(quantile(data, probs = probs, na.rm = TRUE)), keyby = list(year, mp)]

  if (!is.null(worms))
  {
    runs <- runs[worms, on=.(year,mp)]
  }

  if (combined)
  {
    if (!is.null(wormsHistoric))
    {
      om <- om[wormsHistoric, on=.(year)]
    }

    MPs           <- levels(factor(runs.dt$mp))
    selected.cols <- which((names(runs) != "mp") == TRUE)
    nrows         <- floor(length(MPs)^0.5)
    ncols         <- ceiling(length(MPs) / nrows)
    nrow          <- 1
    ncol          <- 1

    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = nrows, ncol = ncols)))

    for (MP in MPs)
    {
      data <- rbind(om, runs[mp==MP, selected.cols, with=FALSE])

      pl <- ggplot(data, aes(x = year, ymin=0)) +
            geom_vline(aes(xintercept = lastHistYr)) +
            geom_vline(aes(xintercept = firstMPYr), linetype=2) +
            ylab(ylab) +
            theme_bw() +
            ggtitle(MP) +
            geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = ribCol, alpha = 0.4) +
            geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = ribCol, alpha = 0.8) +
            geom_line(aes(y = `50%`), size=1)

      if (!is.null(worms))
      {
        pl <- pl + geom_line(aes(y = worm.1), colour = 'blue') +
                   geom_line(aes(y = worm.2), colour = 'red') +
                   geom_line(aes(y = worm.3), colour = 'purple')
      }

      if (!missing(Cref))
      {
        pl <- pl + geom_hline(aes(yintercept = Cref*CScale), colour = "black", linetype = 2)
      }

      if (!missing(limit))
      {
        pl <- pl + geom_hline(aes(yintercept = limit), colour = "red", linetype = 2)
      }

      if (!missing(target))
      {
        pl <- pl + geom_hline(aes(yintercept = target), colour = "green", linetype = 2)
      }

      print(pl, vp = grid::viewport(layout.pos.row = nrow, layout.pos.col = ncol))

      ncol <- ncol + 1

      if (ncol > ncols)
      {
        ncol <- 1
        nrow <- nrow + 1
      }
    }

    grid::popViewport()
  }
  else
  {
    p1 <- ggplot(om, aes(x = year, y = `50%`, ymin=0)) +
          geom_vline(aes(xintercept = lastHistYr)) +
          theme_bw() +
          ylab(ylab) +
          xlab("") +
          ggtitle(title) +
          geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = ribCol, alpha = 0.4) +
          geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = ribCol, alpha = 0.8) +
          geom_line(aes(y = `50%`))

    if (!missing(limit))
    {
      p1 <- p1 + geom_hline(aes(yintercept = limit), colour = "red", linetype = 2)
    }

    if (!missing(target))
    {
      p1 <- p1 + geom_hline(aes(yintercept = target), colour = "green", linetype = 2)
    }

    p2 <- ggplot(runs, aes(x = year, ymin=0)) +
          geom_vline(aes(xintercept = lastHistYr)) +
          geom_vline(aes(xintercept = firstMPYr), linetype=2) +
          ylab(ylab) +
          theme_bw() +
          geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = ribCol, alpha = 0.4) +
          geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = ribCol, alpha = 0.8) +
          geom_line(aes(y = `50%`), size=1)
          facet_wrap(~mp, ncol = 2)

    if (!is.null(worms))
    {
      p2 <- p2 + geom_line(aes(y = worm.1), colour = 'blue') +
                 geom_line(aes(y = worm.2), colour = 'red') +
                 geom_line(aes(y = worm.3), colour = 'purple')
    }

    p2 <- p2 + facet_wrap(~mp, ncol = 2)

    if (!missing(Cref))
    {
      p2 <- p2 + geom_hline(aes(yintercept = Cref*CScale), colour = "black", linetype = 2)
    }

    if (!missing(limit))
    {
      p2 <- p2 + geom_hline(aes(yintercept = limit), colour = "red", linetype = 2)
    }

    if (!missing(target))
    {
      p2 <- p2 + geom_hline(aes(yintercept = target), colour = "green", linetype = 2)
    }

    grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))

    vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)

    print(p1, vp = vplayout(1, 1:2))
    print(p2, vp = vplayout(2:4, 1:2))

    grid::popViewport()
  }
}


#DK Kobe stacked column time series:
plotKobeCols <- function (om, runs, ylab="", lastHistYr=2015, firstMPYr = 2019)
{
   om   <- om[om$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed"),]
   om   <- om[,as.list(mean(as.numeric(data))),keyby=list(year,qname),]
   om$qname <- factor(om$qname, levels=c("PrGreen","PrYellow","PrOrange","PrRed"))

   runs <- runs[runs$qname %in% c("PrGreen","PrOrange","PrYellow","PrRed")]
   runs <- runs[,as.list(mean(as.numeric(data))),keyby=list(year,qname,mp)]
   runs$qname <- factor(runs$qname, levels=c("PrGreen","PrYellow","PrOrange","PrRed"))

   group.colors <- c(PrGreen = "green",PrOrange = "orange", PrYellow = "yellow",PrRed = "red")

    p1 <- ggplot(om, aes(x = year, y = V1, ymin=0)) +
        geom_bar(aes(y = V1, x = year, fill = qname), colour="black", stat="identity") +
        #scale_fill_manual(values = c("green","orange","yellow","red")) +
        scale_fill_manual(values=group.colors) +
        #geom_vline(aes(xintercept = lastHistYr)) + theme_bw() +
        theme(legend.position="none") +
        ylab(ylab) +
        xlab("") +
        #scale_y_discrete(expand = c(0,0)) +
        ggtitle("Kobe Quadrant Probabilities") +
        theme(plot.title = element_text(hjust = 0.5))


    p2 <- ggplot(runs, aes(x = year, y= V1, ymin=0)) +
        #scale_y_discrete(expand = c(0,0)) +
        #scale_x_discrete(expand = c(0,0)) +
        geom_bar(aes(y = V1, x = year, fill = qname), colour="black", stat="identity") +
        #scale_fill_manual(values = c("green","orange","yellow","red")) +
        scale_fill_manual(values=group.colors) +
        #geom_vline(aes(xintercept = lastHistYr))  +
        geom_vline(aes(xintercept = firstMPYr), linetype=2) +
        ylab("Proportion of Outcomes") +
        xlab("") +
        theme_bw() +
        theme(legend.position="none") +
        facet_wrap(~mp, ncol = 2)

    grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,
        2)))

     vplayout <- function(x, y) grid::viewport(layout.pos.row = x,
        layout.pos.col = y)

    print(p1, vp = vplayout(1, 1:2))

    print(p2, vp = vplayout(2:4, 1:2))

    grid::popViewport()
}
