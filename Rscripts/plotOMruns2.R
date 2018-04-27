#DK changes: ribbon colour arg (I think Toshi requested something other than red), y axis always includes 0
#            y label argument, background predominantly white (for printing), historical variability
#            3 worms are plotted if wormNums defined (3 simulation numbers are expected)
#            in this case chosen by the SSB/SSBMSY(2019) 25th, 50, 75th percentiles (of first MP, which should not matter)
plotOMruns2 <- function(om,
                        runs,
                        limit = missing,
                        target = missing,
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
                        ylab = "",
                        ribCol = "grey",
                        lastHistYr = 2015,
                        firstMPYr = 2019,
                        doWorms = TRUE)
{
  worms <- NULL

  if (doWorms)
  {
    MP  <- levels(factor(runs$mp))[1]
    tmp <- runs[runs$mp == MP & runs$year == 2019 & runs$qname == "SSB/SSBMSY",]

    if (nrow(tmp) > 0)
    {
      quants   <- quantile(tmp$data, probs = c(0.25,0.5,0.75), type=1)
      wormNums <- as.integer(unlist(tmp[tmp$data %in% quants, 'iter']))
      worm1    <- runs[iter == wormNums[1] & qname == "SSB/SSBMSY"]
      worm1    <- worm1[,c(1,5,2)]

      colnames(worm1)[3] <- "worm.1"

      worm2   <- runs[iter == wormNums[2] & qname == "SSB/SSBMSY"]
      worm2   <- worm2[,c(1,5,2)]

      colnames(worm2)[3] <- "worm.2"

      worm3   <- runs[iter == wormNums[3] & qname == "SSB/SSBMSY"]
      worm3   <- worm3[,c(1,5,2)]

      colnames(worm3)[3] <- "worm.3"

      worms <- worm1[worm2[worm3, on=.(year,mp)],on=.(year,mp)]
    }
  }

  if (om$qname[1] == "F/FMSY")
  {
    runs$data[runs$data>3] <- 3
  }

  if ("iter" %in% colnames(om) && length(unique(om$iter)) > 1)
  {
    om <- om[, as.list(quantile(data, probs = probs, na.rm = TRUE)), keyby = list(year)]

  } else
  {
    om[, `:=`(`50%`, data)]
  }

  p1 <- ggplot(om, aes(x = year, y = `50%`, ymin=0)) +
        geom_vline(aes(xintercept = lastHistYr)) +
        theme_bw() +
        ylab(ylab) +
        xlab("") +
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

  runs <- runs[, as.list(quantile(data, probs = probs, na.rm = TRUE)), keyby = list(year, mp)]

  if (!is.null(worms))
  {
    runs <- runs[worms, on=.(year,mp)]
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
  invisible()
}

plotOMruns2(histd[histd$qname=="SSB/SSBMSY",], projd[projd$qname=="SSB/SSBMSY",])
