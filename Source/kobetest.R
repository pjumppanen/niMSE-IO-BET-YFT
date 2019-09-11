load(file="Objects/kobe.data.RDA")

m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
 xlim(0.5, 6) +
 ylim(40, 110)
m + geom_density_2d(bins=2)


x <- "S3"
y <- "S4"
xlim <- FLim
ylim <- SBLim
probs <- c(0.1, 0.5, 0.9)
size <- 0.75
alpha <- 1
xmax <- 2
ymax <- 3
ggplot(data, aes(x = `x50%`, y = `y50%`)) + geom_rect(aes(xmin = 1,
    xmax = Inf, ymin = 0, ymax = 1), colour = "green", fill = "green") +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        colour = "yellow", fill = "yellow") + geom_rect(aes(xmin = 1,
    xmax = Inf, ymin = 1, ymax = Inf), colour = "orange",
    fill = "orange") + geom_rect(aes(xmin = 0, xmax = 1,
    ymin = 1, ymax = Inf), colour = "red", fill = "red") +
    geom_hline(aes(yintercept = 1)) + geom_vline(aes(xintercept = 1)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, xmax)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
#    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_blank()) +
#    geom_linerange(aes(ymin = `y10%`, ymax = `y90%`), size = size, alpha = alpha) +

#    geom_linerangeh(aes(xmin = `x10%`, xmax = `x90%`), size = size, alpha = alpha) +
    geom_density_2d(aes(colour=mp)) +

    geom_point(aes(fill = mp),
    shape = 21, size = 4) + scale_shape(solid = FALSE) +
    theme(legend.title = element_blank()) +
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
