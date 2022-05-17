#' Plot posterior estimates results from diffsig_fit
#'
#' @export
#' @param fit stan fit object from diffsig_stan
#' @param pars parameters to plot
#' @param M parameters to plot
#' @param riskfactors labels for each risk factor including intercept
#' @param rowlabels labels for betas in diffsig plot
#' @param ... Arguments passed to ggplot
#' @return Diffsig plot
#'
diffsig_plot <- function(fit, pars, rowlabels, rowgroup=NULL, est_color=NULL, colors=NULL) {
  require(rstan, quietly = T)
  # require(ggplotify)
  require(viridis, quietly = T)

  statmat <- summary(fit)$summary[,c("mean","2.5%", "25%", "50%", "75%", "97.5%")]
  statmat <- as.data.frame(statmat[pars,])
  statmat$group <- factor(rowgroup, levels=unique(rowgroup))

  y <- as.numeric(seq(length(pars), 1, by = -1))
  xlim.use <- c(min(statmat[, 2L]), max(statmat[, 6L]))
  xlim.use <- xlim.use + diff(xlim.use) * c(-0.05, 0.05)
  xy.df <- data.frame(params = rownames(statmat), y, statmat)
  colnames(xy.df) <- c("params", "y", "mean", "ll", "l", "m", "h", "hh")

  p.base <- ggplot2::ggplot(xy.df)
  p.name <- ggplot2::scale_y_continuous(breaks = y, labels = rowlabels,
                                        limits = c(0.8, max(y) + 0.2))
  p.all <- p.base + ggplot2::xlim(xlim.use) + p.name + theme_bw()

  p.ci <- ggplot2::geom_segment(mapping = ggplot2::aes_string(x = "ll", xend = "hh", y = "y", yend = "y"))
  p.all <- p.all + p.ci

  if(is.null(est_color)) {
    est_color <- "#FFC20A"
  }

  if(is.null(rowgroup)) {
    if(length(colors)!=1) {
      stop("If no rowgroup is provided, only one color should be specified or use default.")
    } if(is.null(colors)) {
      color_by <- "#0C7BDC"
    }

    p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "y", yend = "y"), color = color_by, size = 2)
    p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), shape = 19, color=est_color, size = 3)
    p.all <- p.all + p.ci.2 +
      xlab("beta") +
      theme(axis.title.y=element_blank())
    }
  } else {
    color_by <- "statmat$group"

    p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "y", yend = "y", color = color_by), size = 2)
    p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), shape = 19, color=est_color, size = 3)

    if(is.null(colors)) {
      p.all <- p.all +
        p.ci.2 +
        scale_color_viridis(discrete = TRUE, begin = 0.7, end=0.4, option="D", name="Risk Factors") +
        xlab(expression(beta)) +
        theme(axis.title.y=element_blank())
    } else {
      if(length(colors)!=length(unique(rowgroup))) {
        stop("Length of colors should match the number of unique groups in rowgroup")
      }

      p.all <- p.all +
        p.ci.2 +
        scale_color_manual(name="Risk Factors",values=colors) +
        xlab("beta") +
        theme(axis.title.y=element_blank())
    }
  }

  p <- p.all + geom_vline(xintercept=0, linetype="dashed",color="darkgrey") + p.point

  ggbld <- ggplot_build(p)
  p <- p +
    theme(legend.position="bottom",
          # legend.direction="horizontal",
          # legend.background = element_rect(size=0.5, linetype="solid", colour ="black"),
          legend.text = element_text(size=10, face = "plain"),
          # panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          # legend.title = element_blank(),
          axis.title.y=element_blank()) +
    # scale_x_continuous(breaks=ggbld$layout$panel_params[[1]]$x.sec$breaks,
                       # labels=c("less common",ggbld$layout$panel_params[[1]]$x.sec$breaks[-c(1, length(ggbld$layout$panel_params[[1]]$x.sec$breaks))], "more common")) +
   # scale_x_continuous(breaks = c(layer_scales(p)$x$range$range[1],ggbld$layout$panel_params[[1]]$x.sec$breaks,layer_scales(p)$x$range$range[2]),
                       # labels = c("Less Common",ggbld$layout$panel_params[[1]]$x.sec$breaks,"More Common")) +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "both")),
          axis.title.x = element_text(angle = 0),
          axis.text.x = element_text(size="10"),
          panel.grid.major.x = element_line(),
          plot.margin = margin(t = 5,  # Top margin
                               r = 30,  # Right margin
                               b = 5,  # Bottom margin
                               l = 5))
  p
  # pp = ggplot_build(p)
  # ## re-label y-axis with names
  # pp$layout$panel_params[[1]]$y.sec$scale$labels <- rowlabels
  # ppp <- ggplot_gtable(pp)
  # p_fin <- as.ggplot(ppp)

  return(p)
}







