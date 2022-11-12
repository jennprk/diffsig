#' Plot posterior estimates results from diffsig_fit
#'
#' @export
#' @param fit stan fit object from diffsig_stan
#' @param pars vector of row numbers to plot from `fit`
#' @param ci_level numerical value for credible interval percentage (default 80)
#' @param signature_labels vector of signature labels
#' @param riskfactor_labels risk factor group labels
#' @param est_color point estimate color indicator, default set to yellow
#' @param colors vector of colors for 80% credible interval, recommended to have same color for same risk factor group (colors=NULL generate default colors up to 7 risk factors)
#' @param ... Arguments passed to ggplot
#' @return Diffsig plot
#'
diffsig_plot <- function(fit, pars, ci_level=80, signature_labels, riskfactor_labels, est_color=NULL, colors=NULL) {
  require(rstan, quietly = T)
  require(viridis, quietly = T)

  if(length(pars)!=length(signature_labels)*length(riskfactor_labels)) {
    stop("Number of parameters incorrect. length(pars) should equal to length(signature_labels)*length(riskfactor_labels).")
  }

  if(!is.null(colors)) {
    if(length(unique(riskfactor_labels))!=length(colors)) {
      warning("Number of colors does not match the number of unique groups from riskfactor_labels")
    }
  }

  if(!is.null(est_color)) {
    if(length(est_color)!=1) {
      stop("Only 1 color can be specified for the estimation point est_color")
    }
  }

  ci_low = (100-ci_level)/2/100
  ci_up = 1-ci_low

  statmat <- summary(fit, probs=c(ci_low,0.25,0.5,0.75,ci_up))$summary[,c("mean","10%", "25%", "50%", "75%", "90%")]
  # statmat <- statmat[pars,]
  statmat <- as.data.frame(statmat[names(fit)[pars],])

  y <- as.numeric(seq(length(pars), 1, by = -1))
  xlim.use <- c(min(statmat[, 2L]), max(statmat[, 6L]))
  xlim.use <- xlim.use + diff(xlim.use) * c(-0.05, 0.05)
  xy.df <- data.frame(params = rownames(statmat), y, statmat)
  xy.df$group <- factor(rep(riskfactor_labels, times=signatures), levels=riskfactor_labels)
  colnames(xy.df) <- c("params", "y", "mean", "ll", "l", "m", "h", "hh","group")

  xy.df
  p.base <- ggplot2::ggplot(xy.df)

  breakpoints <- seq(from=max(y)-0.5, to=0, by=-2)
  breakpoints_minor <- seq(from=max(y)-1.5, to=0, by=-2)
  p.name <- ggplot2::scale_y_continuous(breaks = breakpoints,
                                        minor_breaks = breakpoints_minor,
                                        labels = signature_labels,
                                        limits = c(0.8, y + 0.2))
  p.all <- p.base + ggplot2::xlim(xlim.use) + p.name + geom_vline(xintercept=0, linetype="dashed",color="darkgrey") +
    theme_bw()

  p.ci <- ggplot2::geom_segment(mapping = ggplot2::aes_string(x = "ll", xend = "hh", y = "y", yend = "y"))
  p.all <- p.all + p.ci +
    theme(panel.grid.major.y = element_line(colour="white", size=0.1),
          panel.grid.minor.y = element_line(colour='grey', linetype='dashed', size=0.2))

  if(is.null(est_color)) {
    est_color <- "#FFC20A"
  }

  if(is.null(riskfactor_labels)) {
    if(length(colors)!=1) {
      stop("If no riskfactor_labels is provided, only one color should be specified or use default.")
    }

    if(is.null(colors)) {
      color_by <- "#0C7BDC"
    }

    p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "ind", yend = "ind"), color = color_by, size = 3)
    p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "ind"), shape = 19, color=color_by, size = 3, show.legend = F)
    p.point2 <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "ind"), shape = 19, color=est_color, size = 2)

    p.all <- p.all + p.ci.2 +
      xlab(expression(beta)) +
      theme(axis.title.y=element_blank())
  } else {
    # rf_labels <- rep(riskfactor_labels,each=signatures)
    color_by <- "group"

    p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "y", yend = "y", color = 'group'), size = 3)
    p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y", color=color_by), shape = 19, size = 3, show.legend = F)
    p.point2 <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), color = est_color, shape = 19, size = 2)

    if(is.null(colors)) {
      null_colors = c("#929292","#2484F6","#00AD35","#E84A35","#CA001C","#C50077","#7C00FF")
      p.all <- p.all +
        p.ci.2 +
        scale_color_manual(values=null_colors[1:length(riskfactor_labels)],name="Risk Factors") +
        xlab(expression(beta)) +
        theme(axis.title.y=element_blank())
    } else {
      if(length(colors)!=length(riskfactor_labels)*signatures) {
        stop("Length of colors should match the number of unique groups in riskfactor_labels")
      }

      p.all <- p.all +
        p.ci.2 +
        scale_color_manual(name="Risk Factors",values=colors) +
        xlab(expression(beta)) +
        theme(axis.title.y=element_blank())
    }
  }

 p <- p.all + p.point +  p.point2

 (p <- p +
    theme(legend.position="bottom",
          # panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(angle = 0, hjust=1,margin = margin(t = -2, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "both"), color="grey27"),
          axis.text.x = element_text(size="10"),
          panel.grid.major.x = element_line(),
          plot.margin = margin(t = 5,  # Top margin
                               r = 30,  # Right margin
                               b = 5,  # Bottom margin
                               l = 5),
          panel.border = element_blank(),
          legend.margin = margin(t=-15))+
      guides(colour = guide_legend(ncol = 3))
    )

  # pp = ggplot_build(p)
  # ## re-label y-axis with names
  # pp$layout$panel_params[[1]]$y.sec$scale$labels <- signature_labels
  # ppp <- ggplot_gtable(pp)
  # p_fin <- as.ggplot(ppp)

  return(p)
}


# setwd("/Users/jieun/Downloads/Results2")
ggsave("tcga_her2_updated.png",width=1200,height=1000,unit="px", dpi = 320)
# png(filename="tcga_her2_updated.png",width=400,height=400,units = "px")
# diffsig_plot(fit,pars,ci_level, signature_labels,riskfactor_labels,NULL,NULL)
# dev.off()


