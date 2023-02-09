#' Plot posterior estimates results from diffsig_fit
#'
#' @param fit stan fit object from diffsig_stan
#' @param pars vector of row numbers to plot from `fit`
#' @param signature_labels vector of signature labels
#' @param riskfactor_labels risk factor group labels
#' @param est_color point estimate color indicator, default set to yellow
#' @param colors vector of colors for 80% credible interval, recommended to have same color for same risk factor group (colors=NULL generate default colors up to 7 risk factors)
#' @param ncol number of columns that indicates how to align the plots e.g. ncol=1: set plots vertical; ncol=(# of risk factors) set plots horizontal
#' @param outlist default FALSE indicates to combine plots for all risk factors into one. outlist=T indicates to output as a list of plots
#' @param ci_level numerical value for credible interval percentage (default 80)
#' @import ggplot2
#' @import cowplot
#' @import grid
#' @return Diffsig plot
#'
diffsig_plot <- function(fit, pars, signature_labels, riskfactor_labels,
                         est_color=NULL, colors=NULL,ncol=1,outlist=FALSE,ci_level=80) {

  ## Errors and warnings
  if(length(pars)!=length(signature_labels)*length(riskfactor_labels)) {
    stop("Number of parameters incorrect. length(pars) should equal to length(signature_labels)*length(riskfactor_labels).")
  }
  if(!is.null(est_color)) {
    if(length(est_color)!=1) {
      stop("Only 1 color can be specified for the estimation point est_color")
    }

  }
  if(!is.null(colors)) {
    if(length(unique(riskfactor_labels))!=length(colors)) {
      warning("Number of colors does not match the number of unique groups from riskfactor_labels")
    }
  }

  ## Setup data
  if(is.null(est_color)) {
    est_color <- "#FFC20A"
  }

  ci_low = (100-ci_level)/2/100
  ci_up = 1-ci_low

  statmat <- rstan::summary(fit, probs=c(ci_low,0.25,0.5,0.75,ci_up))$summary[,c("mean","10%", "25%", "50%", "75%", "90%")]
  statmat <- statmat[pars,]
  number_signatures <- length(signature_labels)
  number_riskfactors <- length(riskfactor_labels)
  statlist <- list()
  for (i in 1:number_riskfactors) {
    statlist[[i]] <- statmat[(number_signatures*i-(number_signatures-1)):(number_signatures*i),]
  }

  y <- as.numeric(seq(number_signatures, 1, by = -1))
  xlim.use <- c(min(statmat[, 2L]), max(statmat[, 6L]))
  xlim.use <- xlim.use + diff(xlim.use) * c(-0.05, 0.05)

  p.list <- list()
  for (i in 1:number_riskfactors) {
    xy.df <- data.frame(params = rownames(statlist[[i]]), y, statlist[[i]])
    xy.df$group <- rep(riskfactor_labels[[i]], times=number_signatures)
    colnames(xy.df) <- c("params", "y", "mean", "ll", "l", "m", "h", "hh","group")

    p.base <- ggplot2::ggplot(xy.df)
    p.name <- ggplot2::scale_y_continuous(breaks = y,
                                          labels = signature_labels,
                                          limits = c(0.8, y + 0.2))

    p.all <- p.base + ggplot2::xlim(xlim.use) + p.name + geom_vline(xintercept=0, linetype="dashed",color="darkgrey") +
      theme_bw()

    p.ci <- ggplot2::geom_segment(mapping = ggplot2::aes_string(x = "ll", xend = "hh", y = "y", yend = "y"))
    p.list[[i]] <- p.all + p.ci +
      theme(panel.grid.major.y = element_line(colour="white", size=0.1),
            panel.grid.minor.y = element_line(colour='grey', linetype='dashed', size=0.2))
  }

  if(!is.null(colors)) {
    if(length(unique(riskfactor_labels))!=length(colors)) {
      warning("Number of colors does not match the number of unique groups from riskfactor_labels")
    }
  }

  ## Colors not specified
  if(is.null(colors)) {
    color_by <- c("#929292","#2484F6","#00AD35","#E84A35","#CA001C","#C50077","#7C00FF")
    for(i in 1:number_riskfactors) {
      p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "y", yend = "y"), color = color_by[i], size = 3)
      p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), shape = 19, color=color_by[i], size = 3, show.legend = F)
      p.point2 <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), shape = 19, color=est_color, size = 2)

      p.list[[i]] <- p.list[[i]] +
        p.ci.2 +
        p.point +
        p.point2 +
        xlab(expression(beta)) +
        labs(caption=riskfactor_labels[i]) +
        theme(
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(angle = 0, hjust=1,margin = margin(t = -2, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "both"), color="grey27"),
          axis.text.x = element_text(size="10"),
          panel.grid.major.x = element_line(),
          plot.margin = margin(t = 5,  # Top margin
                               r = 10,  # Right margin
                               b = 5,  # Bottom margin
                               l = 5),
          panel.border = element_blank(),
          legend.margin = margin(t=-15),
          plot.caption=element_text(size=12, hjust=0.5, vjust=2, margin=margin(0,0,0,0))) +
        guides(colour = guide_legend(ncol = 3))
    }
  } else {
    if(length(colors)!=length(riskfactor_labels)) {
      stop("Length of colors should match the number of unique groups in riskfactor_labels")
    }

    for (i in 1:number_riskfactors) {
      p.ci.2 <- ggplot2::geom_segment(ggplot2::aes(x = l, xend = h, y = y, yend = y), color = colors[i], size = 3)
      p.point <- ggplot2::geom_point(ggplot2::aes(x = m, y = y), color=colors[i], shape = 19, size = 3, show.legend = F)
      p.point2 <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), color = est_color, shape = 19, size = 2)


      p.list[[i]] <- p.list[[i]] +
        p.ci.2 +
        p.point +
        p.point2 +
        xlab(expression(beta)) +
        labs(caption=riskfactor_labels[i]) +
        theme(
              panel.grid.minor.x = element_blank(),
              axis.title.x = element_text(angle = 0, hjust=1,margin = margin(t = -2, r = 0, b = 0, l = 0)),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                             ends = "both"), color="grey27"),
              axis.text.x = element_text(size="10"),
              panel.grid.major.x = element_line(),
              plot.margin = margin(t = 5,  # Top margin
                                   r = 10,  # Right margin
                                   b = 5,  # Bottom margin
                                   l = 5),
              panel.border = element_blank(),
              legend.position = "none",
              plot.caption=element_text(size=12, hjust=0.5, vjust=2, margin=margin(0,0,0,0)))
    }
  }

  if (outlist==TRUE) {
    (p = p.list)
  } else {
    (p=cowplot::plot_grid(plotlist=p.list, ncol = ncol))
  }

  return(p)
}



