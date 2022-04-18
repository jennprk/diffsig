#' Plot posterior estimates results from diffsig_fit
#'
#' @export
#' @param fit stan fit object from diffsig_stan
#' @param pars parameters to plot
#' @param rowlabels labels for betas in diffsig plot
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param ... Arguments passed to ggplot
#' @return Diffsig plot
#'
diffsig_plot <- function(fit, pars="beta", rowlabels=NULL, ...) {
  require(rstan)
  p <- stan_plot(fit=fit,pars=pars)
  p <- p  +
    scale_x_continuous(breaks=c(layer_scales(p)$x$range$range[1], 0, layer_scales(p)$x$range$range[2]),
                       labels = c("Less Common","0","More Common")) +
    ## Add arrow and leave only 0 in x-axis
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "both")),
          axis.title.x = element_text(angle = 0),
          axis.text.x = element_text(size="3cm"),
          plot.margin = margin(t = 5,  # Top margin
                               r = 30,  # Right margin
                               b = 5,  # Bottom margin
                               l = 5)) +
    labs(x = NULL)

  ## re-label y-axis with names
  pp$layout$panel_params[[1]]$y.sec$scale$labels <- rowlabels
  ppp <- ggplot_gtable(pp)
  p_fin <- plot(ppp) + ...

  return(p_fin)
}






