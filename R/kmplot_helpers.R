add_risktable2 <- function(gg,
                           times = NULL,
                           statlist = "n.risk",
                           label = NULL,
                           group = "strata",
                           collapse = FALSE,
                           rowgutter = .16,
                           risk_font_size = 6.0,
                           risk_label_font_size = 12,
                           ...) {
  
  # User input validation ---------------------------------------------------
  
  if (!(is.numeric(rowgutter) == TRUE) || (rowgutter < 0) || (rowgutter > 1)) {
    stop("rowgutter should be a numeric value in range [0, 1]")
  }
  
  # Obtain the relevant table --------------------------------------------------
  tidy_object <- gg$data
  estimate_object <- visR:::.extract_estimate_object(gg)
  
  ggbld <- ggplot2::ggplot_build(gg)
  
  graphtimes <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  
  if (is.null(times)) times <- graphtimes
  
  final <-
    visR:::get_risktable(estimate_object,
                  times = times,
                  statlist = statlist,
                  label = label,
                  group = group,
                  collapse = collapse
    )
  
  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title
  
  attr(final, "time_ticks") <- NULL
  attr(final, "statlist") <- NULL
  attr(final, "title") <- NULL
  
  # Plot requested tables below using list approach with map function -------
  
  tbls <-
    base::Map(function(statlist, title = NA) {
      ggrisk <- ggplot2::ggplot(
        final,
        ggplot2::aes(
          x = time,
          y = stats::reorder(y_values, dplyr::desc(y_values)),
          label = format(get(statlist), nsmall = 0) # = value columns
        )
      ) +
        ggplot2::geom_text(size = risk_font_size, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(
          breaks = graphtimes,
          limits = c(min(graphtimes), max(graphtimes))
        ) +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(
            size = 8,
            vjust = 1,
            hjust = 1
          ),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = risk_label_font_size, colour = "black", face = "plain"),
          plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
          plot.title = ggplot2::element_text(hjust = 0, vjust = 0),
          legend.position = "none"
        ) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL)
      
      if (!is.na(title) && !is.null(title)) {
        ggrisk <- ggrisk +
          ggplot2::ggtitle(title) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
      }
      
      return(ggrisk)
    },
    statlist = as.list(statlist),
    title = as.list(title)
    )
  
  # Align plot and table by adjusting width ---------------------------------
  
  gglist <- list(gg) %>%
    base::append(tbls)
  
  ggA <- gglist %>%
    visR:::align_plots()
  
  # Create plot and add class -----------------------------------------------
  
  ## cowplot allows to align according to an axis (+left) and change the heigth
  ggB <- cowplot::plot_grid(
    plotlist = ggA,
    align = "none",
    nrow = length(ggA),
    rel_heights = c(1 - (rowgutter * (length(ggA) - 1)), rep(rowgutter, length(ggA) - 1))
  )
  
  class(ggB) <- c(class(ggB), intersect(class(gg), c("ggsurvfit", "ggtidycmprsk")))
  
  # Add individual components -----------------------------------------------
  
  components <- append(list(gg), tbls)
  names(components) <- c("visR_plot", title)
  ggB[["components"]] <- components
  
  return(ggB)
}
