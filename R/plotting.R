#' Plot factor vs factor by significance code (ggplot2)
#'
#' @param df Dataframe
#' @param x Unquoted x column
#' @param y Unquoted y column
#' @param signif Unquoted significance column
#' @param pal Colours for codes
#' @examples
#' plot_signif(ggplot2::mpg, manufacturer, class, drv,
#' pal = c("4" = "red", "f" = "blue", "r" = "orange"))
#'
#' @export
plot_signif <- function(df, x, y, signif,
                        pal = c("S" = "red",
                                "NS" = "green",
                                "not-tested" = "grey50")){
  # some rlang stuff
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  signif <- rlang::enquo(signif)

  # lengths of rows and columns
  n_x <- length(unique(dplyr::pull(df, !!x)))
  n_y <- length(unique(dplyr::pull(df, !!y)))

  ggplot2::ggplot(df) +
    ggplot2::aes(x = !!x, y = !!y, colour = !!signif) +
    ggplot2::geom_point(size = 5) +
    ggplot2::geom_hline(
      yintercept = seq(1, n_y + 1) - 0.5,
      colour = "grey50"
    ) +
    ggplot2::geom_vline(
      xintercept = seq(1, n_x + 1) - 0.5,
      colour = "grey50"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.text.y = ggplot2::element_text(),
      legend.position = "none",
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::labs(
      x = "",
      y = ""
    ) +
    ggplot2::coord_equal()
}

#' Plot factor vs factor by significance code (base)
#'
#' @param df Dataframe
#' @param x Unquoted x column
#' @param y Unquoted y column
#' @param signif Unquoted significance column
#' @param pal Colours for codes
#' @param margins vector of x and y margins for text
#' @param ... other arguments for plotting
#'
#' @examples
#' plot_signif_base(ggplot2::mpg, manufacturer, class, drv,
#' pal = c("4" = "red", "f" = "blue", "r" = "orange"))
#'
#' @export
plot_signif_base <- function(df, x, y, signif,
                             pal = c("S" = "red",
                                      "NS" = "green",
                                      "not-tested" = "grey50"),
                             margins = c(3, 3), ...) {

  # some rlang stuff
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  signif <- rlang::enquo(signif)

  # extract vectors from df
  x_dat <- dplyr::pull(df, !!x)
  y_dat <- dplyr::pull(df, !!y)
  signif_dat <- dplyr::pull(df, !!signif)

  # lengths of rows/cols and colours
  n_x <- length(unique(x_dat))
  n_y <- length(unique(y_dat))
  cols <- unname(pal[signif_dat])

  # void plotting frame setup
  plot(as.numeric(factor(x_dat)),
       as.numeric(factor(y_dat)),
       type = "n", axes = FALSE,
       xlab = "", ylab = "",
       xlim = c(-margins[[1]], n_x + 0.5),
       ylim = c(-margins[[2]], n_y + 0.5),
       xaxs = "i", yaxs = "i", ...)

  # the points
  points(as.numeric(factor(x_dat)), as.numeric(factor(y_dat)),
         pch = 20, col = cols, cex= 3, ...)

  # axis text
  x_labels <- unique(data.frame(x_dat, as.numeric(factor(x_dat))))
  y_labels <- unique(data.frame(y_dat, as.numeric(factor(y_dat))))
  text(x_labels[[2]], -1, x_labels[[1]], srt = 90, adj = 1, ...)
  text(-1, y_labels[[2]], y_labels[[1]], adj = 1, ...)

  # the grid
  abline(v = 0:n_x + 0.5)
  abline(h = 0:n_y + 0.5)

  box()
}

# testing
#
# plot_signif(ggplot2::mpg, manufacturer, class, drv,
#   pal = c("4" = "red", "f" = "blue", "r" = "orange"))
#
# plot_signif_base(ggplot2::mpg, manufacturer, class, drv,
#   pal = c("4" = "red", "f" = "blue", "r" = "orange"))
