`%>%` <- magrittr::`%>%`

ft_kelly <- list(
  purple = "#875692",
  orange = "#F38400",
  light_blue = "#A1CAF1",
  red = "#BE0032",
  buff = "#C2B280",
  green = "#008856",
  purplish_pink = "#E68FAC",
  blue = "#0067A5",
  yellowish_pink = "#F99379",
  violet = "#604E97",
  orange_yellow = "#F6A600",
  purplish_red = "#B3446C",
  reddish_brown = "#882D17",
  yellow_green = "#8DB600",
  yellowish_brown = "#654522",
  reddish_orange = "#E25822",
  olive_green = "#2B3D26",
  yellow = "#F3C300",
  gray = "#848482"
)

ft_pthc <- list(
  blue = "#004488",
  yellow = "#DDAA33",
  red = "#BB5566",
  gray = "#848482"
)

ft_ptm <- list(
  ptm_rose = "#CC6677",
  ptm_indigo = "#332288",
  ptm_sand = "#DDCC77",
  ptm_green = "#117733",
  ptm_cyan = "#88CCEE",
  ptm_wine = "#882255",
  ptm_teal = "#44AA99",
  ptm_olive = "#999933",
  ptm_purple = "#AA4499",
  ptm_gray = "#DDDDDD"
)

ft_ptb <- list(
  ptb_blue = "#4477AA",
  ptb_pink = "#EE6677",
  ptb_green = "#228833",
  ptb_olive = "#CCBB44",
  ptb_light_blue = "#66CCEE",
  ptb_purple = "#AA3377",
  ptb_gray = "#BBBBBB"
)

ft_ptv <- list(
  ptv_orange = "#EE7733",
  ptv_blue = "#0077BB",
  ptv_teal = "#009988",
  ptv_pink = "#EE3377",
  ptv_light_blue = "#33BBEE",
  ptv_red = "#CC3311",
  ptv_gray = "#BBBBBB"
)

ft_palette <- list(
  kelly = `names<-`(sapply(ft_kelly, identity, USE.NAMES = FALSE), NULL),
  pthc = `names<-`(sapply(ft_pthc, identity, USE.NAMES = FALSE), NULL),
  ptm = `names<-`(sapply(ft_ptm, identity, USE.NAMES = FALSE), NULL),
  ptb = `names<-`(sapply(ft_ptb, identity, USE.NAMES = FALSE), NULL),
  ptv = `names<-`(sapply(ft_ptv, identity, USE.NAMES = FALSE), NULL)
)


theme_featuretable <- function(base_size = 14) {
  `%+replace%` <- ggplot2::`%+replace%`

  base_color <- "#333333"
  alpha <- 0.1
  width <- 10
  height <- 6.25

  # TODO would be great to always put the zero line a bit darker.

  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(line = ggplot2::element_line(color = base_color),
                   rect = ggplot2::element_rect(color = base_color),
                   text = ggplot2::element_text(color = base_color),
                   title = ggplot2::element_text(color = base_color),
                   panel.grid = ggplot2::element_line(color = "#e8e8e8"),
                   #panel.grid.minor = element_line(size = rel(0.5)),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(margin = ggplot2::margin(-1, 0, 0, 0),
                                                       color = base_color),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(5, 0, 0, 0),
                                                        color = base_color),

                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, -1, 0, 0),
                                                       color = base_color),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 0),
                                                        color = base_color,
                                                        angle = 90),
                   axis.ticks = ggplot2::element_blank())
}

default_theme <- function() theme_featuretable()

darker_line_color <- "#999999"

# TODO add ... to end of samples and feature names if they're too long
# TODO proportions? options for grouping? add docs about how you do that before calling plot
# TODO doc stuff like this: lee$collapse_features(Family)$collapse_samples(char)$map_samples(function(sample) sample / sum(sample) * 100) %>% plot
# TODO test num_features being bad

plot.FeatureTable <- function(ft,
                              # TODO should also handle null/false
                              num_features = 8,
                              fill = TRUE,
                              # if NULL use kelly
                              palette = "kelly",
                              show_legend = TRUE,
                              legend_title = NULL,
                              plot_title = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              axis.text.x = NULL,
                              ...) {
  args <- list(...)

  num_features_to_show <- ifelse(
    is.null(num_features) || isFALSE(num_features),
    # Show all features.
    ft$num_features(),
    # Show how many the user wants.
    num_features
  )

  # These all have gray at the end.
  if (palette == "high contrast") {
    palette <-  ft_palette$pthc[1:(length(ft_palette$pthc) - 1)]
    palette_gray <- ft_palette$pthc[length(ft_palette$pthc)]
  } else if (palette == "muted") {
    palette <-  ft_palette$ptm[1:(length(ft_palette$ptm) - 1)]
    palette_gray <- ft_palette$ptm[length(ft_palette$ptm)]
  } else if (palette == "bright") {
    palette <-  ft_palette$ptb[1:(length(ft_palette$ptb) - 1)]
    palette_gray <- ft_palette$ptb[length(ft_palette$ptb)]
  } else if (palette == "vibrant") {
    palette <-  ft_palette$ptv[1:(length(ft_palette$ptv) - 1)]
    palette_gray <- ft_palette$ptv[length(ft_palette$ptv)]
  } else { # Kelly
    palette <-  ft_palette$kelly[1:(length(ft_palette$kelly) - 1)]
    palette_gray <- ft_palette$kelly[length(ft_palette$kelly)]
  }

  # Recycle palette if necessary
  if (num_features_to_show > length(palette)) {
    rlang::inform(
      "You want to plot more features than your palette can handle.  I will recycle the palette to handle this."
    )

    palette <- rep(
      palette,
      times = ceiling(num_features_to_show / length(palette))
    )[1:num_features_to_show]
  }

  # TODO technically don't need this step if fill is false.
  plot_data <- as.data.frame(ft$data[, order(colSums(ft$data), decreasing = TRUE)])

  # Need to make an other category.
  if (ft$num_features() > num_features_to_show) {
    plot_data_other <- plot_data[, (num_features_to_show + 1):ncol(plot_data)]
    plot_data <- plot_data[, 1:num_features_to_show]
    # TODO make sure other doesn't already have a name in the plot data
    plot_data$Other <- rowSums(plot_data_other)

    # The last thing will be other, so make it gray.
    legend_colors <- rev(c(palette[1:num_features_to_show], palette_gray))
  } else {
    # There is no other category, so just use all kelly.
    legend_colors <- rev(palette[1:num_features_to_show])
  }

  the_levels <- rev(colnames(plot_data))
  legend_labels <- the_levels

  if (isFALSE(fill)) {
    fill <- NULL
    color <- NULL

    # TODO data
    p <- ggplot2::ggplot(
      # We just need the row sums and nothing else.
      data = data.frame(Sample = rownames(plot_data),
                        Value = rowSums(plot_data)),
      mapping = ggplot2::aes_string(x = "Sample", y = "Value")
    )

    p <- p + ggplot2::geom_bar(stat = "identity")
  } else {
    # Use default
    fill <- "Feature"
    color = "#333333"

    # TODO don't use tidyverse associated stuff.
    p <- plot_data %>%
      as.data.frame %>%
      tibble::rownames_to_column("Sample") %>%
      tidyr::pivot_longer(cols = -Sample, names_to = "Feature", values_to = "Value") %>%
      dplyr::mutate(Feature = factor(Feature, levels = the_levels)) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "Sample", y = "Value", fill = fill))

    p <- p + ggplot2::geom_bar(stat = "identity", color = color)
  }

  p <- p +
    theme_featuretable()

  if (!is.null(axis.text.x)) {
    print("beep")
    # TODO actually check what's passed in.....
    if (inherits(ggplot2::element_text(), "element_text")) {
      print("bop")
      p <- p + ggplot2::theme(axis.text.x = axis.text.x)
    } else {
      rlang::abort(
        "You passed axis.text.x, but the argument was not an instance of 'element_text'",
        class = "TESTME"
      )
    }
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }


  if (is.null(show_legend) || isFALSE(show_legend)) {
    p <- p + ggplot2::theme(legend.position = "none")
  }


  if (is.null(legend_title)) {
    p <- p + ggplot2::scale_fill_manual(values = legend_colors, labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_fill_manual(values = legend_colors, labels = legend_labels,
                               name = legend_title)
  }

  if (!is.null(xlab)) {
    p <- p + ggplot2::xlab(xlab)
  }

  if (!is.null(ylab)) {
    p <- p + ggplot2::ylab(ylab)
  }

  if (!is.null(plot_title)) {
    p <- p + ggplot2::ggtitle(plot_title)
  }

  p
}