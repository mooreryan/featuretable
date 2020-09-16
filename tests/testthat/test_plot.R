context("Basic bar plots")

if (isTRUE(requireNamespace("ggplot2", quietly = TRUE)) &&
    isTRUE(requireNamespace("vdiffr", quietly = TRUE)) &&
    isTRUE(requireNamespace("DivNet", quietly = TRUE))) {


  lee <- featuretable:::ft_from.phyloseq(DivNet::Lee)
  lfam <- lee$collapse_features(Family)

  vdiffr::expect_doppelganger("All defaults", plot(lfam))

  for (pal in c("kelly", "high contrast", "muted", "vibrant", "bright")) {
    vdiffr::expect_doppelganger(
      paste(pal, "palette"),
      plot(lfam, palette = pal, plot_title = pal)
    )
  }

  title <- "custom fill with ggplot functions"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title) +
      ggplot2::scale_fill_discrete()
  )

  title <- "custom fill with ggplot brewer"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, num_features = 7) +
      ggplot2::scale_fill_brewer(type = "qual")
  )

  title <- "custom num_features"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, num_features = 5)
  )

  title <- "no fill with fill = FALSE"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, fill = FALSE) # messed up!
  )

  title <- "default fill with fill = NULL"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, fill = NULL)
  )

  title <- "default fill with fill = TRUE"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, fill = TRUE)
  )

  title <- "default fill with fill = 'anything else'"
  vdiffr::expect_doppelganger(
    title,
    plot(lfam, plot_title = title, fill = "anything else")
  )
}

if (isTRUE(requireNamespace("ggplot2", quietly = TRUE)) &&
    isTRUE(requireNamespace("vdiffr", quietly = TRUE))) {

  ft <- otu_feature_table()

  # DOUBLE CHECK
  title <- "num_features > actual number of features"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, num_features = 10)
  )

  # DOUBLE CHECK
  title <- "manually pass axis.text.x as a parameter"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, axis.text.x = ggplot2::element_text(angle = 0))
  )

  title <- "hide legend with show_legend = NULL"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, show_legend = NULL)
  )

  title <- "hide legend with show_legend = FALSE"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, show_legend = FALSE)
  )

  title <- "custom legend title"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, legend_title = "CUSTOM LEGEND TITLE")
  )

  title <- "custom xlab"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, xlab = "CUSTOM X LABEL")
  )

  title <- "custom ylab"
  vdiffr::expect_doppelganger(
    title,
    plot(ft, plot_title = title, ylab = "CUSTOM Y LABEL")
  )
}

if (isTRUE(requireNamespace("ggplot2", quietly = TRUE))) {
  test_that("axis.text.x raises if it isn't an element_text", {
    ft <- otu_feature_table()

    expect_error(plot(ft, axis.text.x = "apple"), class = Error$ArgumentError)
    expect_error(plot(ft, axis.text.x = 1:10), class = Error$ArgumentError)
  })

  test_that("num_features must be at least 1", {
    ft <- otu_feature_table()

    expect_error(plot(ft, num_features = 0), class = Error$ArgumentError)
    expect_error(plot(ft, num_features = "pie"), class = Error$ArgumentError)
  })

  test_that("all reasonable options for num_features work", {
    ft <- otu_feature_table()

    for (i in 1:15) {
      expect_error(plot(ft, num_features = i), NA)
    }
  })

  test_that("num_features one fewer than actual num_features gives a message", {
    ft <- otu_feature_table()

    expect_message(plot(ft, num_features = 4),
                   regex = "num_features")
  })

  test_that("other_feature_name will be 'Other' by default", {
    ft <- otu_feature_table()

    p <- plot(ft, num_features = 3)
    expect_true("Other" %in% p$data$Feature)
  })

  test_that("other_feature_name will be 'Other' if arg is bad", {
    ft <- otu_feature_table()

    p <- plot(ft, num_features = 3, other_feature_name = 234)
    expect_true("Other" %in% p$data$Feature)

    p <- plot(ft, num_features = 3, other_feature_name = 23.4)
    expect_true("Other" %in% p$data$Feature)

    p <- plot(ft, num_features = 3, other_feature_name = NULL)
    expect_true("Other" %in% p$data$Feature)

    p <- plot(ft, num_features = 3, other_feature_name = FALSE)
    expect_true("Other" %in% p$data$Feature)
  })

  test_that("other_feature_name will be what user sets", {
    ft <- otu_feature_table()

    p <- plot(ft, num_features = 3, other_feature_name = "silly thing")
    p$data$Feature
    expect_true("silly thing" %in% p$data$Feature)
  })

  test_that("plot will raise if other_feature_name is already a feature name and other cat. is needed", {
    ft <- otu_feature_table()

    # Feature_1 will be dropped
    expect_error(plot(ft, num_features = 3, other_feature_name = "Feature_1"),
                 class = Error$ArgumentError)

    # Feature_5 will be kept
    expect_error(plot(ft, num_features = 3, other_feature_name = "Feature_5"),
                 class = Error$ArgumentError)
  })

  test_that("plot will NOT raise if other_feature_name is already a feature name and other cat. is NOT needed", {
    ft <- otu_feature_table()

    expect_error(plot(ft, num_features = 10, other_feature_name = "Feature_1"), NA)
  })
}
