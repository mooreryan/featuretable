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