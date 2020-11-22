figure_new_cases_change <- function(csv, county, date_limits = c(NA, "2021-03-01")) {
  this_county <- county
  json <- str_c(tools::file_path_sans_ext(csv), ".json")

  df <-
    read_csv(csv) %>%
    filter(county == this_county, !is.na(count_new_loess_diff))

  df %>%
    ggplot(aes(x = date, y = count_new_loess_diff, color = count_new_loess_diff)) +
    labs(
      y = "Daily change",
      color = NULL,
      caption = str_c(
        "**Note:** daily change in loess estimate of new cases",
        str_c('Data from _data.ca.gov_ (', file.mtime(json), ")"),
        sep = "<br>"
      )
    ) +
    geom_vline(xintercept = as.Date("2021-01-01", tz = ""), linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_line() +
    scale_x_date(
      date_breaks = "2 month",
      date_labels = "%b",
      date_minor_breaks = "1 month",
      limits = as.Date(date_limits, tz = "")
    ) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    scale_color_steps2(
      n.breaks = 5,
      nice.breaks = FALSE, # to always respect n.breaks for manual labeling
      labels = c("← Improving", "", "", "", "Worsening →"),
      limits = c(-1, 1) * max(df$count_new_loess_diff),
      oob = scales::squish,
      low = "green", mid = "black", high = "#fe0099", midpoint = 0
    ) +
    coord_cartesian(ylim = c(-1, 1) * max(df$count_new_loess_diff)) +
    theme(
      legend.key.height = unit(3, "points"),
      legend.key.width  = unit(25, "points"),
      legend.text = element_text(size = 6),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.box.spacing = unit(1, "points"),
      plot.caption = ggtext::element_markdown(hjust = 0, size = 7),
      aspect.ratio = 0.5,
      axis.title.x = element_blank(),
      axis.text.x = element_text(hjust = 0)
    )
}
